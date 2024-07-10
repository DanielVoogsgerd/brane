import os
import glob
import re
import shutil

import contextlib
import os

RESULT_DIR = "result"
INTERMEDIATE_DIR = "tmp"

BIN_FILE_PATTERN = "\.\/(?P<OS>\w+)-(?P<arch>[\w_]+)-artifacts/(?P<basename>(?:lib)?brane[a-z]*[-_]?[a-z]*)?(?P<extension>\.(?:exe))?$"
LIB_FILE_PATTERN = "\.\/(?P<OS>\w+)-(?P<arch>[\w_]+)-artifacts/(?P<prefix>lib)?(?P<basename>brane[a-z]*[-_]?[a-z]*)?(?P<extension>\.(?:dll|so|dylib))$"

INSTANCE_ARCHIVE_PATTERN = "(?P<instance_name>[a-zA-Z_-]*)instance(?:-(?P<OS>\w+))?-(?P<arch>[\w_]+)$"

bin_matcher = re.compile(BIN_FILE_PATTERN)
lib_matcher = re.compile(LIB_FILE_PATTERN)
instance_archive_matcher = re.compile(INSTANCE_ARCHIVE_PATTERN)

def ensure_dir(dir):
    if not os.path.exists(dir):
        os.mkdir(dir)

def ensure_clean_dir(dir):
    if os.path.exists(dir):
        shutil.rmtree(dir)

    os.mkdir(dir)


@contextlib.contextmanager
def pushd(new_dir):
    previous_dir = os.getcwd()
    os.chdir(new_dir)
    try:
        yield
    finally:
        os.chdir(previous_dir)

import tarfile

def create_tar_archive(output_filename, files_to_archive):
    with tarfile.open(output_filename, "w:gz") as tar:
        for file in files_to_archive:
            tar.add(file, arcname=file)

def main():
    ensure_dir(RESULT_DIR)
    ensure_clean_dir(INTERMEDIATE_DIR)

    for file in glob.glob('./**/*'):
        match = bin_matcher.match(file)
        if match:
            # print(match.group("OS"), match.group("arch"), match.group("extension"))
            filename = f"{match.group('basename')}-{match.group('OS')}-{match.group('arch')}{match.group('extension') or ''}"
            shutil.copy(file, os.path.join(RESULT_DIR, filename))

        match = lib_matcher.match(file)
        if match:
            print(match)
            dirname = f"lib{match.group('basename')}-{match.group('OS')}-{match.group('arch')}"
            filename = f"{match.group('prefix') or ''}{match.group('basename')}{match.group('extension') or ''}"

            dir_path = os.path.join(INTERMEDIATE_DIR, dirname)

            ensure_dir(dir_path)

            shutil.copy(file, os.path.join(dir_path, filename))

    with pushd(INTERMEDIATE_DIR):
        # Create archives
        for dir in (d for d in os.listdir(".") if os.path.isdir(d)):
            print(dir)
            archive_name = f"{dir}.tar.gz"
            create_tar_archive(archive_name, (os.path.join(dir, f) for f in os.listdir(dir)))

    for archive_name in (f for f in os.listdir(INTERMEDIATE_DIR) if f.endswith(".tar.gz")):
        os.rename(os.path.join(INTERMEDIATE_DIR, archive_name), os.path.join(RESULT_DIR, archive_name))

    for instance_dir in os.listdir("."):
        if match := instance_archive_matcher.match(instance_dir):
            print(match)
            dirname = f"{match.group('instance_name') or ''}instance-{match.group('arch')}"
            archive_name = f"{dirname}.tar.gz"
            create_tar_archive(os.path.join(RESULT_DIR, archive_name), (os.path.join(instance_dir, f) for f in os.listdir(dirname)))
            print(f"Creating archive: {archive_name}")

if __name__ == "__main__":
    main()

