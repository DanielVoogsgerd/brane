use std::path::{Path, PathBuf};

use anyhow::{Context, bail};
use tar::Builder;
use tokio::fs::File;
use tokio::io::BufReader;

use crate::cli::xtask::PackageKind;

pub(crate) async fn create_package(kind: PackageKind) -> anyhow::Result<()> {
    let os = std::env::consts::OS;
    let arch = std::env::consts::ARCH;
    let lib_suffix = std::env::consts::DLL_SUFFIX;

    let pkgs = match os {
        "linux" => vec!["brane", "branec", "branectl", "branelet"],
        "macos" => vec!["brane", "branec"],
        "windows" => vec!["brane"],
        _ => bail!("Unsupported OS: {os}"),
    };

    let mapping: Vec<_> = match kind {
        PackageKind::Release => pkgs.iter().map(|&src| (src, format_binary_name(src))).collect(),
    };

    let src_dir = PathBuf::from("target/release");
    let mut dst_dir = PathBuf::from("target/package");

    match kind {
        PackageKind::Release => dst_dir.push("release"),
    }

    if !dst_dir.exists() {
        std::fs::create_dir_all(&dst_dir).context("Could not create all dirs leading up to destination dir")?
    }

    for (src, dst) in mapping {
        std::fs::copy(src_dir.join(src), dst_dir.join(dst)).with_context(|| format!("Could not copy over file: {src}"))?;
    }

    if !(os == "linux" && arch == "arm") {
        let libbrane_dst = format!("libbrane_cli-{os}-{arch}{lib_suffix}.gz");
        compress_file(src_dir.join(format!("libbrane_cli{lib_suffix}")), dst_dir.join(&libbrane_dst)).await.context("Could not compress libbrane")?;
    }

    if os == "linux" && arch == "x86" {
        let instance_dst = format!("instance-{arch}.tar.gz");
        create_tar_gz(
            dst_dir.join(&instance_dst),
            ["brane-prx.tar", "brane-plr.tar", "brane-api.tar", "brane-drv.tar"].into_iter().map(|filename| src_dir.join(filename)),
        )
        .context("Could not create 'instance' tar archive")?;

        let worker_instance_dst = format!("worker-instance-{arch}.tar.gz");
        create_tar_gz(
            dst_dir.join(&worker_instance_dst),
            ["brane-prx.tar", "brane-job.tar", "brane-reg.tar"].into_iter().map(|filename| src_dir.join(filename)),
        )
        .context("Could not create 'worker-instance' tar archive")?;
    }

    Ok(())
}

fn format_binary_name(name: &str) -> String {
    format!("{name}-{os}-{arch}{ext}", os = std::env::consts::OS, arch = std::env::consts::ARCH, ext = std::env::consts::EXE_SUFFIX)
}

async fn compress_file(path: impl AsRef<Path>, dest: impl AsRef<Path>) -> anyhow::Result<()> {
    let file = File::open(path).await?;
    let mut reader = BufReader::new(file);
    let dest = File::create(dest).await?;
    let mut encoder = async_compression::tokio::write::GzipEncoder::new(dest);

    tokio::io::copy(&mut reader, &mut encoder).await?;
    Ok(())
}

fn create_tar_gz(archive_name: impl AsRef<Path>, files: impl IntoIterator<Item = PathBuf>) -> anyhow::Result<()> {
    let archive_name = archive_name.as_ref();
    let file = std::fs::File::create(archive_name).context("Couldn't create the archive")?;
    // FIXME: Does this need a bufwriter?
    let encoder = flate2::write::GzEncoder::new(file, flate2::Compression::default());
    let mut archive = Builder::new(encoder);

    let dirname: PathBuf = archive_name
        .file_name()
        .ok_or_else(|| anyhow::anyhow!("Could not get filename from archive"))?
        .to_string_lossy()
        .strip_suffix(".tar.gz")
        .ok_or_else(|| anyhow::anyhow!("Could not extract directory name from archive name"))?
        .into();

    for file in files {
        let filename = file
            .as_path()
            .file_name()
            .ok_or_else(|| anyhow::anyhow!("Could not find filename"))?
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Could not decode filename as UTF-8"))?;

        archive.append_path_with_name(file.as_path(), dirname.join(filename)).context("Could not add file to archive")?;
    }

    Ok(())
}
