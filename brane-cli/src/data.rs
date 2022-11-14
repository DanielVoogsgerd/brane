//  DATA.rs
//    by Lut99
// 
//  Created:
//    12 Sep 2022, 17:39:06
//  Last edited:
//    09 Nov 2022, 17:02:06
//  Auto updated?
//    Yes
// 
//  Description:
//!   Does things relating to datasets (and the `data` subcommand).
// 

use std::borrow::Cow;
use std::collections::HashMap;
use std::fs::{self, DirEntry, File, ReadDir};
use std::path::{Path, PathBuf};
use std::time::Duration;

use async_compression::tokio::bufread::GzipDecoder;
use chrono::Utc;
use console::{pad_str, style, Alignment, Term};
use dialoguer::{Confirm, Select};
use dialoguer::theme::ColorfulTheme;
use hyper::body::Bytes;
use indicatif::HumanDuration;
use prettytable::format::FormatBuilder;
use prettytable::Table;
use rand::prelude::IteratorRandom;
use reqwest::{Client, ClientBuilder, Proxy, Response};
use reqwest::tls::{Certificate, Identity};
use specifications::data::{AccessKind, AssetInfo, DataIndex, DataInfo};
use tempfile::TempDir;
use tokio::fs as tfs;
use tokio::io::{AsyncWriteExt, BufReader};
use tokio_stream::StreamExt;
use tokio_tar::Archive;

use brane_shr::fs::copy_dir_recursively_async;
use brane_tsk::spec::LOCALHOST;
use specifications::registry::RegistryConfig;

use crate::errors::DataError;
use crate::utils::{ensure_dataset_dir, ensure_datasets_dir, get_dataset_dir, get_registry_file};


/***** LIBRARY *****/
/// Reads all the local data assets to a local DataIndex.
pub fn get_data_index() -> Result<DataIndex, DataError> {
    // Make sure the main data folder exists (do not create it, though, since this is read-only)
    let datasets: PathBuf = match ensure_datasets_dir(false) {
        Ok(datasets) => datasets,
        Err(err)     => { return Err(DataError::DatasetsError{ err }); }
    };

    // Start reading the directory
    let dirs: ReadDir = match fs::read_dir(&datasets) {
        Ok(dirs) => dirs,
        Err(err) => { return Err(DataError::DatasetsReadError{ path: datasets, err }); }  
    };

    // Read it and iterate over all of the nested directories
    let mut infos: Vec<DataInfo> = Vec::with_capacity(16);
    for d in dirs {
        // Unwrap the entry
        let d: DirEntry = match d {
            Ok(d)    => d,
            Err(err) => { return Err(DataError::DatasetsReadError { path: datasets, err }); }
        };

        // If it's a directory, tentatively try to find a 'data.yml' file in there
        let d_path    : PathBuf = d.path();
        let info_path : PathBuf = d_path.join("data.yml");
        if d_path.is_dir() && info_path.exists() {
            // Attempt to open the file
            let handle = match File::open(&info_path) {
                Ok(handle) => handle,
                Err(err)   => { return Err(DataError::DataInfoOpenError{ path: info_path, err }); }
            };

            // Attempt to parse it
            let info: DataInfo = match serde_yaml::from_reader(handle) {
                Ok(info) => info,
                Err(err) => { return Err(DataError::DataInfoReadError{ path: info_path, err }); }
            };

            // Add it to the index
            infos.push(info);
        }
    }

    // Return a newly constructed info with it
    match DataIndex::from_infos(infos) {
        Ok(index) => Ok(index),
        Err(err)  => Err(DataError::DataIndexError{ err }),
    }
}

/// Attempts to download the given dataset from the instance.
/// 
/// For now, this function uses a random selection since it assumes there will usually only be one location that advertises having it. However, this is super bad practise and will lead to undefined results if there are multiple.
/// 
/// # Arguments
/// - `certs_dir`: The folder with certificates that we can use to prove who we are to each location.
/// - `endpoint`: The remote `brane-api` endpoint that we use to download the possible registries.
/// - `proxy_addr`: If given, the any data transfers will be proxied through this address.
/// - `name`: The name of the dataset to download.
/// - `access`: The locations where it is available.
/// 
/// # Returns
/// The AccessKind with how to download the dataset if it was downloaded successfully, or `None` if it wasn't available.
/// 
/// # Errors
/// This function errors if we failed to download the dataset somehow.
pub async fn download_data(certs_dir: impl AsRef<Path>, endpoint: impl AsRef<str>, proxy_addr: &Option<String>, name: impl AsRef<str>, access: &HashMap<String, AccessKind>) -> Result<Option<AccessKind>, DataError> {
    let certs_dir : &Path = certs_dir.as_ref();
    let endpoint  : &str  = endpoint.as_ref();
    let name      : &str  = name.as_ref();



    /* Step 1: Get target registry address */
    // Choose a random location to attempt to download the asset from.
    if access.is_empty() { return Ok(None); }
    let mut rng = rand::thread_rng();
    let location: &str = access.keys().choose(&mut rng).unwrap();

    // Send a GET-request to resolve that location to a delegate
    let registry_addr: String = format!("{}/infra/registries/{}", endpoint, location);
    let res: Response = match reqwest::get(&registry_addr).await {
        Ok(res)  => res,
        Err(err) => { return Err(DataError::RequestError{ what: "registry", address: registry_addr, err }); },
    };

    // Attempt to get its body if it was a success
    if !res.status().is_success() {
        return Err(DataError::RequestFailure{ address: registry_addr, code: res.status(), message: res.text().await.ok() })
    }
    let registry_addr: String = match res.text().await {
        Ok(registry_addr) => registry_addr,
        Err(err)          => { return Err(DataError::ResponseTextError{ address: registry_addr, err }); },
    };
    debug!("Remote registry: '{}'", registry_addr);



    /* Step 2: Load the required certificates */
    debug!("Loading certificate for location '{}'...", location);
    let (identity, ca_cert): (Identity, Certificate) = {
        // Compute the paths
        let cert_dir : PathBuf = certs_dir.join(&location);
        let idfile   : PathBuf = cert_dir.join("client-id.pem");
        let cafile   : PathBuf = cert_dir.join("ca.pem");

        // Load the keypair for this location as an Identity file (for which we just smash 'em together and hope that works)
        let ident: Identity = match tfs::read(&idfile).await {
            Ok(raw) => match Identity::from_pem(&raw) {
                Ok(identity) => identity,
                Err(err)     => { return Err(DataError::IdentityFileError{ path: idfile, err }); },
            },
            Err(err) => { return Err(DataError::FileReadError{ what: "client identity", path: idfile, err }); },
        };

        // Load the root store for this location (also as a list of certificates)
        let root: Certificate = match tfs::read(&cafile).await {
            Ok(raw) => match Certificate::from_pem(&raw) {
                Ok(root) => root,
                Err(err) => { return Err(DataError::CertificateError{ path: cafile, err }); },
            },
            Err(err) => { return Err(DataError::FileReadError{ what: "server cert root", path: cafile, err }); },
        };

        // Return them, with the cert and key as identity
        (ident, root)
    };



    /* Step 3: Prepare the filesystem */
    debug!("Preparing filesystem...");

    // Make sure the temporary tarfile directory exists
    let tar_dir: TempDir = match TempDir::new() {
        Ok(tar_dir) => tar_dir,
        Err(err)    => { return Err(DataError::TempDirError{ err }); },
    };
    let tar_path: PathBuf = tar_dir.path().join(format!("data_{}.tar.gz", name));

    // Compute the final data path in the datasets directory
    let data_dir: PathBuf = match ensure_dataset_dir(name, true) {
        Ok(datas_dir) => datas_dir,
        Err(err)      => { return Err(DataError::DatasetDirError { name: name.into(), err }); },
    };
    let data_path: PathBuf = data_dir.join("data");

    // Make sure the old data path doesn't exist anymore
    if data_path.exists() {
        if !data_path.is_dir() { return Err(DataError::DirNotADirError{ what: "target data", path: data_path }); }
        if let Err(err) = tfs::remove_dir_all(&data_path).await { return Err(DataError::DirRemoveError{ what: "target data", path: data_path, err }); }
    }

    // Create a fresh one
    if let Err(err) = tfs::create_dir(&data_path).await {
        return Err(DataError::DirCreateError{ what: "target data", path: data_path, err });
    }



    /* Step 4: Build the client. */
    let download_addr: String = format!("{}/data/download/{}", registry_addr, name);
    debug!("Sending download request to '{}'...", download_addr);
    let mut client: ClientBuilder = Client::builder()
        .use_rustls_tls()
        .add_root_certificate(ca_cert)
        .identity(identity);
    if let Some(proxy_addr) = proxy_addr {
        client = client.proxy(match Proxy::all(proxy_addr) {
            Ok(proxy) => proxy,
            Err(err)  => { return Err(DataError::ProxyCreateError{ address: proxy_addr.into(), err }) },
        });
    }
    let client: Client = match client.build() {
        Ok(client) => client,
        Err(err)   => { return Err(DataError::ClientCreateError{ err }); },
    };

    // Send a reqwest
    let res = match client.get(&download_addr).send().await {
        Ok(res)  => res,
        Err(err) => { return Err(DataError::RequestError{ what: "download", address: download_addr, err }); },
    };
    if !res.status().is_success() {
        return Err(DataError::RequestFailure { address: download_addr, code: res.status(), message: res.text().await.ok() });
    }



    /* Step 5: Download the raw file in parts */
    debug!("Downloading file to '{}'...", tar_path.display());
    {
        let mut handle: tfs::File = match tfs::File::create(&tar_path).await {
            Ok(handle) => handle,
            Err(err)   => { return Err(DataError::TarCreateError { path: tar_path, err }); },
        };
        let mut stream = res.bytes_stream();
        while let Some(chunk) = stream.next().await {
            // Unwrap the chunk
            let mut chunk: Bytes = match chunk {
                Ok(chunk) => chunk,
                Err(err)  => { return Err(DataError::DownloadStreamError { address: download_addr, err }); },  
            };

            // Write it to the file
            if let Err(err) = handle.write_all_buf(&mut chunk).await {
                return Err(DataError::TarWriteError{ path: tar_path, err });
            }
        }
    }



    /* Step 6: Extract the tar. */
    debug!("Unpacking '{}' to '{}'...", tar_path.display(), data_path.display());
    {
        let tar_gz: tfs::File = match tfs::File::open(&tar_path).await {
            Ok(handle) => handle,
            Err(err)   => { return Err(DataError::TarOpenError{ path: tar_path, err }); },
        };
        let tar         : GzipDecoder<_>          = GzipDecoder::new(BufReader::new(tar_gz));
        let mut archive : Archive<GzipDecoder<_>> = Archive::new(tar);
        if let Err(err) = archive.unpack(&data_path).await {
            return Err(DataError::TarExtractError{ source: tar_path, target: data_path, err });
        }
    }



    /* Step 7: In the case of brane-cli, also write a DataInfo. */
    let access: AccessKind = AccessKind::File{ path: data_path };
    {
        let info_path: PathBuf = data_dir.join("data.yml");
        debug!("Writing data info to '{}'...", info_path.display());

        // Populate the info itself
        let info: DataInfo = DataInfo {
            name        : name.into(),
            owners      : None,
            description : None,
            created     : Utc::now(),

            access : HashMap::from([
                (LOCALHOST.into(), access.clone()),
            ]),
        };

        // Write it
        if let Err(err) = info.to_path(&info_path) {
            return Err(DataError::DataInfoWriteError { err });
        }
    }



    /* Step 7: Done */
    Ok(Some(access))
}



/// Builds the given data.yml file to a locally usable package.
/// 
/// # Arguments
/// - `file`: The `data.yml` file to use as the definition.
/// - `workdir`: The directory to resolve all relative paths to.
/// - `keep_files`: Keep any intermediate build files.
/// - `no_links`: Always copy files to the Brane data folder to prevent links going all over the system.
/// 
/// # Returns
/// Nothing, but does build a new dataset in the `~/.local/share/brane/data` folder.
/// 
/// # Errors
/// This function may error if the build failed for any reason. Typically, this may be filesystem/IO errors or malformed data.yml / paths.
pub async fn build(file: impl AsRef<Path>, workdir: impl AsRef<Path>, _keep_files: bool, no_links: bool) -> Result<(), DataError> {
    let file    : &Path = file.as_ref();
    let workdir : &Path = workdir.as_ref();

    /* Step 1: Read the input */
    // Parse the input file as a AssetFile (which is a datafile but with user info attached to it).
    let mut info: AssetInfo = match AssetInfo::from_path(file) {
        Ok(info) => info,
        Err(err) => { return Err(DataError::AssetFileError{ path: file.into(), err }); },
    };
    // Inject the current time if not already
    info.created = Utc::now();

    // Make sure the files exist and resolve them to absolute paths
    match &mut info.access {
        AccessKind::File { ref mut path } => {
            // If it is relative, then make sure it's relative according to the data path
            if path.is_relative() {
                // Create a new relative path
                let apath: PathBuf = workdir.join(&path);
                let apath: PathBuf = match apath.canonicalize() {
                    Ok(apath) => apath,
                    Err(err)  => { return Err(DataError::FileCanonicalizeError{ path: apath.clone(), err }); },
                };
                *path = apath;
            }

            // Make sure exists & it's a file and not a directory
            // Nah, actually, why couldn't it be a directory?
            if !path.exists()  { return Err(DataError::FileNotFoundError { path: path.clone() }); }
            // if !path.is_file() { return Err(DataError::FileNotAFileError{ path: path.clone() }); }
        },
    }



    /* Step 2: Prepare the build directory. */
    // Before we create it though, if it happens to exist, then moan about it
    if let Ok(dir) = get_dataset_dir(&info.name) {
        if dir.exists() { return Err(DataError::DuplicateDatasetError{ name: info.name }); }
    }

    // Simple use our ensure thing for this
    let build_dir: PathBuf = match ensure_dataset_dir(&info.name, true) {
        Ok(build_dir) => build_dir,
        Err(err)      => { return Err(DataError::DatasetDirCreateError{ err }); }
    };



    /* Step 3: Move any files if we don't want no links. */
    if no_links {
        match &mut info.access {
            AccessKind::File { ref mut path } => {
                // Perform the copy
                let target: PathBuf = build_dir.join(path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or("data".into()));
                if let Err(err) = copy_dir_recursively_async(&path, &target).await {
                    return Err(DataError::DataCopyError{ err });
                }

                // Update the path to the target
                *path = target;
            },
        }
    }



    /* Step 4: Write the AssetInfo to a DataInfo. */
    let data_info: DataInfo = info.into();
    if let Err(err) = data_info.to_path(build_dir.join("data.yml")) { return Err(DataError::DataInfoWriteError{ err }); }



    /* Step 5: Done */
    println!("Successfully built dataset {}", style(&data_info.name).bold().cyan());
    Ok(())
}

/// Downloads a dataset from one or more remote hosts.
/// 
/// # Arguments
/// - `names`: The names of the dataset to download.
/// - `locs`: A name=loc keymap to specify locations for each dataset.
/// - `certs_dir`: The directory where all the certificates live.
/// - `proxy_addr`: The proxy address to proxy the transfer through, if any.
/// - `force`: Forces a download, even if the dataset is already available.
/// 
/// # Returns
/// The method for accessing the new data file. Clearly, this means it also creates a new local entry for a dataset upon success.
/// 
/// # Errors
/// This function may error if the download failed for any reason.
pub async fn download(names: Vec<String>, locs: Vec<String>, certs_dir: impl AsRef<Path>, proxy_addr: &Option<String>, force: bool) -> Result<(), DataError> {
    let certs_dir: &Path = certs_dir.as_ref();

    // Parse the locations into a map
    let mut locations: HashMap<String, String> = HashMap::with_capacity(locs.len());
    for l in locs {
        // Go through each comma-separated pair
        for l in l.split(',') {
            // Find the equals
            if let Some(equals_pos) = l.find('=') {
                // Split it and store the halves
                locations.insert(l[..equals_pos].into(), l[equals_pos + 1..].into());
            } else {
                return Err(DataError::NoEqualsInKeyPair{ raw: l.into() });
            }
        }
    }

    // Fetch the endpoint from the login file
    let config: RegistryConfig = match get_registry_file() {
        Ok(config) => config,
        Err(err)   => { return Err(DataError::RegistryFileError{ err }); }
    };

    // Fetch a new, remote DataIndex to get up-to-date entries
    let data_addr: String = format!("{}/data/info", config.url);
    let index: DataIndex = match brane_tsk::api::get_data_index(&data_addr).await {
        Ok(dindex) => dindex,
        Err(err)   => { return Err(DataError::RemoteDataIndexError{ address: data_addr, err }); },
    };

    // Iterate over the to-be-downloaded datasets
    for name in names {
        // Make sure we know it
        let info: &DataInfo = match index.get(&name) {
            Some(info) => info,
            None       => { return Err(DataError::UnknownDataset{ name: name.into() }); },
        };

        debug!("Selecting download location for '{}'...", name);
        let loc: String = {
            // Make sure the dataset is available _somewhere_
            if info.access.is_empty() { return Err(DataError::UnavailableDataset { name: name.clone(), locs: vec![] }); }
            // If we're given one, use it
            if let Some(loc) = locations.get(&name) {
                loc.clone()
            } else {
                // More effort is needed

                // ...unless it's available locally
                if !force && info.access.contains_key(&*LOCALHOST) {
                    println!("Dataset {} is already locally available; not initiating a download", style(name).cyan().bold());
                    return Ok(());
                }

                // Now, pick the only one or ask the user
                if info.access.len() == 1 {
                    info.access.keys().next().unwrap().clone()
                } else {
                    // Prepare the prompt with beautiful themes and such
                    let colorful = ColorfulTheme::default();
                    let items: Vec<&String> = info.access.keys().collect();
                    let mut prompt = Select::with_theme(&colorful);
                    prompt
                        .items(&items)
                        .with_prompt("Select download location")
                        .default(0usize);

                    // Ask the user
                    match prompt.interact_on_opt(&Term::stderr()) {
                        Ok(res)  => res.map(|i| items[i].clone()).unwrap_or(items[0].clone()),
                        Err(err) => { return Err(DataError::DataSelectError{ err }); },
                    }
                }
            }
        };

        println!("Downloading {} from {}...", style(&name).bold().cyan(), style(&loc).bold().cyan());

        // Create an access map with only the location entry
        let mut access: HashMap<String, AccessKind> = HashMap::with_capacity(1);
        if let Some(a) = info.access.get(&loc) {
            access.insert(loc, a.clone());
        } else {
            return Err(DataError::UnknownLocation{ name: loc });
        }

        // Fetch the method of its availability
        let access: AccessKind = match info.access.get(&*LOCALHOST) {
            Some(access) => access.clone(),
            None         => {
                // Attempt to download it instead
                match download_data(certs_dir, &config.url, proxy_addr, name.to_string(), &access).await? {
                    Some(access) => access,
                    None         => { return Err(DataError::UnavailableDataset{ name: name.into(), locs: info.access.keys().map(|k| k.clone()).collect() }); },
                }
            },
        };

        // Write the method of access
        println!("Download {}", style("success").bold().cyan());
        match access {
            AccessKind::File { path } => println!("(It's available under '{}')", path.display()),
        }
    }

    // Done
    Ok(())
}

/// Lists all locally built/available datasets.
/// 
/// # Returns
/// Nothing, but does print a neat table to stdout.
/// 
/// # Errors
/// This function may error if we somehow failed to discover all the files.
pub fn list() -> Result<(), DataError> {
    // Prepare display table.
    let format = FormatBuilder::new()
        .column_separator('\0')
        .borders('\0')
        .padding(1, 1)
        .build();
    let mut table = Table::new();
    table.set_format(format);
    table.add_row(row!["ID/NAME", "KIND", "CREATED", "LINKED?", "ACCESS"]);

    // Get the local DataIndex, which contains the local data infos
    let now   : i64       = Utc::now().timestamp();
    let index : DataIndex = get_data_index()?;
    for d in index {
        // Add the name/id of the dataset
        let name = pad_str(&d.name, 20, Alignment::Left, Some(".."));

        // Add the kind of the dataset
        let (kind, access, is_linked): (&str, String, bool) = match d.access.get("localhost").expect("Local dataset does not have 'localhost' as location; this should never happen!") {
            AccessKind::File { path } => {
                // Determine if this file is linked (it is if the path points outside the data directory itself)
                let is_linked: bool = if let Ok(dir) = get_dataset_dir(&d.name) {
                    !path.starts_with(dir)
                } else {
                    panic!("DataInfo '{}' points to non-existing dataset; this should never happen!", d.name);
                };

                // The kind is the name, the access is the path to the file
                ("File", path.to_string_lossy().into(), is_linked)
            },
        };
        let sis_linked: String = if is_linked { String::from("yes") } else { String::from("no") };
        let (kind, access, is_linked): (Cow<str>, Cow<str>, Cow<str>) = (pad_str(kind, 10, Alignment::Left, Some("..")), pad_str(&access, 60, Alignment::Left, Some("..")), pad_str(&sis_linked, 5, Alignment::Left, Some("..")));

        // Fetch the created (or rather, elapsed)
        let elapsed = Duration::from_secs((now - d.created.timestamp()) as u64);
        let created = format!("{} ago", HumanDuration(elapsed));
        let created = pad_str(&created, 15, Alignment::Left, Some(".."));

        // Finally, add a row with it
        table.add_row(row![name, kind, created, is_linked, access]);
    }
    
    // Write to stdout and done!
    table.printstd();
    Ok(())
}

/// Returns the paths to the locally available datasets.
/// 
/// # Arguments
/// - `datasets`: The names of the datasets to list the paths for.
/// 
/// # Returns
/// Nothing, but does print the paths to stdout in a machine-readable fashion.
/// 
/// # Errors
/// This function may error if we failed to read any of the files or directories.
pub fn path(datasets: Vec<impl AsRef<str>>) -> Result<(), DataError> {
    // Simply attempt to find all of the datasets in the local index
    let index : DataIndex = get_data_index()?;
    for d in datasets {
        let d: &str = d.as_ref();

        // Check if the dataset exists
        if let Some(info) = index.get(d) {
            if let Some(access) = info.access.get(&*LOCALHOST) {
                // Match on the access kind
                match access {
                    AccessKind::File { path } => {
                        println!("{}", path.display());
                    },

                    #[allow(unreachable_patterns)]
                    _ => { println!("<none>") },
                }
            } else {
                return Err(DataError::UnavailableDataset{ name: d.into(), locs: info.access.keys().map(|k| k.clone()).collect() });
            }
        } else {
            return Err(DataError::UnknownDataset{ name: d.into() });
        }
    }

    // Done
    Ok(())
}

/// Removes the dataset with the given identifier from the local database.
/// 
/// # Arguments
/// - `datasets`: The list of datasets to delete.
/// - `force`: Whether or not to force the removal (i.e., if true, do not ask the user for confirmation).
/// 
/// # Returns
/// Nothing, but does delete the datasets from the `~/.local/share/brane/data` folder.
/// 
/// # Errors
/// This function may error if the removal of one of the datasets failed (after which no other will be removed). This is typically due to the dataset not being found or not having permissions to remove it.
pub fn remove(datasets: Vec<impl AsRef<str>>, force: bool) -> Result<(), DataError> {
    // Remove them all
    for d in datasets {
        let d: &str = d.as_ref();

        // Fetch the directory of this dataset
        let dir: PathBuf = match get_dataset_dir(d) {
            Ok(dir)  => dir,
            Err(err) => { return Err(DataError::DatasetDirError{ name: d.into(), err }); }
        };

        // Ask the user if they are sure
        if !force {
            println!("Are you sure you want to remove dataset {}?", style(&d).bold().cyan());
            println!("(Note that, if the dataset is linked, the dataset itself will not be removed)");
            println!();
            let consent: bool = match Confirm::new().interact() {
                Ok(consent) => consent,
                Err(err)    => { return Err(DataError::ConfirmationError{ err }); }
            };
            if !consent { return Ok(()); }
        }

        // Everything checks out so just delete that folder
        if let Err(err) = fs::remove_dir_all(&dir) {
            return Err(DataError::RemoveError{ path: dir.clone(), err });
        }
        println!("Successfully removed dataset {}", style(&d).bold().cyan());
    }

    // Done
    Ok(())
}