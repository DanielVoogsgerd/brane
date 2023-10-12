//  UTILS.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 17:39:38
//  Last edited:
//    12 Oct 2023, 18:07:58
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines utilities for doing stuff.
// 

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::fs::File;
use std::path::{Path, PathBuf};
use std::str::FromStr as _;

use error_trace::ErrorTrace as _;
use log::{debug, info, warn};
use ureq::Response;


/***** ERRORS *****/
/// Defines errors that may occur when downloading.
#[derive(Debug)]
pub enum DownloadError {
    /// Failed to create the output file
    FileCreate { path: PathBuf, err: std::io::Error },
    /// Failed to write to the file
    FileWrite { path: PathBuf, err: std::io::Error },

    /// Failed at sending a GET-request
    GetCreate { url: String, err: ureq::Error },
}
impl Display for DownloadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DownloadError::*;
        match self {
            FileCreate { path, .. } => write!(f, "Failed to create file '{}'", path.display()),
            FileWrite { path, .. }  => write!(f, "Failed to write to file '{}'", path.display()),

            Get { url, .. } => write!(f, "Failed to send GET-request to '{url}'"),
        }
    }
}
impl Error for DownloadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        use DownloadError::*;
        match self {
            FileCreate { err, .. } => Some(err),
            FileWrite { err, .. }  => Some(err),

            Get { err, .. } => Some(err),
        }
    }
}





/***** LIBRARY *****/
/// Downloads a file from the World Wide Web.
/// 
/// :nerd_face:
/// 
/// # Arguments
/// - `url`: The URL of the file to download.
/// - `path`: The path to download the file to.
/// 
/// # Errors
/// This function errors if we failed to download the file or write it.
pub fn download_file(url: impl AsRef<str>, path: impl AsRef<Path>) -> Result<(), DownloadError> {
    let url: &str = url.as_ref();
    let path: &Path = path.as_ref();
    info!("Downloading '{}' to '{}'", url, path.display());

    // Open the file to write to
    debug!("Creating output file '{}'...", path.display());
    let mut handle: File = match File::create(path) {
        Ok(handle) => handle,
        Err(err)   => { return Err(DownloadError::FileCreate { path: path.into(), err }); },
    };

    // Attempt to send out the request
    debug!("Sending GET-request to '{url}'...");
    let res: Response = match ureq::get(url).call() {
        Ok(res)  => res,
        Err(err) => { return Err(DownloadError::Get { url: url.into(), err }); },
    };

    // Attempt to read the header
    debug!("Writing response stream to '{}'...", path.display());
    let content_length: Option<usize> = res.header("Content-Length").map(|val: &str| usize::from_str(val).map_err(|err| warn!("Failed to parse response 'Content-Length': {}", err.trace())).ok()).flatten();
    let mut res_handle = res.into_reader();
    loop {
        // Read from the web stream
        let mut buf: [u8; 65536] = [0; 65536];
        let mut n_bytes: usize = match res_handle.read(&mut buf) {
            Ok(n_bytes) => n_bytes,
            Err(err)    => { return Err(DownloadError::RequestRead { url: url.into(), err }); },
        };
        if n_bytes == 0 { break; }

        // Write to the file buffer
        
    }

    // Alrighty
    Ok(())
}
