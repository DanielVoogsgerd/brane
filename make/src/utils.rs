//  UTILS.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 17:39:38
//  Last edited:
//    13 Oct 2023, 11:03:03
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines utilities for doing stuff.
// 

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::str::FromStr as _;

use console::Style;
use error_trace::ErrorTrace as _;
use indicatif::{ProgressBar, ProgressStyle};
use log::{debug, info, warn};
use sha2::{Digest as _, Sha256};
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
    /// Failed at reading from a GET-response body
    ResponseRead { url: String, err: std::io::Error },
    /// Failed to verify the file's checksum.
    Checksum { path: PathBuf, got: String, expected: String },
}
impl Display for DownloadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DownloadError::*;
        match self {
            FileCreate { path, .. } => write!(f, "Failed to create file '{}'", path.display()),
            FileWrite { path, .. }  => write!(f, "Failed to write to file '{}'", path.display()),

            GetCreate { url, .. }            => write!(f, "Failed to send GET-request to '{url}'"),
            ResponseRead { url, .. }         => write!(f, "Failed to read GET-response body sent from '{url}'"),
            Checksum { path, got, expected } => write!(f, "Checksum of downloaded file '{}' does not match: got '{}', expected '{}'", path.display(), got, expected),
        }
    }
}
impl Error for DownloadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        use DownloadError::*;
        match self {
            FileCreate { err, .. } => Some(err),
            FileWrite { err, .. }  => Some(err),

            GetCreate { err, .. }    => Some(err),
            ResponseRead { err, .. } => Some(err),
            Checksum { .. }          => None,
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
/// - `hash`: If not [`None`], then the downloaded file will be verified to the given hash.
/// 
/// # Errors
/// This function errors if we failed to download the file or write it.
pub fn download_file(url: impl AsRef<str>, path: impl AsRef<Path>, hash: Option<Vec<u8>>) -> Result<(), DownloadError> {
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
        Err(err) => { return Err(DownloadError::GetCreate { url: url.into(), err }); },
    };

    // Attempt to read the header
    debug!("Writing response stream to '{}'...", path.display());
    let content_length: Option<usize> = res.header("Content-Length").map(|val: &str| usize::from_str(val).map_err(|err| warn!("Failed to parse response 'Content-Length': {}", err.trace())).ok()).flatten();
    let mut res_handle = res.into_reader();

    // Prepare getting a checksum if that is our method of choice
    let mut hasher: Option<Sha256> = if hash.is_some() {
        Some(Sha256::new())
    } else {
        None
    };

    // Now read the contents of the file
    let prgs: ProgressBar = if let Some(len) = content_length {
        ProgressBar::new(len as u64).with_style(ProgressStyle::with_template("    {bar:60} {bytes}/{total_bytes} {bytes_per_sec} ETA {eta_precise}").unwrap())
    } else {
        ProgressBar::new_spinner().with_style(ProgressStyle::with_template("    {elapsed_precise} {bar:60} {bytes} {binary_bytes_per_sec}").unwrap())
    };
    loop {
        // Read from the web stream
        let mut buf: [u8; 65536] = [0; 65536];
        let n_bytes: usize = match res_handle.read(&mut buf) {
            Ok(n_bytes) => n_bytes,
            Err(err)    => { return Err(DownloadError::ResponseRead { url: url.into(), err }); },
        };
        if n_bytes == 0 { break; }

        // Update the hasher, if any
        if let Some(hasher) = &mut hasher {
            hasher.update(&buf[..n_bytes]);
        }

        // Write to the file buffer
        if let Err(err) = handle.write_all(&mut buf[..n_bytes]) {
            return Err(DownloadError::FileWrite { path: path.into(), err });
        }

        // Update what we've written
        prgs.update(|state| state.set_pos(state.pos() + n_bytes as u64));
    }
    prgs.finish_and_clear();

    // Assert the checksums are the same if we're doing that
    if let Some(hash) = hash {
        // Finalize the hasher first
        let result = hasher.unwrap().finalize();
        debug!("Verifying checksum...");

        // Assert the checksums check out (wheezes)
        if &result[..] != hash { return Err(DownloadError::Checksum{ path: path.into(), expected: hex::encode(hash), got: hex::encode(&result[..]) }); }

        // Write the update to the user
        let dim    : Style = Style::new().dim();
        let accent : Style = Style::new().green().bold().dim();
        println!("{}{}{}", dim.apply_to(" > Checksum "), accent.apply_to(hex::encode(&result[..])), dim.apply_to(" OK"));
    }

    // Alrighty
    debug!("Download of file '{}' to '{}' OK", url, path.display());
    Ok(())
}
