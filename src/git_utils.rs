use git2::{Oid, Repository};
use lazy_static::lazy_static;
use std::fs;
use std::path::Path;
use std::sync::Mutex;

lazy_static! {
    static ref CACHE_DIR: Mutex<String> =
        Mutex::new(format!("{}/.cache/visilog", std::env::var("HOME").unwrap()));
}

pub fn initialize_cache_dir(path: &str) {
    let mut cache_dir = CACHE_DIR.lock().unwrap();
    *cache_dir = path.to_string();
}

pub fn shallow_clone_and_cache(
    repo_url: &str,
    commit_hash: &str,
    subdir: &str,
) -> Result<(), git2::Error> {
    let cache_dir = CACHE_DIR.lock().unwrap();
    let cache_path = format!(
        "{}/{}_{}_{}",
        *cache_dir,
        repo_url.replace("/", "_"),
        commit_hash,
        subdir.replace("/", "_")
    );

    if Path::new(&cache_path).exists() {
        return Ok(());
    }

    let repo = Repository::clone(repo_url, &cache_path)?;
    let oid = Oid::from_str(commit_hash)?;
    let commit = repo.find_commit(oid)?;
    let tree = commit.tree()?;
    let subdir_entry = tree
        .get_name(subdir)
        .ok_or(git2::Error::from_str("Subdirectory not found"))?;
    let subdir_oid = subdir_entry.id();
    let subdir_tree = repo.find_tree(subdir_oid)?;

    fs::create_dir_all(&cache_path).map_err(|e| git2::Error::from_str(&e.to_string()))?;
    for entry in subdir_tree.iter() {
        let entry_path = format!("{}/{}", cache_path, entry.name().unwrap());
        if entry.kind() == Some(git2::ObjectType::Blob) {
            let blob = repo.find_blob(entry.id())?;
            fs::write(entry_path, blob.content())
                .map_err(|e| git2::Error::from_str(&e.to_string()))?;
        }
    }

    Ok(())
}
