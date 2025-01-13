#[cfg(test)]
mod tests {
    use super::*;
    use crate::git_utils::{initialize_cache_dir, shallow_clone_and_cache};
    use std::fs;
    use std::path::Path;

    #[test]
    fn test_shallow_clone_and_cache() {
        let repo_url = "https://github.com/user/repo.git";
        let commit_hash = "abc123";
        let subdir = "src";
        let cache_dir = "test_cache";

        initialize_cache_dir(cache_dir);

        match shallow_clone_and_cache(repo_url, commit_hash, subdir) {
            Ok(_) => {
                let cache_path = format!("{}/{}_{}_{}", cache_dir, repo_url.replace("/", "_"), commit_hash, subdir.replace("/", "_"));
                assert!(Path::new(&cache_path).exists());
                fs::remove_dir_all(cache_dir).unwrap();
            },
            Err(e) => panic!("Error: {}", e),
        }
    }
}
