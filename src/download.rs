use reqwest;
use std::fs;
use std::path::Path;
use tokio;

pub async fn download_file(url: &str, dest: &str) -> Result<(), Box<dyn std::error::Error>> {
    let response = reqwest::get(url).await?;
    let content = response.bytes().await?;
    fs::write(dest, content)?;
    Ok(())
}

pub fn is_cached(file_path: &str) -> bool {
    Path::new(file_path).exists()
}

pub async fn get_file(url: &str, cache_dir: &str) -> Result<String, Box<dyn std::error::Error>> {
    let file_name = url.split('/').last().unwrap();
    let file_path = format!("{}/{}", cache_dir, file_name);

    if is_cached(&file_path) {
        Ok(file_path)
    } else {
        download_file(url, &file_path).await?;
        Ok(file_path)
    }
}
