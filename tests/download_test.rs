#[cfg(test)]
mod tests {
    use super::*;
    use tokio::runtime::Runtime;
    use std::process::{Command, Child};
    use std::thread::sleep;
    use std::time::Duration;

    fn start_local_http_server() -> Child {
        Command::new("python3")
            .arg("-m")
            .arg("http.server")
            .spawn()
            .expect("Failed to start local HTTP server")
    }

    fn stop_local_http_server(child: &mut Child) {
        child.kill().expect("Failed to stop local HTTP server");
    }

    #[test]
    fn test_download_file() {
        let mut server = start_local_http_server();
        sleep(Duration::from_secs(2)); // Give the server some time to start

        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let url = "http://localhost:8000/test_file.txt";
            let cache_dir = "cache";
            let file_path = get_file(url, cache_dir).await.unwrap();
            assert!(is_cached(&file_path));
        });

        stop_local_http_server(&mut server);
    }
}
