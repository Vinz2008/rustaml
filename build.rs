use std::{env, path::PathBuf, process::Command};

fn main(){
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target_dir = out_dir.ancestors().nth(3).unwrap().to_path_buf();
    if std::env::var("CARGO_FEATURE_BUILD_BDWGC").is_ok() {
        let repo_dir = target_dir.join("bdwgc");
        if !repo_dir.exists(){
            let git_clone_status = 
                Command::new("git").args([
                    "clone",
                    "--depth",
                    "1",
                    "https://github.com/bdwgc/bdwgc",
                    repo_dir.to_str().unwrap(),
                ]).status().unwrap();
            if !git_clone_status.success(){
                panic!("git clone failed");
            }
        }
    }

    if std::env::var("CARGO_FEATURE_MUSL").is_ok() {
        let repo_dir = target_dir.join("musl");
        if !repo_dir.exists(){
            let git_clone_status = 
                Command::new("git").args([
                    "clone",
                    "--depth",
                    "1",
                    "https://git.musl-libc.org/git/musl",
                    repo_dir.to_str().unwrap(),
                ]).status().unwrap();
            if !git_clone_status.success(){
                panic!("git clone failed");
            }
        }
    }

    println!("cargo:rerun-if-changed=build.rs");
}