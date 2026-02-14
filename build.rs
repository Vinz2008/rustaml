use std::{env, ffi::OsStr, fs, iter, path::PathBuf, process::Command};

fn main(){
    println!("cargo:rerun-if-changed=build.rs");

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

    


    if std::env::var("CARGO_FEATURE_NATIVE").is_ok(){
        let project_dir = target_dir.ancestors().nth(2).unwrap();
        let std_folder = project_dir.join("std");
        println!("cargo:rerun-if-changed={}", std_folder.display());

        let std_prelude_path = std_folder.join("prelude.h");
        println!("cargo:rerun-if-changed={}", std_prelude_path.display());
        
        
        let c_files = fs::read_dir(&std_folder).unwrap()
            .map(|f| f.unwrap())
            .filter(|f| f.path().extension() == Some(OsStr::new("c")) && f.file_name() != "std.c").collect::<Vec<_>>();

        for c_file in &c_files {
            println!("cargo:rerun-if-changed={}", c_file.path().display())
        }

        let c_files_content = 
            c_files.into_iter()
            .map(|f| fs::read_to_string(f.path()).unwrap() + "\n");
        let prelude_content = fs::read_to_string(std_prelude_path).unwrap() + "\n";
        let files_content = iter::once(prelude_content).chain(c_files_content).collect::<String>();
        let std_output_path = std_folder.join("std.c");
        fs::write(std_output_path, files_content).unwrap();
    }

}