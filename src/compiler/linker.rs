use std::{io::Write, path::{MAIN_SEPARATOR, Path}, process::{Command, Stdio}};

use crate::{compiler::OptionalArgs, rustaml::RustamlContext};
use inkwell::OptimizationLevel;
use pathbuf::pathbuf;

#[cfg(any(feature =  "musl", feature = "build-bdwgc"))]
use std::path::PathBuf;

// TODO : instead install file in filesystem ?
pub(crate) const STD_C_CONTENT: &str = include_str!("../../std.c");

// TODO : add a way to select the path of bdwgc for building it
// TODO : add a way to select the path of musl for building it

#[cfg(feature = "musl")]
fn build_musl(current_exe_folder : &Path, opt_level : OptimizationLevel, optional_args : &OptionalArgs) -> PathBuf {
    
    let musl_path = current_exe_folder.join("musl");

    let sysroot_musl = musl_path.join("musl_sysroot");
    if !sysroot_musl.exists(){
        std::fs::create_dir(&sysroot_musl).unwrap();
    }

    let mut configure_cmd = Command::new("./configure");
    configure_cmd.arg("--disable-shared").arg(&format!("--prefix={}", sysroot_musl.to_str().unwrap()));
    // TODO : make the CFLAGS work in one function/place (to deduplicate code with the cflags handling after that)
    let mut cflags = format!("-emit-llvm -O{} -fno-stack-protector", opt_level as u32);
        
       
    if !matches!(opt_level, OptimizationLevel::None){
        cflags += " -DNDEBUG";  // TODO : should I do this ?
        cflags += " -flto";
    }

    if optional_args.enable_debuginfos {
        cflags += " -g";
    } 

    if optional_args.march_native {
        cflags += " -march=native";
    }

    if optional_args.freestanding {
        panic!("freestanding not supported with musl"); // TODO ?
    }

    configure_cmd.envs([
        ("CC", "clang"),
        ("CFLAGS", &cflags),
        ("AR", "llvm-ar"),
        ("RANLIB", "llvm-ranlib"), 
    ]);
    configure_cmd.current_dir(&musl_path);
    configure_cmd.stdout(Stdio::null());

    configure_cmd.spawn().unwrap().wait().unwrap();     

    let available_threads = std::thread::available_parallelism().unwrap().get();
    let threads_flag = format!("-j{}", available_threads);

    let mut make_cmd = Command::new("make");

    make_cmd.arg(&threads_flag);
    make_cmd.current_dir(&musl_path);
    make_cmd.stdout(Stdio::null());

    make_cmd.spawn().unwrap().wait().unwrap();   

    let mut make_install_cmd = Command::new("make");
    make_install_cmd.arg("install").arg(&threads_flag);
    make_install_cmd.current_dir(&musl_path);
    make_install_cmd.stdout(Stdio::null());
    make_install_cmd.spawn().unwrap().wait().unwrap();

    sysroot_musl
}


#[cfg(feature = "build-bdwgc")]
fn build_bdwgc(current_exe_folder : &Path, temp_dir : &Path, sysroot_musl : &Path, opt_level : OptimizationLevel, optional_args : &OptionalArgs) -> (PathBuf, PathBuf) {
    let bdwgc_path = current_exe_folder.join("bdwgc");
    let bdwgc_src_path = pathbuf![&bdwgc_path, "extra", "gc.c"];

    let out_bdwgc_path = pathbuf![temp_dir, "bdwgc.bc"];
    let mut bdwgc_link_cmd = Command::new("clang");
    bdwgc_link_cmd.args(["-emit-llvm", "-c"]);
    let include_flag = format!("-I{}", bdwgc_path.join("include").to_str().unwrap());
    bdwgc_link_cmd.arg(include_flag);
    bdwgc_link_cmd.arg("-o").arg(out_bdwgc_path.as_os_str()).arg(bdwgc_src_path.as_os_str());
    bdwgc_link_cmd.arg(format!("-O{}", opt_level as u32));
    bdwgc_link_cmd.arg("-DNO_GETCONTEXT");

    // TODO : should I do this ?
    if !matches!(opt_level, OptimizationLevel::None){
        bdwgc_link_cmd.arg("-DNDEBUG");
    }

    if optional_args.enable_debuginfos {
        bdwgc_link_cmd.arg("-g");
    }

    if optional_args.march_native {
        bdwgc_link_cmd.arg("-march=native");
    }

    if optional_args.freestanding {
        panic!("freestanding not supported with build bdwgc"); // TODO ?
    }

    #[cfg(feature = "musl")]
    if optional_args.musl {
        bdwgc_link_cmd.arg("-fno-stack-protector").arg(&format!("--sysroot={}", sysroot_musl.to_str().unwrap()));
    }        
        
    bdwgc_link_cmd.spawn().unwrap().wait().unwrap();
    (out_bdwgc_path, bdwgc_path)
}

pub(crate) fn link_exe(rustaml_context: &mut RustamlContext, filename_out : &Path, bitcode_file : &Path, shared_libs : &[String], opt_level : OptimizationLevel, optional_args : &OptionalArgs){
    // use cc ?
    // TODO : use lld (https://github.com/mun-lang/lld-rs) for linking instead ?
    // TODO : use libclang ? (clang-rs ? https://github.com/llvm/llvm-project/blob/main/clang/tools/driver/cc1_main.cpp#L85 ?)

    #[cfg(not(feature = "build-bdwgc"))]
    if optional_args.build_bdwgc {
        panic!("Can't link a custom built bdwgc without enabling the build-bdwgc feature");
    }

    // TODO : only make the musl work with linux
    #[cfg(not(feature = "musl"))]
    if optional_args.musl {
        panic!("Can't link a custom built static musl without enabling the musl feature");
    }


    let temp_dir = std::env::temp_dir();
    let current_exe_path = std::env::current_exe().unwrap();
    let current_exe_folder = current_exe_path.parent().unwrap().to_path_buf();

    #[cfg(feature = "musl")]
    let sysroot_musl = if optional_args.musl {
        rustaml_context.start_section("build-musl");
        let sysroot_musl = build_musl(&current_exe_folder, opt_level, optional_args);
        rustaml_context.end_section("build-musl");

        Some(sysroot_musl)
    } else {
        None
    };

    #[cfg(feature = "build-bdwgc")]
    let (out_bdwgc_path, bdwgc_src_path) = if optional_args.build_bdwgc {
        
        rustaml_context.start_section("build-bdwgc");

        let (out_bdwgc_path, bdwgc_path) = build_bdwgc(&current_exe_folder, &temp_dir, sysroot_musl.as_ref().unwrap(), opt_level, optional_args);

        rustaml_context.end_section("build-bdwgc");
        (Some(out_bdwgc_path), Some(bdwgc_path))
        
    } else {
        (None, None)
    };

    let _ = current_exe_folder;
    
    
    let out_std_path = pathbuf![&temp_dir, "std.bc"];
    let out_std_path_str = out_std_path.as_os_str();

    rustaml_context.start_section("std");

    let mut clang_std = Command::new("clang");
    clang_std.arg("-x").arg("c").arg("-emit-llvm").arg(format!("-O{}", opt_level as u32)).arg("-c");

    if !matches!(opt_level, OptimizationLevel::None){
        clang_std.arg("-DNDEBUG");
    }

    if !optional_args.disable_gc {
        clang_std.arg("-D_GC_");
    }

    if optional_args.enable_debuginfos {
        clang_std.arg("-g");
    }

    if optional_args.march_native {
        clang_std.arg("-march=native");
    }


    // TODO : work on freestanding (add set target to make the os unknown in target triplet, pass linker script or just pass linker arg ?)
    if optional_args.freestanding {
        clang_std.arg("-ffreestanding").arg("-nostdlib");
    }

    #[cfg(feature = "musl")]
    if optional_args.musl {
        clang_std.arg("-fno-stack-protector");
        clang_std.arg(&format!("--sysroot={}", sysroot_musl.as_ref().unwrap().to_str().unwrap()));
    }

    #[cfg(feature = "build-bdwgc")]
    if optional_args.build_bdwgc {
        let include_arg = bdwgc_src_path.unwrap().join("include");
        clang_std.arg(&format!("-I{}", include_arg.to_str().unwrap()));
    }
    
    let mut clang_std = clang_std.arg("-").arg("-o").arg(out_std_path_str).stdin(Stdio::piped()).spawn().expect("compiling std failed");
    clang_std.stdin.as_mut().unwrap().write_all(STD_C_CONTENT.as_bytes()).unwrap();
    clang_std.wait().unwrap();

    rustaml_context.end_section("std");

    rustaml_context.start_section("linker");

    let mut link_cmd = Command::new("clang");

    if !matches!(opt_level, OptimizationLevel::None) {
        link_cmd.arg("-flto");
    }

    if optional_args.march_native {
        link_cmd.arg("-march=native");
    }

    if !optional_args.disable_gc && !optional_args.build_bdwgc {
        link_cmd.arg("-lgc");
    }

    if optional_args.freestanding {
        link_cmd.arg("-ffreestanding").arg("-nostdlib");
    }

    #[cfg(feature = "musl")]
    if optional_args.musl {
        link_cmd.arg(&format!("--sysroot={}", sysroot_musl.unwrap().to_str().unwrap()));
        link_cmd.arg("-static");
    }

    for search_path in &optional_args.lib_search_paths {
        link_cmd.arg("-L".to_owned() + search_path);
    }

    for lib in shared_libs {
        if lib.starts_with("..") || lib.starts_with(MAIN_SEPARATOR){
            // full path
            link_cmd.arg(lib);
        } else {
            let lib = if lib.starts_with("./") || lib.starts_with(".\\"){
                lib[2..].to_owned()
            } else if lib.starts_with("lib") && lib.ends_with(".so"){
                let suffix_stripped = lib.strip_suffix(".so").unwrap();
                let lib = "-l".to_owned() + &suffix_stripped[3..];
                lib
            } else {
                lib.to_owned()
            };
            // local or global
            link_cmd.arg(lib);
        }
    }

    link_cmd.arg("-lm").arg("-o").arg(filename_out).arg(out_std_path_str).arg(bitcode_file);
    if optional_args.enable_sanitizer {
        // TODO : need undefined sanitizer ? or just reimplement the checks (some are already implemented like overflow, but need a flag to deactivate them, and need one to replace the errors with a trap instruction)
        link_cmd.arg("-fsanitize=address");
        //link_cmd.arg("-Wl,--no-gc-sections");
    }

    #[cfg(feature = "build-bdwgc")]
    if optional_args.build_bdwgc {
        link_cmd.arg(&out_bdwgc_path.as_ref().unwrap());
    }

    if !link_cmd.spawn().expect("linker failed").wait().unwrap().success() {
        panic!("linker failed");
    }

    rustaml_context.end_section("linker");

    std::fs::remove_file(&out_std_path).expect("Couldn't delete std bitcode file");

    #[cfg(feature = "build-bdwgc")]
    if optional_args.build_bdwgc {
        std::fs::remove_file(&out_bdwgc_path.unwrap()).expect("Couldn't delete bdwgc bitcode file");
    }
}