// TODO : invalidate cache depending of targets, flags, etc

use dirs::cache_dir;
use inkwell::OptimizationLevel;
use pathbuf::pathbuf;
use std::{fs::{self, File}, path::Path};
use std::io::Write;
use blake3::Hash;

use crate::compiler::{CachedCompMeta, CachedCompilation};

// for now format to json, TODO : format to binary format supported by serde like postcard, MesagePack or another ?
// TODO : use also in the hash is gc is enabled, the sanitizer is enabled, debuginfos enabled


fn get_llvm_ir_hash(content : &str, opt_level : OptimizationLevel) -> Hash {
    let mut hasher = blake3::Hasher::new();
    hasher.update(&(opt_level as u32).to_be_bytes());
    hasher.update(content.as_bytes());
    hasher.finalize()
}

pub(crate) fn get_cached_llvm_ir(content : &str, opt_level : OptimizationLevel) -> Option<CachedCompilation> {
    let cache_dir = cache_dir()?;
    let ast_cache_dir = pathbuf![&cache_dir, "rustaml", "llvm-ir"];
    if !ast_cache_dir.exists(){
        return None;
    }
    
    let hash = get_llvm_ir_hash(content, opt_level);
    let mut cached_file = ast_cache_dir.clone();
    cached_file.push(format!("{}.bc", hash));
    if !cached_file.exists(){
        return None;
    }

    let mut cached_file_meta = ast_cache_dir;
    cached_file_meta.push(format!("{}.meta", hash));

    let cached_meta_str = fs::read_to_string(&cached_file_meta).unwrap();
    let cached_comp_meta = serde_json::from_str::<CachedCompMeta>(&cached_meta_str).unwrap();

    Some(CachedCompilation {
        bitcode_path: cached_file,
        metadata: cached_comp_meta,
    })
}

pub(crate) fn write_cached_llvm_ir(bitcode_path : &Path, opt_level : OptimizationLevel, content : &str, shared_libs : &[String]){
    let cache_dir = if let Some(cache_dir) = cache_dir() {
        cache_dir
    } else {
        return;
    };

    let hash = get_llvm_ir_hash(content, opt_level);

    let ast_cache_dir = pathbuf![&cache_dir, "rustaml", "llvm-ir"];
    if !ast_cache_dir.exists(){
        fs::create_dir_all(&ast_cache_dir).unwrap();
    }
    let mut cached_file_bc = ast_cache_dir.clone();
    cached_file_bc.push(format!("{}.bc", hash));
    // TODO : should it do this ?
    if cached_file_bc.exists(){
        return;
    }
    fs::copy(bitcode_path, cached_file_bc).unwrap();
    let mut cached_file_meta = ast_cache_dir;
    cached_file_meta.push(format!("{}.meta", hash));
    let cached_meta = CachedCompMeta {
        shared_libs: shared_libs.to_vec(),
    };
    let s = serde_json::to_string(&cached_meta).unwrap();
    let mut f = File::create(cached_file_meta).unwrap();
    write!(f, "{}", s).unwrap();
}