use std::{fs::File, io::Write, time::{Duration, Instant}};

#[derive(Clone)]
pub struct ProfilerSection {
    name : String,
    length : Duration,
}

#[derive(Clone)]
pub struct Profiler {
    pub section_delims : Vec<ProfilerSection>,
    start : Instant,
    start_current_section : Option<(String, Instant)>,
}

impl Profiler {
    pub fn new() -> Profiler {
        Profiler { 
            section_delims: Vec::new(),
            start_current_section: None,
            start: Instant::now(),
        }
    }
    // for now, no nested sections, TODO ? (return a section index ? use the string ?)
    pub fn start_section(&mut self, name : String){
        let start = Instant::now();
        self.start_current_section = Some((name, start));
    }

    pub fn end_section(&mut self){
        let (name, start) = self.start_current_section.take().unwrap();
        let elapsed = start.elapsed();
        self.section_delims.push(ProfilerSection { 
            name, 
            length: elapsed, 
        });
    }

    // TODO : implement chrome format also, with a flag for the format
    pub fn dump(self){
        let total = self.start.elapsed();
        let mut f = File::create("profiling.txt").unwrap();
        for section in self.section_delims {
            writeln!(f, "{}: {:?}", section.name, section.length).unwrap();
        }
        writeln!(f, "Total : {:?}", total).unwrap();
    }
}