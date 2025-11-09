use std::{fs::File, io::Write, time::{Duration, Instant}};

use clap::ValueEnum;
use serde_json::json;

#[derive(Clone)]
pub struct ProfilerSection {
    name : String,
    start: Instant,
    length : Duration,
}

#[derive(Clone)]
pub struct Profiler {
    pub section_delims : Vec<ProfilerSection>,
    start : Instant,
    start_current_section : Option<(String, Instant)>,
}

#[derive(Clone, Copy, ValueEnum, Debug)]
pub enum ProfilerFormat {
    #[value(name = "txt")]
    Txt,

    #[value(name = "chrome")]
    Chrome,
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
            start,
            length: elapsed, 
        });
    }

    pub fn dump(self, format : ProfilerFormat){
        let total = self.start.elapsed();
        match format {
            ProfilerFormat::Txt => {
                let mut f = File::create("profiling.txt").unwrap();
                for section in self.section_delims {
                    writeln!(f, "{}: {:?}", section.name, section.length).unwrap();
                }
                writeln!(f, "Total : {:?}", total).unwrap();
            }
            // TODO : remove serde_json dependency ?
            ProfilerFormat::Chrome => {
                let mut f = File::create("profiling.json").unwrap();
                let mut events = Vec::new();
                for section in self.section_delims {
                    let length = section.length.as_micros();
                    let start = section.start.duration_since(self.start).as_micros();

                    events.push(json!({
                        "name": section.name,
                        "cat": section.name, // TODO : add category (after nested ?)
                        "ph": "X",
                        "ts": start,
                        "dur": length,
                    }));
                }

                let events_val = serde_json::Value::Array(events);
                let chrome_trace_val = json! ({
                    "traceEvents": events_val,
                    //"displayTimeUnit": "ms",
                });
                write!(f, "{}", chrome_trace_val.to_string()).unwrap();
            }
        }
        
    }
}