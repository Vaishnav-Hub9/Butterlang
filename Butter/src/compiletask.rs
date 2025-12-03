use std::process::Command;

pub fn compiletobinary(name: &str, Olevel: i8) {
    Command::new("gcc")
        .arg(format!("{name}.c"))
        .arg(format!("-O{Olevel}"))
        .arg("-march=native")
        .arg("-flto")
        .arg("-o")
        .arg(format!("{name}"))
        .status()
        .expect("failed to compile");
    }