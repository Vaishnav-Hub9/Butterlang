use std::env;
use crate::compiletask;

pub fn parse_termargs(args: Vec<String>) {
    match args[1].as_str() {
        "build" => compiletask::compiletobinary("main", 3),
        "run" => {
            compiletask::compiletobinary("main", 3);
            compiletask::runbinary("main");
        },
        other => {
            panic!("Fk off bro noone knows {}", other)
        }
    }
}