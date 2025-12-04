use crate::compiletask::compiletobinary;
use crate::parser::{
    Program, Stmt, Expr, InfixOp, PrefixOp, Type, Block, BlockOrIf,
};

use std::fmt::Write as FmtWrite;
use std::fs;

fn indent(out: &mut String, level: usize) {
    for _ in 0..level {
        out.push_str("    ");
    }
}
fn type_to_c(t: &Type) -> String {
    match t {
        Type::Int       => "int64_t".to_string(),
        Type::Float     => "double".to_string(),
        Type::Bool      => "bool".to_string(),
        Type::String    => "String".to_string(),   // custom String struct
        Type::Nil       => "void".to_string(),
        Type::Custom(name) => name.clone(),        // struct name
    }
}

fn escape_c_string(s: &str) -> String {
    let mut out = String::new();
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"'  => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _    => out.push(ch),
        }
    }
    out
}

fn emit_expr(out: &mut String, expr: &Expr) {
    match expr {
        Expr::Int(v) => {
            write!(out, "{v}").unwrap();
        }
        Expr::Float(v) => {
            write!(out, "{}", v).unwrap();
        }
        Expr::Bool(b) => {
            if *b {
                out.push_str("true");
            } else {
                out.push_str("false");
            }
        }
        Expr::String(s) => {
            let esc = escape_c_string(s);
            write!(out, "string_from_literal(\"{}\")", esc).unwrap();
        }
        Expr::Nil => {
            // rarely used as value; treat as 0/null-ish
            out.push_str("0");
        }
        Expr::Ident(name) => {
            out.push_str(name);
        }

        Expr::Group(inner) => {
            out.push('(');
            emit_expr(out, inner);
            out.push(')');
        }

        Expr::Prefix { op, rhs } => {
            match op {
                PrefixOp::Neg => {
                    out.push('-');
                    emit_expr(out, rhs);
                }
                PrefixOp::Not => {
                    out.push('!');
                    emit_expr(out, rhs);
                }
            }
        }

        Expr::Infix { op, lhs, rhs } => {
            match op {
                InfixOp::Assign => {
                    emit_expr(out, lhs);
                    out.push_str(" = ");
                    emit_expr(out, rhs);
                }
                InfixOp::AddAssign => {
                    emit_expr(out, lhs);
                    out.push_str(" += ");
                    emit_expr(out, rhs);
                }
                InfixOp::SubAssign => {
                    emit_expr(out, lhs);
                    out.push_str(" -= ");
                    emit_expr(out, rhs);
                }
                InfixOp::MulAssign => {
                    emit_expr(out, lhs);
                    out.push_str(" *= ");
                    emit_expr(out, rhs);
                }
                InfixOp::DivAssign => {
                    emit_expr(out, lhs);
                    out.push_str(" /= ");
                    emit_expr(out, rhs);
                }

                InfixOp::Add => {
                    emit_expr(out, lhs);
                    out.push_str(" + ");
                    emit_expr(out, rhs);
                }
                InfixOp::Sub => {
                    emit_expr(out, lhs);
                    out.push_str(" - ");
                    emit_expr(out, rhs);
                }
                InfixOp::Mul => {
                    emit_expr(out, lhs);
                    out.push_str(" * ");
                    emit_expr(out, rhs);
                }
                InfixOp::Div => {
                    emit_expr(out, lhs);
                    out.push_str(" / ");
                    emit_expr(out, rhs);
                }
                InfixOp::Mod => {
                    emit_expr(out, lhs);
                    out.push_str(" % ");
                    emit_expr(out, rhs);
                }

                InfixOp::Eq => {
                    emit_expr(out, lhs);
                    out.push_str(" == ");
                    emit_expr(out, rhs);
                }
                InfixOp::Ne => {
                    emit_expr(out, lhs);
                    out.push_str(" != ");
                    emit_expr(out, rhs);
                }
                InfixOp::Lt => {
                    emit_expr(out, lhs);
                    out.push_str(" < ");
                    emit_expr(out, rhs);
                }
                InfixOp::Le => {
                    emit_expr(out, lhs);
                    out.push_str(" <= ");
                    emit_expr(out, rhs);
                }
                InfixOp::Gt => {
                    emit_expr(out, lhs);
                    out.push_str(" > ");
                    emit_expr(out, rhs);
                }
                InfixOp::Ge => {
                    emit_expr(out, lhs);
                    out.push_str(" >= ");
                    emit_expr(out, rhs);
                }

                InfixOp::And => {
                    emit_expr(out, lhs);
                    out.push_str(" && ");
                    emit_expr(out, rhs);
                }
                InfixOp::Or => {
                    emit_expr(out, lhs);
                    out.push_str(" || ");
                    emit_expr(out, rhs);
                }
            }
        }

        Expr::Call { callee, args } => {
            emit_expr(out, callee);
            out.push('(');
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_expr(out, arg);
            }
            out.push(')');
        }

        Expr::Index { target, index } => {
            emit_expr(out, target);
            out.push('[');
            emit_expr(out, index);
            out.push(']');
        }

        Expr::StructLiteral { name, fields } => {
            // (person){ .name = ..., .age = ... }
            write!(out, "({}){{", name).unwrap();
            for (i, (fname, fexpr)) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write!(out, ".{} = ", fname).unwrap();
                emit_expr(out, fexpr);
            }
            out.push('}');
        }

        Expr::FieldAccess { target, field } => {
            emit_expr(out, target);
            out.push('.');
            out.push_str(field);
        }
    }
}

fn emit_block(out: &mut String, block: &Block, level: usize) {
    out.push_str("{\n");
    for stmt in block {
        emit_stmt(out, stmt, level + 1);
    }
    indent(out, level);
    out.push_str("}\n");
}

fn emit_if(out: &mut String, cond: &Expr, then_branch: &Block, else_branch: &Option<BlockOrIf>, level: usize) {
    indent(out, level);
    out.push_str("if (");
    emit_expr(out, cond);
    out.push_str(") ");
    emit_block(out, then_branch, level);

    if let Some(eb) = else_branch {
        match eb {
            BlockOrIf::Block(b) => {
                indent(out, level);
                out.push_str("else ");
                emit_block(out, b, level);
            }
            BlockOrIf::If(nested_if_stmt) => {
                // Stmt::If inside
                if let Stmt::If { cond, then_branch, else_branch } = &**nested_if_stmt {
                    indent(out, level);
                    out.push_str("else ");
                    emit_if(out, cond, then_branch, else_branch, level);
                } else {
                    panic!("BlockOrIf::If did not contain an If statement");
                }
            }
        }
    }
}

fn emit_stmt(out: &mut String, stmt: &Stmt, level: usize) {
    match stmt {
        Stmt::Let { name, valuetype, value, .. } => {
            indent(out, level);
            let cty = type_to_c(valuetype);
            write!(out, "{} {}", cty, name).unwrap();
            if let Some(expr) = value {
                out.push_str(" = ");
                emit_expr(out, expr);
            }
            out.push_str(";\n");
        }

        Stmt::ExprStmt(expr) => {
            indent(out, level);
            emit_expr(out, expr);
            out.push_str(";\n");
        }

        Stmt::Return(expr_opt) => {
            indent(out, level);
            if let Some(expr) = expr_opt {
                out.push_str("return ");
                emit_expr(out, expr);
                out.push_str(";\n");
            } else {
                out.push_str("return;\n");
            }
        }

        Stmt::While { cond, body } => {
            indent(out, level);
            out.push_str("while (");
            emit_expr(out, cond);
            out.push_str(") ");
            emit_block(out, body, level);
        }

        Stmt::If { cond, then_branch, else_branch } => {
            emit_if(out, cond, then_branch, else_branch, level);
        }

        Stmt::Block(block) => {
            indent(out, level);
            emit_block(out, block, level);
        }

        Stmt::Out => {
            // 'break'
            indent(out, level);
            out.push_str("break;\n");
        }

        Stmt::Skip => {
            // 'continue'
            indent(out, level);
            out.push_str("continue;\n");
        }

        Stmt::Import { .. } => {
            // ignore for now / handled at lexer/loader level
        }

        Stmt::Struct { .. } => {
            // already emitted as typedef at top-level in emit_structs()
        }

        Stmt::Func { .. } => {
            // handled at top-level in emit_functions()
        }
    }
}

fn emit_structs(out: &mut String, prog: &Program) {
    for stmt in &prog.stmts {
        if let Stmt::Struct { name, fields } = stmt {
            write!(out, "typedef struct {} {{\n", name).unwrap();
            for (fname, ftype) in fields {
                let cty = type_to_c(ftype);
                write!(out, "    {} {};\n", cty, fname).unwrap();
            }
            write!(out, "}} {};\n\n", name).unwrap();
        }
    }
}

fn emit_functions(out: &mut String, prog: &Program) {
    for stmt in &prog.stmts {
        if let Stmt::Func { name, params, returntype, body } = stmt {
            let is_main = name == "main";
            if is_main {
                out.push_str("void* flip_main(void)");
            } else {
                let ret = type_to_c(returntype);
                write!(out, "{} {}", ret, name).unwrap();
                out.push('(');
                for (i, (pname, pty)) in params.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    let cty = type_to_c(pty);
                    write!(out, "{} {}", cty, pname).unwrap();
                }
                out.push(')');
            }
            out.push(' ');
            emit_block(out, body, 0);
            out.push('\n');
        }
    }
}

/// Emit the C prelude gud name
fn emit_prelude(out: &mut String) {
    out.push_str(
r#"#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct {
    unsigned char *base;
    size_t capacity;
    size_t offset;
} Arena;

Arena global_arena;

void arena_init(size_t cap) {
    global_arena.base = malloc(cap);
    global_arena.capacity = cap;
    global_arena.offset = 0;
}

void arena_destroy() {
    free(global_arena.base);
}

void arena_grow(size_t min_extra) {
    size_t new_cap = global_arena.capacity * 2;
    size_t needed = global_arena.offset + min_extra;

    if (new_cap < needed) {
        new_cap = needed * 2;
    }

    unsigned char *new_base = realloc(global_arena.base, new_cap);
    if (!new_base) {
        printf("Arena realloc failed\n");
        exit(1);
    }

    global_arena.base = new_base;
    global_arena.capacity = new_cap;
}

void *arena_alloc(size_t size) {
    size = (size + 7) & ~((size_t)7);

    if (global_arena.offset + size > global_arena.capacity) {
        arena_grow(size);
    }

    void *ptr = global_arena.base + global_arena.offset;
    global_arena.offset += size;
    return ptr;
}

typedef struct {
    char *data;
    size_t len;
    size_t cap;
} String;

String string_new(void) {
    String s;
    s.len = 0;
    s.cap = 16;
    s.data = (char *)arena_alloc(s.cap + 1);
    s.data[0] = '\0';
    return s;
}

void string_grow(String *s, size_t extra) {
    size_t needed = s->len + extra;
    if (needed > s->cap) {
        size_t cap = s->cap;
        if (cap < 16) cap = 16;
        while (cap < needed) {
            cap *= 2;
        }

        char *new_data = (char *)arena_alloc(cap + 1);
        memcpy(new_data, s->data, s->len + 1);

        s->data = new_data;
        s->cap = cap;
    }
}

void string_push(String *s, const char *suffix) {
    size_t add = strlen(suffix);
    string_grow(s, add);

    memcpy(s->data + s->len, suffix, add);
    s->len += add;
    s->data[s->len] = '\0';
}

String string_from_literal(const char *lit) {
    size_t len = strlen(lit);

    String s;
    s.len = len;
    s.cap = len;
    s.data = (char *)arena_alloc(s.cap + 1);
    memcpy(s.data, lit, len + 1);

    return s;
}

#define print(x) _Generic((x), \
    int64_t: print_int,            \
    double: print_double,      \
    String: print_string       \
)(x)

#define println(x) _Generic((x), \
    int64_t: println_int,            \
    double: println_double,      \
    String: println_string       \
)(x)

void print_int(int64_t x) { printf("%d", x); }
void print_double(double x) { printf("%f", x); }
void print_string(String s) { printf("%s", s.data); }

void println_int(int64_t x) { printf("\n%d", x); }
void println_double(double x) { printf("\n%f", x); }
void println_string(String s) { printf("\n%s", s.data); }

void panic(String msg) {
    fprintf(stderr, "Runtime panic: %s\n", msg.data);
    abort();
}


"#
    );
}

pub fn transpile(program: Program) {
    let mut out = String::new();

    emit_prelude(&mut out);

    emit_structs(&mut out, &program);

    emit_functions(&mut out, &program);

    out.push_str(
r#"
int main(void) {
    arena_init(1024 * 1024 * 16);
    flip_main();
    arena_destroy();
    return 0;
}
"#
    );
    fs::write("main.c", out);
    compiletobinary("main", 3);
}
