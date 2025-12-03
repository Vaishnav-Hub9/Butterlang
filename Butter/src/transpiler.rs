use crate::compiletask::compiletobinary;

use crate::parser::{
    Program,
    Stmt,
    Expr,
    InfixOp,
    PrefixOp,
    Type,
    Block,
    BlockOrIf,
};

use std::fs;
use std::fmt::Write as FmtWrite;

fn indent(out: &mut String, level: usize) {
    for _ in 0..level {
        out.push_str("    "); // 4 spaces
    }
}

fn type_to_c(t: &Type) -> &'static str {
    match t {
        Type::Int       => "int64_t",
        Type::Float     => "double",
        Type::Bool      => "bool",
        Type::String    => "String",
        Type::Nil       => "void",
        Type::Custom(_) => "void*", // later: map to struct name etc.
    }
}

fn emit_expr(out: &mut String, expr: &Expr) {
    match expr {
        Expr::Int(v) => {
            write!(out, "{v}").unwrap();
        }

        Expr::Float(v) => {
            write!(out, "{v}").unwrap();
        }

        Expr::Bool(true) => out.push_str("1"),
        Expr::Bool(false) => out.push_str("0"),

        Expr::String(s) => {
            write!(out, "string_from_literal(\"{}\")", s).unwrap();
        }

        Expr::Nil => {
            out.push_str("0");
        }

        Expr::Ident(name) => {
            out.push_str(name);
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
            if let InfixOp::AddAssign = op {
                if let Expr::Ident(var_name) = &**lhs {
                    if let Expr::String(s) = &**rhs {
                        write!(out, "string_push(&{}, \"{}\")", var_name, s).unwrap();
                        return;
                    }
                }
            }
            out.push('(');
            emit_expr(out, lhs);
            out.push(' ');

            let op_str = match op {
                InfixOp::Add => "+",
                InfixOp::Sub => "-",
                InfixOp::Mul => "*",
                InfixOp::Div => "/",
                InfixOp::Mod => "%",

                InfixOp::Eq => "==",
                InfixOp::Ne => "!=",
                InfixOp::Lt => "<",
                InfixOp::Le => "<=",
                InfixOp::Gt => ">",
                InfixOp::Ge => ">=",

                InfixOp::And => "&&",
                InfixOp::Or  => "||",

                InfixOp::Assign    => "=",
                InfixOp::AddAssign => "+=",
                InfixOp::SubAssign => "-=",
                InfixOp::MulAssign => "*=",
                InfixOp::DivAssign => "/=",
            };

            out.push_str(op_str);
            out.push(' ');
            emit_expr(out, rhs);
            out.push(')');
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

        Expr::Index { .. }
        | Expr::StructLiteral { .. }
        | Expr::FieldAccess { .. } => {
            // TODO: arrays, structs, fields
            out.push_str("/* TODO complex expr */");
        }

        Expr::Group(inner) => {
            out.push('(');
            emit_expr(out, inner);
            out.push(')');
        }
    }
}

fn emit_stmt(out: &mut String, stmt: &Stmt, indent_level: usize) {
    match stmt {
        Stmt::Let { name, mutable: _, valuetype, value } => {
            indent(out, indent_level);

            // for now, assume locals are int64_t
            out.push_str(&(type_to_c(valuetype).to_owned() + " "));
            out.push_str(name);

            if let Some(expr) = value {
                out.push_str(" = ");
                emit_expr(out, expr);
            }

            out.push_str(";\n");
        }

        Stmt::ExprStmt(expr) => {
            indent(out, indent_level);
            emit_expr(out, expr);
            out.push_str(";\n");
        }

        Stmt::Return(expr_opt) => {
            indent(out, indent_level);
            out.push_str("return");
            if let Some(expr) = expr_opt {
                out.push(' ');
                emit_expr(out, expr);
            }
            out.push_str(";\n");
        }

        Stmt::Import { name } => {
            out.push_str(&format!("#include<{name}>\n"));
        }

        Stmt::While { cond, body } => {
            indent(out, indent_level);
            out.push_str("while (");
            emit_expr(out, cond);
            out.push_str(") {\n");
            emit_block(out, body, indent_level + 1);
            indent(out, indent_level);
            out.push_str("}\n");
        }

        Stmt::If { cond, then_branch, else_branch } => {
            indent(out, indent_level);
            out.push_str("if (");
            emit_expr(out, cond);
            out.push_str(") {\n");
            emit_block(out, then_branch, indent_level + 1);
            indent(out, indent_level);
            out.push_str("}");

            if let Some(else_part) = else_branch {
                match else_part {
                    BlockOrIf::Block(block) => {
                        out.push_str(" else {\n");
                        emit_block(out, block, indent_level + 1);
                        indent(out, indent_level);
                        out.push_str("}\n");
                    }
                    BlockOrIf::If(if_stmt) => {
                        out.push_str(" else ");
                        emit_stmt(out, if_stmt, indent_level); // else if
                    }
                }
            } else {
                out.push('\n');
            }
        }

        Stmt::Block(block) => {
            indent(out, indent_level);
            out.push_str("{\n");
            emit_block(out, block, indent_level + 1);
            indent(out, indent_level);
            out.push_str("}\n");
        }

        Stmt::Struct { .. } => {
            // TODO: map to C struct decl
            indent(out, indent_level);
            out.push_str("/* TODO struct decl */\n");
        }

        Stmt::Out => {
            indent(out, indent_level);
            out.push_str("break;\n");
        }

        Stmt::Skip => {
            indent(out, indent_level);
            out.push_str("continue;\n");
        }

        Stmt::Func { .. } => {
            // Shouldn't appear here; functions handled at top-level
            indent(out, indent_level);
            out.push_str("/* nested func? TODO */\n");
        }
    }
}

fn emit_block(out: &mut String, body: &Block, indent_level: usize) {
    for stmt in body {
        emit_stmt(out, stmt, indent_level);
    }
}

fn emit_function(
    out: &mut String,
    name: &str,
    params: &[(String, Type)],
    return_type: &Type,
    body: &Block,
) {
    let ret_ty = type_to_c(return_type);

    // function signature
    indent(out, 0);
    if name == "main" { write!(out, "{} {}(", ret_ty, "flip_main").unwrap(); } else { write!(out, "{} {}(", ret_ty, name).unwrap(); }

    for (i, (pname, pty)) in params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        let c_ty = type_to_c(pty);
        write!(out, "{} {}", c_ty, pname).unwrap();
    }

    out.push_str(") {\n");

    // body with one level of indent
    emit_block(out, body, 1);

    // closing brace
    indent(out, 0);
    out.push_str("}\n\n");
}

pub fn transpile(program: Program, name: &str) {
    let mut genned_code = String::new();

    // headers
    genned_code.push_str("#include <stdint.h>\n");
    genned_code.push_str("#include <stdbool.h>\n");
    genned_code.push_str("#include <stdio.h>\n");
    genned_code.push_str("#include <string.h>\n");
    genned_code.push_str("#include <stdlib.h>\n");
    genned_code.push_str("\n");

    // String type support IN C
    let stringimplementation = String::from(r#"
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
                memcpy(new_data, s->data, s->len + 1); // copy old contents + '\0'

                s->data = new_data;
                s->cap = cap;
                // old buffer stays in arena, will be reclaimed when arena_destroy() runs
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
            memcpy(s.data, lit, len + 1); // includes '\0'

            return s;
        }
    "#);
    let maincodeimplementation = String::from(r#"
        int main(void) {
            arena_init(1024 * 1024 * 16); // 16 MB for now
            flip_main();
            arena_destroy();
        }
    "#);

    let arenaallocimplementation = String::from(r#"
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

    "#);

    let basefuncimplementation = fs::read_to_string("basefuncs").expect("Failed to read file");


    genned_code.push_str(&arenaallocimplementation);
    genned_code.push_str(&stringimplementation);
    genned_code.push_str(&basefuncimplementation);

    for stmt in &program.stmts {
        match stmt {
            Stmt::Func { name, params, returntype, body } => {
                emit_function(&mut genned_code, name, params, returntype, body);
            }

            _ => {
                // TODO: handle top-level non-function stmts
            }
        }
    }
    genned_code.push_str(&maincodeimplementation);

    fs::write(format!("{name}.c"), genned_code).expect("failed to write result.c");

    compiletobinary(name, 3);
}
