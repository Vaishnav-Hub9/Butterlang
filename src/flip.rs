mod lexer;

fn main() {
    println!("{:?}", lexer::lex("let a = 1 + 2 * 5
    while a < 13 {
        a +=1
        print(a)
    }"));
}
