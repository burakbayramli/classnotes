fn take_the_s(s: &String) {
    println!("Fonksiyonda s degeri {}", s);
}
fn main() {
    let s = String::from("string");
    take_the_s(&s);
    println!("s is {}", s);
}
