fn add_one(a: u8) -> u8 {
    let g = a + 1;
    g
}

fn main() {
    let x = 1;
    let z = add_one(x);
    println!("{}", z);
}
