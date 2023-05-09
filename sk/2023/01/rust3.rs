
fn main() {
    let num1 = 1;
    let num2 = num1;
    let s1 = String::from("meep");
    let s2 = s1;
    println!("Number num1 is {}", num1);
    println!("Number num2 is {}", num2);
    println!("String s1 is {}", s1);
    println!("String s2 is {}", s2);
}
