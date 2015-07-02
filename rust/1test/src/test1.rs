fn fibo_rec(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibo_rec(n - 1) + fibo_rec(n - 2)
    }
}

fn fibo_iter(n: u64) -> u64 {
    let mut a = 0u64;
    let mut b = 1u64;
    let mut c : u64;
    for _ in 0..n {
        c = b;
        b = a + b;
        a = c;
    }
    a
}

fn main() {
    for i in 0..25 {
        println!("{}", fibo_rec(i));
    }
    for i in 0..100 {
        println!("{}", fibo_iter(i));
    }
}
