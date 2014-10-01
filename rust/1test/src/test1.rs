fn fibo_rec(n: int) -> int {
    match n {
        0 => 0,
        1 => 1,
        _ => fibo_rec(n - 1) + fibo_rec(n - 2)
    }
}

fn fibo_iter(n: uint) -> u64 {
    let mut a = 0u64;
    let mut b = 1u64;
    let mut c : u64;
    for _ in range(0, n) {
        c = b;
        b = a + b;
        a = c;
    }
    a
}

fn main() {
    for i in range(0i, 25i) {
        println!("{}", fibo_rec(i));
    }
    for i in range(0u, 100u) {
        println!("{}", fibo_iter(i));
    }
}
