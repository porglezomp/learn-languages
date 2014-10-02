extern crate lodepng;

use std::io::File;

fn test_reading() {
    let path = Path::new("test.txt");
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => fail!("couldn't open {}: {}", display, why.desc),
        Ok(file) => file,
    };

    let string = match file.read_to_string() {
        Err(e) => fail!("couldn't read {}: {}", display, e.desc),
        Ok(s)  => s,
    };

    println!("{:s}", string);
}

fn test_writing() {
    let path = Path::new("test_write.txt");
    let display = path.display();
    let mut file = match File::create(&path) {
        Err(why) => fail!("couldn't open {}: {}", display, why.desc),
        Ok(file) => file,
    };

    let _ = file.write_str("This is a test!");
}

fn main() {
    test_reading();
    test_writing();
}
