use std::io::BufferedReader;
use std::io::File;

fn main() {
    let path = Path::new("test.txt");
    let mut file = BufferedReader::new(File::open(&path));
    let mut line_no: u64 = 0;
    for line in file.lines() {
        line_no += 1;
        print!("{} {}", line_no, line.unwrap());
    }
}
