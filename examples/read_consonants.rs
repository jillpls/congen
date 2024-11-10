pub fn main() {
    let sounds = congen::sounds::parse_consonants("examples/pulmonic-consonants.csv");
    for s in sounds {
        println!("{:?}", s);
    }
}
