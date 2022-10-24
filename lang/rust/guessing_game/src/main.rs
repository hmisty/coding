extern crate rand;

use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    let secret = rand::thread_rng().gen_range(1..101);
    println!("The secret is: {}", secret);

    loop {
        let mut guess = String::new();

        println!("Input your guess:");
        io::stdin().read_line(&mut guess).expect("guess required.");
        let guess: u32 = guess.trim().parse().expect("invalid number!");
        println!("Your guess: {}", guess);

        match guess.cmp(&secret) {
            Ordering::Less => println!("Too small"),
            Ordering::Greater => println!("Too big"),
            Ordering::Equal => {
                println!("You win!");
                break;
            }
        }
    }
    
}
