use std::collections::HashMap;

fn solve (xs: &Vec<usize>, n: usize) -> usize {
    let mut last = HashMap::new();
    for i in 0..xs.len()-1 {
        last.insert(xs[i],i);
    }
    let mut l = xs[xs.len()-1];
    let mut x;
    for i in (xs.len()-1)..(n-1) {
        let oval = last.get(&l);
        x = if oval == None { 0 } else { i - oval.unwrap() };
        last.insert(l,i);
        l = x;
    }
    return l;           
}

fn main() {
    let input = std::fs::read_to_string("../inputs/day15.txt")
        .unwrap().trim_end().split(",").filter_map(|s| s.parse::<usize>().ok()).collect::<Vec<_>>();
    println!("{}",solve(&input,2020));
    println!("{}",solve(&input,30000000));
}
