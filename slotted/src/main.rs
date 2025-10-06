use slotted_egraphs::*;
use std::env;

define_language! {
    enum Expr {
        Zero() = "0",
        Succ(AppliedId) = "succ",
        Var(Symbol),

        Add(AppliedId, AppliedId) = "+",
        Mul(AppliedId, AppliedId) = "*",
        Pow(AppliedId, AppliedId) = "^",
    }
}

define_language! {
    enum Linalg {
        Mat() = "mat",
        Mul(AppliedId, AppliedId) = "*",
    }
}

// set the cost of + to 10, * to 100 and ^ to 1000
struct ExprCost;
impl CostFunction<Expr> for ExprCost {
    type Cost = usize;

    fn cost<C>(&self, enode: &Expr, costs: C) -> usize
    where
        C: Fn(Id) -> usize,
    {
        match enode {
            Expr::Zero() | Expr::Var(_) => 1,
            Expr::Succ(x) => 1 + costs(x.id),
            Expr::Add(x, y) => 10 + costs(x.id) + costs(y.id),
            Expr::Mul(x, y) => 100 + costs(x.id) + costs(y.id),
            Expr::Pow(x, y) => 1000 + costs(x.id) + costs(y.id),
        }
    }
}

#[derive(Debug)]
#[derive(Clone)]
struct DimAndCost {
    rows: i64,
    cols: i64,
    cost: usize,
}

impl PartialEq for DimAndCost {
    fn eq(&self, other: &Self) -> bool {
        self.cost == other.cost
    }
}

impl Eq for DimAndCost {}

impl PartialOrd for DimAndCost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cost.cmp(&other.cost))
    }
}

impl Ord for DimAndCost {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cost.cmp(&other.cost)
    }
}

struct LinalgCost; // cost is number of scalar multiplications, so for A (m x n) * B (n x p) it's m*n*p
impl CostFunction<Linalg> for LinalgCost {
    type Cost = DimAndCost;

    fn cost<C>(&self, enode: &Linalg, costs: C) -> Self::Cost
    where
        C: Fn(Id) -> Self::Cost
    {
        match enode {
            Linalg::Mat() => {
                DimAndCost { rows: 10, cols: 10, cost: 0 }
            },
            Linalg::Mul(x, y) => {
                let a = costs(x.id);
                let b = costs(y.id);
                assert!(a.cols == b.rows);
                let cost = (a.rows * a.cols * b.cols) as usize;
                DimAndCost { rows: a.rows, cols: b.cols, cost: a.cost + b.cost + cost }
            }
        }
    }

}

fn poly(degree: usize) {
    let mut succs = vec!["0".to_string()];
    for i in 1..=degree {
        succs.push(format!("(succ {})", succs[i-1]));
    }

    let mut poly_expr = "a".to_string();
    for i in 1..=degree {
        let coeff = ((b'a' + i as u8) as char).to_string();
        let term = format!("(* {} (^ x {}))", coeff, succs[i]);
        poly_expr = format!("(+ {} {})", term, poly_expr);
    }

    let poly = RecExpr::<Expr>::parse(&poly_expr).unwrap();
    // println!("poly: {}, cost: {}", poly, ExprCost.cost_rec(&poly));
    // assert!(ExprCost.cost_rec(&poly) == 4579);

    let add_commutativity: Rewrite<Expr, ()> = Rewrite::new("add-comm", "(+ ?a ?b)", "(+ ?b ?a)");
    let mul_commutativity: Rewrite<Expr, ()> = Rewrite::new("mul-comm", "(* ?a ?b)", "(* ?b ?a)");
    let add_associativity1: Rewrite<Expr, ()> = Rewrite::new("add-assoc1", "(+ (+ ?a ?b) ?c)", "(+ ?a (+ ?b ?c))");
    let add_associativity2: Rewrite<Expr, ()> = Rewrite::new("add-assoc2", "(+ ?a (+ ?b ?c))", "(+ (+ ?a ?b) ?c)");
    let mul_associativity1: Rewrite<Expr, ()> = Rewrite::new("mul-assoc1", "(* (* ?a ?b) ?c)", "(* ?a (* ?b ?c))");
    let mul_associativity2: Rewrite<Expr, ()> = Rewrite::new("mul-assoc2", "(* ?a (* ?b ?c))", "(* (* ?a ?b) ?c)");
    let distributivity1: Rewrite<Expr, ()> = Rewrite::new("distrib1", "(* ?x (+ ?a ?b))", "(+ (* ?x ?a) (* ?x ?b))");
    let distributivity2: Rewrite<Expr, ()> = Rewrite::new("distrib2", "(+ (* ?x ?a) (* ?x ?b))", "(* ?x (+ ?a ?b))");
    let mul_id: Rewrite<Expr, ()> = Rewrite::new("mul-id", "(* ?a (succ 0))", "?a");
    let zero_exp: Rewrite<Expr, ()> = Rewrite::new("zero-exp", "(^ ?a 0)", "(succ 0)");
    let exp_def: Rewrite<Expr, ()> = Rewrite::new("exp-def", "(^ ?a (succ ?n))", "(* ?a (^ ?a ?n))");

    let rules = vec![
        add_commutativity,
        mul_commutativity,
        add_associativity1,
        add_associativity2,
        mul_associativity1,
        mul_associativity2,
        distributivity1,
        distributivity2,
        mul_id,
        zero_exp,
        exp_def,
    ];

    let mut egraph = EGraph::<Expr, ()>::default();
    let root = egraph.add_expr(poly);
    // print!("Initial e-graph size: {}\n", egraph.total_number_of_nodes());

    let mut runner = Runner::<Expr, ()>::default().with_egraph(egraph);
    let report = runner.run(&rules);
    // print!("Final e-graph size: {}\n", runner.egraph.total_number_of_nodes());

    let extractor = Extractor::new(&runner.egraph, ExprCost);
    let best_expr = extractor.extract(&root, &runner.egraph);
    let best_cost = ExprCost.cost_rec(&best_expr);
    // print!("Best expression: {}, cost: {}\n", best_expr, best_cost);
}

fn mm(n: usize) {
    let mut m = "(mat)".to_string();
    for _ in 1..=n {
        m = format!("(* {m} (mat))");
    }
    let mm = RecExpr::<Linalg>::parse(&m).unwrap();
    // println!("MM{n}: {}, cost: {}", mm, LinalgCost.cost_rec(&mm).cost);

    let rules: Vec<Rewrite<Linalg, ()>> = vec![
        Rewrite::new("assoc1", "(* (* ?a ?b) ?c)", "(* ?a (* ?b ?c))"),
        Rewrite::new("assoc2", "(* ?a (* ?b ?c))", "(* (* ?a ?b) ?c)"),
    ];

    let mut egraph = EGraph::<Linalg, ()>::default();
    let root = egraph.add_expr(mm);
    // println!("Initial e-graph size: {}", egraph.total_number_of_nodes());

    let mut runner = Runner::<Linalg, ()>::default().with_egraph(egraph);
    let _report = runner.run(&rules);
    // println!("Final e-graph size: {}", runner.egraph.total_number_of_nodes());

    let extractor = Extractor::new(&runner.egraph, LinalgCost);
    let best_expr = extractor.extract(&root, &runner.egraph);
    let _best_cost = LinalgCost.cost_rec(&best_expr);
    // println!("Best expression: {}, cost: {}\n", best_expr, best_cost.cost);
}

fn average_ms(times: &Vec<u128>) -> f64 {
    if times.is_empty() {
        return 0.0;
    }
    let sum: f64 = times.iter().map(|&t| t as f64).sum();
    (sum / times.len() as f64) / 1000.0
}

fn main() {
    // Parse the number of seconds to run from the first CLI argument; default to 60 if not provided/invalid.
    let args: Vec<String> = env::args().collect();
    let total_time: u64 = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(60);

    let mut results: Vec<(String, f64)> = Vec::new();

    // Benchmark poly
    let poly_sizes = [5usize, 6];
    for &degree in poly_sizes.iter() {
        let mut times: Vec<u128> = Vec::new();
        let start = std::time::Instant::now();
        let end = start + std::time::Duration::from_secs(total_time);
        while std::time::Instant::now() < end {
            let time_start = std::time::Instant::now();
            poly(degree);
            let duration = std::time::Instant::now().duration_since(time_start);
            times.push(duration.as_micros());
        }
        results.push((format!("poly{}", degree), average_ms(&times)));
    }

    // Benchmark matrix-chain multiplications
    let mm_sizes = [40usize, 80];
    for &n in mm_sizes.iter() {
        let mut times: Vec<u128> = Vec::new();
        let start = std::time::Instant::now();
        let end = start + std::time::Duration::from_secs(total_time);
        while std::time::Instant::now() < end {
            let time_start = std::time::Instant::now();
            mm(n);
            let duration = std::time::Instant::now().duration_since(time_start);
            times.push(duration.as_micros());
        }
        results.push((format!("mm{}", n), average_ms(&times)));
    }

    // Print CSV output
    println!("benchmark,avg_ms");
    for (name, avg_ms) in results {
        println!("{},{}", name, avg_ms);
    }
}
