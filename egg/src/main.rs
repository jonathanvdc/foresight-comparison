use std::fmt::Display;
use std::path;
use std::env;

use egg::Id;

use egg::*;

define_language! {
    enum Expr {
        "0" = Zero,
        "succ" = Succ(Id),
        Var(Symbol),
        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),
        "^" = Pow([Id; 2]),
    }
}

define_language! {
    enum Linalg {
        Cst(i64),
        "mat" = Mat([Id; 2]), // rows, cols
        "*" = Mul([Id; 2]),
    }
}

// set the cost of + to 10, * to 100 and ^ to 1000
struct ExprCost;
impl CostFunction<Expr> for ExprCost {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &Expr, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost
    {
        let op_cost = match enode {
            Expr::Add(_) => 10,
            Expr::Mul(_) => 100,
            Expr::Pow(_) => 1000,
            _ => 1
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
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

impl PartialOrd for DimAndCost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cost.cmp(&other.cost))
    }
}

struct LinalgCost; // cost is number of scalar multiplications, so for A (m x n) * B (n x p) it's m*n*p
impl CostFunction<Linalg> for LinalgCost {
    type Cost = DimAndCost;

    fn cost<C>(&mut self, enode: &Linalg, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost
    {
        match enode {
            Linalg::Cst(c) => DimAndCost { rows: *c, cols: *c, cost: 0 },
            Linalg::Mat([rows_id, cols_id]) => {
                let rows = costs(*rows_id).rows;
                let cols = costs(*cols_id).cols;
                DimAndCost { rows, cols, cost: 0 }
            },
            Linalg::Mul([a_id, b_id]) => {
                let a = costs(*a_id);
                let b = costs(*b_id);
                assert!(a.cols == b.rows);
                let cost = (a.rows * a.cols * b.cols) as usize;
                DimAndCost { rows: a.rows, cols: b.cols, cost: a.cost + b.cost + cost }
            }
        }
    }

}

// fn render_egraph<L: Language + Display, N: Analysis<L>>(
//     egraph: &EGraph<L, N>,
//     dir: &str,
//     name: &str,
// ) {
//     // render the e-graph as a dot file
//     let dot_filename = format!("{}/{}.dot", dir, name);
//     let png_filename = format!("{}/{}.png", dir, name);

//     let path = path::Path::new(&dot_filename);
//     egraph.dot().to_dot(path).expect("Couldn't write e-graph to file");

//     // render dot file into a png
//     std::process::Command::new("dot")
//         .arg("-Tpng")
//         .arg(&dot_filename)
//         .arg("-o")
//         .arg(&png_filename)
//         .output()
//         .expect("Couldn't render dot file to png");
// }

fn poly(n: usize) -> (usize, RecExpr<Expr>) {
    // Build a polynomial a0 + a1*x + a2*x^2 + ... + an*x^n
    let mut poly: RecExpr<Expr> = RecExpr::default();

    // Natural numbers 0..=n encoded as 0, succ(0), succ(succ(0)), ...
    let mut ns: Vec<Id> = Vec::with_capacity(n + 1);
    let c0 = poly.add(Expr::Zero);
    ns.push(c0);
    for _i in 1..=n {
        let prev = *ns.last().unwrap();
        ns.push(poly.add(Expr::Succ(prev)));
    }

    // Variable x
    let x = poly.add(Expr::Var(Symbol::from("x")));

    // Start with constant term a0
    let mut acc = poly.add(Expr::Var(Symbol::from("a0")));

    // Add higher-degree terms a_k * x^k
    for k in 1..=n {
        let ak = poly.add(Expr::Var(Symbol::from(format!("a{}", k))));
        let xk = poly.add(Expr::Pow([x, ns[k]]));
        let term = poly.add(Expr::Mul([ak, xk]));
        acc = poly.add(Expr::Add([term, acc]));
    }

    // Rewrites (same as before)
    let add_commutativity: Rewrite<Expr, ()> = rewrite!("add-comm"; "(+ ?a ?b)" => "(+ ?b ?a)");
    let mul_commutativity: Rewrite<Expr, ()> = rewrite!("mul-comm"; "(* ?a ?b)" => "(* ?b ?a)");
    let add_associativity1: Rewrite<Expr, ()> = rewrite!("add-assoc1"; "(+ (+ ?a ?b) ?c)" => "(+ ?a (+ ?b ?c))");
    let add_associativity2: Rewrite<Expr, ()> = rewrite!("add-assoc2"; "(+ ?a (+ ?b ?c))" => "(+ (+ ?a ?b) ?c)");
    let mul_associativity1: Rewrite<Expr, ()> = rewrite!("mul-assoc1"; "(* (* ?a ?b) ?c)" => "(* ?a (* ?b ?c))");
    let mul_associativity2: Rewrite<Expr, ()> = rewrite!("mul-assoc2"; "(* ?a (* ?b ?c))" => "(* (* ?a ?b) ?c)");
    let distributivity1: Rewrite<Expr, ()> = rewrite!("distrib1"; "(* ?x (+ ?a ?b))" => "(+ (* ?x ?a) (* ?x ?b))");
    let distributivity2: Rewrite<Expr, ()> = rewrite!("distrib2"; "(+ (* ?x ?a) (* ?x ?b))" => "(* ?x (+ ?a ?b))");
    let mul_id: Rewrite<Expr, ()> = rewrite!("mul-id"; "(* ?a (succ 0))" => "?a");
    let zero_exp: Rewrite<Expr, ()> = rewrite!("zero-exp"; "(^ ?a 0)" => "(succ 0)");
    let exp_def: Rewrite<Expr, ()> = rewrite!("exp-def"; "(^ ?a (succ ?n))" => "(* ?a (^ ?a ?n))");

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
    let root = egraph.add_expr(&poly);

    let runner = Runner::default()
        .with_egraph(egraph)
        .run(&rules);
    let new_egraph = runner.egraph;

    let extractor = Extractor::new(&new_egraph, ExprCost);
    extractor.find_best(root)
}

fn mm(n: usize) -> (DimAndCost, RecExpr<Linalg>){
    let mut mm: RecExpr<Linalg> = RecExpr::default();

    let c10 = mm.add(Linalg::Cst(10));
    let mut m = mm.add(Linalg::Mat([c10, c10])); // 10x10
    for _i in 1..=n {
        let mi = mm.add(Linalg::Mat([c10, c10])); // 10x10
        m = mm.add(Linalg::Mul([m, mi])); // 10x10 * ix10
    }

    let rules: Vec<Rewrite<Linalg, ()>> = rewrite!("assoc"; "(* (* ?a ?b) ?c)" <=> "(* ?a (* ?b ?c))");

    let mut egraph = EGraph::<Linalg, ()>::default();
    let root = egraph.add_expr(&mm);

    let runner = Runner::default()
        .with_egraph(egraph)
        .run(&rules);
    let new_egraph = runner.egraph;

    let extractor = Extractor::new(&new_egraph, LinalgCost);
    extractor.find_best(root)
}

fn average_ms(times: &[u128]) -> f64 {
    if times.is_empty() {
        return 0.0;
    }
    let total_micros: u128 = times.iter().copied().sum();
    (total_micros as f64 / times.len() as f64) / 1000.0
}

fn main() {
    // Parse the number of seconds to run from the first CLI argument; default to 60 if not provided/invalid.
    let args: Vec<String> = env::args().collect();
    let total_time: u64 = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(60);

    let mut results: Vec<(String, f64)> = Vec::new();

    // Benchmark poly
    let poly_sizes = [5usize, 6];
    for &n in poly_sizes.iter() {
        let mut times: Vec<u128> = Vec::new();
        let start = std::time::Instant::now();
        let end = start + std::time::Duration::from_secs(total_time);
        while std::time::Instant::now() < end {
            let time_start = std::time::Instant::now();
            let (_best_cost, _best_expr) = poly(n);
            let duration = std::time::Instant::now().duration_since(time_start);
            times.push(duration.as_micros());
        }
        results.push((format!("poly{}", n), average_ms(&times)));
    }

    // Benchmark matrix-chain multiplications
    let mm_sizes = [20usize, 40, 80]; // [3usize, 5, 10, 20, 40, 80];
    for &n in mm_sizes.iter() {
        let mut times: Vec<u128> = Vec::new();
        let start = std::time::Instant::now();
        let end = start + std::time::Duration::from_secs(total_time);
        while std::time::Instant::now() < end {
            let time_start = std::time::Instant::now();
            let (_best_dim_cost, _best_expr) = mm(n);
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
