#!/usr/bin/env python3
import argparse
import csv
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Dict, Tuple

def run_cargo_and_capture(project_path: Path, seconds: int) -> str:
    """Run `cargo run --release -- <seconds>` in project_path and return stdout (CSV text)."""
    cmd = ["cargo", "run", "--release", "--", str(seconds)]
    try:
        proc = subprocess.run(
            cmd,
            cwd=str(project_path),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        sys.stderr.write(f"\n[ERROR] Failed running {' '.join(cmd)} in {project_path}\n")
        sys.stderr.write(e.stderr)
        raise
    return proc.stdout

def run_stack_and_capture_csv(project_path: Path, seconds: int, out_csv_path: Path) -> None:
    """Run `stack run -- --time-limit <seconds> --csv <out_csv_path>` in project_path."""
    cmd = [
        "stack", "run", "--",
        "--time-limit", str(seconds),
        "--csv", str(out_csv_path)
    ]
    try:
        proc = subprocess.run(
            cmd,
            cwd=str(project_path),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        sys.stderr.write(f"\n[ERROR] Failed running {' '.join(cmd)} in {project_path}\n")
        sys.stderr.write(e.stderr)
        raise

def _strip_hegg_prefix(name: str) -> str:
    idx = name.find("/rewrite ")
    if idx != -1:
        return name[idx + len("/rewrite "):]
    return name

def load_or_run_rust(project_name: str, project_path: Path, seconds: int, cache_dir: Path, force: bool) -> Path:
    """Return path to cached CSV for this project/seconds, running if needed."""
    cache_dir.mkdir(parents=True, exist_ok=True)
    cache_file = cache_dir / f"{project_name}_s{seconds}.csv"
    if cache_file.exists() and not force:
        print(f"[cache] Using existing {cache_file}")
        return cache_file

    print(f"[run] Running {project_name} for {seconds}s per benchmark in {project_path} ...")
    stdout = run_cargo_and_capture(project_path, seconds)
    # Basic sanity check: should look like CSV with header benchmark,avg_ms
    if not stdout.strip().startswith("benchmark,avg_ms"):
        print("[warn] Output doesn't start with 'benchmark,avg_ms'; saving anyway.")
    cache_file.write_text(stdout, encoding="utf-8")
    print(f"[save] Wrote {cache_file}")
    return cache_file

def load_or_run_hegg(project_path: Path, seconds: int, cache_dir: Path, force: bool) -> Path:
    cache_dir.mkdir(parents=True, exist_ok=True)
    cache_file = cache_dir / f"hegg_s{seconds}.csv"
    if cache_file.exists() and not force:
        print(f"[cache] Using existing {cache_file}")
        return cache_file

    print(f"[run] Running hegg for {seconds}s per benchmark in {project_path} ...")
    with tempfile.TemporaryDirectory() as td:
        tmp_csv = Path(td) / "criterion_raw.csv"
        run_stack_and_capture_csv(project_path, seconds, tmp_csv)
        data = read_criterion_csv_to_dict(tmp_csv)

    # Write normalized 2-col CSV
    with cache_file.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["benchmark", "avg_ms"])
        for name in sorted(data.keys()):
            clean_name = _strip_hegg_prefix(name)
            writer.writerow([clean_name, f"{data[name]:.13f}"])
    print(f"[save] Wrote {cache_file}")
    return cache_file

def read_project_csv(csv_path: Path) -> Dict[str, float]:
    """Read 'benchmark,avg_ms' CSV to dict benchmark->avg_ms."""
    out: Dict[str, float] = {}
    with csv_path.open(newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        if "benchmark" not in reader.fieldnames or "avg_ms" not in reader.fieldnames:
            raise ValueError(f"CSV {csv_path} missing required columns: {reader.fieldnames}")
        for row in reader:
            name = row["benchmark"].strip()
            try:
                val = float(row["avg_ms"].strip())
            except ValueError:
                # Skip non-numeric rows
                continue
            out[name] = val
    return out

def _sanitize_benchmark_name(name: str) -> str:
    name = name.strip().strip('"')
    if name.startswith("rewrite "):
        name = name[len("rewrite "):]
    return name

def read_criterion_csv_to_dict(csv_path: Path) -> Dict[str, float]:
    """Read Criterion's CSV and return dict benchmark->avg_ms (using Mean (ps))."""
    out: Dict[str, float] = {}
    with csv_path.open(newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        # Criterion typically exposes columns: Name, Mean (ps), ...
        if "Name" not in reader.fieldnames:
            raise ValueError(f"CSV {csv_path} missing 'Name' column: {reader.fieldnames}")
        # Try preferred 'Mean (ps)'; fall back to 'Mean (ns)' if present
        ps_col = None
        if "Mean (ps)" in reader.fieldnames:
            ps_col = "Mean (ps)"
            scale = 1e12  # ps per second
        elif "Mean (ns)" in reader.fieldnames:
            ps_col = "Mean (ns)"
            scale = 1e9
        else:
            # As a last resort, try a column literally named "Mean"
            if "Mean" in reader.fieldnames:
                ps_col = "Mean"
                # Assume seconds; convert to ms
                for row in reader:
                    name = _sanitize_benchmark_name(row["Name"]) 
                    try:
                        sec = float(row[ps_col])
                    except ValueError:
                        continue
                    out[name] = sec * 1000.0
                return out
            raise ValueError(f"CSV {csv_path} missing a Mean column (got {reader.fieldnames})")
        for row in reader:
            name = _sanitize_benchmark_name(row["Name"]) 
            try:
                mean_units = float(row[ps_col])
            except ValueError:
                continue
            # Convert to milliseconds
            ms = (mean_units / scale) * 1000.0
            out[name] = ms
    return out

def write_combined_csv(out_path: Path, merged_rows: Dict[str, Dict[str, float]], project_columns: Tuple[str, ...]):
    """Write a combined CSV with 'benchmark' + project columns."""
    fieldnames = ["benchmark", *project_columns]
    with out_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for bench in sorted(merged_rows.keys()):
            row = {"benchmark": bench}
            for col in project_columns:
                val = merged_rows[bench].get(col)
                row[col] = f"{val:.3f}" if isinstance(val, float) else ""
            writer.writerow(row)
    print(f"[save] Combined CSV -> {out_path}")

def main():
    p = argparse.ArgumentParser(description="Run benchmark projects and combine results.")
    p.add_argument("--slotted-path", type=Path, default=Path("slotted"), help="Path to the 'slotted' project directory (contains Cargo.toml)" )
    p.add_argument("--egg-path", type=Path, default=Path("egg"), help="Path to the 'egg' project directory (contains Cargo.toml)" )
    p.add_argument("--hegg-path", type=Path, default=Path("hegg-bench"),
                   help="Path to the 'hegg' (Haskell/stack) project directory")
    p.add_argument("--seconds", type=int, default=60, help="Seconds to run each benchmark for (passed to the benchmark programs)" )
    p.add_argument("--cache-dir", type=Path, default=Path(".bench_cache"), help="Directory to store per-project CSV outputs" )
    p.add_argument("--out", type=Path, default=Path("results.csv"), help="Output CSV path for merged results" )
    p.add_argument("--force", action="store_true", help="Re-run projects even if cached CSV exists" )
    args = p.parse_args()

    # Resolve absolute paths early
    slotted_path = args.slotted_path.resolve()
    egg_path = args.egg_path.resolve()
    hegg_path = args.hegg_path.resolve()
    cache_dir = args.cache_dir.resolve()
    out_path = args.out.resolve()

    # Validate paths
    for name, path in (("slotted", slotted_path), ("egg", egg_path)):
        cargo_toml = path / "Cargo.toml"
        if not cargo_toml.exists():
            p.error(f"{name} path does not look like a Cargo project (missing Cargo.toml): {path}")

    hegg_stack = hegg_path / "stack.yaml"
    if not hegg_stack.exists():
        p.error(f"hegg path does not look like a Stack project (missing stack.yaml): {hegg_path}")

    # Per-project run (or load from cache)
    slotted_csv = load_or_run_rust("slotted", slotted_path, args.seconds, cache_dir, args.force)
    egg_csv = load_or_run_rust("egg", egg_path, args.seconds, cache_dir, args.force)
    hegg_csv = load_or_run_hegg(hegg_path, args.seconds, cache_dir, args.force)

    # Read individual CSVs (all normalized to benchmark,avg_ms)
    slotted_data = read_project_csv(slotted_csv)
    egg_data = read_project_csv(egg_csv)
    hegg_data = read_project_csv(hegg_csv)

    # Merge by benchmark name
    all_benchmarks = set(slotted_data.keys()) | set(egg_data.keys()) | set(hegg_data.keys())
    merged: Dict[str, Dict[str, float]] = {b: {} for b in all_benchmarks}
    for b, v in slotted_data.items():
        merged[b]["slotted"] = v
    for b, v in egg_data.items():
        merged[b]["egg"] = v
    for b, v in hegg_data.items():
        merged[b]["hegg"] = v

    # Write combined
    write_combined_csv(out_path, merged, ("slotted", "egg", "hegg"))

if __name__ == "__main__":
    main()
