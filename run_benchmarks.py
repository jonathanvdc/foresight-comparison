
#!/usr/bin/env python3
import argparse
import csv
import os
import subprocess
import sys
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

def load_or_run(project_name: str, project_path: Path, seconds: int, cache_dir: Path, force: bool) -> Path:
    """Return path to cached CSV for this project/seconds, running if needed."""
    cache_dir.mkdir(parents=True, exist_ok=True)
    cache_file = cache_dir / f"{project_name}_s{seconds}.csv"
    if cache_file.exists() and not force:
        print(f"[cache] Using existing {cache_file}")
        return cache_file

    print(f"[run] Running {project_name} for {seconds}s in {project_path} ...")
    stdout = run_cargo_and_capture(project_path, seconds)
    # Basic sanity check: should look like CSV with header benchmark,avg_ms
    if not stdout.strip().startswith("benchmark,avg_ms"):
        print("[warn] Output doesn't start with 'benchmark,avg_ms'; saving anyway.")
    cache_file.write_text(stdout, encoding="utf-8")
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
    p.add_argument("--seconds", type=int, default=60, help="Seconds to run each benchmark for (passed to the benchmark programs)" )
    p.add_argument("--cache-dir", type=Path, default=Path(".bench_cache"), help="Directory to store per-project CSV outputs" )
    p.add_argument("--out", type=Path, default=Path("results.csv"), help="Output CSV path for merged results" )
    p.add_argument("--force", action="store_true", help="Re-run projects even if cached CSV exists" )
    args = p.parse_args()

    # Resolve absolute paths early
    slotted_path = args.slotted_path.resolve()
    egg_path = args.egg_path.resolve()
    cache_dir = args.cache_dir.resolve()
    out_path = args.out.resolve()

    # Validate paths
    for name, path in (("slotted", slotted_path), ("egg", egg_path)):
        cargo_toml = path / "Cargo.toml"
        if not cargo_toml.exists():
            p.error(f"{name} path does not look like a Cargo project (missing Cargo.toml): {path}")

    # Per-project run (or load from cache)
    slotted_csv = load_or_run("slotted", slotted_path, args.seconds, cache_dir, args.force)
    egg_csv = load_or_run("egg", egg_path, args.seconds, cache_dir, args.force)

    # Read individual CSVs
    slotted_data = read_project_csv(slotted_csv)
    egg_data = read_project_csv(egg_csv)

    # Merge by benchmark name
    all_benchmarks = set(slotted_data.keys()) | set(egg_data.keys())
    merged: Dict[str, Dict[str, float]] = {b: {} for b in all_benchmarks}
    for b, v in slotted_data.items():
        merged[b]["slotted"] = v
    for b, v in egg_data.items():
        merged[b]["egg"] = v

    # Write combined
    write_combined_csv(out_path, merged, ("slotted", "egg"))

if __name__ == "__main__":
    main()
