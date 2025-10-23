#!/usr/bin/env python3
import argparse
import csv
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Dict, Tuple
from typing import List

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

def run_sbt_jmh_and_capture_csv(project_path: Path, thread_count: int, seconds: int, out_csv_path: Path, mutable_egraph: bool, sbt_jvm_opts: List[str], jmh_jvm_opts: List[str]) -> None:
    """
    Run sbt JMH in `project_path` and write a CSV (JMH's reporter CSV) to `out_csv_path`.
    We set average time mode in milliseconds, 1 fork, measurement time per iteration ~= seconds.
    """
    # Use sbt to run the benchmarks in the 'benchmarks' subproject if present.
    # We ask JMH to export CSV via -rf csv -rff <file>.
    # We set -bm avgt -tu ms so Score is ms/op; -i 1 measurement iteration with -r <seconds>.
    jmh_args = [
        "-i", "1",
        "-wi", "1",
        "-f", "1",
        "-bm", "avgt",
        "-tu", "ms",
        "-rf", "csv",
        "-rff", str(out_csv_path),
        "-r", str(seconds),
        "-p", f"threadCount={thread_count}",
        "-p", f"mutableEGraph={'true' if mutable_egraph else 'false'}",
        ".*(MatmulBenchmarks\\.nmm|PolyBenchmarks\\.polynomial).*"
    ]
    # If we want to pass JVM args (e.g., -Xmx) to the JMH forked JVM, append them via -jvmArgsAppend
    if jmh_jvm_opts:
        jvm_args_str = " ".join(jmh_jvm_opts)
        jmh_args.extend(["-jvmArgsAppend", f'"{jvm_args_str}"'])
        
    # Build sbt launcher JVM opts (each must be prefixed with -J for sbt)
    sbt_prefix = ["sbt"] + [f"-J{opt}" for opt in sbt_jvm_opts]
    def _try(cmd: List[str]) -> None:
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
            raise RuntimeError(f"sbt JMH failed with command: {' '.join(cmd)}\nSTDOUT:\n{e.stdout}\nSTDERR:\n{e.stderr}") from e
    # Build the sbt command strings with properly quoted JMH args.
    quoted = " ".join(jmh_args)
    _try(sbt_prefix + [f'benchmarks/jmh:run {quoted}'])

def run_egglog_and_capture_csv(project_path: Path, seconds: int, egglog_bin: str, egglog_exp_bin: str) -> str:
    """Run `python bench.py --seconds <seconds> [--egglog <egglog_bin>] [--egglog-experimental <egglog_exp_bin>]` in project_path and return stdout (CSV)."""
    bench_py = project_path / "bench.py"
    if not bench_py.exists():
        raise FileNotFoundError(f"egglog bench script not found: {bench_py}")

    cmd = [sys.executable, str(bench_py), "--seconds", str(seconds)]
    if egglog_bin:
        cmd.extend(["--egglog", egglog_bin])
    if egglog_exp_bin:
        cmd.extend(["--egglog-experimental", egglog_exp_bin])

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

def _strip_hegg_prefix(name: str) -> str:
    idx = name.find("/rewrite ")
    if idx != -1:
        return name[idx + len("/rewrite "):]
    return name

def load_or_run_egglog(project_path: Path, seconds: int, cache_dir: Path, force: bool, egglog_bin: str, egglog_exp_bin: str) -> Path:
    """Return path to cached CSV for egglog/bench.py, running if needed."""
    cache_dir.mkdir(parents=True, exist_ok=True)
    cache_file = cache_dir / f"egglog_s{seconds}.csv"
    if cache_file.exists() and not force:
        print(f"[cache] Using existing {cache_file}")
        return cache_file

    print(f"[run] Running egglog for {seconds}s per benchmark in {project_path} ...")
    stdout = run_egglog_and_capture_csv(project_path, seconds, egglog_bin, egglog_exp_bin)
    if not stdout.strip().startswith("benchmark,avg_ms"):
        print("[warn] egglog output doesn't start with 'benchmark,avg_ms'; saving anyway.")
    cache_file.write_text(stdout, encoding="utf-8")
    print(f"[save] Wrote {cache_file}")
    return cache_file

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

def load_or_run_foresight(project_path: Path, seconds: int, thread_count: int, cache_dir: Path, force: bool, mutable_egraph: bool, sbt_jvm_opts: List[str], jmh_jvm_opts: List[str]) -> Path:
    """
    Return path to cached CSV for Foresight JMH at a given thread_count and mutable_egraph setting, running if needed.
    The CSV saved is a normalized two-column file: benchmark,avg_ms
    """
    cache_dir.mkdir(parents=True, exist_ok=True)
    me_tag = "mut" if mutable_egraph else "imm"
    cache_file = cache_dir / f"foresight_{me_tag}_t{thread_count}_s{seconds}.csv"
    if cache_file.exists() and not force:
        print(f"[cache] Using existing {cache_file}")
        return cache_file

    print(f"[run] Running Foresight JMH (mutableEGraph={'true' if mutable_egraph else 'false'}) with threadCount={thread_count} for {seconds}s per benchmark in {project_path} ...")
    with tempfile.TemporaryDirectory() as td:
        jmh_csv = Path(td) / "jmh.csv"
        run_sbt_jmh_and_capture_csv(project_path, thread_count, seconds, jmh_csv, mutable_egraph, sbt_jvm_opts, jmh_jvm_opts)
        # Parse JMH CSV and normalize to 'benchmark,avg_ms'
        data = read_jmh_csv_to_dict(jmh_csv)
    with cache_file.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["benchmark", "avg_ms"])
        for name in sorted(data.keys()):
            writer.writerow([name, f"{data[name]:.13f}"])
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

def read_jmh_csv_to_dict(csv_path: Path) -> Dict[str, float]:
    """
    Read JMH's CSV (-rf csv) and return dict benchmark->avg_ms.
    Assumes -bm avgt and -tu ms so Score is in ms/op.
    Restricts to:
      - MatmulBenchmarks.nmm        -> mm{size}
      - PolyBenchmarks.polynomial   -> poly{size}
    Uses 'Param: size' (case-insensitive) to derive {size}.
    """
    out: Dict[str, float] = {}
    with csv_path.open(newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        # Normalize header names to lowercase for robust matching
        headers_lc = { (h or "").strip().lower(): (h or "") for h in (reader.fieldnames or []) }
        required = {"benchmark", "mode", "score", "unit"}
        if not required.issubset(set(headers_lc.keys())):
            raise ValueError(f"JMH CSV {csv_path} missing required columns; got {reader.fieldnames}")

        # Find size column name case-insensitively (prefer exact 'Param: size' if present)
        size_header = None
        for cand in ("param: size", "param:size", "size"):
            if cand in headers_lc:
                size_header = headers_lc[cand]
                break
        # If not found by simple names, scan for any header that endswith ': size'
        if size_header is None:
            for k_lc, k in headers_lc.items():
                if k_lc.endswith(": size"):
                    size_header = k
                    break

        for row in reader:
            mode = (row.get(headers_lc["mode"]) or "").strip().lower()
            unit = (row.get(headers_lc["unit"]) or "").strip().lower()
            if mode != "avgt" or not unit.startswith("ms/op"):
                continue

            bench_full = (row.get(headers_lc["benchmark"]) or "").strip()
            parts = bench_full.split(".")
            short = ".".join(parts[-2:]) if len(parts) >= 2 else bench_full

            if short not in ("MatmulBenchmarks.nmm", "PolyBenchmarks.polynomial"):
                continue

            size_val = ""
            if size_header:
                size_val = (row.get(size_header) or "").strip()
            if not size_val:
                # No size -> skip; we require size to build the mm{size}/poly{size} key
                continue

            try:
                score = float(row[headers_lc["score"]])
            except (ValueError, TypeError, KeyError):
                continue

            key = f"mm{size_val}" if short == "MatmulBenchmarks.nmm" else f"poly{size_val}"
            out[key] = score
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
    p.add_argument("--egglog-path", type=Path, default=Path("egglog"),
                   help="Path to the 'egglog' project directory (contains bench.py)")
    p.add_argument("--egglog-bin", type=str, default="egglog",
                   help="Path or name of the egglog binary to pass through to egglog/bench.py")
    p.add_argument("--egglog-experimental-bin", type=str, default="egglog-experimental",
                   help="Path or name of the egglog-experimental binary to pass through to egglog/bench.py")
    p.add_argument("--hegg-path", type=Path, default=Path("hegg-bench"),
                   help="Path to the 'hegg' (Haskell/stack) project directory")
    p.add_argument("--foresight-path", type=Path, default=Path("foresight"),
                   help="Path to the Foresight SBT project directory")
    p.add_argument("--foresight-thread-counts", type=int, nargs="+", default=[1],
                   help="List of Foresight threadCount values to benchmark (JMH -p threadCount=...)")
    p.add_argument("--foresight-mutable-egraph", type=str, nargs="+", default=["true", "false"], choices=["true", "false"],
                   help="List of mutableEGraph settings to benchmark for Foresight (true/false)")
    p.add_argument("--sbt-jvm-opts", type=str, nargs="+", default=[],
                   help="Extra JVM options for the sbt launcher JVM (e.g., -Xms16g -Xmx128g or -XX:MaxRAMPercentage=70)")
    p.add_argument(
        "--jmh-jvm-opts",
        type=str,
        nargs="+",
        default=["-XX:MaxRAMPercentage=50"],
        help="Extra JVM options for the JMH forked JVM (e.g., -Xms16g -Xmx128g or -XX:MaxRAMPercentage=70). Defaults to 50%% of physical RAM via -XX:MaxRAMPercentage=50."
    )
    p.add_argument("--seconds", type=int, default=60, help="Seconds to run each benchmark for (passed to the benchmark programs)" )
    p.add_argument("--cache-dir", type=Path, default=Path(".bench_cache"), help="Directory to store per-project CSV outputs" )
    p.add_argument("--out", type=Path, default=Path("results.csv"), help="Output CSV path for merged results" )
    p.add_argument("--force", action="store_true", help="Re-run projects even if cached CSV exists" )
    args = p.parse_args()

    # Default JMH forked JVM heap to ~50% of physical RAM if not overridden.
    # This uses JDK 10+'s MaxRAMPercentage to size the heap relative to available memory (or container limit).
    if not args.jmh_jvm_opts:
        args.jmh_jvm_opts = ["-XX:MaxRAMPercentage=50"]
        print("[info] JMH JVM: defaulting to -XX:MaxRAMPercentage=50 (≈50% of physical RAM). Use --jmh-jvm-opts to override.")
        try:
            import psutil
            total_bytes = psutil.virtual_memory().total
            approx_heap_bytes = total_bytes * 0.5
            total_gb = total_bytes / (1024**3)
            heap_gb = approx_heap_bytes / (1024**3)
            print(f"[info] Detected physical RAM: {total_gb:.2f} GiB -> JMH heap ≈ {heap_gb:.2f} GiB (50%)")
        except Exception as e:
            print(f"[warn] Could not determine physical RAM for heap estimate: {e}")

    # Resolve absolute paths early
    slotted_path = args.slotted_path.resolve()
    egg_path = args.egg_path.resolve()
    egglog_path = args.egglog_path.resolve()
    hegg_path = args.hegg_path.resolve()
    foresight_path = args.foresight_path.resolve()
    cache_dir = args.cache_dir.resolve()
    out_path = args.out.resolve()

    # Validate paths
    for name, path in (("slotted", slotted_path), ("egg", egg_path)):
        cargo_toml = path / "Cargo.toml"
        if not cargo_toml.exists():
            p.error(f"{name} path does not look like a Cargo project (missing Cargo.toml): {path}")

    egglog_bench = egglog_path / "bench.py"
    if not egglog_bench.exists():
        p.error(f"egglog path does not look like the expected project (missing bench.py): {egglog_path}")

    hegg_stack = hegg_path / "stack.yaml"
    if not hegg_stack.exists():
        p.error(f"hegg path does not look like a Stack project (missing stack.yaml): {hegg_path}")

    foresight_build = foresight_path / "build.sbt"
    if not foresight_build.exists():
        p.error(f"foresight path does not look like an SBT project (missing build.sbt): {foresight_path}")

    # Per-project run (or load from cache)
    slotted_csv = load_or_run_rust("slotted", slotted_path, args.seconds, cache_dir, args.force)
    egg_csv = load_or_run_rust("egg", egg_path, args.seconds, cache_dir, args.force)
    egglog_csv = load_or_run_egglog(egglog_path, args.seconds, cache_dir, args.force, args.egglog_bin, args.egglog_experimental_bin)
    hegg_csv = load_or_run_hegg(hegg_path, args.seconds, cache_dir, args.force)

    foresight_csvs = []  # list of tuples: (tc, mutable_egraph_bool, csv_path)
    me_values: List[bool] = [ (v.lower() == "true") for v in args.foresight_mutable_egraph ]
    for tc in args.foresight_thread_counts:
        for me in me_values:
            csv_path = load_or_run_foresight(foresight_path, args.seconds, tc, cache_dir, args.force, me, args.sbt_jvm_opts, args.jmh_jvm_opts)
            foresight_csvs.append((tc, me, csv_path))

    # Read individual CSVs (all normalized to benchmark,avg_ms)
    slotted_data = read_project_csv(slotted_csv)
    egg_data = read_project_csv(egg_csv)
    egglog_data = read_project_csv(egglog_csv)
    hegg_data = read_project_csv(hegg_csv)

    foresight_dicts = []  # list of tuples: (tc, me_bool, dict)
    for tc, me, path in foresight_csvs:
        foresight_dicts.append((tc, me, read_project_csv(path)))

    # Merge by benchmark name
    all_benchmarks = set(slotted_data.keys()) | set(egg_data.keys()) | set(hegg_data.keys()) | set(egglog_data.keys())
    merged: Dict[str, Dict[str, float]] = {b: {} for b in all_benchmarks}
    for b, v in slotted_data.items():
        merged[b]["slotted"] = v
    for b, v in egg_data.items():
        merged[b]["egg"] = v
    for b, v in hegg_data.items():
        merged[b]["hegg"] = v
    for b, v in egglog_data.items():
        merged[b]["egglog"] = v

    for tc, me, d in foresight_dicts:
        col = f"foresight_{'mut' if me else 'imm'}_t{tc}"
        for b, v in d.items():
            merged.setdefault(b, {})
            merged[b][col] = v

    # Build the ordered list of Foresight columns, preserving the order of generation
    foresight_cols: Tuple[str, ...] = tuple(
        f"foresight_{'mut' if me else 'imm'}_t{tc}" for (tc, me, _path) in foresight_csvs
    )
    write_combined_csv(out_path, merged, ("slotted", "egg", "hegg", "egglog", *foresight_cols))

if __name__ == "__main__":
    main()
