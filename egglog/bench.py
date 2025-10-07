import subprocess
import time
import statistics
import os
import argparse
import sys

# Defaults, can be overridden via CLI
DEFAULT_EGGLOG = "egglog"
DEFAULT_EGGLOG_EXP = "egglog-experimental"


def parse_args():
    parser = argparse.ArgumentParser(
        description="Run egglog benchmarks and emit CSV of average run times."
    )
    parser.add_argument(
        "--egglog",
        default=DEFAULT_EGGLOG,
        help="Path or name of the egglog binary (default: 'egglog')",
    )
    parser.add_argument(
        "--egglog-experimental",
        dest="egglog_exp",
        default=DEFAULT_EGGLOG_EXP,
        help="Path or name of the egglog-experimental binary (default: 'egglog-experimental')",
    )
    parser.add_argument(
        "--seconds",
        type=int,
        default=60,
        help="Seconds to run each benchmark for (default: 60)",
    )
    return parser.parse_args()


def run_benchmark(executable: str, filename: str, duration: int) -> float:
    """Run a single benchmark file repeatedly for up to `duration` seconds.
    Returns the mean time per iteration in milliseconds.
    """
    start = time.time()
    times = []
    reported_unexpected = False
    try:
        while time.time() - start < duration:
            proc = subprocess.run(
                [executable, filename],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                text=True,
                check=False,
            )
            if proc.returncode != 0 or not proc.stdout:
                # Report error to stderr and stop using this benchmark
                sys.stderr.write(
                    f"Error: command '{executable} {filename}' exited with code {proc.returncode}.\n"
                )
                sys.exit(1)

            last_line = proc.stdout.strip().split("\n")[-1]
            try:
                # Expect format like: "time: 123.45ms"
                time_ms = float(last_line.split(": ")[1].replace("ms", ""))
            except Exception:
                time_ms = time.time() - start  # Fallback to elapsed time
                if not reported_unexpected:
                    reported_unexpected = True
                    sys.stderr.write(
                        f"Warning: unexpected output from '{executable} {filename}': '{last_line}'\n"
                    )

            times.append(time_ms)
    except FileNotFoundError:
        sys.stderr.write(f"Error: executable '{executable}' not found.\n")
        sys.exit(1)

    if not times:
        sys.exit(1)
    return statistics.mean(times)


def bench_name_from_filename(fname: str) -> str:
    """Produce a concise benchmark name from a filename.
    Examples:
      'poly.egg' -> 'poly'
      '5mm.egg'  -> 'mm5' (swap order to match 'mm40' style)
      'poly6.egg' -> 'poly6'
    """
    base = os.path.basename(fname)
    stem = base[:-4] if base.endswith(".egg") else base
    # If it looks like '<num>mm', rename to 'mm<num>'
    if stem.endswith("mm") and stem[:-2].isdigit():
        return f"mm{stem[:-2]}"
    return stem


def main():
    args = parse_args()

    results = []  # list of (name, avg_ms)

    # Polynomial benchmark sizes, use standard egglog
    poly_sizes = [5]
    for size in poly_sizes:
        fname = f"poly{size}.egg"
        avg = run_benchmark(args.egglog, fname, args.seconds)
        results.append((bench_name_from_filename(fname), avg))

    # Matrix multiply sizes, use experimental egglog
    for n in [40, 80]:
        fname = f"{n}mm.egg"
        avg = run_benchmark(args.egglog_exp, fname, args.seconds)
        results.append((bench_name_from_filename(fname), avg))

    # Emit CSV to stdout (only)
    print("benchmark,avg_ms")
    for name, avg in results:
        print(f"{name},{avg}")


if __name__ == "__main__":
    main()