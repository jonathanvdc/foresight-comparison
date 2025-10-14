# Foresight Comparison Benchmarks

This repository runs performance comparisons across multiple equality-saturation systems including **Foresight**, **egg**, **egglog**, **hegg**, and **slotted** inside a reproducible Docker container.
The benchmarks measure runtime (ms/op) for a shared suite of programs: **matrix multiplication (mm)** and **polynomial (poly)**.

## Docker Environment

The Dockerfile defines a complete benchmarking environment:
- Installs Rust, Cargo, Stack, Haskell, Python 3, and SBT with OpenJDK 21.
- Builds `egglog` and `egglog-experimental`.
- Runs all benchmarks through `run_benchmarks.py`.

## Running the Benchmarks

To build and run benchmarks locally:
```bash
docker build -t foresight-comparison -f dockerfile .
docker run --rm -e BENCH_SECONDS=10 -e FORESIGHT_THREAD_COUNTS="1 2" foresight-comparison > results.csv
cat results.csv
```

The `BENCH_SECONDS` and `FORESIGHT_THREAD_COUNTS` environment variables are configurable.

| Variable | Description | Default |
|-----------|-------------|----------|
| `BENCH_SECONDS` | Duration (in seconds) to run each benchmark | `60` |
| `FORESIGHT_THREAD_COUNTS` | Space-separated list of Foresight JMH thread counts | `"1"` |

Example:
```bash
docker run --rm \
  -e BENCH_SECONDS=10 \
  -e FORESIGHT_THREAD_COUNTS="1 2 4 8" \
  foresight-comparison > results.csv
```

## Benchmark Output

Each benchmark writes a unified CSV `results.csv` with average runtime (in ms/op) per system and configuration.

The script automatically merges results from:
- **Rust crates:** `slotted`, `egg`
- **Haskell Criterion:** `hegg`
- **Command-line tools:** `egglog`
- **Scala JMH (SBT):** `foresight`

## Continuous Integration

The included GitHub Actions workflow automatically builds the benchmarking environment and runs short-duration tests on every push.

## Extending Benchmarks

You can add new systems or workloads by editing:
- `run_benchmarks.py`: to add project runners or CSV parsing logic.
- `dockerfile`: to install additional dependencies or tools.
- JMH, Criterion, or Python benchmark sources in respective subdirectories.
