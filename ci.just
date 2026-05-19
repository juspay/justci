# Run all checks (build + flake check) concurrently with the binary smoke
# test, since run-check only depends on build (not flake-check). The
# [linux] [macos] attributes fan the pipeline out across both lanes: every
# reachable recipe gets a (recipe, platform) node, the local lane runs
# against the worktree, and the macos lane SSHs to whatever
# `~/.config/ci/hosts.json` maps `macos` to.
#
# [parallel] on `default` is what lets run-check start as soon as build
# finishes, in parallel with flake-check. Without it, the runner would
# chain run-check behind checks (and therefore behind flake-check too)
# even though run-check has no flake-check dependency.
[linux]
[macos]
[parallel]
[metadata("ci")]
default: checks run-check

# Fan out `build`, `flake-check`, and `fmt-check` to run in parallel.
[parallel]
checks: build flake-check fmt-check

# Build the package with Nix.
build:
    nix build --print-build-logs

# Run the flake's checks (formatting, hlint, etc. via haskell-flake).
flake-check:
    nix flake check --print-build-logs

# Fail the build if any Haskell source isn't fourmolu-formatted. The
# dev shell exposes fourmolu via haskell-flake; --mode check exits
# non-zero on any drift and prints a unified diff of what would change.
# Style settings live in `fourmolu.yaml` at the repo root.
fmt-check:
    nix develop -c fourmolu --mode check $(find src test app -name '*.hs')

# Smoke-test the dump-yaml subcommand. The pipeline root is `ci::default`,
# tagged [linux] [macos] [metadata("ci")] — so the fanout produces both
# lanes and every process key is `ci::<name>@<platform>`. Each platform's
# smoke verifies its own lane is present plus the structural invariants
# (--no-deps, restart: no, exit_on_skipped: false) for cross-lane failure
# tolerance. `dump-yaml` runs in `DumpRun` mode so it never prompts for
# hosts and never shells out to git — safe to invoke on the macos remote
# even when `~/.config/ci/hosts.json` there has no linux entry.
[linux]
run-check: build
    echo "Running on Linux $(uname -srm)"
    nix run . -- dump-yaml | tee /tmp/ci-out
    # Only assert the current platform's lane is present — each remote
    # has its own hosts.json and may not list the other platform, so a
    # cross-platform grep would fail on the remote's local dump.
    grep -qE '^  ci::default@x86_64-linux:' /tmp/ci-out
    grep -qE -- '--no-deps' /tmp/ci-out
    grep -qE "^      restart: 'no'$" /tmp/ci-out
    grep -qE '^      exit_on_skipped: false$' /tmp/ci-out

[macos]
run-check: build
    echo "Running on macOS $(sw_vers -productVersion)"
    nix run . -- dump-yaml | tee "${TMPDIR%/}/ci-out"
    grep -qE '^  ci::default@aarch64-darwin:' "${TMPDIR%/}/ci-out"
    grep -qE -- '--no-deps' "${TMPDIR%/}/ci-out"
    grep -qE "^      restart: 'no'$" "${TMPDIR%/}/ci-out"
    grep -qE '^      exit_on_skipped: false$' "${TMPDIR%/}/ci-out"
