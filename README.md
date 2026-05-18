# ci

A Haskell pipeline runner: translates a `just` recipe graph into a [process-compose](https://f1bonacc1.github.io/process-compose/) DAG and drives it. Sibling recipes keep running after one fails; the final exit code is derived from a per-node outcome map that the central observer accumulates, not from process-compose's own exit. In strict mode, posts per-node GitHub commit statuses live as the pipeline runs.

The pipeline root is the recipe annotated `[metadata("ci")]` — exactly one across the justfile and its submodules (zero or multiple is a startup error). Its reachable dependency subgraph becomes the pipeline; submodule recipes appear under their fully-qualified `mod::recipe` names. Each (recipe, platform) pair becomes a separate process-compose node — see *Platform fanout* below.

```
just --dump → root → reachable subgraph → fan out per platform → process-compose YAML → run
```

## Modes

Gated on the `CI` environment variable:

| Mode | Trigger | Tree | Status posts | Runtime files |
|---|---|---|---|---|
| Local | `CI` unset | live working tree | none | `.ci/pc.log`, `.ci/pc.sock` |
| Strict | `CI=true` | `git worktree` pinned to HEAD | `<recipe>@<platform>` per transition | `.ci/pc.log`, `.ci/pc.sock`, `.ci/worktree/`, `.ci/<sha>/<platform>/<recipe>.log` |

Strict mode refuses to run if the working tree is dirty — the SHA on the green check must exactly match the bytes tested. A central observer subscribes to process-compose's `/process/states/ws` stream over a Unix domain socket; in strict mode it posts a status (`pending`, then `success`/`failure`, or `error` for skipped nodes) for every state transition, and in both modes it folds each terminal state into a per-node outcome map. At end-of-run that map is printed as a per-node summary and reduced to the process's exit code (zero only if every node finished `Success`). Each node's stdout/stderr is split into its own `.ci/<sha>/<platform>/<recipe>.log`, and the GitHub status `description` embeds that path — so a red check links straight to the failing log. The SHA-keyed directory keeps prior runs' logs alongside the latest. All runtime artifacts live under `$PWD/.ci/` (gitignored); process-compose binds the same UDS in both modes, so two concurrent ci runs in the same checkout collide on the socket and the second fails fast — the intended mutex.

### Platform fanout

The pipeline's target platforms come from the root recipe's `just` OS attributes:

```just
[linux] [macos] [metadata("ci")]
ci: build run-check
```

…declares a pipeline that runs across both Linux and macOS. A root recipe with no OS attribute defaults to the local platform only (single-lane pipeline, identical to the pre-fanout shape). Each (recipe × platform) pair becomes a separate process-compose node, with `depends_on` edges replicated within each platform lane — no cross-lane edges, so a failure on one platform doesn't block the other.

Process-compose node names are `<recipe>@<platform>` (e.g. `ci::build@linux`), and the same string is the GitHub commit-status context.

### Remote builds over SSH

A node whose platform doesn't match the local host runs via SSH: the runner pipes a `git bundle` through `ssh <host>`, the remote shell clones it into a tempdir, checks out the pipeline's `HEAD` SHA, and runs `just --no-deps <recipe>` there. Per-node stdout/stderr streams back over SSH and lands in `.ci/<sha>/<platform>/<recipe>.log` exactly as a local node would.

Hosts are configured in `~/.config/ci/hosts.json`, keyed by **Nix system tuple**:

```json
{
  "x86_64-linux": "builder.example.com",
  "aarch64-darwin": "mac-runner.example.com"
}
```

The pipeline's fanout = (root recipe's OS families × configured systems matching those families) ∪ {local system if its family matches}. A `[linux]` attribute on the root matches any `*-linux` system in `hosts.json`; `[macos]` matches any `*-darwin`. Systems without entries are silently dropped — the user opts in by writing the file.

**Local platform override.** An entry for the *local* system takes precedence over inline execution: configure `"x86_64-linux": "pu connect srid1"` from an x86_64-linux host and the linux lane routes through `pu` instead of running in the worktree. The path for exercising remote runners (or testing failure modes) without leaving the local box.

**One-shot CLI overrides.** A repeatable `--host PLATFORM=ADDR` option on `ci run` overlays onto whatever `hosts.json` contains, with CLI entries winning on collision: `ci run --host x86_64-linux=root@lxc-foo` redirects the linux lane to a throwaway LXC container for that invocation without touching the JSON file. Platforms not named on the CLI still consult `hosts.json` as usual.

The remote host needs `nix`, `git`, and any tools the recipes themselves use available on its PATH. **`just` does not need to be pre-installed** — the runner ships the target-platform `just` *derivation* (a small file of build metadata) via `nix-store --export | ssh <host> nix-store --import`, then the remote `nix-store --realise`s it. The remote's substituter chain (typically `cache.nixos.org`) fetches the natively-built binary for its own arch, so the linux runner never tries to execute a darwin binary and vice versa.

Host strings are whatever `ssh` knows how to dial — bare `hostname`, `user@host`, an alias from `~/.ssh/config`. Incus instances are reached via an ssh-config alias that names them; no special-case client at the runner layer.

### Cross-lane failure tolerance

Every emitted process is `restart: no` and `exit_on_skipped: false`, so one failing node leaves sibling lanes free to keep running and skipped dependents don't tear the project down. Process-compose's own exit code is therefore not authoritative — a failed node leaves pc exiting 0 — and the verdict step that consults the outcome map is what surfaces the failure.

## Subcommands

- `ci run [--tui] [--host PLATFORM=ADDR ...] [-- <args>]` (default): drive the pipeline; anything after `--` is forwarded verbatim to `process-compose up`. `--tui` swaps process-compose's headless logger for its interactive tcell view — useful for poking at long-running pipelines locally. `--host PLATFORM=ADDR` is repeatable and overlays onto `~/.config/ci/hosts.json` (see _Remote builds over SSH_ above).
- `ci dump-yaml`: emit the assembled YAML to stdout for inspection. Runs in a side-effect-free mode — no host prompts, no `git rev-parse` shell-out — so it works offline, on a remote VM with no TTY, and outside a git checkout. Unresolved hosts render as `<unconfigured>` and the SSH `checkout` carries a `0000000-dump-yaml-placeholder` token; the YAML's *structure* (process keys, depends_on edges) still reflects the real fanout.

## Roadmap

- Expose process-compose's state and control surface as an [MCP](https://modelcontextprotocol.io/) server so agent CLIs can introspect mid-run.
- Per-recipe OS-attribute filtering: today a recipe is replicated to every pipeline platform regardless of its own `[linux]/[macos]` attribute (and the remote `just` refuses if the recipe isn't enabled on that host). A future pass at our layer would prune those nodes upfront so the verdict surface doesn't show them as `Failed`.
