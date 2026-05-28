# justci

A `just`-recipe runner that turns a recipe graph into a [process-compose](https://f1bonacc1.github.io/process-compose/) pipeline, fans out per-platform over SSH, and **posts GitHub commit statuses live** as the run progresses. _Sibling recipes keep running after one fails_ — the final exit code comes from a per-node outcome map a central observer accumulates, not from process-compose's own exit.

The pipeline root is the recipe annotated `[metadata("ci")]` — exactly one across the justfile and its submodules (zero or multiple is a startup error). Its reachable dependency subgraph is the pipeline; submodule recipes appear under fully-qualified `mod::recipe` names. Each `(recipe, platform)` pair becomes its own process-compose node — see [_Platform fanout_](#platform-fanout).

```
just --dump → root → reachable subgraph → fan out per platform → process-compose YAML → run
```

## Modes

**Strict by default.** `justci run` refuses a dirty tree, snapshots HEAD via `git worktree`, posts GitHub commit statuses, and routes per-recipe logs under `.ci/<sha>/<plat>/<recipe>.log`. Two flags relax pieces of that policy for the cases where it's wrong:

| Flag(s) | Tree | HEAD pin | Status posts | Runtime files |
|---|---|---|---|---|
| _(none — default)_ | clean (refuses dirty) | `git worktree` at HEAD | `<recipe>@<platform>` per transition | `.ci/lock`, `.ci/pc.log`, `.ci/pc.sock`, `.ci/worktree/`, `.ci/<sha>/<platform>/<recipe>.log` |
| `--no-post` | clean | `git worktree` at HEAD | none | same as default (logs still SHA-keyed) |
| `--no-snapshot` (implies `--no-post`) | live working tree | none | none | `.ci/lock`, `.ci/pc.log`, `.ci/pc.sock` |
| `--no-strict` (meta — same as `--no-snapshot --no-post`) | live working tree | none | none | `.ci/lock`, `.ci/pc.log`, `.ci/pc.sock` |

The dirty-tree refuse, `gh repo view`, and `git rev-parse HEAD` all run **before** `process-compose` starts — a misconfigured environment (dirty tree, missing `gh` auth, no github remote) halts at the front door, not mid-run. Both modes still take an exclusive `flock` on `.ci/lock`, so a second `justci run` in the same checkout fails fast with _"another justci run is in progress"_ instead of ghost-attaching to the first run's socket and posting stale check results. Runtime artifacts live under `$PWD/.ci/` (gitignored); the SHA-keyed log directory keeps prior runs alongside the latest.

_The pre-flip `CI=true` env-var gate is gone. Existing scripts that set `CI=true` keep working — the var is now a harmless no-op, since strict is the default._

## Status mechanics

A central observer subscribes to process-compose's `/process/states/ws` stream over a Unix domain socket and folds each transition into a per-node outcome map. The map drives the verdict (exit zero only if every node finished `Success`); when posts are enabled (default; suppressed by `--no-post` / `--no-snapshot` / `--no-strict`), it's also translated to GitHub commit-status posts.

| Wire state | GH post | Notes |
|---|---|---|
| Running | `pending` | the moment of "now executing" — no pre-run seed |
| Success | `success` | terminal |
| Failure | `failure` | `description` links to `.ci/<sha>/<platform>/<recipe>.log` |
| Launch failure | `failure` + _"Errored (did not start)"_ | defect in the recipe's launch path, not a cascade |
| Cascade-skipped | _no post_ | rides on GitHub's _"Expected — Waiting for status to be reported"_ placeholder |

The cascade-skipped row is unposted because the placeholder — driven by `justci protect`'s required-checks list — already encodes "required but unreported." Posting our own `Skipped` row used to overwrite a prior `success` on partial re-runs; deferring to the placeholder fixes that. _Both unposted required checks and `pending` posts block merge_, so the path to green after a failure is to re-run the failing root recipe (which re-runs the cascade downstream).

The local CLI summary uses the same vocabulary (`skipped`, `failed`) as the PR check rows.

### Setup nodes — visible but not required

Remote-platform setup (the `_ci-setup@<platform>` SSH bundle-ship + drv-copy step every recipe on that platform depends on) posts its own statuses, so a setup failure surfaces as one red row on the PR instead of a wall of "Expected — Waiting" on downstream recipes. But setup is **not** a required check: a local-only run schedules no setup nodes, and a required check that never receives a status would permanently block merge. Two predicates in `JustCI.CommitStatus` carry the split — `shouldPostStatus` (setup included) and `isRequiredCheck` (setup excluded).

### Aggregator filtering

Pure dependency aggregators — recipes whose `just` body is empty and that exist only to fan out, like `default: checks run-check` — are dropped from both the commit-status surface and the `justci protect` required-checks list. Their state is fully derivative of their leaves: if every dep is green the aggregator's check could only be green; if a dep fails the aggregator is skipped the moment process-compose decides not to run it.

The wedge case is downstream retries: re-running a single failed leaf (e.g. `justci run e2e@x86_64-linux`) succeeds and overwrites the leaf's check to green, but a required aggregator would still be merge-blocked on a recipe that no per-leaf retry will ever clear. _Removing aggregators from both surfaces means required checks are exactly the recipes that do real work, and a successful leaf retry is sufficient to clear the PR._

The aggregator still runs as the DAG entrypoint and still contributes to the local exit code; only its GitHub presence is suppressed. The filter is structural — keyed off the recipe's `body` field in `just --dump --dump-format json` — so it covers both the canonical `[metadata("ci")]` root and any intermediate body-less aggregator (e.g. a `checks: build flake-check fmt-check` fan-out node).

## Platform fanout

The pipeline's target platforms come from the root recipe's `just` OS attributes:

```just
[linux] [macos] [metadata("ci")]
ci: build run-check
```

…declares a pipeline that runs across both Linux and macOS. **A root recipe with no OS attribute defaults to the local platform only** (single-lane pipeline, identical to the pre-fanout shape). Each `(recipe × platform)` pair becomes its own process-compose node; `depends_on` edges are replicated within each lane but never crossed, so a failure on one platform doesn't block the other.

Node names — and GitHub commit-status contexts — are `<recipe>@<platform>` (e.g. `ci::build@linux`).

### Cross-lane failure tolerance

Every emitted process is `restart: no` and `exit_on_skipped: false`, so one failing node leaves sibling lanes free to keep running and skipped dependents don't tear the project down. Process-compose's own exit code is therefore not authoritative — _a failed node leaves pc exiting 0_ — and the verdict step that consults the outcome map is what surfaces the failure.

### Remote builds over SSH

A node whose platform doesn't match the local host runs via SSH. The runner pipes a `git bundle` through `ssh <host>`, the remote shell clones it into a tempdir, checks out the pipeline's `HEAD` SHA, and runs `just --no-deps <recipe>` there. Per-node stdout/stderr streams back over SSH and lands in `.ci/<sha>/<platform>/<recipe>.log` exactly as a local node would.

The remote-side checkout lives under `$JUSTCI_CACHE_DIR` (defaults to `${XDG_STATE_HOME:-$HOME/.local/state}/justci`) keyed by `<short-sha>/<platform>/`, persisting across runs so same-SHA reruns skip the bundle+clone. **Per-SHA dirs are pruned on every setup**: anything older than `--cache-ttl-hours` (default 48) is removed, except the current run's own dir. Set `--cache-ttl-hours 0` to disable eviction. The exclusion of the current dir means concurrent runs from separate orchestrators targeting the same remote can't evict each other's in-progress clone.

Hosts go in `~/.config/justci/hosts.json`, keyed by **Nix system tuple**:

```json
{
  "x86_64-linux": "builder.example.com",
  "aarch64-darwin": "mac-runner.example.com"
}
```

The fanout = `(root's OS families × configured systems matching those families) ∪ {local system if its family matches}`. `[linux]` matches any `*-linux`; `[macos]` matches any `*-darwin`. Systems without entries are silently dropped — _the user opts in by writing the file._

The remote needs `nix`, `git`, and any tools the recipes themselves use on its PATH. **`just` does not need to be pre-installed** — the runner ships the target-platform `just` _derivation_ via `nix-store --export | ssh <host> nix-store --import`, then the remote `nix-store --realise`s it. The remote's substituter chain (typically `cache.nixos.org`) fetches the natively-built binary for its arch, so the linux runner never tries to execute a darwin binary and vice versa.

Host strings are whatever `ssh` knows how to dial — bare `hostname`, `user@host`, an alias from `~/.ssh/config`. Incus instances are reached via an ssh-config alias; no special-case client at the runner layer.

**Local-system entry takes precedence over inline execution.** Configure `"x86_64-linux": "pu connect srid1"` from an x86_64-linux host and the linux lane routes through `pu` instead of running in the worktree — useful for exercising remote runners (or testing failure modes) without leaving the local box.

**`--host PLATFORM=ADDR` overlays onto `hosts.json` for one invocation.** Repeatable; CLI entries win on collision. `justci run --host x86_64-linux=root@lxc-foo` redirects the linux lane to a throwaway LXC container without touching the JSON file. Platforms not named on the CLI still consult `hosts.json` as usual.

**`--platform PLATFORM` restricts the fanout itself.** Repeatable; the pipeline universe becomes `(root OS families ∩ configured systems) ∩ --platform set`. `justci run --platform x86_64-linux` runs the linux lane only, regardless of how many other platforms the root recipe declares. Distinct from the positional `RECIPE@PLATFORM` selector: that pins one named recipe to one platform via post-fanout reachability; `--platform` slices the platform universe pre-fanout, so it composes orthogonally with positional selectors that don't name a platform (e.g. `justci run e2e --platform x86_64-linux` runs `e2e` + its deps on linux only). Composes with `--no-strict` / `--no-snapshot` / `--no-post` too — handy for testing strict-mode behavior on one lane without the full remote fanout.

## Subcommands

| Command | Purpose |
|---|---|
| `justci run` | drive the pipeline (default) |
| `justci dump-yaml` | emit the assembled YAML to stdout |
| `justci protect` | sync GitHub branch-protection required-checks to the DAG |
| `justci status` / `logs` / `monitor` | passthroughs to `process-compose` against the live socket |

### `justci run`

```
justci run [--tui] [--no-strict | --no-snapshot | --no-post] [--host PLATFORM=ADDR ...] [--platform PLATFORM ...] [--root RECIPE] [--no-deps] [--cache-ttl-hours N] [RECIPE[@PLATFORM]...] [-- <args>]
```

- `--tui` — swap process-compose's headless logger for its interactive tcell view; useful for poking at long-running pipelines locally
- `--no-strict` — dev-mode shortcut: run against the live working tree and skip GitHub commit-status posts (equivalent to `--no-snapshot --no-post`). The pre-flight (clean-tree refuse + `gh repo view` + `git rev-parse HEAD`) is skipped entirely, so a misconfigured environment doesn't block the run.
- `--no-snapshot` — run against the live working tree (skip the clean-tree refuse and the HEAD `git worktree` pin). Implies `--no-post` — a SHA-tagged status against unpinned bytes violates the "SHA matches tested bytes" invariant. _Distinct from process-compose's own `--no-snapshot` flag (which is forwarded after `--`); same name, different layers._
- `--no-post` — skip GitHub commit-status posts. Clean-tree refuse and HEAD worktree pin still apply; useful for non-github strict consumers and for debugging strict runs without writing to the PR's checks list.
- `--platform PLATFORM` — restrict the run to this platform; repeatable to opt into a subset (e.g. `--platform x86_64-linux --platform aarch64-darwin` runs two of three lanes). Intersected with the natural fanout: requested platforms outside it are silently dropped, an empty intersection errors with a message naming `--platform` as the cause. Composes with the strict-mode opt-outs too — useful for testing strict-mode behavior on one lane without spinning up every remote. See [_Platform fanout_](#platform-fanout).
- `--root RECIPE` — replace the DAG root that `[metadata("ci")]` would have picked
- `--no-deps` — the `just`-style escape hatch: keep only the named selectors, skip their dependency closure (setup nodes still auto-included on remote platforms so the YAML doesn't reference dropped dependencies)
- `--cache-ttl-hours N` — prune per-SHA cache dirs older than `N` hours on every remote setup (default 48). `0` disables eviction; the current run's dir is never evicted. See [_Remote builds over SSH_](#remote-builds-over-ssh).
- positional `RECIPE[@PLATFORM]` selectors restrict the run to those nodes and their transitive deps (e.g. `justci run e2e@x86_64-linux` re-runs just that one node after a flaky `e2e` lane). _The status context (`<recipe>@<platform>`) is unchanged, so a partial re-run overwrites the same GitHub check the full run wrote._
- anything after `--` is forwarded verbatim to `process-compose up`

### `justci dump-yaml`

Emits the assembled YAML to stdout for inspection. **Side-effect-free** — no host prompts, no `git rev-parse` shell-out — so it works offline, on a remote VM with no TTY, and outside a git checkout. Unresolved hosts render as `<unconfigured>` and the SSH `checkout` carries a `0000000-dump-yaml-placeholder` token; the YAML's _structure_ (process keys, `depends_on` edges) still reflects the real fanout.

### `justci protect [--branch BRANCH] [--dry-run]`

One-shot: PATCH GitHub branch protection's `required_status_checks` to the `(recipe, platform)` contexts the canonical DAG produces. Runs the same DumpRun-mode pipeline build `dump-yaml`/`graph` use, filters to user-facing nodes (setup nodes and pure-aggregator recipes excluded — see _Aggregator filtering_), and sends the list to GitHub. `--branch` defaults to the repo's default branch (queried via `gh repo view`); `--dry-run` prints what would be PATCHed and exits.

Set up the protection ruleset once in the GH UI; `justci protect` keeps the required-check list in sync with the DAG every time the recipe set changes. _The DAG root stays the canonical `[metadata("ci")]` recipe — partial-run flags like `--root`/`--no-deps` belong on `run`, not on the required-check source of truth._

### `justci status` / `logs` / `monitor`

Thin passthroughs to `process-compose process list` / `logs` / `monitor` against `$PWD/.ci/pc.sock`, the UDS that a live `justci run` binds. Useful when `justci run` is in the background and the caller wants fine-grained per-node state:

```sh
justci status -o json                 # one-shot snapshot
justci logs -f <recipe>@<platform>    # tail one node
justci monitor                        # live event stream
```

Each resolves the socket via the same `RunDir` the runner uses and shells out to the same compile-time-baked `process-compose` binary the server runs, so client and server never disagree on wire format. If no run is in progress in the checkout, the subcommand exits non-zero with a clear _"no socket at `.ci/pc.sock`"_ message. Unknown flags pass through to `process-compose` directly — no flag re-declaration here.

## Consume `justci` as an [APM](https://microsoft.github.io/apm/) package

This repo ships a `/ci` reference skill — a cheat-sheet for which subcommand to invoke (full pipeline, single recipe, platform-pinned re-run, `dump-yaml`/`graph`/`protect`, live-introspection `status`/`logs`/`monitor` against a backgrounded run, `hosts.json` overrides). Downstream projects pick it up by adding one line to their own [`apm.yml`](https://microsoft.github.io/apm/reference/manifest-schema/):

```yaml
dependencies:
  apm:
    - juspay/justci
```

`apm install` lands the skill at `.claude/skills/ci/SKILL.md` (or the equivalent path for the consumer's harness). When the consumer's agent reaches a "run justci" / "re-run a flaky check" task, the skill triggers and dispatches the right `justci ...` invocation against the consumer's checkout.

_The skill is just documentation — it doesn't ship the runner itself._ The consumer's project gets `justci` from this flake (`nix run github:juspay/justci -- run`) or a pinned version in its own `flake.nix`.

## Roadmap

- **Per-recipe OS-attribute filtering.** Today a recipe is replicated to every pipeline platform regardless of its own `[linux]/[macos]` attribute (and the remote `just` refuses if the recipe isn't enabled on that host). A future pass at our layer would prune those nodes upfront so the verdict surface doesn't show them as `Failed`.
