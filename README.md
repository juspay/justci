# justci

A Haskell pipeline runner: translates a `just` recipe graph into a [process-compose](https://f1bonacc1.github.io/process-compose/) DAG and drives it. Sibling recipes keep running after one fails; the final exit code is derived from a per-node outcome map that the central observer accumulates, not from process-compose's own exit. In strict mode, posts per-node GitHub commit statuses live as the pipeline runs.

The pipeline root is the recipe annotated `[metadata("ci")]` — exactly one across the justfile and its submodules (zero or multiple is a startup error). Its reachable dependency subgraph becomes the pipeline; submodule recipes appear under their fully-qualified `mod::recipe` names. Each (recipe, platform) pair becomes a separate process-compose node — see *Platform fanout* below.

```
just --dump → root → reachable subgraph → fan out per platform → process-compose YAML → run
```

## Modes

Gated on the `CI` environment variable:

| Mode | Trigger | Tree | Status posts | Runtime files |
|---|---|---|---|---|
| Local | `CI` unset | live working tree | none | `.ci/lock`, `.ci/pc.log`, `.ci/pc.sock` |
| Strict | `CI=true` | `git worktree` pinned to HEAD | `<recipe>@<platform>` per transition | `.ci/lock`, `.ci/pc.log`, `.ci/pc.sock`, `.ci/worktree/`, `.ci/<sha>/<platform>/<recipe>.log` |

Strict mode refuses to run if the working tree is dirty — the SHA on the green check must exactly match the bytes tested. A central observer subscribes to process-compose's `/process/states/ws` stream over a Unix domain socket; in strict mode it translates each transition into a GitHub commit-status post (`pending` when the step actually starts running, then `success` or `failure` at terminal), and in both modes it folds each terminal state into a per-node outcome map. At end-of-run that map is printed as a per-node summary and reduced to the process's exit code (zero only if every node finished `Success`). Each node's stdout/stderr is split into its own `.ci/<sha>/<platform>/<recipe>.log`, and the GitHub status `description` embeds that path so a red check links straight to the failing log. The pending row is the moment of "this step is now executing" — there is no pre-run seed, and an unposted required check shows up via GitHub's own "Expected — Waiting for status to be reported" placeholder (driven by the `justci protect` required-checks list). Cascade-skipped recipes (an upstream dep failed and pc never started this one) ride on that placeholder rather than producing a parallel `pending` + `"Skipped"` post — the placeholder is the canonical encapsulation of "required but unreported", and posting our own duplicate row used to overwrite a prior `success` on partial re-runs. Launch failures (process-compose couldn't start the process) still post `failure` + `"Errored (did not start)"` because that's a real defect in the recipe's launch path, not a cascade. The local CLI summary calls the same wire-states `skipped` and `failed` respectively, so the PR checks panel and the CLI verdict describe them with consistent vocabulary. Both still block merge — GitHub treats both `pending` posts and unposted required checks as "not yet met" — so re-running the failed root recipe (which re-runs the cascade downstream) is still the path to green. The SHA-keyed directory keeps prior runs' logs alongside the latest. All runtime artifacts live under `$PWD/.ci/` (gitignored); both modes take an exclusive `flock` on `.ci/lock` for the duration of a run, so a second `justci run` in the same checkout fails fast with `"another justci run is in progress"` instead of ghost-attaching to the first run's socket and posting stale check results.

Remote-platform setup nodes (the `_ci-setup@<platform>` SSH bundle-ship + drv-copy step that every recipe on that platform depends on) post their own commit statuses too — so a setup failure surfaces as one red row on the PR instead of leaving the user staring at a wall of "Expected — Waiting" on downstream recipes with no visible cause. Setup statuses are visible but **not** registered as required checks: a local-only run schedules no setup nodes, and a required check that never receives a status would permanently block merge. The visibility-vs-required split is owned by two distinct predicates in `JustCI.CommitStatus` — `shouldPostStatus` (setup included) and `isRequiredCheck` (setup excluded) — so the asymmetry is encoded in the types, not in side-effects.

At the top of every strict run, the runner heals the per-SHA commit-status feed before the observer comes up: it fetches the combined commit-status for `HEAD`, identifies justci-owned contexts (those that parse as a `NodeId`) whose backing node is no longer in the **canonical** DAG, and overwrites each non-success one with `success` + `"Reset: not scheduled in current run"`. "Canonical" means the full fanout from the `[metadata("ci")]` root with no user selectors applied — same definition `justci protect` uses — so partial re-runs (`--no-deps`, `--root`) leave their siblings alone; only contexts that no canonical run could ever re-post (e.g. a platform that was unconfigured from `hosts.json` between runs) qualify as stale. The wedge it fixes: GitHub stacks status posts indefinitely and renders the latest per context, so without this pass a one-off bad SSH setup would poison the PR until someone manually cleared the post by hand.

### Aggregator filtering

Pure dependency aggregators — recipes whose `just` body is empty and that exist only to fan out to other recipes, like `default: checks run-check` — are dropped from the GH commit-status surface and from the `justci protect` required-checks list. Their state is fully derivative of their leaves: if every dep is green the aggregator's check could only be green, and if a dep fails the aggregator gets marked skipped the moment process-compose decides not to run it. The wedge case is downstream retries — re-running a single failed leaf (e.g. `justci run e2e@x86_64-linux`) succeeds and overwrites the leaf's check to green, but if the aggregator were on the required list it would still be merge-blocked on a recipe that no per-leaf retry will ever clear. Removing aggregators from both the post surface and the required list means the required checks are exactly the recipes that do real work, and a successful retry of a single leaf is sufficient to clear the PR.

The aggregator recipe itself still runs as the DAG entrypoint and still contributes to the local exit code; only its GitHub presence is suppressed. The filter is structural — keyed off the recipe's `body` field in `just --dump --dump-format json` — so it covers both the canonical `[metadata("ci")]` root and any intermediate body-less aggregators (e.g. a `checks: build flake-check fmt-check` fan-out node).

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

Hosts are configured in `~/.config/justci/hosts.json`, keyed by **Nix system tuple**:

```json
{
  "x86_64-linux": "builder.example.com",
  "aarch64-darwin": "mac-runner.example.com"
}
```

The pipeline's fanout = (root recipe's OS families × configured systems matching those families) ∪ {local system if its family matches}. A `[linux]` attribute on the root matches any `*-linux` system in `hosts.json`; `[macos]` matches any `*-darwin`. Systems without entries are silently dropped — the user opts in by writing the file.

**Local platform override.** An entry for the *local* system takes precedence over inline execution: configure `"x86_64-linux": "pu connect srid1"` from an x86_64-linux host and the linux lane routes through `pu` instead of running in the worktree. The path for exercising remote runners (or testing failure modes) without leaving the local box.

**One-shot CLI overrides.** A repeatable `--host PLATFORM=ADDR` option on `justci run` overlays onto whatever `hosts.json` contains, with CLI entries winning on collision: `justci run --host x86_64-linux=root@lxc-foo` redirects the linux lane to a throwaway LXC container for that invocation without touching the JSON file. Platforms not named on the CLI still consult `hosts.json` as usual.

The remote host needs `nix`, `git`, and any tools the recipes themselves use available on its PATH. **`just` does not need to be pre-installed** — the runner ships the target-platform `just` *derivation* (a small file of build metadata) via `nix-store --export | ssh <host> nix-store --import`, then the remote `nix-store --realise`s it. The remote's substituter chain (typically `cache.nixos.org`) fetches the natively-built binary for its own arch, so the linux runner never tries to execute a darwin binary and vice versa.

Host strings are whatever `ssh` knows how to dial — bare `hostname`, `user@host`, an alias from `~/.ssh/config`. Incus instances are reached via an ssh-config alias that names them; no special-case client at the runner layer.

### Cross-lane failure tolerance

Every emitted process is `restart: no` and `exit_on_skipped: false`, so one failing node leaves sibling lanes free to keep running and skipped dependents don't tear the project down. Process-compose's own exit code is therefore not authoritative — a failed node leaves pc exiting 0 — and the verdict step that consults the outcome map is what surfaces the failure.

## Subcommands

- `justci run [--tui] [--host PLATFORM=ADDR ...] [--root RECIPE] [--no-deps] [RECIPE[@PLATFORM]...] [-- <args>]` (default): drive the pipeline; anything after `--` is forwarded verbatim to `process-compose up`. `--tui` swaps process-compose's headless logger for its interactive tcell view — useful for poking at long-running pipelines locally. `--host PLATFORM=ADDR` is repeatable and overlays onto `~/.config/justci/hosts.json` (see _Remote builds over SSH_ above). `--root` replaces the DAG root that `[metadata("ci")]` would have picked; positional `RECIPE[@PLATFORM]` selectors restrict the run to those nodes and their transitive dependencies (e.g. `justci run e2e@x86_64-linux` re-runs just that one node after a flaky `e2e` lane). The status context (`<recipe>@<platform>`) is unchanged, so a partial re-run overwrites the same GitHub check the full run wrote. `--no-deps` is the `just`-style escape hatch: keep only the named selectors, skip their dependency closure (setup nodes are still auto-included on remote platforms so the YAML doesn't reference dropped dependencies).
- `justci dump-yaml`: emit the assembled YAML to stdout for inspection. Runs in a side-effect-free mode — no host prompts, no `git rev-parse` shell-out — so it works offline, on a remote VM with no TTY, and outside a git checkout. Unresolved hosts render as `<unconfigured>` and the SSH `checkout` carries a `0000000-dump-yaml-placeholder` token; the YAML's *structure* (process keys, depends_on edges) still reflects the real fanout.
- `justci protect [--branch BRANCH] [--dry-run]`: PATCH GitHub branch protection's `required_status_checks` to the `(recipe, platform)` contexts the canonical DAG produces. One-shot — runs the same DumpRun-mode pipeline build `dump-yaml`/`graph` use, filters to user-facing nodes (recipes that do real work; setup nodes never post statuses, and pure-aggregator recipes with an empty `body` are excluded too — see *Aggregator filtering* above), and sends the list to GitHub. `--branch` defaults to the repo's default branch (queried via `gh repo view`); `--dry-run` prints what would be PATCHed and exits. Setup the protection ruleset once in the GH UI; `justci protect` keeps the required-check list in sync with the DAG every time the recipe set changes. The DAG root stays the canonical `[metadata("ci")]` recipe — partial-run flags like `--root`/`--no-deps` belong on `run`, not on the required-check source of truth.
- `justci status [ARGS...]` / `justci logs [ARGS...]` / `justci monitor [ARGS...]`: thin passthroughs to `process-compose process list` / `logs` / `monitor` against `$PWD/.ci/pc.sock`, the UDS that a live `justci run` binds. Useful when `justci run` is in the background and the caller wants fine-grained per-node state — `justci status -o json` for a one-shot snapshot, `justci logs -f <recipe>@<platform>` to tail one node, `justci monitor` for a live event stream. Each resolves the socket via the same `RunDir` the runner uses and shells out to the same compile-time-baked `process-compose` binary the server runs, so the client never disagrees with the server on wire format. If no run is in progress in the checkout, the subcommand exits non-zero with a clear "no socket at `.ci/pc.sock`" message. Unknown flags pass through to `process-compose` directly — no flag re-declaration here.

## Consume `justci` as an [APM](https://microsoft.github.io/apm/) package

This repo ships a `/ci` reference skill — a cheat-sheet for which subcommand to invoke (full pipeline, single recipe, platform-pinned re-run, `dump-yaml`/`graph`/`protect`, live-introspection `status`/`logs`/`monitor` against a backgrounded run, `hosts.json` overrides). Downstream projects pick it up by adding one line to their own [`apm.yml`](https://microsoft.github.io/apm/reference/manifest-schema/):

```yaml
dependencies:
  apm:
    - juspay/justci
```

`apm install` lands the skill at `.claude/skills/ci/SKILL.md` (or the equivalent path for the consumer's harness). When the consumer's agent reaches a "run justci" / "re-run a flaky check" task, the skill triggers and dispatches the right `justci ...` invocation against the consumer's checkout.

The skill is just documentation — it doesn't ship the runner itself. The consumer's project gets `justci` from this flake (`nix run github:juspay/justci -- run`) or a pinned version in its own `flake.nix`.

## Roadmap

- Per-recipe OS-attribute filtering: today a recipe is replicated to every pipeline platform regardless of its own `[linux]/[macos]` attribute (and the remote `just` refuses if the recipe isn't enabled on that host). A future pass at our layer would prune those nodes upfront so the verdict surface doesn't show them as `Failed`.
