---
name: justci
description: Reference for the `justci` runner — how to invoke a full pipeline, a single recipe, or a platform-pinned node from a project that depends on `juspay/justci`. Trigger when the user asks to "run justci", "run the pipeline", "re-run a check", or names a specific recipe by `<recipe>@<platform>`.
---

# justci

`justci` translates a project's `just` recipe DAG into a `process-compose` pipeline and runs it. Multi-platform lanes fan out via SSH; commit statuses get posted (in strict mode) under `<recipe>@<platform>` contexts. Full background in the [repo README](https://github.com/juspay/justci/blob/main/README.md); the subcommand surface below is what you'll reach for most often.

## Modes

| Variable | Effect |
| --- | --- |
| `CI` unset (default) | **Local mode.** Runs against the live working tree. No GitHub status posts, no clean-tree refuse. Use for iterating. |
| `CI=true` | **Strict mode.** Refuses a dirty tree, snapshots `HEAD` via `git worktree`, posts commit statuses, splits per-recipe logs into `.justci/<sha>/<plat>/<recipe>.log`. Use for "real" CI runs. |

Both modes share the same verdict-summary at the end (`── justci run summary ──`) and exit non-zero if any node failed.

## Common invocations

```sh
# Full pipeline (canonical [metadata("ci")] root, every platform in the fanout)
justci run                # local mode
CI=true justci run        # strict mode

# Re-run a single failed recipe on a specific lane — overwrites the same
# GitHub commit-status context the full run wrote (closes the red check).
justci run e2e@x86_64-linux

# Re-run a single recipe across every pipeline platform.
justci run e2e

# Multiple positional selectors compose — `e2e` AND `lint` both run.
justci run e2e lint

# Skip the dependency closure; run ONLY the named nodes. Setup nodes
# auto-ride for remote-platform recipes regardless.
justci run --no-deps e2e@aarch64-darwin

# Use a different DAG root instead of the [metadata("ci")] recipe.
justci run --root release-pipeline

# One-shot redirect of a platform to a throwaway host (LXC container,
# alternate SSH alias). Repeatable per platform.
justci run --host x86_64-linux=root@lxc-foo

# Drive process-compose's interactive TUI instead of headless logs.
justci run --tui

# Forward arbitrary args to `process-compose up` after --.
justci run -- -t=false
```

## Inspection subcommands (no side effects)

```sh
# Print the assembled process-compose YAML — no host prompts, no git
# rev-parse, works offline.
justci dump-yaml

# Print the dependency graph in Mermaid flowchart syntax.
justci graph

# PATCH GitHub branch-protection's required_status_checks to the
# (recipe, platform) contexts the canonical DAG produces. --dry-run
# prints what would be PATCHed without touching the API.
justci protect --dry-run
justci protect                  # writes to default branch
justci protect --branch develop
```

## Decision flow

1. **Full canonical run?** → `justci run` (or `CI=true justci run` for strict mode).
2. **Flaky check on a PR, only one lane is red?** → `justci run <recipe>@<platform>` — same status context, overwrites the failure.
3. **Iterating on one recipe locally?** → `justci run <recipe>` (no platform pin = fans out to every pipeline platform; `<recipe>@<localPlat>` if you only want the local lane).
4. **Investigating "what would this run?"** → `justci dump-yaml` or `justci graph`.
5. **Setting up a new repo?** → run `justci protect --dry-run` after at least one full run, verify the contexts look right, then `justci protect` to lock them in.

## Hosts config

`justci` reads `~/.config/justci/hosts.json`:

```json
{
  "x86_64-linux":   "srid1",
  "aarch64-darwin": "sincereintent"
}
```

Keys are full Nix system tuples (`x86_64-linux`, `aarch64-linux`, `aarch64-darwin`). Values are anything `ssh` knows how to dial — bare hostname, `user@host`, alias from `~/.ssh/config`. Missing platforms silently drop from the fanout (the user opts in by adding the entry). Override per-run with `--host PLATFORM=ADDR`.

## When NOT to use this skill

- The user is asking *about* justci's internals (how the YAML is shaped, what `_justci-setup` does, why `[metadata("ci")]` matters) — that's a docs question, point them at the [repo README](https://github.com/juspay/justci/blob/main/README.md).
- The user wants the runner to do something it doesn't support (parallel cross-platform within one recipe, mid-run config reload, MCP introspection) — those are not supported today; check the README's Roadmap section.
