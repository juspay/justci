# do

Project hooks for the `do` skill.

## Check command

```sh
nix develop -c cabal build
```

## Test command

```sh
just ci::run-check
```

## CI command

```sh
nix run . -- run
```

`justci run` self-hosts: it translates the `just` recipe graph into a
`process-compose` config and drives the pipeline through `justci run-step
<recipe>` wrappers. Strict by default — every wrapper posts a GitHub
commit status (`<recipe>@<platform>`) at start, success, and failure;
the clean-tree refuse and HEAD `git worktree` pin run as pre-flight
before `process-compose` boots. No separate hosted CI workflow.

Verify via exit code and stdout (no remote CI status check needed locally).

## Documentation

- `README.md` — keep up to date.
- `.apm/skills/ci/SKILL.md` — the source for the downstream `/ci`
  skill. **Edit only the `.apm/` source**, then run `apm install` to
  regenerate `.claude/skills/ci/SKILL.md`. The `.claude/` copy is
  generated; direct edits get overwritten.
