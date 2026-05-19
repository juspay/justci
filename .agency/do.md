# do

Project hooks for the `do` skill.

## Check command

```sh
nix develop -c cabal build
```

## Test command

```sh
just justci run-check
```

## CI command

```sh
CI=true nix run . -- run
```

`justci run` self-hosts: it translates the `just` recipe graph into a
`process-compose` config and drives the pipeline through `justci run-step
<recipe>` wrappers. `CI=true` flips each wrapper into status-posting
mode, so a GitHub commit status (`justci/<recipe>`) is posted at start,
success, and failure for every recipe — there is no separate hosted
CI workflow.

Verify via exit code and stdout (no remote CI status check needed locally).

## Documentation

- `README.md` — keep up to date.
