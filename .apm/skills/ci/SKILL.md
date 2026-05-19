---
name: ci
description: ci runner MCP launcher — a single bash script that spawns `ci run --mcp` against the consumer's checkout via Nix. See repo README for the consumer-side `apm.yml` snippet and the just-recipe requirements.
user-invocable: false
---

# ci

Drop-in launcher for the `ci` runner's MCP server (process-compose's built-in MCP, exposed via `ci run --mcp`). Self-contained — `bin/serve` resolves the runner via `nix run` against this repo's flake.

Full docs in the [repo README](https://github.com/juspay/ci/blob/main/README.md).

This skill primitive exists for APM's deployment convention — it ensures `bin/serve` lands at `.agents/skills/ci/bin/serve` in the consumer's working tree (per APM's [skills convergence](https://microsoft.github.io/apm/reference/targets-matrix/#skills-convergence) path), which keeps the launcher available even before `apm install` runs on a fresh clone. The package is mechanically a "skill" in APM's primitive vocabulary; semantically it's a tool.
