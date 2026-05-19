mod ci 'justci.just'

# List all recipes.
default:
    @just --list

# Watch sources and auto-recompile + re-run main on change.
ghcid:
    ghcid -T :main
