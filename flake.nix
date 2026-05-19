{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = { self, nixpkgs, haskell-flake, ... }:
    let
      eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;

      # Drv paths of `just` for every Nix system the runner can target.
      # Baked into the Haskell binary at TH-splice time (see CI.Nix);
      # the runner ships these tiny .drv files to remotes, which then
      # `nix-store --realise` to get a natively-built `just` binary.
      #
      # `unsafeDiscardStringContext` strips Nix's automatic dependency
      # tracking on the drv path strings — without it, the linux build
      # would try to instantiate the darwin tree (and fail), since
      # cross-platform .drv eval pulls in source patches Nix has no
      # way to fetch from the wrong host.
      drvStr = drv: builtins.unsafeDiscardStringContext drv;
      justDrvEnv = {
        CI_JUST_DRV_X86_64_LINUX   = drvStr nixpkgs.legacyPackages.x86_64-linux.just.drvPath;
        CI_JUST_DRV_AARCH64_LINUX  = drvStr nixpkgs.legacyPackages.aarch64-linux.just.drvPath;
        CI_JUST_DRV_AARCH64_DARWIN = drvStr nixpkgs.legacyPackages.aarch64-darwin.just.drvPath;
      };

      perSystem = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          project = (haskell-flake.lib { inherit pkgs; }).evalHaskellProject {
            projectRoot = self;
            modules = [{
              # Package build: env vars land in the derivation so TH
              # picks them up during cabal compilation.
              settings.ci = {
                extraBuildTools = [ pkgs.just pkgs.process-compose pkgs.gh pkgs.git ];
                custom = drv: drv.overrideAttrs (_: justDrvEnv);
              };
              # Dev shell: same env vars, so `cabal build` inside
              # `nix develop` produces the same baked-in drv paths.
              devShell.mkShellArgs = {
                nativeBuildInputs = [ pkgs.just pkgs.process-compose pkgs.gh pkgs.git ];
                shellHook = nixpkgs.lib.concatStringsSep "\n"
                  (nixpkgs.lib.mapAttrsToList
                    (k: v: "export ${k}=${v}")
                    justDrvEnv);
              };
            }];
          };
        in
        {
          packages.default = project.packages.ci.package;
          devShells.default = project.devShell;
        };

      systemOutputs = eachSystem perSystem;
    in
    {
      packages = nixpkgs.lib.mapAttrs (_: s: s.packages) systemOutputs;
      devShells = nixpkgs.lib.mapAttrs (_: s: s.devShells) systemOutputs;
    };
}
