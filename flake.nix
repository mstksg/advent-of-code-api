{
  description = "Basic Haskell Project Flake";
  inputs = {
    haskellProjectFlake.url = "github:mstksg/haskell-project-flake";
    nixpkgs.follows = "haskellProjectFlake/nixpkgs";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , haskellProjectFlake
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      name = "advent-of-code-api";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ haskellProjectFlake.overlays."${system}".default ];
      };
      toolCompiler = "ghc9124";
      project-flake = pkgs.haskell-project-flake
        {
          inherit name;
          src = ./.;
          # ghc914/ghc915 ship base >=4.22, too new for servant-client-core's
          # base <4.22 bound at this Hackage index state.
          excludeCompilerMajors = [ "ghc810" "ghc90" "ghc92" "ghc94" "ghc914" "ghc915" ];
          defaultCompiler = "ghc912";
        };
      formatTools = pkgs.haskell-nix.tools toolCompiler {
        hlint = { };
        fourmolu = { };
      };
      checkFormat = pkgs.runCommandLocal "checkHaskell"
        {
          src = ./.;
          nativeBuildInputs = [
            formatTools.fourmolu
            formatTools.hlint
            pkgs.haskellPackages.cabal-fmt
          ];
        } ''
        cd $src
        fourmolu --mode check .
        cabal-fmt --check $(find . -type f -name "*.cabal")
        hlint .
        touch $out
      '';
      runCheck =
        pkgs.writeShellApplication {
          name = "check-haskell";
          runtimeInputs = [
            formatTools.fourmolu
            formatTools.hlint
            pkgs.haskellPackages.cabal-fmt
          ];
          text = ''
            # shellcheck disable=SC2046
            fourmolu --mode check $(git ls-files '*.hs')
            # shellcheck disable=SC2046
            cabal-fmt --check $(git ls-files '*.cabal')
            # shellcheck disable=SC2046
            hlint $(git ls-files '*.hs')
          '';
        };
      runFormat =
        pkgs.writeShellApplication {
          name = "format-haskell";
          runtimeInputs = [ formatTools.fourmolu pkgs.haskellPackages.cabal-fmt ];
          text = ''
            # shellcheck disable=SC2046
            fourmolu --mode inplace $(git ls-files '*.hs')
            # shellcheck disable=SC2046
            cabal-fmt --inplace $(git ls-files '*.cabal')
          '';
        };
    in
    {
      packages = project-flake.packages;
      apps = project-flake.apps // {
        format = {
          type = "app";
          program = "${runFormat}/bin/format-haskell";
        };
      };
      checks = project-flake.checks // {
        inherit checkFormat;
      };
      devShells = project-flake.devShells // {
        default = project-flake.projects.default.shellFor {
          tools = {
            cabal = { };
          };
          buildInputs = [ runFormat runCheck ];
        };
      };
      legacyPackages."${name}" = project-flake;
    }
    );
}
