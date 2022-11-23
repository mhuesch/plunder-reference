{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        hPkgs =
          pkgs.haskell.packages."ghc924"; # must match `stack.yaml` resolver

        devPkgs = [
          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.ormolu
          hPkgs.hlint
          hPkgs.hoogle
          hPkgs.haskell-language-server
          hPkgs.implicit-hie
          hPkgs.retrie

          stack-wrapped

          pkgs.lmdb
          pkgs.zlib
        ];

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = devPkgs;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devPkgs;
        };
      });
}
