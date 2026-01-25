{
  description = "Blood on the Clocktower ASP Game Explorer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            # ASP solver and testing
            pkgs.clingo
            pkgs.git
            (pkgs.python3.withPackages (ps: [ ps.rich ]))

            # Web UI build toolchain
            pkgs.nodejs_20
            pkgs.purescript
            pkgs.spago
          ];

          # Environment variables for clingo library access
          CLINGO_LIBRARY_PATH = "${pkgs.clingo}/lib";
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath [ pkgs.clingo pkgs.stdenv.cc.cc.lib ]}";
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";

          shellHook = ''
            echo "botc-asp development environment loaded (flake)"
            echo "  clingo:     $(clingo --version | head -1)"
            echo "  node:       $(node --version)"
            echo "  spago:      $(spago --version)"
            echo "  purescript: $(purs --version)"
          '';
        };
      }
    );
}
