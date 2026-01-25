# Development shell with all tools needed for botc-asp
#
# Usage:
#   nix-shell          # Enter shell with all dependencies
#   nix-shell --pure   # Enter isolated shell (no host tools)
#
# With direnv:
#   echo "use nix" > .envrc && direnv allow
#   cd botc-asp/       # Automatic environment activation
#
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
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
    echo "botc-asp development environment loaded"
    echo "  clingo:     $(clingo --version | head -1)"
    echo "  node:       $(node --version)"
    echo "  spago:      $(spago --version)"
    echo "  purescript: $(purs --version)"
  '';
}
