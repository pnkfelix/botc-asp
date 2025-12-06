# Old pinned path (restore if network fetches return):
# { pkgs ? import /nix/store/ibbb1fi5gm4f1d79xqadajzjazpmc484-source {} }:
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.clingo
    (pkgs.python3.withPackages (ps: [ ps.rich ]))
  ];
  CLINGO_LIBRARY_PATH = "${pkgs.clingo}/lib";
  LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath [ pkgs.clingo pkgs.stdenv.cc.cc.lib ]}";
  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
}
