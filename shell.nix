# Pin to local nixpkgs to avoid network fetches
{ pkgs ? import /nix/store/ibbb1fi5gm4f1d79xqadajzjazpmc484-source {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.clingo
  ];
  CLINGO_LIBRARY_PATH = "${pkgs.clingo}/lib";
  LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath [ pkgs.clingo pkgs.stdenv.cc.cc.lib ]}";
  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
}
