{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.stack
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.hlint
  ];
}