{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.libelf
    pkgs.pkgsCross.riscv32-embedded.buildPackages.gcc
    pkgs.qemu
  ];
}
