{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.libelf
    pkgs.pkgsCross.riscv32-embedded.buildPackages.gcc
    pkgs.spirv-tools
    pkgs.spirv-llvm-translator
    pkgs.llvmPackages_19.clang-unwrapped
    pkgs.llvm
    pkgs.qemu
  ];
}
