{ pkgs ? import <nixpkgs> {} }:

let
  gcc =
    pkgs.pkgsCross.riscv32.buildPackages.gcc;
in

# Generate a cross compiler foc Riscv32, this compiler assume linux so we must modify
# the output to remove all the unsuported annotations of the generated assembly
pkgs.mkShell {
  buildInputs = [
    (pkgs.compcert.overrideAttrs {
      target = "rv32-linux";

      configurePhase = ''
            ./configure -clightgen \
            -prefix $out \
            -coqdevdir $lib/lib/coq/${pkgs.coq.coq-version}/user-contrib/compcert/ \
            -toolprefix ${gcc}/bin/riscv32-unknown-linux-gnu- \
            -use-external-Flocq \
            -use-external-MenhirLib \
            rv32-linux \
          '';
    })
  ];
}
