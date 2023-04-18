{
  description = "A flake demonstrating how to build OCaml projects with Dune";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    # Construct an output set that supports a number of default systems
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Legacy packages that have not been converted to flakes
        legacyPackages = nixpkgs.legacyPackages.${system};
        # OCaml packages available on nixpkgs
        ocamlPackages = legacyPackages.ocamlPackages;
        # Library functions from nixpkgs
        lib = legacyPackages.lib;
      in
      {
        packages = {
          default = self.packages.${system}.modular-lang-proto;

          modular-lang-proto = ocamlPackages.buildDunePackage rec {
            pname = "modular-lang-proto";
            version = "0.1";

            minimumOCamlVersion = "4.10";
            useDune2 = true;

            src = ./.;

            buildInputs = with ocamlPackages; [
              ppx_deriving
              ppxlib
              merlin
              ocp-indent
            ];
          };
        };
      });
}
