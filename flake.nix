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
            duneVersion = "3";
            meta.mainProgram = "tagless-demo";

            src = ./.;

            buildInputs = with ocamlPackages; [
              ppx_deriving
              ppxlib
              merlin
              ocp-indent
            ];
          };
        };

        devShells = {
          default = legacyPackages.mkShell {
            packages = [
              legacyPackages.nixpkgs-fmt
              legacyPackages.ocamlformat
              legacyPackages.fswatch
              ocamlPackages.odoc
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat-rpc-lib
              ocamlPackages.utop
            ];

            # Tools from packages
            inputsFrom = [
              self.packages.${system}.modular-lang-proto
            ];
          };
        };

        apps.calc-demo = {
          type = "app";
          program = "${self.packages."${system}".modular-lang-proto}/bin/calc-demo";
        };

        apps.algo-demo = {
          type = "app";
          program = "${self.packages."${system}".modular-lang-proto}/bin/algo-demo";
        };

        apps.tagless-demo = {
          type = "app";
          program = "${self.packages."${system}".modular-lang-proto}/bin/tagless-demo";
        };
      });
}
