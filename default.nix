{
  nixpkgs ? (builtins.fetchGit {
    name = "nixos-unstable-2021-04-21";
    url = "https://github.com/nixos/nixpkgs/";
    # ref = "refs/heads/nixos-unstable";
    rev = "0a5f5bab0e08e968ef25cff393312aa51a3512cf";
  })
}:

with import nixpkgs {};

ocamlPackages.buildDunePackage rec {
  pname = "modular-lang-proto";
  version = "0.1";

  minimumOCamlVersion = "4.10";
  useDune2 = true;

  src = ./.;

  buildInputs = with ocamlPackages; [
    # dune_2
    ppx_deriving
    ppxlib
  # "ppx_deriving" {>= "4.4.1"}
  # "merlin"
  # "ocp-indent"
  # "dune"
  ];
}

