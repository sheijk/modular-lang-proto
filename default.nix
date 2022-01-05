{
  nixpkgs ? (builtins.fetchGit {
    name = "nixos-stable-21.11";
    url = "https://github.com/nixos/nixpkgs";
    ref = "refs/tags/21.11";
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
    ppx_deriving
    ppxlib
    merlin
    ocp-indent
  ];
}

