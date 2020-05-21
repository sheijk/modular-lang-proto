(lang dune 2.5)
(name modular-lang-proto)

(generate_opam_files true)

(source (github sheijk/modular-lang-proto))
(license MIT)
(authors "Jan Rehders")
(maintainers "jan@sheijk.net")
(version "0.1")

(package
 (name modular-lang-proto)
 (description "A simple ppx rewriter for string interpolation.")
 (depends
  ("ocaml-base-compiler" (>= "4.10.0"))
  ("ppx_deriving" (>= "4.4.1"))
  ("merlin" (>= "1.0"))
  ("ocp-indent" (>= "1.0"))))
