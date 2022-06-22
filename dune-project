(lang dune 3.2)

(name cgol)

(generate_opam_files true)

(source
 (github benw23/mlcgol))

(authors "Ben Williams")

(package
 (name cgol)
 (synopsis "Conway's Game of Life")
 (description "Conway's Game of Life")
 (depends ocaml dune graphics)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
