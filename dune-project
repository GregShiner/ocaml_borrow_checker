(lang dune 3.14)

(name ocaml_borrow_checker)

(generate_opam_files true)

(source
 (github GregShiner/ocaml_borrow_checker))

(authors "Gregory Shiner" "Hoang Nguyen")

(maintainers "Gregory Shiner" "Hoang Nguyen")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml_borrow_checker)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune sexplib)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
