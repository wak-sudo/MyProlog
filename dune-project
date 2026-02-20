(lang dune 3.14)

(name myProlog)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name myProlog)
 (synopsis "Prolog implementation by Wojciech Kieloch 02/2026")
 (description "Wojciech Kieloch is not the author of the parser.")
 (depends ocaml dune)
 (tags
  (topics "Prolog implementation" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
