(lang dune 3.7)

(name yamlcv)

(generate_opam_files true)

(source
 (github tbrugere/yamlcv))

(authors "Tristan Brugère")
(maintainers "Tristan Brugère")


(license CeCILL-B)

; (documentation https://url/to/documentation)

(package
 (name yamlcv)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune yaml containers tyxml tyxml-ppx ppx_deriving ppx_string)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
