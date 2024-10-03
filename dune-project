(lang dune 3.7)

(name yamlcv)
(version 5aa98f2-dirty)

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
 (depends ocaml dune yaml containers tyxml tyxml-ppx 
          ppx_deriving ppx_string cmdliner fmt
          yojson
          (pandoc (= 0.2.2)) 
          (alcotest :with-test)
          logs
          )
 (tags
  (topics "to describe" your project)))


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
