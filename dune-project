(lang dune 3.23)

(name spendwise)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name spendwise)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml
  (ounit2 :with-test)
  js_of_ocaml js_of_ocaml-lwt js_of_ocaml-ppx lwt lwt_ppx)
 (allow_empty)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
