(lang dune 2.5)
(name smallestRUNN)
(version 0.1)

(generate_opam_files true)

;;(source (github pat227/ocaml-db-model.git))
(license GPLv3)
(authors "tbd@gmail.com")
(maintainers "tbd@gmail.com")

(package
  (name smallestRUNN)
  (synopsis "Generates a random unordered non-negative set of ints and find the smallest int not present in the RUNN set.")
  (depends
    (ocaml (>= 4.12.0))
    ;;(bignum (>= v0.14.0))
    (core (>= v0.14.0))
    ;;(uint (>= 2.0.1))
  )
)