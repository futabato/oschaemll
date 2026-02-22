(lang dune 3.0)

(name oschaemll)

(generate_opam_files true)

(source
 (github futabato/oschaemll))

(authors "futabato")

(maintainers "futabato")

(license MIT)

(package
 (name oschaemll)
 (synopsis "A type-driven mini shell in OCaml")
 (description
  "A minimal shell implementation designed with algebraic data types. Types define the structure, pattern matching ensures exhaustive handling.")
 (depends
  (ocaml
   (>= 4.14))
  dune
  (alcotest
   (and
    :with-test
    (>= 1.7)))))
