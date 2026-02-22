(lang dune 3.0)

(name mini_shell)

(generate_opam_files true)

(source
 (github futabato/oschaemll))

(authors "futabato")

(license MIT)

(package
 (name mini_shell)
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
