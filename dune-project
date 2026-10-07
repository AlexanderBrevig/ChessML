(lang dune 3.11)

(name chessml)

(generate_opam_files true)

(source
 (github alexanderbrevig/chessml))

(authors "Alexander Brevig <alexander.brevig@hey.com>")

(maintainers "Alexander Brevig <alexander.brevig@hey.com>")

(license MIT)

(documentation https://github.com/alexanderbrevig/chessml)

(package
 (name chessml)
 (synopsis "Fast chess library, suitable for use in chess engines")
 (description
  "A fast Chess library with a clean, functional API. Supports FEN notation, UCI notation, move generation, and performance testing (perft).")
 (depends
  (ocaml
   (>= 5.3))
  dune
  domainslib
  (alcotest :with-test)
  (bisect_ppx
   (and
    :with-test
    (>= 2.5.0)))))
