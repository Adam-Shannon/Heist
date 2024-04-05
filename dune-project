(lang dune 3.10)

(name Heist)

(generate_opam_files true)

(source
 (github Adam-Shannon/BigraphStrategies))

(authors "Adam Shannon")

(maintainers "Adam Shannon")

(license MIT)

(package
 (name Heist)
 (synopsis "A Strategy Language for Bigraphs")
 (description
  "A Strategy lanaguge for the modelling of Bigraphical reactive systems ")
 (depends
  (alcotest
   (and
    (>= 1.7.0)
    :with-test))
  (yojson
   (>= 2.1.0))
  (ppx_inline_test
   (>= v0.15.1))
  (ppx_deriving_yojson
   (>= 3.7.0))
  (menhirLib
   (>= 20230608))
  (cmdliner
   (>= 1.2.0))
  (base
   (>= v0.15.1))
  ocaml
  dune)
 (tags
  (topics PL Bigraphs))
  )

(using menhir 2.1)

(package
 (name Stakeout)
 (synopsis "Testing Infrastructure for Heist")
 (description
  "Mocking and infrastructure fundamnetals for testing with Heist")
 (depends
  (ppx_deriving_yojson
   (>= 3.7.0))
  (cmdliner
   (>= 1.2.0))
  (bigraph
   (>= 0))
  (base
   (>= v0.15.1))
  (alcotest
   (>= 1.7.0))
  ocaml
  dune
  Heist)
 (tags
  (topics Heist Bigraphs Testing)))