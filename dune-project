(lang dune 3.16)

(name constructive_reals)

(generate_opam_files true)

(source
 (github joelburget/constructive-reals))

(authors "Joel Burget")

(maintainers "Joel Burget")

(license Apache-2.0)

(package
 (name constructive_reals)
 (synopsis "Constructive real numbers")
 (description
  "OCaml implementation of Hans-J. Boehm's 'Towards an API for the Real Numbers': a real number type whose approximations are computed lazily to any requested precision, with decidable equality in common cases.")
 (depends
  ocaml
  dune
  base
  stdio
  fmt
  zarith
  ppx_jane
  js_of_ocaml
  js_of_ocaml-compiler
  zarith_stubs_js))
