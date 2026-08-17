# Constructive Reals

OCaml implementation of [Towards an API for the Real Numbers](https://dl.acm.org/doi/pdf/10.1145/3385412.3386037) by Hans-J. Boehm, which provides a real number type with decidable equality in common cases.

Here's a [nice Twitter exposition](https://x.com/ChadNauseam/status/1890889465322786878) of the main ideas behind the paper.

## Web demo

An interactive demo lives at <https://joelburget.github.io/constructive-reals/>: a calculator that evaluates each expression both with constructive reals and with ordinary 64-bit floats, highlights where the float answer goes wrong, and visualizes the lazy term DAG (with its cached approximations) behind each result.

To build and serve it locally:

```
dune build @site
python3 -m http.server -d _build/default/web
```

then open <http://localhost:8000>. (A real HTTP server is needed because the page runs its computations in a Web Worker.) The demo is deployed to GitHub Pages automatically on pushes to `main` — see `.github/workflows/deploy.yml`.

## Terminal demo

There's also a lightweight calculator REPL, which can be run with `dune exec ./calculator.exe`. (`.exe` is a dune thing, not a Windows thing)
