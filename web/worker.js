/* Web Worker wrapper around the js_of_ocaml calculator. Keeping evaluation
   off the main thread means the page stays responsive while (say) 10,000
   digits of pi are computed, and lets the user cancel a runaway computation
   (e.g. 1/0, whose sign search never terminates) by terminating the worker. */
'use strict';

importScripts('web_main.bc.js');

const calc = globalThis.crCalc;

onmessage = (e) => {
  const { reqId, method, args } = e.data;
  let result;
  try {
    result = calc[method](...args);
  } catch (err) {
    result = { ok: false, error: String(err && err.message || err) };
  }
  postMessage({ reqId, result });
};
