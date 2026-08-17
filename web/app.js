/* UI glue for the constructive reals demo. The OCaml side (compiled with
   js_of_ocaml) exposes globalThis.crCalc:
     parse(str)        -> {ok, id} | {ok: false, error}
     evalCr(id, n)     -> {ok, value}
     evalFloat(str)    -> {ok, value}
     repr(id)          -> {ok, pp, debug}
*/
'use strict';

const calc = globalThis.crCalc;

const DIGIT_STEPS = [20, 50, 100, 300, 1000];

const EXAMPLES = [
  { expr: '0.1 + 0.2', note: 'the classic' },
  { expr: 'sqrt(2)*sqrt(2) - 2', note: 'catastrophic cancellation' },
  { expr: 'sin(pi)', note: 'should be exactly 0' },
  { expr: '10000000000000000 + 1 - 10000000000000000', note: 'the 1 vanishes in a float' },
  { expr: 'ln(exp(50)) - 50', note: 'round trip' },
  { expr: 'pi', note: 'as many digits as you like' },
];

const resultsEl = document.getElementById('results');
const inputEl = document.getElementById('expr');
const formEl = document.getElementById('repl-form');
const chipsEl = document.getElementById('chips');

/* Compare the float's decimal string against the constructive real's.
   Returns HTML for the float pane plus a verdict. Characters past the CR
   string's length can't be checked at this precision, so they're marked
   "unverified" rather than wrong. */
function compareValues(crValue, floatValue) {
  const n = Math.min(crValue.length, floatValue.length);
  let i = 0;
  while (i < n && crValue[i] === floatValue[i]) i++;

  const esc = (s) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;');
  const okPart = esc(floatValue.slice(0, i));

  if (i < floatValue.length && i < crValue.length) {
    // Confirmed divergence within the compared window.
    const digitsOk = (floatValue.slice(0, i).match(/[0-9]/g) || []).length;
    return {
      html: okPart + '<span class="wrong">' + esc(floatValue.slice(i)) + '</span>',
      verdict: digitsOk > 0
        ? `diverges after ${digitsOk} digit${digitsOk === 1 ? '' : 's'}`
        : 'completely different',
      cls: 'bad',
    };
  }
  if (i < floatValue.length) {
    // Float has more digits than we've computed — not yet checkable.
    return {
      html: okPart + '<span class="unverified">' + esc(floatValue.slice(i)) + '</span>',
      verdict: 'grey digits are beyond the computed precision — ask for more digits to check them',
      cls: '',
    };
  }
  return { html: okPart, verdict: 'matches at this precision ✓', cls: 'good' };
}

function refreshComparison(card) {
  const crValue = card.dataset.crValue;
  const floatValue = card.dataset.floatValue;
  card.querySelector('.cr-value').textContent = crValue;
  const cmp = compareValues(crValue, floatValue);
  card.querySelector('.float-value').innerHTML = cmp.html;
  const verdict = card.querySelector('.verdict');
  verdict.textContent = cmp.verdict;
  verdict.className = 'verdict ' + cmp.cls;
}

function refreshRepr(card) {
  const r = calc.repr(Number(card.dataset.id));
  if (!r.ok) return;
  card.querySelector('.repr .pp').textContent = String(r.pp);
  card.querySelector('.repr .debug').textContent = String(r.debug);
}

function moreDigits(card) {
  const button = card.querySelector('.more-digits');
  const stepIndex = Number(card.dataset.stepIndex) + 1;
  if (stepIndex >= DIGIT_STEPS.length) return;
  const digits = DIGIT_STEPS[stepIndex];
  const r = calc.evalCr(Number(card.dataset.id), digits);
  if (!r.ok) {
    card.querySelector('.verdict').textContent = 'error: ' + r.error;
    return;
  }
  card.dataset.stepIndex = String(stepIndex);
  card.dataset.crValue = String(r.value);
  refreshComparison(card);
  refreshRepr(card);
  if (stepIndex + 1 >= DIGIT_STEPS.length) {
    button.disabled = true;
    button.textContent = `${digits} digits`;
  } else {
    button.textContent = `more digits (${DIGIT_STEPS[stepIndex + 1]})`;
  }
}

function errorCard(expr, message) {
  const card = document.createElement('article');
  card.className = 'card';
  const exprEl = document.createElement('div');
  exprEl.className = 'card-expr';
  exprEl.textContent = expr;
  const msg = document.createElement('div');
  msg.className = 'card-error';
  msg.textContent = 'Error: ' + message;
  card.append(exprEl, msg);
  return card;
}

function evaluate(expr) {
  const parsed = calc.parse(expr);
  if (!parsed.ok) {
    resultsEl.prepend(errorCard(expr, String(parsed.error)));
    return;
  }
  const id = parsed.id;
  const crResult = calc.evalCr(id, DIGIT_STEPS[0]);
  if (!crResult.ok) {
    resultsEl.prepend(errorCard(expr, String(crResult.error)));
    return;
  }
  const floatResult = calc.evalFloat(expr);
  const floatValue = floatResult.ok ? String(floatResult.value) : 'error: ' + floatResult.error;

  const card = document.createElement('article');
  card.className = 'card';
  card.dataset.id = String(id);
  card.dataset.stepIndex = '0';
  card.dataset.crValue = String(crResult.value);
  card.dataset.floatValue = floatValue;
  card.innerHTML = `
    <div class="card-expr"></div>
    <div class="panes">
      <div class="pane">
        <div class="pane-label">constructive real</div>
        <code class="value cr-value"></code>
        <button class="more-digits">more digits (${DIGIT_STEPS[1]})</button>
      </div>
      <div class="pane">
        <div class="pane-label">64-bit float (what JavaScript computes)</div>
        <code class="value float-value"></code>
        <div class="verdict"></div>
      </div>
    </div>
    <details class="repr">
      <summary>How is this represented?</summary>
      <div class="repr-label">expression (pretty-printed)</div>
      <div class="pp"></div>
      <div class="repr-label">internal term, with cached approximations — watch these change as you ask for more digits</div>
      <pre class="debug"></pre>
    </details>`;
  card.querySelector('.card-expr').textContent = expr;
  card.querySelector('.more-digits').addEventListener('click', () => moreDigits(card));
  refreshComparison(card);
  refreshRepr(card);
  resultsEl.prepend(card);
}

formEl.addEventListener('submit', (ev) => {
  ev.preventDefault();
  const expr = inputEl.value.trim();
  if (expr) evaluate(expr);
  inputEl.select();
});

for (const { expr, note } of EXAMPLES) {
  const chip = document.createElement('button');
  chip.type = 'button';
  chip.className = 'chip';
  chip.textContent = expr;
  chip.title = note;
  chip.addEventListener('click', () => {
    inputEl.value = expr;
    evaluate(expr);
  });
  chipsEl.append(chip);
}

// Start with the flagship example on screen.
evaluate('0.1 + 0.2');
