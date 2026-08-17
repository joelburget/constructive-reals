/* UI glue for the constructive reals demo. All computation happens in a Web
   Worker (worker.js) hosting the js_of_ocaml build, which exposes:
     parse(str)        -> {ok, id} | {ok: false, error}
     evalCr(id, n)     -> {ok, value}
     evalFloat(str)    -> {ok, value}
     repr(id)          -> {ok, pp, debug}
     dag(id)           -> {ok, root, nodes}
   Terms live in the worker's registry; if the worker is terminated (cancel),
   cards transparently re-parse their expression in the fresh worker.
*/
'use strict';

const DIGIT_STEPS = [10, 20, 50, 100, 300, 1000, 3000, 10000];
const INITIAL_STEP = 1; // 20 digits

const EXAMPLES = [
  { expr: '0.1 + 0.2', note: 'the classic' },
  { expr: 'sqrt(2)*sqrt(2) - 2', note: 'catastrophic cancellation' },
  { expr: 'sin(pi)', note: 'should be exactly 0' },
  { expr: '10000000000000000 + 1 - 10000000000000000', note: 'the 1 vanishes in a float' },
  { expr: 'ln(exp(50)) - 50', note: 'round trip' },
  { expr: 'exp(1000)', note: 'the float overflows to Infinity' },
  { expr: 'pi', note: 'as many digits as you like' },
];

const resultsEl = document.getElementById('results');
const inputEl = document.getElementById('expr');
const formEl = document.getElementById('repl-form');
const chipsEl = document.getElementById('chips');
const busyEl = document.getElementById('busy');

/* ---------- worker plumbing ---------- */

let worker = null;
let workerGen = 0;
let nextReq = 0;
const pending = new Map();

function startWorker() {
  worker = new Worker('worker.js');
  workerGen += 1;
  worker.onmessage = (e) => {
    const p = pending.get(e.data.reqId);
    if (p) {
      pending.delete(e.data.reqId);
      p.resolve(e.data.result);
    }
    updateBusy();
  };
}

function call(method, ...args) {
  return new Promise((resolve, reject) => {
    const reqId = nextReq++;
    pending.set(reqId, { resolve, reject });
    worker.postMessage({ reqId, method, args });
    updateBusy();
  });
}

function cancelAll() {
  worker.terminate();
  for (const p of pending.values()) p.reject(new Error('cancelled'));
  pending.clear();
  startWorker();
  updateBusy();
}

/* Show the busy bar only if a computation is still running after a beat, so
   quick evaluations don't flicker. */
let busyTimer = null;
function updateBusy() {
  if (pending.size > 0) {
    if (!busyTimer && busyEl.hidden) {
      busyTimer = setTimeout(() => {
        busyTimer = null;
        if (pending.size > 0) busyEl.hidden = false;
      }, 300);
    }
  } else {
    if (busyTimer) { clearTimeout(busyTimer); busyTimer = null; }
    busyEl.hidden = true;
  }
}

document.getElementById('cancel').addEventListener('click', cancelAll);

/* The worker's term registry dies with it on cancel; re-parse on demand. */
async function ensureId(card) {
  if (Number(card.dataset.gen) === workerGen) return Number(card.dataset.id);
  const parsed = await call('parse', card.dataset.expr);
  if (!parsed.ok) throw new Error(String(parsed.error));
  card.dataset.id = String(parsed.id);
  card.dataset.gen = String(workerGen);
  return parsed.id;
}

/* ---------- comparison rendering ---------- */

/* Compare the float's decimal string against the constructive real's.
   Characters past the CR string's length can't be checked at this
   precision, so they're marked "unverified" rather than wrong. */
/* Parse a decimal string (plain or scientific notation) into
   sign * digits * 10^exp, all exact. */
function parseDecimal(s) {
  const m = /^(-?)(\d+)(?:\.(\d+))?(?:e([+-]?\d+))?$/.exec(s);
  if (!m) return null;
  const frac = m[3] || '';
  return {
    sign: m[1] ? -1n : 1n,
    digits: m[2] + frac,
    exp: (m[4] ? parseInt(m[4], 10) : 0) - frac.length,
  };
}

/* The value as a BigInt scaled by 10^scale, truncating digits below that. */
function scaledBigInt(d, scale) {
  let v = BigInt(d.digits);
  const e = d.exp + scale;
  if (e >= 0) v *= 10n ** BigInt(e);
  else v /= 10n ** BigInt(-e);
  return d.sign * v;
}

/* Format absDiff * 10^-scale as e.g. "4.0e-17", exactly enough for a
   verdict. String-based so enormous differences can't overflow a double. */
function formatErr(absDiff, scale) {
  const s = absDiff.toString();
  const mant = s[0] + '.' + (s[1] || '0');
  const e = s.length - 1 - scale;
  return e === 0 ? mant : `${mant}e${e > 0 ? '+' : ''}${e}`;
}

function compareValues(crValue, floatValue) {
  const n = Math.min(crValue.length, floatValue.length);
  let i = 0;
  while (i < n && crValue[i] === floatValue[i]) i++;

  const esc = (s) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;');
  const okPart = esc(floatValue.slice(0, i));
  const tail = i < floatValue.length;
  const wrongHtml = okPart + '<span class="wrong">' + esc(floatValue.slice(i)) + '</span>';

  const fd = parseDecimal(floatValue);
  if (!fd) {
    // NaN / Infinity / error text — nothing numeric to compare against.
    const verdict = floatValue.includes('Infinity')
      ? 'the float overflowed to infinity'
      : floatValue === 'NaN' ? 'the float computation produced NaN (not a number at all)'
      : '';
    return { html: '<span class="wrong">' + esc(floatValue) + '</span>', verdict, cls: 'bad' };
  }

  /* Compare numerically, not just textually: a float like 1.2e-16 for a true
     value of 0 shares no characters with "0.0000…" yet is an excellent
     absolute approximation. scale = decimal places the CR display carries. */
  const scale = (crValue.split('.')[1] || '').length;
  const diff = scaledBigInt(fd, scale) - scaledBigInt(parseDecimal(crValue), scale);
  const absDiff = diff < 0n ? -diff : diff;

  if (absDiff <= 1n) {
    if (tail) {
      return {
        html: okPart + '<span class="unverified">' + esc(floatValue.slice(i)) + '</span>',
        verdict: 'grey digits are beyond the computed precision — raise the digits slider to check them',
        cls: '',
      };
    }
    return { html: okPart, verdict: 'matches at this precision ✓', cls: 'good' };
  }

  // The CR display itself is only exact to ±1 in its last digit, so don't
  // over-claim a tiny difference.
  if (absDiff < 10n) {
    return {
      html: wrongHtml,
      verdict: 'differs around the last displayed digit — raise the digits slider to resolve',
      cls: '',
    };
  }

  const err = formatErr(absDiff, scale);
  const digitsOk = (floatValue.slice(0, i).match(/[0-9]/g) || []).length;
  const places = scale - absDiff.toString().length; // decimal places still correct
  let verdict;
  if (digitsOk > 0) {
    verdict = `diverges after ${digitsOk} digit${digitsOk === 1 ? '' : 's'} — off by ≈ ${err}`;
  } else if (places >= 1) {
    verdict = `off by ≈ ${err}: every printed digit differs, yet it agrees with the true value to ${places} decimal place${places === 1 ? '' : 's'}`;
  } else {
    verdict = `completely different — off by ≈ ${err}`;
  }
  return { html: wrongHtml, verdict, cls: 'bad' };
}

function refreshComparison(card) {
  const crValue = card.dataset.crValue;
  const floatValue = card.dataset.floatValue;
  card.querySelector('.cr-value').textContent = crValue;
  if (!floatValue) return; // float result hasn't arrived yet
  const cmp = compareValues(crValue, floatValue);
  card.querySelector('.float-value').innerHTML = cmp.html;
  const verdict = card.querySelector('.verdict');
  verdict.textContent = cmp.verdict;
  verdict.className = 'verdict ' + cmp.cls;
}

/* ---------- DAG rendering ---------- */

const OP_INFO = {
  int: { label: (n) => n.arg, desc: 'integer constant' },
  assumed_int: { label: () => 'assumed int', desc: 'assumed to be an integer — never evaluated past the decimal point' },
  add: { label: () => '+', desc: 'sum of two reals' },
  shift: { label: (n) => `× 2^${n.arg}`, desc: 'shift: multiplication by a power of two' },
  neg: { label: () => 'negate', desc: 'negation' },
  select: { label: () => 'select', desc: 'chooses between two reals based on the sign of a selector' },
  mult: { label: () => '×', desc: 'product of two reals' },
  inv: { label: () => '1 ∕ x', desc: 'reciprocal' },
  exp: { label: () => 'exp', desc: 'exponential (argument prescaled for fast convergence)' },
  cos: { label: () => 'cos', desc: 'cosine (argument prescaled for fast convergence)' },
  ln: { label: () => 'ln', desc: 'natural log (argument prescaled for fast convergence)' },
  asin: { label: () => 'asin', desc: 'arcsine (argument prescaled for fast convergence)' },
  sqrt: { label: () => '√', desc: 'square root (Newton iteration)' },
  pi: { label: () => 'π', desc: 'Gauss–Legendre pi' },
};

function abbreviate(digits, keep = 24) {
  if (digits.length <= keep) return digits;
  const half = Math.floor(keep / 2);
  return `${digits.slice(0, half)}…${digits.slice(-half)} (${digits.replace('-', '').length} digits)`;
}

/* Render the term DAG as a nested tree. Shared subterms render their
   children once; later occurrences become a reference marker. */
function renderDag(container, dagData) {
  // Remember which nodes were collapsed across re-renders.
  const closed = new Set(
    [...container.querySelectorAll('details.dag-node:not([open])')]
      .map((d) => d.dataset.path)
  );
  container.textContent = '';

  const nodes = dagData.nodes;
  const renderedIds = new Set();

  function renderNode(id, path) {
    const node = nodes[id];
    const info = OP_INFO[node.op] || { label: () => node.op, desc: node.op };

    const line = document.createElement('span');
    line.className = 'dag-line';
    const opEl = document.createElement('span');
    opEl.className = 'dag-op';
    opEl.textContent = info.label(node);
    opEl.title = info.desc;
    line.append(opEl);

    const apprEl = document.createElement('span');
    if (node.valid) {
      apprEl.className = 'dag-appr';
      apprEl.textContent = ` ≈ ${node.approx}`;
      apprEl.title = `cached approximation: ${abbreviate(node.maxAppr, 40)} × 2^${node.minPrec}`;
    } else {
      apprEl.className = 'dag-appr dag-unevaluated';
      apprEl.textContent = ' — not evaluated yet';
      apprEl.title = 'this node has no cached approximation: nothing has demanded its value';
    }
    line.append(apprEl);

    if (renderedIds.has(id)) {
      const wrap = document.createElement('div');
      wrap.className = 'dag-leaf dag-shared';
      line.append(' (shared node, shown above)');
      wrap.append(line);
      return wrap;
    }
    renderedIds.add(id);

    if (node.children.length === 0) {
      const wrap = document.createElement('div');
      wrap.className = 'dag-leaf';
      wrap.append(line);
      return wrap;
    }

    const details = document.createElement('details');
    details.className = 'dag-node';
    details.dataset.path = path;
    details.open = !closed.has(path);
    const summary = document.createElement('summary');
    summary.append(line);
    details.append(summary);
    const kids = document.createElement('div');
    kids.className = 'dag-children';
    node.children.forEach((childId, i) => {
      kids.append(renderNode(childId, `${path}.${i}`));
    });
    details.append(kids);
    return details;
  }

  container.append(renderNode(dagData.root, 'r'));
}

async function refreshRepr(card) {
  const id = await ensureId(card);
  const [r, d] = await Promise.all([call('repr', id), call('dag', id)]);
  if (r.ok) card.querySelector('.repr .pp').textContent = String(r.pp);
  if (d.ok) renderDag(card.querySelector('.repr .dag'), d);
}

/* ---------- cards ---------- */

function setStatus(card, text, isError) {
  const verdict = card.querySelector('.verdict');
  verdict.textContent = text;
  verdict.className = 'verdict' + (isError ? ' bad' : '');
}

/* Snap the slider display back to the last precision that actually
   computed, so a failed/cancelled request doesn't leave the thumb lying. */
function resetSlider(card) {
  const goodStep = card.dataset.goodStep;
  if (goodStep === undefined || goodStep === '') return;
  card.querySelector('.digits-slider').value = goodStep;
  card.querySelector('.digits-value').textContent =
    String(DIGIT_STEPS[Number(goodStep)]);
}

async function setDigits(card, stepIndex) {
  const digits = DIGIT_STEPS[stepIndex];
  const seq = Number(card.dataset.seq || 0) + 1;
  card.dataset.seq = String(seq);
  setStatus(card, `computing ${digits} digits…`, false);
  try {
    const id = await ensureId(card);
    const r = await call('evalCr', id, digits);
    if (Number(card.dataset.seq) !== seq) return; // superseded by a newer request
    if (!r.ok) {
      setStatus(card, 'error: ' + r.error, true);
      resetSlider(card);
      return;
    }
    card.dataset.goodStep = String(stepIndex);
    card.dataset.crValue = String(r.value);
    refreshComparison(card);
    await refreshRepr(card);
  } catch (err) {
    if (Number(card.dataset.seq) === seq) {
      setStatus(card, String(err.message || err) + ' — move the slider to retry', true);
      resetSlider(card);
    }
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

async function evaluate(expr) {
  let parsed;
  try {
    parsed = await call('parse', expr);
  } catch (err) {
    resultsEl.prepend(errorCard(expr, String(err.message || err)));
    return;
  }
  if (!parsed.ok) {
    resultsEl.prepend(errorCard(expr, String(parsed.error)));
    return;
  }

  const card = document.createElement('article');
  card.className = 'card';
  card.dataset.expr = expr;
  card.dataset.id = String(parsed.id);
  card.dataset.gen = String(workerGen);
  card.dataset.crValue = '';
  card.dataset.floatValue = '';
  card.innerHTML = `
    <div class="card-expr"></div>
    <div class="panes">
      <div class="pane">
        <div class="pane-label">constructive real</div>
        <code class="value cr-value">…</code>
        <label class="digits-control">
          digits: <span class="digits-value"></span>
          <input type="range" class="digits-slider"
                 min="0" max="${DIGIT_STEPS.length - 1}" value="${INITIAL_STEP}">
        </label>
      </div>
      <div class="pane">
        <div class="pane-label">64-bit float (what JavaScript computes)</div>
        <code class="value float-value">…</code>
        <div class="verdict"></div>
      </div>
    </div>
    <details class="repr">
      <summary>How is this represented?</summary>
      <div class="repr-label">expression (pretty-printed)</div>
      <div class="pp"></div>
      <div class="repr-label">term DAG with cached approximations — watch them change as you raise the digits slider</div>
      <div class="dag"></div>
    </details>`;
  card.querySelector('.card-expr').textContent = expr;

  const slider = card.querySelector('.digits-slider');
  const digitsValue = card.querySelector('.digits-value');
  digitsValue.textContent = String(DIGIT_STEPS[INITIAL_STEP]);
  slider.addEventListener('input', () => {
    digitsValue.textContent = String(DIGIT_STEPS[Number(slider.value)]);
  });
  slider.addEventListener('change', () => setDigits(card, Number(slider.value)));

  resultsEl.prepend(card);

  call('evalFloat', expr)
    .then((floatResult) => {
      card.dataset.floatValue = floatResult.ok
        ? String(floatResult.value)
        : 'error: ' + floatResult.error;
      if (card.dataset.crValue) refreshComparison(card);
    })
    .catch(() => {});

  await setDigits(card, INITIAL_STEP);
}

/* ---------- wiring ---------- */

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

startWorker();
// Start with the flagship example on screen.
evaluate('0.1 + 0.2');
