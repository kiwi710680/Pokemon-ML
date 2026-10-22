// runner.js — 把頁面裡的 R 程式格（.cell）接到瀏覽器裡的 R（webR, WebAssembly）
// 每一頁共用同一個 R 全域環境；執行某一格前，會先自動執行前面還沒跑過的格。
import { WebR } from 'https://webr.r-wasm.org/v0.5.8/webr.mjs';

const statusEl = document.getElementById('status');
const setStatus = (text, kind = '') => {
  statusEl.textContent = text;
  statusEl.dataset.kind = kind;
};

const cells = [...document.querySelectorAll('.cell')];
const ran = new Array(cells.length).fill(false);
let busy = false;

function autosize(ta) {
  ta.style.height = 'auto';
  ta.style.height = `${ta.scrollHeight + 4}px`;
}

cells.forEach((cell, i) => {
  const code = cell.querySelector('script[type="text/r"]').textContent.replace(/^\n/, '').replace(/\s+$/, '');
  const ta = document.createElement('textarea');
  ta.spellcheck = false;
  ta.value = code;
  ta.dataset.original = code;
  const bar = document.createElement('div');
  bar.className = 'bar';
  const run = document.createElement('button');
  run.textContent = '▶ 執行';
  run.className = 'run';
  run.disabled = true;
  const reset = document.createElement('button');
  reset.textContent = '還原程式碼';
  reset.className = 'ghost';
  const label = document.createElement('span');
  label.className = 'label';
  label.textContent = `程式格 ${i + 1}`;
  bar.append(label, run, reset);
  const out = document.createElement('div');
  out.className = 'out';
  cell.append(bar, ta, out);
  requestAnimationFrame(() => autosize(ta));
  ta.addEventListener('input', () => {
    autosize(ta);
    for (let j = i; j < cells.length; j++) ran[j] = false; // 改了程式，這格與後面都要重跑
  });
  ta.addEventListener('keydown', (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
      e.preventDefault();
      runUpTo(i);
    }
  });
  run.addEventListener('click', () => runUpTo(i));
  reset.addEventListener('click', () => {
    ta.value = ta.dataset.original;
    autosize(ta);
    for (let j = i; j < cells.length; j++) ran[j] = false;
  });
  cell._ta = ta;
  cell._out = out;
  cell._run = run;
});

const webR = new WebR();
let shelter;

async function boot() {
  setStatus('正在載入瀏覽器裡的 R（第一次約 10–30 秒）…', 'busy');
  await webR.init();
  shelter = await new webR.Shelter();
  const files = (document.body.dataset.files || '').split(',').map((s) => s.trim()).filter(Boolean);
  for (const f of files) {
    const res = await fetch(`./${f}`, { cache: 'no-store' });   // always the current R/ and data files, never a stale cached copy
    if (!res.ok) throw new Error(`讀不到 ${f}（請用 start_lab 啟動本機伺服器，不要直接雙擊 HTML）`);
    const buf = new Uint8Array(await res.arrayBuffer());
    const dir = f.split('/').slice(0, -1).join('/');
    if (dir) await webR.evalRVoid(`dir.create("${dir}", recursive = TRUE, showWarnings = FALSE)`);
    await webR.FS.writeFile(`/home/web_user/${f}`, buf);
  }
  await webR.evalRVoid('setwd("/home/web_user"); options(width = 90, digits = 4)');
  const pkgs = (document.body.dataset.packages || '').split(',').map((s) => s.trim()).filter(Boolean);
  if (pkgs.length) {
    setStatus(`正在安裝 R 套件：${pkgs.join('、')}…`, 'busy');
    await webR.installPackages(pkgs, { quiet: true });
  }
  cells.forEach((c) => (c._run.disabled = false));
  document.getElementById('runall')?.removeAttribute('disabled');
  setStatus('R 已就緒。按 ▶ 執行，或在程式格裡按 Ctrl+Enter。', 'ok');
}

async function runCell(i) {
  const cell = cells[i];
  const out = cell._out;
  out.innerHTML = '';
  out.classList.add('running');
  const t0 = performance.now();
  try {
    const res = await shelter.captureR(cell._ta.value, {
      withAutoprint: true,
      captureStreams: true,
      captureConditions: false,
      captureGraphics: { width: 900, height: 540, pointsize: 17 },
    });
    const text = res.output.map((o) => o.data).join('\n');
    if (text.trim()) {
      const pre = document.createElement('pre');
      pre.textContent = text;
      if (res.output.some((o) => o.type === 'stderr')) pre.classList.add('has-err');
      out.append(pre);
    }
    for (const img of res.images) {
      const canvas = document.createElement('canvas');
      canvas.width = img.width;
      canvas.height = img.height;
      canvas.getContext('2d').drawImage(img, 0, 0);
      out.append(canvas);
    }
    ran[i] = true;
  } catch (err) {
    const pre = document.createElement('pre');
    pre.className = 'error';
    pre.textContent = `錯誤：${err.message}`;
    out.append(pre);
    throw err;
  } finally {
    await shelter.purge();
    out.classList.remove('running');
    const t = document.createElement('div');
    t.className = 'timing';
    t.textContent = `${((performance.now() - t0) / 1000).toFixed(1)} 秒`;
    out.append(t);
  }
}

async function runUpTo(i) {
  if (busy) return;
  busy = true;
  cells.forEach((c) => (c._run.disabled = true));
  try {
    for (let j = 0; j <= i; j++) {
      if (j < i && ran[j]) continue;
      setStatus(`正在執行程式格 ${j + 1}…`, 'busy');
      await runCell(j);
    }
    setStatus('完成。可以改程式碼再跑一次。', 'ok');
  } catch {
    setStatus('有程式格出錯了，看紅色訊息。', 'err');
  } finally {
    busy = false;
    cells.forEach((c) => (c._run.disabled = false));
  }
}

document.getElementById('runall')?.addEventListener('click', () => runUpTo(cells.length - 1));

boot().catch((e) => setStatus(`webR 載入失敗：${e.message}`, 'err'));
