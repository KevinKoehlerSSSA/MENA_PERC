const documentSelect = document.querySelector("#documentSelect");
const searchInput = document.querySelector("#searchInput");
const statusEl = document.querySelector("#status");
const rawFileEl = document.querySelector("#rawFile");
const translationFileEl = document.querySelector("#translationFile");
const rawTextEl = document.querySelector("#rawText");
const translationTextEl = document.querySelector("#translationText");
const handoffListEl = document.querySelector("#handoffList");
const handoffCountEl = document.querySelector("#handoffCount");
const translateButton = document.querySelector("#translateButton");
const selectedLineEl = document.querySelector("#selectedLine");
const additionalNoteEl = document.querySelector("#additionalNote");
const addHandoffButton = document.querySelector("#addHandoffButton");
const reviewStatusEl = document.querySelector("#reviewStatus");

let documents = [];
let filteredDocuments = [];
let currentDocument = null;
let translationPoll = null;
let selectedRawLine = null;

async function fetchJson(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Request failed: ${response.status}`);
  }
  return response.json();
}

async function postJson(url, body) {
  const response = await fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  const payload = await response.json();
  if (!response.ok) {
    throw new Error(payload.error || `Request failed: ${response.status}`);
  }
  return payload;
}

function shortEvidence(text) {
  const compact = (text || "").replace(/\s+/g, " ").trim();
  return compact.length > 130 ? `${compact.slice(0, 127)}...` : compact;
}

function renderDocumentOptions() {
  const selected = documentSelect.value;
  documentSelect.innerHTML = "";
  for (const doc of filteredDocuments) {
    const option = document.createElement("option");
    option.value = doc.sourceFile;
    option.textContent = `${doc.sourceFile} (${doc.handoffCount})`;
    documentSelect.append(option);
  }
  const stillSelected = filteredDocuments.some((doc) => doc.sourceFile === selected);
  if (stillSelected) {
    documentSelect.value = selected;
  }
}

function applyFilter() {
  const needle = searchInput.value.trim().toLowerCase();
  filteredDocuments = documents.filter((doc) => doc.sourceFile.toLowerCase().includes(needle));
  renderDocumentOptions();
  if (filteredDocuments.length && !filteredDocuments.some((doc) => doc.sourceFile === currentDocument?.sourceFile)) {
    loadDocument(filteredDocuments[0].sourceFile);
  }
}

function lineTitle(lineNumber, eventsByLine, speechesByStart) {
  const event = eventsByLine.get(lineNumber);
  const speech = speechesByStart.get(lineNumber);
  const pieces = [];
  if (event) {
    pieces.push(`Handoff: ${event.candidateClean || event.candidateRaw}`);
    pieces.push(`Method: ${event.assignmentMethod}`);
  }
  if (speech) {
    pieces.push(`Speech start: ${speech.speakerTargetClean}`);
    if (speech.needsReview) pieces.push("Needs review");
  }
  return pieces.join("\n");
}

function currentReview() {
  if (!currentDocument.review) {
    currentDocument.review = { sourceFile: currentDocument.sourceFile, decisions: {}, additionalHandoffs: [] };
  }
  currentDocument.review.decisions ||= {};
  currentDocument.review.additionalHandoffs ||= [];
  return currentDocument.review;
}

function detectedDecision(lineNumber) {
  return currentReview().decisions[String(lineNumber)] || "";
}

function additionalHandoffsByLine() {
  const byLine = new Map();
  for (const handoff of currentReview().additionalHandoffs) {
    const line = Number(handoff.line);
    if (!Number.isFinite(line)) continue;
    if (!byLine.has(line)) byLine.set(line, []);
    byLine.get(line).push(handoff);
  }
  return byLine;
}

const FULL_TRANSLATION_MAX_LINES = 800;

function rawLineCount() {
  return currentDocument ? currentDocument.rawText.split(/\r?\n/).length : 0;
}

function translationForLine(lineNumber) {
  if (currentDocument.translationText) {
    return currentDocument.translationText.split(/\r?\n/)[lineNumber - 1] || "";
  }
  return (currentDocument.lineTranslations || {})[String(lineNumber)] || "";
}

// One row per raw line, always — untranslated lines render as placeholders,
// which keeps the two panes line-aligned for scroll sync.
function renderTranslationGrid() {
  translationTextEl.innerHTML = "";
  const total = rawLineCount();
  const fullLines = currentDocument.translationText
    ? currentDocument.translationText.split(/\r?\n/)
    : null;
  const cached = currentDocument.lineTranslations || {};
  for (let lineNumber = 1; lineNumber <= total; lineNumber += 1) {
    const line = fullLines ? fullLines[lineNumber - 1] || "" : cached[String(lineNumber)] || "";
    const row = document.createElement("div");
    row.className = line ? "numberedLine translationLine" : "numberedLine translationLine untranslated";
    row.id = `translation-line-${lineNumber}`;

    const number = document.createElement("span");
    number.className = "lineNo";
    number.textContent = String(lineNumber);

    const content = document.createElement("span");
    content.className = "lineText";
    content.textContent = line || " ";

    row.append(number, content);
    translationTextEl.append(row);
  }
}

function updateTranslationLines(lines) {
  currentDocument.lineTranslations ||= {};
  for (const [lineNumber, text] of Object.entries(lines)) {
    if (!text) continue;
    currentDocument.lineTranslations[String(lineNumber)] = text;
    const row = document.querySelector(`#translation-line-${lineNumber}`);
    if (row) {
      row.classList.remove("untranslated");
      row.querySelector(".lineText").textContent = text;
    }
  }
}

function translatedLineCount() {
  return Object.values(currentDocument.lineTranslations || {}).filter(Boolean).length;
}

function renderTranslationMessage(message) {
  translationTextEl.innerHTML = "";
  const empty = document.createElement("div");
  empty.className = "emptyState";
  empty.textContent = message;
  translationTextEl.append(empty);
}

function renderRawText(rawText, events, speeches) {
  rawTextEl.innerHTML = "";
  const eventsByLine = new Map(events.map((event) => [event.line, event]));
  const speechesByStart = new Map(speeches.map((speech) => [speech.startLine, speech]));
  const additionsByLine = additionalHandoffsByLine();
  const lines = rawText.split(/\r?\n/);

  lines.forEach((line, index) => {
    const lineNumber = index + 1;
    const row = document.createElement("div");
    row.className = "numberedLine rawLine";
    row.id = `line-${lineNumber}`;
    if (eventsByLine.has(lineNumber)) row.classList.add("handoffStart");
    if (additionsByLine.has(lineNumber)) row.classList.add("additionalHandoff");
    if (detectedDecision(lineNumber) === "accepted") row.classList.add("acceptedHandoff");
    if (detectedDecision(lineNumber) === "rejected") row.classList.add("rejectedHandoff");
    if (speechesByStart.has(lineNumber)) row.classList.add("speechStart");
    if (selectedRawLine === lineNumber) row.classList.add("selectedRawLine");
    row.title = lineTitle(lineNumber, eventsByLine, speechesByStart);
    row.addEventListener("click", () => selectRawLine(lineNumber));

    const number = document.createElement("span");
    number.className = "lineNo";
    number.textContent = String(lineNumber);

    const text = document.createElement("span");
    text.className = "lineText";
    text.textContent = line || " ";

    row.append(number, text);
    rawTextEl.append(row);
  });
}

function selectRawLine(lineNumber) {
  selectedRawLine = lineNumber;
  selectedLineEl.textContent = String(lineNumber);
  renderRawText(currentDocument.rawText, currentDocument.events, currentDocument.speeches);
}

function scrollToLine(lineNumber) {
  const row = document.querySelector(`#line-${lineNumber}`);
  if (!row) return;
  syncPauseUntil = Date.now() + 1200; // let both smooth scrolls finish unfought
  row.scrollIntoView({ block: "center", behavior: "smooth" });
  const translationRow = document.querySelector(`#translation-line-${lineNumber}`);
  if (translationRow) translationRow.scrollIntoView({ block: "center", behavior: "smooth" });
  for (const target of [row, translationRow]) {
    if (!target) continue;
    target.animate(
      [
        { outlineColor: "rgba(180, 83, 9, 0)" },
        { outlineColor: "rgba(180, 83, 9, 0.95)" },
        { outlineColor: "rgba(180, 83, 9, 0)" },
      ],
      { duration: 1200, easing: "ease-out" },
    );
  }
}

// --- Scroll synchronization: the panes are line-aligned by row id, so map
// the first visible line of the scrolled pane onto the other pane.
// Time-based locks (not counters): scroll events coalesce unpredictably, so
// counting programmatic sets leaks and deadlocks the sync.
let syncPauseUntil = 0;
let syncEchoUntil = { raw: 0, translation: 0 };

function firstVisibleRow(container) {
  const rows = container.children;
  if (!rows.length) return null;
  const containerTop = container.getBoundingClientRect().top;
  let lo = 0;
  let hi = rows.length - 1;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    const rect = rows[mid].getBoundingClientRect();
    if (rect.bottom - containerTop <= 0) {
      lo = mid + 1;
    } else {
      hi = mid;
    }
  }
  return rows[lo];
}

function lineNumberOfRow(row) {
  const match = /(\d+)$/.exec(row?.id || "");
  return match ? Number(match[1]) : null;
}

function syncScroll(fromEl, toEl, fromKey, toKey, toPrefix) {
  const now = Date.now();
  if (now < syncPauseUntil || now < syncEchoUntil[fromKey]) return;
  const row = firstVisibleRow(fromEl);
  const lineNumber = lineNumberOfRow(row);
  if (!lineNumber) return;
  const targetRow = document.querySelector(`#${toPrefix}${lineNumber}`);
  if (!targetRow) return;
  // .rawText / .translationText are position:relative, so offsetTop is
  // relative to the scroll container's content box.
  const offset = targetRow.offsetTop;
  if (Math.abs(toEl.scrollTop - offset) < 2) return;
  syncEchoUntil[toKey] = now + 200;
  toEl.scrollTop = offset;
}

function setupScrollSync() {
  // No rAF throttling: rAF stalls in background tabs and jams the tick flag.
  // The handler is cheap (binary search over rows), direct calls are fine.
  rawTextEl.addEventListener("scroll", () => {
    syncScroll(rawTextEl, translationTextEl, "raw", "translation", "translation-line-");
  });
  translationTextEl.addEventListener("scroll", () => {
    syncScroll(translationTextEl, rawTextEl, "translation", "raw", "line-");
  });
}

function renderHandoffs(events) {
  handoffListEl.innerHTML = "";
  const additions = currentReview().additionalHandoffs;
  handoffCountEl.textContent = String(events.length + additions.length);
  if (!events.length && !additions.length) {
    const empty = document.createElement("div");
    empty.className = "emptyState";
    empty.textContent = "No handoff starts found for this file.";
    handoffListEl.append(empty);
    return;
  }

  for (const event of events) {
    const card = document.createElement("div");
    const decision = detectedDecision(event.line);
    card.className = `handoffItem ${decision || ""}`;
    card.addEventListener("click", () => scrollToLine(event.line));

    const meta = document.createElement("div");
    meta.className = "handoffMeta";
    meta.innerHTML = `<span>Line ${event.line}</span><span>${decision || event.speakerType || "candidate"}</span>`;

    const name = document.createElement("div");
    name.className = "handoffName";
    name.textContent = event.candidateClean || event.candidateRaw || "Unnamed candidate";

    const evidence = document.createElement("div");
    evidence.className = "handoffEvidence";
    evidence.textContent = shortEvidence(event.evidenceText);

    const actions = document.createElement("div");
    actions.className = "handoffActions";
    const accept = document.createElement("button");
    accept.className = `acceptButton ${decision === "accepted" ? "active" : ""}`;
    accept.type = "button";
    accept.textContent = "Accept";
    accept.addEventListener("click", (eventObject) => {
      eventObject.stopPropagation();
      setDecision(event.line, "accepted");
    });
    const reject = document.createElement("button");
    reject.className = `rejectButton ${decision === "rejected" ? "active" : ""}`;
    reject.type = "button";
    reject.textContent = "Reject";
    reject.addEventListener("click", (eventObject) => {
      eventObject.stopPropagation();
      setDecision(event.line, "rejected");
    });
    actions.append(accept, reject);

    card.append(meta, name, evidence, actions);
    handoffListEl.append(card);
  }

  additions.forEach((handoff, index) => {
    const card = document.createElement("div");
    card.className = "handoffItem additional";
    card.addEventListener("click", () => scrollToLine(Number(handoff.line)));

    const meta = document.createElement("div");
    meta.className = "handoffMeta";
    meta.innerHTML = `<span>Line ${handoff.line}</span><span>added</span>`;

    const name = document.createElement("div");
    name.className = "handoffName";
    name.textContent = "Reviewer-added handoff";

    const evidence = document.createElement("div");
    evidence.className = "handoffEvidence";
    evidence.textContent = handoff.note || shortEvidence(handoff.text || "");

    const actions = document.createElement("div");
    actions.className = "handoffActions";
    const remove = document.createElement("button");
    remove.className = "rejectButton";
    remove.type = "button";
    remove.textContent = "Remove";
    remove.addEventListener("click", (eventObject) => {
      eventObject.stopPropagation();
      removeAdditionalHandoff(index);
    });
    actions.append(remove);

    card.append(meta, name, evidence, actions);
    handoffListEl.append(card);
  });
}

async function saveReview() {
  reviewStatusEl.textContent = "Saving review...";
  const saved = await postJson("/api/review", currentReview());
  currentDocument.review = saved;
  reviewStatusEl.textContent = `Saved to ${saved.reviewFile}`;
}

function setDecision(lineNumber, decision) {
  const review = currentReview();
  const key = String(lineNumber);
  if (review.decisions[key] === decision) {
    delete review.decisions[key];
  } else {
    review.decisions[key] = decision;
  }
  renderHandoffs(currentDocument.events);
  renderRawText(currentDocument.rawText, currentDocument.events, currentDocument.speeches);
  saveReview().catch((error) => {
    reviewStatusEl.textContent = error.message;
  });
}

function lineText(lineNumber) {
  return (currentDocument.rawText.split(/\r?\n/)[lineNumber - 1] || "").trim();
}

function addAdditionalHandoff() {
  if (!selectedRawLine) {
    reviewStatusEl.textContent = "Select a raw Arabic line first.";
    return;
  }
  const review = currentReview();
  const exists = review.additionalHandoffs.some((handoff) => Number(handoff.line) === selectedRawLine);
  if (exists) {
    reviewStatusEl.textContent = `Line ${selectedRawLine} is already added.`;
    return;
  }
  review.additionalHandoffs.push({
    line: selectedRawLine,
    note: additionalNoteEl.value.trim(),
    text: lineText(selectedRawLine),
    createdAt: new Date().toISOString(),
  });
  additionalNoteEl.value = "";
  renderHandoffs(currentDocument.events);
  renderRawText(currentDocument.rawText, currentDocument.events, currentDocument.speeches);
  saveReview().catch((error) => {
    reviewStatusEl.textContent = error.message;
  });
}

function removeAdditionalHandoff(index) {
  currentReview().additionalHandoffs.splice(index, 1);
  renderHandoffs(currentDocument.events);
  renderRawText(currentDocument.rawText, currentDocument.events, currentDocument.speeches);
  saveReview().catch((error) => {
    reviewStatusEl.textContent = error.message;
  });
}

async function loadDocument(sourceFile) {
  if (translationPoll) clearInterval(translationPoll);
  translationPoll = null;
  selectedRawLine = null;
  selectedLineEl.textContent = "none";
  statusEl.textContent = "Loading document...";
  const payload = await fetchJson(`/api/document?source=${encodeURIComponent(sourceFile)}`);
  currentDocument = payload;
  currentReview();
  documentSelect.value = sourceFile;
  rawFileEl.textContent = payload.sourceFile;
  renderRawText(payload.rawText, payload.events, payload.speeches);
  renderTranslationGrid();
  updateTranslateControls();
  reviewStatusEl.textContent = currentDocument.review.reviewFile ? `Saved to ${currentDocument.review.reviewFile}` : "Review not saved yet";
  renderHandoffs(payload.events);
  statusEl.textContent = `${payload.events.length} handoff starts highlighted`;
}

function updateTranslateControls() {
  const total = rawLineCount();
  if (currentDocument.translationText) {
    translationFileEl.textContent = currentDocument.translationFile;
    translateButton.disabled = true;
    translateButton.textContent = "Cached";
    return;
  }
  const cached = translatedLineCount();
  translationFileEl.textContent = cached
    ? `${cached} of ${total} lines translated`
    : "No translation yet";
  translateButton.disabled = false;
  if (total > FULL_TRANSLATION_MAX_LINES) {
    translateButton.textContent = "Translate section";
    translateButton.title =
      "Large document: translates only the selected speech window or the visible lines (max 300 per click).";
  } else {
    translateButton.textContent = "Generate";
    translateButton.title = "Translate the whole document.";
  }
}

// Pick a bounded line range worth translating: the speech window around the
// selected line if any, otherwise the lines currently visible in the raw pane.
function sectionRange() {
  const total = rawLineCount();
  if (selectedRawLine) {
    const speech = (currentDocument.speeches || []).find(
      (item) => item.startLine <= selectedRawLine && selectedRawLine <= item.endLine,
    );
    if (speech) {
      return { start: Math.max(1, speech.evidenceLine || speech.startLine), end: speech.endLine };
    }
    return { start: Math.max(1, selectedRawLine - 20), end: Math.min(total, selectedRawLine + 80) };
  }
  const firstRow = firstVisibleRow(rawTextEl);
  const first = lineNumberOfRow(firstRow) || 1;
  const visibleLines = Math.ceil(rawTextEl.clientHeight / (firstRow?.offsetHeight || 24));
  return { start: first, end: Math.min(total, first + Math.max(visibleLines, 40) + 20) };
}

async function translateSection() {
  const range = sectionRange();
  translateButton.disabled = true;
  translateButton.textContent = `Translating ${range.start}-${range.end}...`;
  statusEl.textContent = `Translating lines ${range.start}-${range.end}`;
  try {
    const payload = await postJson("/api/translate-range", {
      source: currentDocument.sourceFile,
      start: range.start,
      end: range.end,
    });
    updateTranslationLines(payload.lines || {});
    statusEl.textContent = `Translated lines ${payload.start}-${payload.end} (${payload.requestedLines} new)`;
  } catch (error) {
    statusEl.textContent = error.message;
  }
  updateTranslateControls();
}

async function generateTranslation() {
  if (!currentDocument?.sourceFile) return;
  if (rawLineCount() > FULL_TRANSLATION_MAX_LINES && !currentDocument.translationText) {
    await translateSection();
    return;
  }
  if (translationPoll) clearInterval(translationPoll);
  translateButton.disabled = true;
  translateButton.textContent = "Starting...";
  renderTranslationMessage("Starting English translation job.");
  try {
    await postJson("/api/translate", { source: currentDocument.sourceFile });
    await pollTranslationStatus(currentDocument.sourceFile);
    translationPoll = setInterval(() => {
      pollTranslationStatus(currentDocument.sourceFile).catch((error) => {
        if (translationPoll) clearInterval(translationPoll);
        translationPoll = null;
        renderTranslationMessage(error.message);
        statusEl.textContent = "Translation status unavailable";
        translateButton.disabled = false;
        translateButton.textContent = "Generate";
      });
    }, 2500);
  } catch (error) {
    renderTranslationMessage(error.message);
    statusEl.textContent = "Translation failed";
    translateButton.disabled = false;
    translateButton.textContent = "Generate";
  }
}

async function pollTranslationStatus(sourceFile) {
  const payload = await fetchJson(`/api/translation-status?source=${encodeURIComponent(sourceFile)}`);
  if (sourceFile !== currentDocument?.sourceFile) return;

  const done = Number(payload.doneChunks || 0);
  const total = Number(payload.totalChunks || 0);
  const progress = total ? `chunk ${done} of ${total}` : "preparing chunks";

  if (payload.status === "done") {
    if (translationPoll) clearInterval(translationPoll);
    translationPoll = null;
    currentDocument.translationFile = payload.translationFile;
    currentDocument.translationText = payload.translationText;
    renderTranslationGrid();
    updateTranslateControls();
    statusEl.textContent = `Translation generated${payload.model ? ` with ${payload.model}` : ""}`;
    return;
  }

  if (payload.status === "error") {
    if (translationPoll) clearInterval(translationPoll);
    translationPoll = null;
    renderTranslationMessage(payload.error || "Translation failed.");
    statusEl.textContent = "Translation failed";
    translateButton.disabled = false;
    translateButton.textContent = "Generate";
    return;
  }

  translateButton.disabled = true;
  translateButton.textContent = "Generating...";
  statusEl.textContent = `Translation running: ${progress}`;
  if (payload.partialText) {
    translationFileEl.textContent = "Partial translation cache";
    currentDocument.translationText = payload.partialText;
    renderTranslationGrid();
    currentDocument.translationText = "";
  } else {
    renderTranslationMessage(`Generating English translation: ${progress}.`);
  }
}

async function init() {
  try {
    const payload = await fetchJson("/api/documents");
    documents = payload.documents;
    filteredDocuments = documents;
    renderDocumentOptions();
    statusEl.textContent = `${documents.length} raw Arabic files`;
    const firstWithHandoffs = documents.find((doc) => doc.handoffCount > 0) || documents[0];
    if (firstWithHandoffs) await loadDocument(firstWithHandoffs.sourceFile);
  } catch (error) {
    statusEl.textContent = error.message;
  }
}

searchInput.addEventListener("input", applyFilter);
documentSelect.addEventListener("change", () => loadDocument(documentSelect.value));
translateButton.addEventListener("click", generateTranslation);
addHandoffButton.addEventListener("click", addAdditionalHandoff);

setupScrollSync();
init();
