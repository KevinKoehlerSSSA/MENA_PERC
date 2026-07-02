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

function renderTranslationText(text) {
  translationTextEl.innerHTML = "";
  const lines = (text || "").split(/\r?\n/);
  lines.forEach((line, index) => {
    const lineNumber = index + 1;
    const row = document.createElement("div");
    row.className = "numberedLine translationLine";
    row.id = `translation-line-${lineNumber}`;

    const number = document.createElement("span");
    number.className = "lineNo";
    number.textContent = String(lineNumber);

    const content = document.createElement("span");
    content.className = "lineText";
    content.textContent = line || " ";

    row.append(number, content);
    translationTextEl.append(row);
  });
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
  row.scrollIntoView({ block: "center", behavior: "smooth" });
  row.animate(
    [
      { outlineColor: "rgba(180, 83, 9, 0)" },
      { outlineColor: "rgba(180, 83, 9, 0.95)" },
      { outlineColor: "rgba(180, 83, 9, 0)" },
    ],
    { duration: 1200, easing: "ease-out" },
  );
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
  translationFileEl.textContent = payload.translationFile || "No generated translation yet";
  if (payload.translationText) {
    renderTranslationText(payload.translationText);
  } else {
    renderTranslationMessage("No generated translation yet. Click Generate to translate this document and cache it locally.");
  }
  translateButton.disabled = Boolean(payload.translationText);
  translateButton.textContent = payload.translationText ? "Cached" : "Generate";
  reviewStatusEl.textContent = currentDocument.review.reviewFile ? `Saved to ${currentDocument.review.reviewFile}` : "Review not saved yet";
  renderRawText(payload.rawText, payload.events, payload.speeches);
  renderHandoffs(payload.events);
  statusEl.textContent = `${payload.events.length} handoff starts highlighted`;
}

async function generateTranslation() {
  if (!currentDocument?.sourceFile) return;
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
    translationFileEl.textContent = payload.translationFile;
    renderTranslationText(payload.translationText);
    statusEl.textContent = `Translation generated${payload.model ? ` with ${payload.model}` : ""}`;
    translateButton.disabled = true;
    translateButton.textContent = "Cached";
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
    renderTranslationText(payload.partialText);
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

init();
