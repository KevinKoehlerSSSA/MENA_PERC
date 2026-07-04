const statusEl = document.getElementById("status");
const listEl = document.getElementById("sampleList");
const translateAllButton = document.getElementById("translateAllButton");

let pollTimer = null;

function speakerLabel(speech) {
  if (speech.mp_name_transliterated) {
    return `${speech.mp_name_ar} (${speech.mp_name_transliterated})`;
  }
  return speech.mp_name_ar || speech.speaker_role || speech.speaker_target_clean || "unknown";
}

function render(speeches, job) {
  const translated = speeches.filter((s) => s.translation).length;
  statusEl.textContent = `${speeches.length} sampled speeches — ${translated} translated`;

  if (job && job.status === "running") {
    translateAllButton.disabled = true;
    translateAllButton.textContent = `Translating ${job.doneChunks || 0}/${job.totalChunks || "?"}...`;
  } else {
    translateAllButton.disabled = false;
    translateAllButton.textContent = translated < speeches.length ? "Translate All" : "All Translated";
    if (job && job.status === "error") {
      statusEl.textContent = `Translation error: ${job.error}`;
    }
  }

  listEl.replaceChildren(
    ...speeches.map((speech, index) => {
      const card = document.createElement("article");
      card.className = "speechCard";

      const meta = document.createElement("div");
      meta.className = "speechMeta";
      const tagClass = speech.speaker_type === "mp" ? "tag" : "tag unmatched";
      meta.innerHTML = `
        <span>#${index + 1}</span>
        <span class="speaker"></span>
        <span class="${tagClass}"></span>
        <span></span>
        <span></span>
        <span></span>
        <span></span>`;
      const [, speakerEl, typeEl, dateEl, periodEl, fileEl, confEl] = meta.children;
      speakerEl.textContent = speakerLabel(speech);
      typeEl.textContent = speech.speaker_type;
      dateEl.textContent = speech.inferred_date || "";
      periodEl.textContent = speech.legislature_period || "";
      fileEl.textContent = speech.source_file;
      confEl.textContent = speech.assignment_confidence ? `conf ${speech.assignment_confidence}` : "";
      card.appendChild(meta);

      const body = document.createElement("div");
      body.className = "speechBody";
      const en = document.createElement("div");
      en.className = speech.translation ? "speechEn" : "speechEn pending";
      en.textContent = speech.translation || "Not translated yet.";
      const ar = document.createElement("div");
      ar.className = "speechAr";
      ar.textContent = speech.text;
      body.append(en, ar);
      card.appendChild(body);
      return card;
    })
  );
}

async function refresh() {
  const response = await fetch("/api/speech-sample");
  const payload = await response.json();
  render(payload.speeches || [], payload.job || {});
  const running = payload.job && payload.job.status === "running";
  clearTimeout(pollTimer);
  if (running) {
    pollTimer = setTimeout(refresh, 4000);
  }
}

translateAllButton.addEventListener("click", async () => {
  translateAllButton.disabled = true;
  await fetch("/api/speech-sample/translate", { method: "POST" });
  refresh();
});

refresh().catch((error) => {
  statusEl.textContent = `Failed to load sample: ${error}`;
});
