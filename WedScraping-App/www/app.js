// Web Scraper Studio - small client behaviours (no framework)
(function () {
  // ---- Enter in the URL box triggers Analyze -------------------------------
  // Push the current field value to Shiny first (a paste+Enter can outrun the
  // input binding), then click Analyze on the next tick.
  document.addEventListener("keydown", function (e) {
    if (e.key === "Enter" && e.target && e.target.id === "url") {
      e.preventDefault();
      var el = e.target;
      if (window.Shiny) Shiny.setInputValue("url", el.value, { priority: "event" });
      el.dispatchEvent(new Event("change", { bubbles: true }));
      setTimeout(function () {
        var btn = document.getElementById("analyze");
        if (btn) btn.click();
      }, 80);
    }
  });

  // ---- Copy-to-clipboard for every generated-code panel --------------------
  function copyText(text, label) {
    var done = function () { toast((label || "Copied") + " to clipboard"); };
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(done, function () { legacyCopy(text); done(); });
    } else { legacyCopy(text); done(); }
  }
  function legacyCopy(text) {
    var ta = document.createElement("textarea");
    ta.value = text; ta.style.position = "fixed"; ta.style.opacity = "0";
    document.body.appendChild(ta); ta.select();
    try { document.execCommand("copy"); } catch (e) {}
    document.body.removeChild(ta);
  }
  function toast(msg) {
    var t = document.createElement("div");
    t.className = "copy-toast";
    t.textContent = msg;
    document.body.appendChild(t);
    requestAnimationFrame(function () { t.classList.add("show"); });
    setTimeout(function () {
      t.classList.remove("show");
      setTimeout(function () { t.remove(); }, 250);
    }, 1600);
  }

  // Add a Copy button beside any code <pre>. The button goes in a WRAPPER,
  // not inside the <pre> — Shiny's text-output binding overwrites the pre's
  // content on every render and would wipe a child button.
  function decorateCodeBlocks() {
    document.querySelectorAll(".code-accordion pre").forEach(function (pre) {
      if (pre.parentElement && pre.parentElement.classList.contains("code-wrap")) return;
      var wrap = document.createElement("div");
      wrap.className = "code-wrap";
      pre.parentNode.insertBefore(wrap, pre);
      wrap.appendChild(pre);
      var btn = document.createElement("button");
      btn.type = "button";
      btn.className = "copy-code-btn";
      btn.textContent = "Copy";
      btn.addEventListener("click", function (ev) {
        ev.stopPropagation();
        copyText(pre.innerText, "R script");
      });
      wrap.appendChild(btn);
    });
  }
  document.addEventListener("shiny:value", decorateCodeBlocks);
  document.addEventListener("shiny:idle", decorateCodeBlocks);
  window.addEventListener("load", decorateCodeBlocks);

  // ---- Server-driven copy (e.g. "Copy data (TSV)") -------------------------
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("copy_text", function (m) {
      copyText(m.text || "", m.label || "Copied");
    });
  }
})();
