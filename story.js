/* ==========================================================
   STORY.JS v2 - scroll asli + zoom antar slide
   Letakkan di folder www/ (sejajar dengan style.css)

   Prinsip: scroll DIKERJAKAN BROWSER (tidak dibajak lewat JS).
   JS hanya menandai slide mana yang sedang aktif untuk animasi
   zoom, mengisi progress bar, titik, dan tombol navigasi.
========================================================== */

(function () {
  "use strict";

  function init() {
    var deck = document.getElementById("story-deck");
    if (!deck) return;

    var slides = Array.prototype.slice.call(deck.children);
    var total = slides.length;
    if (!total) return;

    var cur = 0;

    deck.setAttribute("tabindex", "-1");

    /* ---------- elemen navigasi ---------- */
    var progress = document.createElement("div");
    progress.className = "story-progress";

    var dots = document.createElement("div");
    dots.className = "story-dots";

    var dotBtns = slides.map(function (_, i) {
      var b = document.createElement("button");
      b.type = "button";
      b.setAttribute("aria-label", "Slide " + (i + 1));
      b.addEventListener("click", function () { goTo(i); });
      dots.appendChild(b);
      return b;
    });

    var nav = document.createElement("div");
    nav.className = "story-nav";

    var prevBtn = document.createElement("button");
    prevBtn.type = "button";
    prevBtn.setAttribute("aria-label", "Slide sebelumnya");
    prevBtn.innerHTML = "&#9650;";
    prevBtn.addEventListener("click", function () { go(-1); });

    var count = document.createElement("div");
    count.className = "story-count";

    var nextBtn = document.createElement("button");
    nextBtn.type = "button";
    nextBtn.setAttribute("aria-label", "Slide berikutnya");
    nextBtn.innerHTML = "&#9660;";
    nextBtn.addEventListener("click", function () { go(1); });

    nav.appendChild(prevBtn);
    nav.appendChild(count);
    nav.appendChild(nextBtn);

    document.body.appendChild(progress);
    document.body.appendChild(dots);
    document.body.appendChild(nav);

    /* ---------- tandai slide aktif ---------- */
    function setActive(i) {
      cur = i;
      slides.forEach(function (s, idx) {
        s.classList.toggle("active", idx === i);
        s.classList.toggle("is-before", idx < i);
        s.classList.toggle("is-after", idx > i);
      });
      dotBtns.forEach(function (b, idx) { b.classList.toggle("active", idx === i); });
      count.textContent = (i + 1) + " / " + total;
      prevBtn.disabled = i === 0;
      nextBtn.disabled = i === total - 1;

      if (history.replaceState) history.replaceState(null, "", "#" + (i + 1));
    }

    // Slide aktif = slide yang menyentuh garis tengah layar
    // (bekerja juga untuk slide yang lebih tinggi dari layar)
    if ("IntersectionObserver" in window) {
      var io = new IntersectionObserver(function (entries) {
        entries.forEach(function (en) {
          if (en.isIntersecting) {
            var idx = slides.indexOf(en.target);
            if (idx !== -1 && idx !== cur) setActive(idx);
          }
        });
      }, { root: deck, rootMargin: "-50% 0px -50% 0px", threshold: 0 });

      slides.forEach(function (s) { io.observe(s); });
    }

    /* ---------- progress bar mengikuti posisi scroll ---------- */
    var ticking = false;
    function updateProgress() {
      var max = deck.scrollHeight - deck.clientHeight;
      var p = max > 0 ? deck.scrollTop / max : 0;
      progress.style.width = (p * 100) + "%";
      ticking = false;
    }
    deck.addEventListener("scroll", function () {
      if (!ticking) { ticking = true; requestAnimationFrame(updateProgress); }
    }, { passive: true });

    /* ---------- pindah slide (tombol, titik, keyboard) ---------- */
    function goTo(i) {
      if (i < 0 || i > total - 1) return;
      slides[i].scrollIntoView({ behavior: "smooth", block: "start" });
    }
    function go(dir) { goTo(cur + dir); }

    document.addEventListener("keydown", function (e) {
      var tag = (e.target.tagName || "").toLowerCase();
      if (tag === "input" || tag === "textarea" || tag === "select" || e.target.isContentEditable) return;
      if (e.altKey || e.ctrlKey || e.metaKey) return;

      // Panah atas/bawah dibiarkan scroll biasa; tombol ini lompat per slide
      switch (e.key) {
        case "ArrowRight":
        case "PageDown":
        case " ":
          e.preventDefault(); go(1); break;
        case "ArrowLeft":
        case "PageUp":
          e.preventDefault(); go(-1); break;
        case "Home":
          e.preventDefault(); goTo(0); break;
        case "End":
          e.preventDefault(); goTo(total - 1); break;
      }
    });

    /* ---------- peta Leaflet tidak boleh "menangkap" scroll ----------
       Scroll mouse di atas peta tetap menggulir halaman.
       Peta baru bisa di-zoom dengan scroll setelah diklik. */
    var activeMap = null;

    function deactivate() {
      if (activeMap) activeMap.classList.remove("map-active");
      activeMap = null;
    }

    document.addEventListener("wheel", function (e) {
      var m = e.target.closest && e.target.closest(".leaflet-container");
      if (m && m !== activeMap) e.stopPropagation(); // cegah Leaflet zoom; scroll halaman tetap jalan
    }, { capture: true, passive: true });

    document.addEventListener("mousedown", function (e) {
      var m = e.target.closest && e.target.closest(".leaflet-container");
      if (m === activeMap) return;
      deactivate();
      if (m) { activeMap = m; m.classList.add("map-active"); }
    });

    document.addEventListener("mouseout", function (e) {
      if (activeMap && !e.relatedTarget) deactivate();
      else if (activeMap && e.relatedTarget && !activeMap.contains(e.relatedTarget)) deactivate();
    });

    /* ---------- mulai (mendukung #nomor-slide) ---------- */
    var h = parseInt((location.hash || "").replace("#", ""), 10);
    if (!isNaN(h) && h >= 1 && h <= total) {
      deck.style.scrollBehavior = "auto";
      slides[h - 1].scrollIntoView({ block: "start" });
      deck.style.scrollBehavior = "";
      cur = h - 1;
    }
    setActive(cur);
    updateProgress();

    // aktifkan animasi zoom setelah keadaan awal terpasang (tanpa kedip)
    requestAnimationFrame(function () {
      requestAnimationFrame(function () { deck.classList.add("js-ready"); });
    });

    deck.focus({ preventScroll: true });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
