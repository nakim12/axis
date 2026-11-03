(function () {
  'use strict';

  var BRASS = '160, 118, 58';
  var BRASS_DEEP = '111, 79, 31';
  var INK = '29, 26, 21';
  var PARCHMENT = '#f1ebdf';

  var DEG = Math.PI / 180;
  var OBLIQUITY = 23.44 * DEG;
  var LATITUDE = 38 * DEG;
  var PERSPECTIVE = 5.5;

  var reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  // ── Geometry ──────────────────────────────────────────────────────────────

  function rotX(p, a) {
    var c = Math.cos(a), s = Math.sin(a);
    return [p[0], p[1] * c - p[2] * s, p[1] * s + p[2] * c];
  }
  function rotY(p, a) {
    var c = Math.cos(a), s = Math.sin(a);
    return [p[0] * c + p[2] * s, p[1], -p[0] * s + p[2] * c];
  }
  function rotZ(p, a) {
    var c = Math.cos(a), s = Math.sin(a);
    return [p[0] * c - p[1] * s, p[0] * s + p[1] * c, p[2]];
  }

  // Circle in the local XZ plane at height y, optionally re-oriented by fn.
  function circle(r, y, n, fn) {
    var pts = [];
    for (var i = 0; i <= n; i++) {
      var t = (i / n) * Math.PI * 2;
      var p = [r * Math.cos(t), y, r * Math.sin(t)];
      pts.push(fn ? fn(p) : p);
    }
    return pts;
  }

  // Radial graduation ticks for a ring of radius r, from r to r + len.
  function ticks(r, count, majorEvery, len, majorLen, fn) {
    var out = [];
    for (var i = 0; i < count; i++) {
      var t = (i / count) * Math.PI * 2;
      var l = i % majorEvery === 0 ? majorLen : len;
      var a = [r * Math.cos(t), 0, r * Math.sin(t)];
      var b = [(r + l) * Math.cos(t), 0, (r + l) * Math.sin(t)];
      out.push({ a: fn ? fn(a) : a, b: fn ? fn(b) : b, at: i / count, major: i % majorEvery === 0 });
    }
    return out;
  }

  var toMeridianPlane = function (p) { return rotX(p, Math.PI / 2); };
  var toEcliptic = function (p) { return rotX(p, OBLIQUITY); };
  var toColureA = function (p) { return rotX(p, Math.PI / 2); };
  var toColureB = function (p) { return rotY(rotX(p, Math.PI / 2), Math.PI / 2); };

  var tropic = Math.sin(OBLIQUITY);
  var polar = Math.sin(Math.PI / 2 - OBLIQUITY);

  // space: 'frame' stays put (meridian + horizon), 'cage' spins about the polar axis.
  var RINGS = [
    { space: 'frame', pts: circle(1.16, 0, 220, toMeridianPlane), w: 1.1, t0: 0.10, dur: 1.3 },
    { space: 'frame', pts: circle(1.24, 0, 220, toMeridianPlane), w: 1.1, t0: 0.15, dur: 1.3 },
    { space: 'frame', pts: circle(1.30, 0, 220), w: 1.0, t0: 0.30, dur: 1.4 },
    { space: 'frame', pts: circle(1.40, 0, 220), w: 1.0, t0: 0.35, dur: 1.4 },

    { space: 'cage', pts: circle(1.0, 0, 180), w: 1.0, t0: 0.55, dur: 1.2 },
    { space: 'cage', pts: circle(Math.cos(OBLIQUITY), tropic, 150), w: 0.7, t0: 0.80, dur: 1.1 },
    { space: 'cage', pts: circle(Math.cos(OBLIQUITY), -tropic, 150), w: 0.7, t0: 0.85, dur: 1.1 },
    { space: 'cage', pts: circle(Math.cos(Math.PI / 2 - OBLIQUITY), polar, 90), w: 0.7, t0: 0.95, dur: 1.0 },
    { space: 'cage', pts: circle(Math.cos(Math.PI / 2 - OBLIQUITY), -polar, 90), w: 0.7, t0: 1.00, dur: 1.0 },
    { space: 'cage', pts: circle(1.0, 0, 180, toColureA), w: 0.8, t0: 0.70, dur: 1.2 },
    { space: 'cage', pts: circle(1.0, 0, 180, toColureB), w: 0.8, t0: 0.75, dur: 1.2 },
    { space: 'cage', pts: circle(0.965, 0, 180, toEcliptic), w: 0.9, t0: 0.60, dur: 1.3, strong: true },
    { space: 'cage', pts: circle(1.035, 0, 180, toEcliptic), w: 0.9, t0: 0.65, dur: 1.3, strong: true }
  ];

  var TICK_SETS = [
    { space: 'frame', ticks: ticks(1.16, 72, 6, 0.04, 0.08, toMeridianPlane), ring: 0 },
    { space: 'frame', ticks: ticks(1.30, 72, 9, 0.05, 0.10), ring: 2 },
    { space: 'cage', ticks: ticks(0.965, 72, 6, 0.035, 0.07, toEcliptic), ring: 11 }
  ];

  var MERIDIAN_LABELS = [];
  for (var deg = 0; deg < 360; deg += 30) {
    var a = deg * DEG;
    MERIDIAN_LABELS.push({
      p: toMeridianPlane([1.075 * Math.cos(a), 0, 1.075 * Math.sin(a)]),
      text: String(deg === 0 ? 0 : deg <= 180 ? deg : 360 - deg)
    });
  }

  var EARTH_R = 0.17;
  var EARTH_LINES = [
    circle(EARTH_R, 0, 60),
    circle(EARTH_R, 0, 60, toColureA),
    circle(EARTH_R * Math.cos(35 * DEG), EARTH_R * Math.sin(35 * DEG), 50),
    circle(EARTH_R * Math.cos(35 * DEG), -EARTH_R * Math.sin(35 * DEG), 50)
  ];

  // Seeded scatter so the star field is identical on every load.
  var STARS = (function () {
    var seed = 7, out = [];
    function rnd() { seed = (seed * 16807) % 2147483647; return (seed - 1) / 2147483646; }
    for (var i = 0; i < 170; i++) {
      out.push({ x: rnd(), y: rnd(), r: 0.5 + rnd() * 1.1, a: 0.12 + rnd() * 0.3 });
    }
    return out;
  })();

  // ── Renderer ──────────────────────────────────────────────────────────────

  function Armillary(canvas) {
    this.canvas = canvas;
    this.ctx = canvas.getContext('2d');
    this.dpr = 1;
    this.w = 0;
    this.h = 0;
    this.start = null;
    this.scroll = 0;
    this.mouse = [0, 0];
    this.mouseLerp = [0, 0];
  }

  Armillary.prototype.resize = function () {
    var rect = this.canvas.getBoundingClientRect();
    if (!rect.width || !rect.height) return false;
    this.dpr = Math.min(window.devicePixelRatio || 1, 2);
    this.w = rect.width;
    this.h = rect.height;
    this.canvas.width = Math.round(rect.width * this.dpr);
    this.canvas.height = Math.round(rect.height * this.dpr);
    return true;
  };

  Armillary.prototype.state = function (t) {
    var s = this.scroll;
    this.mouseLerp[0] += (this.mouse[0] - this.mouseLerp[0]) * 0.04;
    this.mouseLerp[1] += (this.mouse[1] - this.mouseLerp[1]) * 0.04;
    return {
      spin: 0.6 + t * 0.09 + s * 2.4,
      yaw: -0.55 + t * 0.02 + s * 0.8 + this.mouseLerp[0] * 0.12,
      pitch: 0.3 + s * 0.12 + this.mouseLerp[1] * 0.06
    };
  };

  Armillary.prototype.project = function (p, space, st) {
    var q = p;
    if (space === 'cage') {
      q = rotY(q, st.spin);
      q = rotZ(q, -(Math.PI / 2 - LATITUDE));
    }
    q = rotY(q, st.yaw);
    q = rotX(q, st.pitch);
    var k = PERSPECTIVE / (PERSPECTIVE - q[2]);
    return [this.cx + q[0] * this.S * k, this.cy - q[1] * this.S * k, q[2]];
  };

  function ease(x) {
    x = Math.max(0, Math.min(1, x));
    return 1 - Math.pow(1 - x, 3);
  }

  // Draws one pass (back or front) of a projected polyline up to fraction f.
  Armillary.prototype.strokeRuns = function (proj, f, width, front, strong) {
    var ctx = this.ctx;
    var last = Math.floor((proj.length - 1) * f);
    if (last < 1) return;
    ctx.beginPath();
    var open = false;
    for (var i = 0; i < last; i++) {
      var a = proj[i], b = proj[i + 1];
      var isFront = (a[2] + b[2]) / 2 > -0.02;
      if (isFront !== front) { open = false; continue; }
      if (!open) { ctx.moveTo(a[0], a[1]); open = true; }
      ctx.lineTo(b[0], b[1]);
    }
    ctx.lineWidth = width * this.dpr * (front ? 1 : 0.8);
    ctx.strokeStyle = front
      ? 'rgba(' + (strong ? BRASS_DEEP : BRASS) + ',' + (strong ? 0.85 : 0.75) + ')'
      : 'rgba(' + BRASS + ',0.26)';
    ctx.setLineDash(front ? [] : [1.5 * this.dpr, 4 * this.dpr]);
    ctx.stroke();
  };

  Armillary.prototype.draw = function (t) {
    var ctx = this.ctx;
    var W = this.canvas.width, H = this.canvas.height;
    var st = this.state(t);

    this.S = Math.min(W, H) * (W < H ? 0.36 : 0.3);
    this.cx = W / 2;
    this.cy = H * 0.5;

    ctx.clearRect(0, 0, W, H);
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';

    // Star field + astrolabe plate
    var starFade = ease(t / 1.6);
    for (var i = 0; i < STARS.length; i++) {
      var s = STARS[i];
      ctx.beginPath();
      ctx.arc(s.x * W, s.y * H, s.r * this.dpr, 0, Math.PI * 2);
      ctx.fillStyle = 'rgba(' + INK + ',' + (s.a * starFade) + ')';
      ctx.fill();
    }
    var plateF = ease(t / 1.4);
    var plateR = this.S * 1.72;
    ctx.save();
    ctx.translate(this.cx, this.cy);
    ctx.rotate(st.yaw * 0.25);
    ctx.setLineDash([1 * this.dpr, 5 * this.dpr]);
    ctx.lineWidth = 0.8 * this.dpr;
    ctx.strokeStyle = 'rgba(' + BRASS + ',' + 0.35 * plateF + ')';
    ctx.beginPath();
    ctx.arc(0, 0, plateR, -Math.PI / 2, -Math.PI / 2 + Math.PI * 2 * plateF);
    ctx.stroke();
    ctx.setLineDash([]);
    for (var k = 0; k < 120; k++) {
      if (k / 120 > plateF) break;
      var ang = (k / 120) * Math.PI * 2 - Math.PI / 2;
      var len = (k % 10 === 0 ? 10 : 4) * this.dpr;
      ctx.beginPath();
      ctx.moveTo(Math.cos(ang) * plateR, Math.sin(ang) * plateR);
      ctx.lineTo(Math.cos(ang) * (plateR + len), Math.sin(ang) * (plateR + len));
      ctx.strokeStyle = 'rgba(' + BRASS + ',' + (k % 10 === 0 ? 0.4 : 0.22) * plateF + ')';
      ctx.stroke();
    }
    ctx.restore();

    // Project every ring once, then draw back halves, earth, front halves.
    var self = this;
    var projected = RINGS.map(function (ring) {
      return ring.pts.map(function (p) { return self.project(p, ring.space, st); });
    });
    var fractions = RINGS.map(function (ring) { return ease((t - ring.t0) / ring.dur); });

    // Polar axis rod
    var axisF = ease((t - 0.2) / 0.9);
    var top = this.project([0, 1.42 * axisF, 0], 'cage', st);
    var bot = this.project([0, -1.42 * axisF, 0], 'cage', st);

    [false, true].forEach(function (front) {
      for (var r = 0; r < RINGS.length; r++) {
        self.strokeRuns(projected[r], fractions[r], RINGS[r].w, front, RINGS[r].strong);
      }
      self.drawTicks(st, front);

      if (!front) {
        ctx.setLineDash([]);
        ctx.beginPath();
        ctx.moveTo(bot[0], bot[1]);
        ctx.lineTo(top[0], top[1]);
        ctx.lineWidth = 1.3 * self.dpr;
        ctx.strokeStyle = 'rgba(' + BRASS_DEEP + ',' + 0.7 * axisF + ')';
        ctx.stroke();
        self.drawEarth(st, t);
      }
    });

    if (axisF > 0.95) {
      [top, bot].forEach(function (p) {
        ctx.beginPath();
        ctx.arc(p[0], p[1], 3.2 * self.dpr, 0, Math.PI * 2);
        ctx.fillStyle = PARCHMENT;
        ctx.fill();
        ctx.lineWidth = 1 * self.dpr;
        ctx.strokeStyle = 'rgba(' + BRASS_DEEP + ',0.8)';
        ctx.stroke();
      });
    }

    this.drawLabels(st, ease((t - 1.4) / 0.8));
  };

  Armillary.prototype.drawTicks = function (st, front) {
    var ctx = this.ctx, self = this;
    ctx.setLineDash([]);
    TICK_SETS.forEach(function (set) {
      var ring = RINGS[set.ring];
      var f = ease((self.lastT - ring.t0) / ring.dur);
      set.ticks.forEach(function (tk) {
        if (tk.at > f) return;
        var a = self.project(tk.a, set.space, st);
        var b = self.project(tk.b, set.space, st);
        var isFront = (a[2] + b[2]) / 2 > -0.02;
        if (isFront !== front) return;
        ctx.beginPath();
        ctx.moveTo(a[0], a[1]);
        ctx.lineTo(b[0], b[1]);
        ctx.lineWidth = (tk.major ? 0.9 : 0.6) * self.dpr;
        ctx.strokeStyle = 'rgba(' + BRASS + ',' + (front ? (tk.major ? 0.8 : 0.55) : 0.15) + ')';
        ctx.stroke();
      });
    });
  };

  Armillary.prototype.drawEarth = function (st, t) {
    var ctx = this.ctx, self = this;
    var f = ease((t - 1.0) / 0.7);
    if (f <= 0) return;
    var c = this.project([0, 0, 0], 'cage', st);
    var r = this.S * EARTH_R * (PERSPECTIVE / (PERSPECTIVE - c[2]));
    ctx.setLineDash([]);
    ctx.beginPath();
    ctx.arc(c[0], c[1], r, 0, Math.PI * 2);
    ctx.fillStyle = PARCHMENT;
    ctx.globalAlpha = f;
    ctx.fill();
    ctx.lineWidth = 1 * this.dpr;
    ctx.strokeStyle = 'rgba(' + INK + ',0.55)';
    ctx.stroke();
    EARTH_LINES.forEach(function (pts) {
      var proj = pts.map(function (p) { return self.project(p, 'cage', st); });
      ctx.beginPath();
      var open = false;
      for (var i = 0; i < proj.length - 1; i++) {
        if (proj[i][2] < c[2] || proj[i + 1][2] < c[2]) { open = false; continue; }
        if (!open) { ctx.moveTo(proj[i][0], proj[i][1]); open = true; }
        ctx.lineTo(proj[i + 1][0], proj[i + 1][1]);
      }
      ctx.lineWidth = 0.6 * self.dpr;
      ctx.strokeStyle = 'rgba(' + INK + ',0.4)';
      ctx.stroke();
    });
    ctx.globalAlpha = 1;
  };

  Armillary.prototype.drawLabels = function (st, f) {
    if (f <= 0) return;
    var ctx = this.ctx, self = this;
    ctx.font = 500 + ' ' + Math.round(9 * this.dpr) + 'px "JetBrains Mono", ui-monospace, monospace';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    MERIDIAN_LABELS.forEach(function (lb) {
      var p = self.project(lb.p, 'frame', st);
      if (p[2] < 0) return;
      ctx.fillStyle = 'rgba(' + BRASS_DEEP + ',' + (0.75 * f * Math.min(1, p[2] * 3)) + ')';
      ctx.fillText(lb.text, p[0], p[1]);
    });
  };

  // ── Page wiring ───────────────────────────────────────────────────────────

  function onReady(fn) {
    if (document.readyState !== 'loading') fn();
    else document.addEventListener('DOMContentLoaded', fn);
  }

  onReady(function () {
    var canvas = document.getElementById('armillary');
    var hero = document.querySelector('.landing-hero');
    var wrap = document.querySelector('.landing-sphere-wrap');
    var progress = document.querySelector('.landing-progress');
    if (!canvas || !hero) return;

    var sphere = new Armillary(canvas);
    var heroVisible = true;
    var rafId = null;
    var clockStart = null;
    var elapsedBeforePause = 0;

    function isHome() {
      var pane = hero.closest('.tab-pane');
      return !pane || pane.classList.contains('active');
    }

    function frame(now) {
      if (clockStart === null) clockStart = now;
      var t = elapsedBeforePause + (now - clockStart) / 1000;
      sphere.lastT = t;
      sphere.draw(t);
      rafId = requestAnimationFrame(frame);
    }

    function stop() {
      if (rafId === null) return;
      cancelAnimationFrame(rafId);
      rafId = null;
      elapsedBeforePause = sphere.lastT || 0;
      clockStart = null;
    }

    function drawStatic() {
      sphere.lastT = 10;
      sphere.draw(10);
    }

    function sync() {
      if (!isHome()) { stop(); return; }
      if (!sphere.resize()) return;
      if (reduceMotion) { drawStatic(); return; }
      if (heroVisible && rafId === null) rafId = requestAnimationFrame(frame);
      if (!heroVisible) stop();
    }

    if ('IntersectionObserver' in window) {
      new IntersectionObserver(function (entries) {
        heroVisible = entries[0].isIntersecting;
        sync();
      }).observe(hero);
    }

    function onScroll() {
      var y = window.scrollY || window.pageYOffset;
      var heroH = hero.offsetHeight || 1;
      var h = Math.min(1, y / heroH);
      sphere.scroll = h;
      if (wrap && !reduceMotion) {
        wrap.style.transform = 'translateY(' + (h * -10) + 'vh) scale(' + (1 - h * 0.1) + ')';
        wrap.style.opacity = String(1 - h * 0.85);
      }
      document.body.classList.toggle('is-scrolled', y > 24);
      if (progress) {
        var max = document.documentElement.scrollHeight - window.innerHeight;
        progress.style.setProperty('--progress', max > 0 ? (y / max).toFixed(4) : 0);
      }
      if (reduceMotion && isHome()) drawStatic();
    }

    window.addEventListener('scroll', onScroll, { passive: true });
    window.addEventListener('resize', function () { sync(); onScroll(); });
    window.addEventListener('mousemove', function (e) {
      sphere.mouse = [e.clientX / window.innerWidth - 0.5, e.clientY / window.innerHeight - 0.5];
    }, { passive: true });

    // Scroll-triggered reveals
    var revealEls = document.querySelectorAll('[data-reveal]');
    if ('IntersectionObserver' in window && !reduceMotion) {
      var revealer = new IntersectionObserver(function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            entry.target.classList.add('is-visible');
            revealer.unobserve(entry.target);
          }
        });
      }, { threshold: 0.15, rootMargin: '0px 0px -8% 0px' });
      revealEls.forEach(function (el) { revealer.observe(el); });
    } else {
      revealEls.forEach(function (el) { el.classList.add('is-visible'); });
    }

    var cue = document.querySelector('.landing-scroll-cue');
    if (cue) {
      cue.addEventListener('click', function (e) {
        e.preventDefault();
        var target = document.getElementById('landing-gap');
        if (target) target.scrollIntoView({ behavior: reduceMotion ? 'auto' : 'smooth' });
      });
    }

    // Tab switches: the Home pane is display:none while hidden, so re-measure on return.
    $(document).on('shown.bs.tab', function () { setTimeout(function () { sync(); onScroll(); }, 0); });
    $(document).on('shiny:inputchanged', function (e) {
      if (e.name === 'main_nav') setTimeout(function () { sync(); onScroll(); }, 0);
    });

    sync();
    onScroll();
  });
})();
