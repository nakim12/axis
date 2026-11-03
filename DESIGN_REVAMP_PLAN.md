# Axis — Design Revamp Plan

Goal: make Axis look and feel like the rest of the portfolio (Juno, Iris, Romus, Dialed) instead of a dark-themed Bootstrap dashboard. The analytics stay; the presentation, structure, and motion change.

---

## 1. What the reference sites share

Studied: juno.nakim.me, useiris.tech, romus.nakim.me, dialed.mykm.dev.

| Trait | Juno | Iris | Romus | Dialed |
|---|---|---|---|---|
| Signature visual | Laurel mark + drifting gold particles and wave lines | Scroll-scrubbed chrome flower (image sequence on canvas) | Flow-field line canvas | Zoom from space into a sunset cloudscape |
| Intro moment | Blur-in of mark + wordmark | ASCII-art flower loader with % | — | Earth zoom |
| Wordmark | `juno.` serif, huge | `iris.` lowercase, huge | `ROMUS` caps, huge | `dialed` italic serif, huge |
| Type | Fraunces + Inter + JetBrains Mono | Geist + Geist Mono | Rajdhani + Geist Mono | Fraunces / Cormorant + Bricolage + JetBrains Mono |
| Palette | Ink navy + one warm gold accent | Pure black + chrome | Near-black monochrome | Sunset gradient + white |

The common pattern:

1. **One signature visual that fills the hero.** It's tied to the product idea (an iris is a flower, a laurel suggests trust, the clouds sit "above the noise"). It's a single strong motif, not several overlapping effects.
2. **The wordmark is the hero.** A huge name, often lowercase with a trailing period, then a short tagline (sometimes italic serif), one line of copy, and one or two pill CTAs.
3. **Three typefaces with clear jobs.** A display face (usually a serif) for headings, a quiet sans for body text, and a mono for eyebrows, labels, numbers, and nav.
4. **A restrained palette.** A deep or monochrome base with one accent color.
5. **A scrolling narrative.** The hero is `100svh` with a "scroll" cue, followed by spaced-out sections (`max-w-6xl`, `py-20`–`py-24`). Section headers use numbered rules (`01 ———————— LABEL`), and content reveals as you scroll.
6. **Chrome that gets out of the way.** A minimal top nav that condenses into a floating pill on scroll, plus a thin scroll progress bar.
7. **Content shaped like a product story.** Problem/proof pairs, a 3-step how-it-works, a capability grid with hairline dividers, a metrics strip in mono, a final CTA card, and a simple footer.

## 2. Why Axis currently feels off

Checked by running the app locally:

- **No signature visual.** The landing stacks cyan, teal, and indigo blur orbs with a moving grid, a sheen bar, a shine divider, and scanning lines on each card. That's many small effects with no single motif behind them.
- **Everything is crammed into one viewport.** `landing_ui.R` deliberately locks `body` to `100vh` with `overflow: hidden` and flex-stretches cards to fill the space. The result is a small left-aligned hero, three tall mostly-empty step cards, and a capability grid with a dangling fifth card.
- **Generic type.** DM Sans everywhere, so headings have no display character.
- **Bootstrap 3 chrome.** The full-width `navbarPage` with 9 tabs, `wellPanel` sidebars, and `btn-success` / `btn-warning` / `btn-danger` buttons all look like default Shiny.
- **The charts break the theme.** Every chart is a stock `theme_minimal()` ggplot on a white background with `#2c3e50` lines and default ggplot hue colors, so they render as bright white boxes on a near-black page. Allocation is shown as pies.
- **Layout bugs.** The KPI row on Build Portfolio overflows horizontally, and KPI values render inside `textOutput` boxes.
- **CSS lives in R strings** (`GLOBAL_APP_CSS`, `landing_page_css`, `app_css`), which makes iteration painful.

## 3. Design direction

### Brand

- Wordmark: **`axis.`**, lowercase with a period, matching `juno.`, `iris.`, and `dialed.`
- Tagline (italic display serif): *"Decisions first."*
- Mono eyebrow: `portfolio intelligence · risk · allocation`

### Signature visual: an armillary sphere

An **armillary sphere**: concentric rings rotating around a tilted central axis. It ties directly to the name and suggests balance and allocation. In the Observatory direction it's drawn as an **antique brass instrument in fine engraved linework**, not a glossy 3D render.

- Implementation: a hand-rolled 3D projection on Canvas 2D (no library), which gives precise control over hairline widths and lets the far halves of rings render dashed and faint like an engraving. Rings carry tick marks and degree graduations, and a faint star chart sits behind it, drawn as dotted ink.
- On load: the rings draw themselves in, stroke by stroke (the intro moment).
- On scroll: the sphere slowly rotates and moves aside as section 01 comes in.
- Fallbacks: `prefers-reduced-motion` gets a single fully drawn static frame. The render loop pauses whenever the hero is off-screen or another tab is active.

### Palette: Observatory

A warm, light, paper-and-brass identity: an antique scientific instrument or a private-bank ledger. It's deliberately unlike Juno, Iris, Romus, and Dialed.

| Token | Value | Use |
|---|---|---|
| `--parchment` | `#f1ebdf` | page background |
| `--vellum` | `#f8f4ec` | cards, chart plates |
| `--sand` | `#e7decb` | sunken surfaces, hover, chips |
| `--ink` | `#1d1a15` | headings, wordmark |
| `--ink-soft` | `#4a4339` | body copy |
| `--muted` | `#857b6a` | captions, axis labels |
| `--line` | `rgba(29,26,21,0.12)` | hairlines, card borders |
| `--brass` | `#a0763a` | the one accent: CTAs, portfolio line, active states |
| `--brass-deep` | `#6f4f1f` | accent hover/pressed, text on brass wash |
| `--slate` | `#4f6b80` | benchmark / secondary data series |
| `--gain` / `--loss` | `#4e7a56` / `#a5463a` | sage and oxide red, data only |

Chart ramp for multi-series data: brass, slate, sage `#6f8f63`, oxide, ochre `#c9a35b`, plum `#6e5470`, deep teal `#3f7a78`, stone `#8a8170`.

Drop the cyan/teal/indigo set entirely. Color is reserved for the accent and for data.

### Type

- Display: **Fraunces** (h1–h3, wordmark, KPI hero numbers), plus italic for taglines.
- Body: **Inter**.
- Mono: **JetBrains Mono** (eyebrows, nav, labels, table numbers, metric strips).
- Scale: wordmark `clamp(5rem, 14vw, 11rem)`, h2 `clamp(2rem, 4vw, 3rem)`, body 16–17px, eyebrows 11–12px with `0.2em` tracking.

### Components

- Pill buttons: primary is an ink fill with parchment text (brass on hover); ghost is a hairline border.
- Cards: `--vellum` background, 1px `--line` border, 14–18px radius, no shadows beyond a 1px paper lift.
- Section header: `01 ———————— HOW IT WORKS` in mono, then a serif h2 and a muted lead.
- Metric strip: a hairline-divided row of mono numbers (as in Juno's evaluation block).
- Insight card: a quote-style card with a confidence tag (`high` / `medium` / `low`) in mono.

## 4. Landing page (scrollable)

Replace the single-viewport layout with a real scroll.

0. **Intro.** About 1.2s: the sphere assembles and the wordmark blurs in. Skippable, and shown once per session.
1. **Hero (`100svh`).** Sphere behind, then the eyebrow, `axis.`, the *Decisions first.* tagline, one line of copy, the CTAs **Analyze a sample portfolio →** (primary) and **Build your own** (ghost), and a metric strip: `30 tickers · 5 sectors · 4 stress episodes · 4 allocation models`. "scroll" cue at the bottom.
2. **01 — The gap.** Two cards (tool/proof style): *"You see returns."* / *"You don't see risk."* Uses the problem framing from `PROJECT_PLAN.md`.
3. **02 — How it works.** Three steps (Build → Diagnose → Decide). Each has a small real visual, such as a weight bar, a drawdown sparkline, or a before/after allocation, instead of empty stretched cards.
4. **03 — A diagnosis, in plain language.** A mock Diagnosis card with KPI numbers and 2–3 example insights, e.g. *"Two holdings explain 61% of portfolio variance."*
5. **04 — Capabilities.** A 3×2 grid with hairline dividers (no dangling card): portfolio-native metrics, benchmark grounding, concentration signals, scenarios & stress, Allocation Lab, transparent methodology.
6. **05 — Built to be trusted.** Methodology highlights and the educational disclaimer, presented as a design element rather than a yellow warning box.
7. **Final CTA card,** then a footer with GitHub, Methodology, and the disclaimer line.

Motion: scroll-triggered fade/translate reveals (IntersectionObserver), a scroll progress bar, and a nav that condenses into a floating pill after the hero. All of it respects `prefers-reduced-motion`.

## 5. App shell and inner pages

### Navigation

Swap Bootstrap 3 `navbarPage` for **bslib (Bootstrap 5)** with a custom header. Cut the 9 tabs down to a floating pill nav with 5 primary destinations:

`axis.` · **Portfolio** · **Diagnosis** · **Stress** · **Allocation** · **Explore** · *(Methodology lives in the footer and in a "?" link)*

- **Diagnosis** absorbs portfolio-level **Performance** (benchmark path, excess return, drawdown, rolling stats) as sections.
- **Explore** absorbs **Holdings Explorer** (price trend, forecast) and per-ticker **Risk Analysis**, using sub-tabs styled as mono pills.
- Keep `input$main_nav` values stable, or map them, so server logic and `updateNavbarPage` calls keep working.

### Page template (shared)

Every page uses the same pattern:

- A mono eyebrow (`02 — DIAGNOSIS`), a serif title, and a one-line muted lead.
- A left control rail (a slim card, not a `wellPanel`) with grouped, labelled controls.
- A main column of sections separated by numbered hairline headers.
- An empty state with an illustrated ring motif and a single CTA: *"Analyze a sample portfolio"*.

### Specific fixes

- **Build Portfolio:** replace the holdings `tableOutput` with a styled list (ticker in mono, weight bar, sector chip, remove ×). Replace both pies with a single stacked allocation bar plus a sector bar. Make the KPI strip responsive (fixes the overflow). Turn "Analyze Portfolio" into the one primary CTA.
- **Diagnosis:** a hero metric strip (return, vol, Sharpe, max DD, beta, HHI) in Fraunces numerals, an insight card grid with confidence tags, then sections for performance vs benchmark, risk contribution, sectors, and attribution. Downloads move into a tidy "Export" menu.
- **Stress / Allocation:** the same template. Before/after weights as paired horizontal bars in stone vs brass.
- **Tables:** custom CSS (mono numbers, right-aligned, hairline rows, no zebra striping).
- **Spinners:** replace the `shinycssloaders` type-4 spinner with a small rotating-ring loader in brass, matching the motif.

### Charts

Add `theme_axis()` in a new `R/plot_theme.R` and apply it to every `ggplot`:

- Transparent background (`renderPlot(bg = "transparent")`), hairline gridlines on y only, `--muted` axis text, and Inter via `systemfonts` / `ragg`.
- Palette: portfolio = `--brass`, benchmark = `--slate`, holdings = the 8-color chart ramp; gains and losses use `--gain` / `--loss`.
- Correlation heatmap: diverging `--slate` → vellum → `--loss` instead of blue/white/red.
- Replace pies with bars. Use thin 1–1.25px lines with an area fill under the portfolio line.
- Optional later step: switch key charts to `plotly` for hover, styled to match.

## 6. Technical approach

- Move all CSS and JS out of R strings into `www/`:
  - `www/css/tokens.css`, `base.css`, `landing.css`, `app.css`
  - `www/js/landing.js` (Canvas 2D sphere, reveals, nav condense, progress bar)
  - `www/img/` (sphere fallback, OG image, favicon)
- Build the landing as an `htmlTemplate("www/landing.html")` (or `tags` + `includeHTML`) inside the Home panel. Home gets normal document scrolling; delete the `app-home-tab` viewport lock.
- Add `bslib` (and `ragg`, `systemfonts`) to `DESCRIPTION` so the shinyapps.io CI installs them.
- Load fonts from Google Fonts (Fraunces, Inter, JetBrains Mono). The sphere needs no external library.
- Remove the Shadertoy iframe option and the old orb/grid/sheen CSS.
- Update `README.md` with new screenshots once the revamp is done.

## 7. Phases

1. **Foundations.** Tokens, fonts, `www/` asset structure, bslib theme, `theme_axis()`, and deletion of the old effects. Charts stop being white boxes immediately.
2. **Landing.** Scroll layout and sections with static content first, then the sphere, intro, and scroll motion.
3. **App shell.** Pill nav, the 9 → 5 page consolidation, and the shared page template.
4. **Pages.** Portfolio → Diagnosis → Stress → Allocation → Explore → Methodology.
5. **Polish and QA.** Reduced motion, mobile (≤ 640px), performance (sphere under 16ms/frame, pauses when off-screen), screenshot pass on every page, and a shinyapps deploy check.

Each phase should be shippable on its own.

## 8. Decisions

- **Signature visual:** armillary sphere. Decided.
- **Where the landing lives:** inside the Shiny app via `htmlTemplate` + `www/`. Decided.
- **Base mood / palette:** Observatory. Decided. See section 3.

### Palette directions considered

1. **Observatory (chosen).** A light theme: warm parchment `#f1ebdf`, ink-black serif, aged-brass sphere drawn in fine engraved linework. It reads like an antique scientific instrument or a private-bank ledger. None of the other projects is warm and light.
2. **Cyanotype.** Deep Prussian blue `#0f2a4a` with off-white line art. The sphere is drawn as a technical blueprint, as if the portfolio is being engineered. Charts are white and pale-blue linework.
3. **Midnight verdigris.** Very dark petrol green `#0b1a1a` with a patinated copper/verdigris sphere and a bone-white type. It suggests navigation, old money, and calm.
4. **Oxblood & bone.** Near-black burgundy `#160c0e`, bone `#ece4d8`, and copper. A members'-club, private-wealth mood that's moody and distinctive.
5. **Graphite & signal.** Cool graphite `#141517` with a single sharp signal color (e.g. chartreuse `#d4ff3a`) for the portfolio line and CTAs. More trading-terminal, less ornamental.
