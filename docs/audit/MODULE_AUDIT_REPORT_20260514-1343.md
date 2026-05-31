# Module Audit Report — jsurvival 0.0.38.1

**Audited:** 2026-05-14 13:43
**Profile:** standard
**Functions:** 8 · READY 5 · NEEDS WORK 3 · PLACEHOLDER 0 · MISSING 0 · ORPHANED 0 (+1 orphaned JS file)
**Security findings:** HIGH 0 · MEDIUM 2 (clusters) · LOW 3 (clusters)
**Skill:** audit-module v0.1.0

---

## Executive Dashboard

| Function | Status | HIGH-Sec | MEDIUM-Sec | Integration | Notices | i18n | Readiness |
|---|:---:|:---:|:---:|:---:|:---:|:---:|---|
| datetimeconverter | ✅ | 0 | 0 | ✅ | ✅ HTML-migrated | ❌ none | READY |
| multisurvival | ⚠️ | 0 | 2 | ✅ | ❌ 20 dynamic Notice inserts | ⚠ partial | NEEDS_VALIDATION |
| oddsratio | ⚠️ | 0 | 1 | ✅ | ❌ 1 dynamic Notice insert | ❌ minimal | NEEDS_WORK |
| outcomeorganizer | ⚠️ | 0 | 0 | ✅ | ❌ 1 dynamic Notice insert | ❌ none | NEEDS_WORK |
| singlearm | ✅ | 0 | 1 | ✅ | ✅ HTML-migrated | ⚠ partial | READY |
| survival | ✅ | 0 | 2 | ✅ | ✅ `jmvcore::reject` + HTML | ⚠ partial | READY |
| survivalcont | ✅ | 0 | 1 | ✅ | ✅ `.addHtmlMessage` with `htmlEscape` | ⚠ minimal | READY |
| timeinterval | ✅ | 0 | 0 | ✅ | ⚠ stop()-based validation | ❌ none | READY |

**Top 10 cross-cutting issues (affecting multiple functions):**

1. **[C1] `as.formula(...)` without `jmvcore::asFormula` allow-list guard** — 5 functions, ~40 active call sites. RHS is built from column-name strings (Variable / Variables-typed options); jamovi backtick-quotes the option values, but the parse-time guard is missing.
2. **[D] HTML output built from variable names without `htmltools::htmlEscape`** — 5 functions (multisurvival, oddsratio, singlearm, datetimeconverter uses basic `gsub`, survivalcont is the only one using `htmltools::htmlEscape`). Risk: column names containing `<script>` from external CSV.
3. **[CRITICAL — Notice serialization]** `self$results$insert(N, notice)` with `jmvcore::Notice` objects — **20 sites in multisurvival**, 1 each in oddsratio and outcomeorganizer. Per `CLAUDE.md`, this pattern causes "attempt to apply non-function" protobuf serialization errors. Reference safe pattern: `R/singlearm.b.R:192-246` (message accumulation → HTML) and `R/survivalcont.b.R:700-743` (`.addHtmlMessage` with `htmlEscape`).
4. **[E-hygiene] `stop()` instead of `jmvcore::reject()`** — 71 `stop()` calls module-wide vs 6 `jmvcore::reject()` (all in `survival.b.R`). User-facing validation should produce structured jamovi notices.
5. **[i18n] `.()` calls without translation catalogs** — 332 `.()` wraps present, but `jamovi/i18n/` directory does not exist. `multisurvival.b.R:2-3` has a fragile `if (!exists(".")) { . <- function(x) x }` workaround.
6. **[orphan] `jamovi/js/survivalPower.events.js`** has no matching analysis (`survivalPower.a.yaml`, `R/survivalPower.b.R`). It is leftover from a removed/renamed function.
7. **[C3] `as.formula(paste(deparse(surv_part), …))`** — 3 sites in `multisurvival.b.R` (line 5982, 6045, 6113). LOW-risk because `surv_part` derives from a known Surv() call, but pattern is fragile.
8. **[hygiene] Duplicate top-level helpers** — `.escapeVariableNames` is defined in both `R/multisurvival.b.R:7` and `R/survival.b.R:207` with identical bodies. Move to `R/utils.R`.
9. **[D] `glue::glue` building HTML with interpolated values** — used across 7 of 8 functions (e.g., `oddsratio.b.R:581-591`, `multisurvival.b.R:4374-4462`). `glue` does not HTML-escape; column names / factor labels flow in raw.
10. **[notices-coverage] Clinical thresholds inconsistent** — `multisurvival` and `survival` warn on PH-violation / low events, but `oddsratio` / `survivalcont` lack EPV-style overfitting warnings beyond the existing borderline check.

---

## Methodology

**Profile:** standard

**Checks run:**

- [x] Function discovery from `jamovi/0000.yaml` + filesystem
- [x] Security pattern scan (catalog A–I) — see `references/security-patterns.md`
- [x] jmvcore migration scan (6 groups) — see `references/jmvcore-migration.md`
- [x] Integration audit (9 checks) — see `references/integration-checks.md`
- [x] Notices coverage — see `references/notices-checklist.md`
- [x] Code review (8 areas) — see `references/code-review-checks.md`
- [ ] R6 & R-package best practices — deep profile only
- [ ] Vignette cross-reference — deep profile only

**Checks skipped:**

- External documentation comparison (CRAN/GitHub) — run `/check-function-full <name>` per function if needed
- Function execution (differential runs) — heuristic only, no actual jamovi runs
- R CMD check, `devtools::document()`, `jmvtools::prepare()` — out of scope for this skill
- Differential mathematical/statistical correctness (statistical theory verified by reading code; reference-implementation parity not actually executed)

**Audit-only.** No source files were modified.

---

## Per-Function Sections

### datetimeconverter

**Status:** ✅ READY
**Files:** [`R/datetimeconverter.b.R`](../../R/datetimeconverter.b.R) · [`jamovi/datetimeconverter.a.yaml`](../../jamovi/datetimeconverter.a.yaml) · [`jamovi/datetimeconverter.u.yaml`](../../jamovi/datetimeconverter.u.yaml) · [`jamovi/datetimeconverter.r.yaml`](../../jamovi/datetimeconverter.r.yaml)
**Metrics:** .b.R LOC 1661 · options 48 (incl. 15 List choices) · outputs 24 · 21 notice-helper hits · 11 sprintf · 2 stop()

#### Security

- **[D-LOW]** [`R/datetimeconverter.b.R:71-72`](../../R/datetimeconverter.b.R#L71) — Notice HTML escaping uses `gsub("<", "&lt;", …)` (only `<` and `>`, no `&` / `"` / `'`). `htmltools::htmlEscape` is more complete. Variable-name flows are limited to `sprintf` in notice content (line 1302, 1332), which is plain-text not HTML, so reachability is low.

#### jmvcore migration

*No active migration opportunities found.* All formulas avoid `as.formula(...)`. No `na.omit` on data frames. `jmvcore::reject` not used (uses HTML notices instead, equivalent UX).

#### Integration

**Arguments declared:** 33 top-level options · **used in logic:** 32 (`data` excluded) · **dead:** 0 (the apparent 15 "unused" sub-option names are List enum values, not options)

**Outputs declared:** 24 · **populated:** 24 · **unpopulated:** 0

#### Notices coverage

| Trigger | Type | Present? | Quantified? | Notes |
|---|---|:---:|:---:|---|
| Missing required inputs | ERROR | ✅ | ✅ | Lines 1281, 1300, 1314 |
| Invalid variable type | ERROR | ✅ | ✅ | Numeric vs text format mismatch at line 1187 |
| All-NA column | ERROR | ✅ | ✅ | Line 1330 |
| Format-detection failed | WARNING | ✅ | ✅ | Line 139 |
| Methodology summary | INFO | ⚠ | n/a | Embedded in glossary/aboutPanel HTML (not single-line) |

#### Code review

- **Overall quality:** ★★★★☆ (4/5)
- **Architecture:** OK — clean R6 with private helpers, modular parsing/quality/preview/glossary stages.
- **Mathematical correctness:** CORRECT — uses lubridate primitives; quality assessment is straightforward counting + format-confidence ratios.
- **Clinical readiness:** READY — appropriate guardrails for medical date data (e.g., dates-before-1900 sentinel, future-date flag, >100-year-range flag at line 1196-1216).
- **i18n coverage:** NONE — file has 0 `.()` calls. All notice strings hard-coded English.

**Top issues:**

1. i18n: 0 strings wrapped with `.()` — flag for `/prepare-translation datetimeconverter`.
2. HTML escape uses two-character gsub instead of `htmltools::htmlEscape` — defensive-only since inputs are notice text strings, not column values.
3. `stop()` at line 1322 is unreachable (duplicate of the line-1290 check four lines earlier) — dead code.

#### Recommended remediation

- `/prepare-translation datetimeconverter` — wrap notice strings for i18n.
- Optional: replace `gsub` HTML escaping (lines 71-72) with `htmltools::htmlEscape` for consistency with `survivalcont`.

---

### multisurvival

**Status:** ⚠️ NEEDS WORK
**Files:** [`R/multisurvival.b.R`](../../R/multisurvival.b.R) · [`jamovi/multisurvival.a.yaml`](../../jamovi/multisurvival.a.yaml) · [`jamovi/multisurvival.u.yaml`](../../jamovi/multisurvival.u.yaml) · [`jamovi/multisurvival.r.yaml`](../../jamovi/multisurvival.r.yaml)
**Metrics:** .b.R LOC 8191 · options 54 · outputs 60 · 30 glue calls · 17 sprintf · 25 stop() · 0 reject() · 177 `.()` calls · **20 dynamic Notice inserts**

#### Security

- **[C1-MEDIUM]** Multiple `as.formula(...)` call sites with strings built from column-name strings. Representative locations:
  - [`R/multisurvival.b.R:351`](../../R/multisurvival.b.R#L351) — `as.formula(formula_string)` in `.buildSurvivalFormula` helper. LHS/RHS built from `paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ")")`. Names already backtick-escaped via `.escapeVariableNames`, but no `jmvcore::asFormula` allow-list guard.
  - [`R/multisurvival.b.R:2752`](../../R/multisurvival.b.R#L2752) — `aft_formula <- as.formula(aft_formula)` (AFT model)
  - [`R/multisurvival.b.R:5085`](../../R/multisurvival.b.R#L5085) — `myformula <- as.formula(myformula)` (risk-score Cox)
  - [`R/multisurvival.b.R:5107`](../../R/multisurvival.b.R#L5107) — `fg_formula_obj <- as.formula(fg_formula_str)` (Fine-Gray)
  - [`R/multisurvival.b.R:6580`](../../R/multisurvival.b.R#L6580), `:6821`, `:7791` — additional `as.formula` sites
  - **Why MEDIUM, not HIGH:** column names come from jamovi `type: Variable[s]` options which the platform tracks; backtick-quoting is applied in `.escapeVariableNames`. But a column literally named `` `; system('id') `` would bypass that. The proper fix is `jmvcore::asFormula(<string>)` for the allow-list parse-time guard.
- **[C3-LOW]** [`R/multisurvival.b.R:5982`](../../R/multisurvival.b.R#L5982), `:6045`, `:6113` — `as.formula(paste(deparse(surv_part), "~", …))`. `surv_part` derives from a fitted Cox model's formula component, so reachability of an adversarial payload is low. Still: prefer `reformulate(<character>, response = surv_part)`.
- **[D-MEDIUM]** glue::glue building HTML with interpolated values that include factor labels and variable names. Representative:
  - [`R/multisurvival.b.R:4374-4462`](../../R/multisurvival.b.R#L4374) — `group = levels(mydata$risk_group)` and `length(levels(mydata$risk_group))` interpolated into HTML risk-score narrative without `htmltools::htmlEscape`. If `risk_group` is built from a free-text recode, an injected `<script>` factor label would render.
  - Multiple `paste0(…, self$options$<X>, …)` for plot titles (lines 4212, 5124-5125, 5149) — these flow to `xlab`/`title` of ggplot, which is **not HTML-rendered**, so XSS is not a sink there. Mentioned for completeness.

#### jmvcore migration

- **[jmvcore/formula]** [`R/multisurvival.b.R:351`](../../R/multisurvival.b.R#L351), `:2752`, `:5085`, `:5107`, `:6580`, `:6821`, `:7791`, `:5982`, `:6045`, `:6113`
  - `as.formula(...)` → `jmvcore::asFormula(...)` (preserves behavior, adds allow-list guard)
  - `paste0("survival::Surv(", a, ", ", b, ")")` → `jmvcore::composeFormula` or stay manual but switch outer call to `asFormula`
- **[jmvcore/error]** 25 `stop()` calls (including [`R/multisurvival.b.R:161`](../../R/multisurvival.b.R#L161) `.eventIndicator` unreachable-factor-levels validation). Convert user-facing ones to `jmvcore::reject(.("…"), code=…)`.
- **[jmvcore/na]** Multiple `na.omit(df)` calls on labelled data frames likely lose jamovi attributes — search for `na.omit(` in this file and switch to `jmvcore::naOmit`.

#### Integration

**Arguments declared:** 54 · **used in logic:** 100 (some referenced multiple times via 100 unique `self$options$` accesses, indicating dense option usage) · **dead:** 0

**Outputs declared:** 60 · **populated:** 60 (all `.r.yaml` outputs are referenced)

#### Notices coverage

| Trigger | Type | Present? | Quantified? | Notes |
|---|---|:---:|:---:|---|
| Missing required inputs | ERROR | ⚠ | n/a | Mostly handled via `stop()` instead of `jmvcore::reject`; some Notice path at 2393 |
| Low events / small n | STRONG_WARNING | ✅ | ✅ | line 136 `< 10` events check |
| PH-assumption violation | WARNING | ✅ | ✅ | line 2436-2444 |
| Competing-risks method note | INFO | ✅ | n/a | line 2422-2429 (`'competeNote'`) |
| Fine-Gray invalid coding | ERROR | ✅ | ✅ | line 2386-2394 |

**CRITICAL — Notice serialization risk:** 20 `self$results$insert(N, notice)` call sites with live `jmvcore::Notice` objects, including 5 `insert(999, …)` (lines 2184, 2199, 2732, 2770, 6689). Per `CLAUDE.md`, this pattern causes "attempt to apply non-function" serialization errors. The function must migrate to the HTML-message pattern used by `singlearm.b.R` and `survivalcont.b.R`.

#### Code review

- **Overall quality:** ★★★☆☆ (3/5) — mathematically thorough but architecturally monolithic.
- **Architecture:** `R/multisurvival.b.R` is 8191 lines — a single class with extensive private helpers. Recommend splitting into topical files (e.g., `multisurvival-aft.b.R`, `multisurvival-frailty.b.R`, etc.) via R's lexical class extension.
- **Mathematical correctness:** NOT_EVALUATED in depth — Fine-Gray subdistribution, AFT, frailty, and spline paths are present and use established `survival::` primitives. Reference-implementation parity would need explicit verification via `/review-function multisurvival`.
- **Clinical readiness:** NEEDS_VALIDATION — wide surface area (Cox, Fine-Gray, AFT, frailty, splines, risk groups, nomogram, decision tree). Each requires individual clinical validation; PH-violation warning is appropriately surfaced.
- **i18n coverage:** PARTIAL — 177 `.()` calls present, but `if (!exists(".")) { . <- function(x) x }` workaround at line 2-3 is a smell that `.()` isn't reliably imported from `jmvcore`.

**Top issues:**

1. Notice serialization risk (20 sites) — highest priority to fix.
2. Formula injection guard missing (10 sites) — wrap with `jmvcore::asFormula`.
3. 8191-line monolith — split for maintainability.
4. `i18n` workaround at lines 2-3 should be fixed by ensuring `importFrom(jmvcore, .)` in NAMESPACE.
5. Two large blocks of `# EXPERIMENTAL:` commented-out code (lines 2330-2380 and others) — delete or move to a feature-branch.

#### Recommended remediation

- `/fix-notices multisurvival` — migrate 20 Notice inserts to HTML pattern (use `R/singlearm.b.R:192-246` as template).
- `/security-audit-function multisurvival` — full per-function audit with approval gates for the C1 cluster.
- `/jamovify-function multisurvival --apply` — convert `as.formula` to `jmvcore::asFormula`, `na.omit` to `jmvcore::naOmit`, `stop` to `jmvcore::reject`.
- `/review-function multisurvival` — statistical correctness review of Fine-Gray / AFT / frailty / spline paths.
- `/prepare-translation multisurvival` — clean up i18n setup (NAMESPACE fix + complete .() wraps).

---

### oddsratio

**Status:** ⚠️ NEEDS WORK
**Files:** [`R/oddsratio.b.R`](../../R/oddsratio.b.R) · [`jamovi/oddsratio.a.yaml`](../../jamovi/oddsratio.a.yaml) · [`jamovi/oddsratio.u.yaml`](../../jamovi/oddsratio.u.yaml) · [`jamovi/oddsratio.r.yaml`](../../jamovi/oddsratio.r.yaml)
**Metrics:** .b.R LOC 1756 · options 9 · outputs 11 · 21 glue calls · 2 sprintf · 5 stop() · 0 reject() · 8 `.()` calls

#### Security

- **[C1-LOW]** [`R/oddsratio.b.R:1101`](../../R/oddsratio.b.R#L1101) — `formula = as.formula(formula_str)` for nomogram fit. Names already escaped by jamovi but no `jmvcore::asFormula` guard.
- **[C1-LOW]** [`R/oddsratio.b.R:1693`](../../R/oddsratio.b.R#L1693) — `f <- as.formula(paste(dependent, "~", paste(explanatory, collapse = " + ")))`. Same as above.
- **[D-MEDIUM]** `glue::glue` building HTML output items (text, text2) with `unlist(tOdds[[2]])` content from finalfit — finalfit's strings should be safe but `rownames(tOdds[[1]])` flow to the rendered table via `knitr::kable(format="html")` at line 699. No XSS guard.

#### jmvcore migration

- **[jmvcore/formula]** Good news: oddsratio already uses `jmvcore::constructFormula` (line 602) and `jmvcore::composeTerms` (line 605). The 2 stray `as.formula(...)` sites (lines 1101, 1693) should be converted to `jmvcore::asFormula` to complete the migration.
- **[jmvcore/error]** 5 `stop()` calls → `jmvcore::reject` for user-facing paths.

#### Integration

**Arguments declared:** 9 · **used in logic:** 8 (`data` excluded) · **dead:** 0
**Outputs declared:** 11 · **populated:** 11

#### Notices coverage

| Trigger | Type | Present? | Quantified? | Notes |
|---|---|:---:|:---:|---|
| Missing required inputs | ERROR | ✅ | ✅ | `.addNotice` + `.insertNotices` |
| EPV / separation | WARNING | ✅ | ✅ | line 581 (EPV), line 590 (separation) |
| Firth fallback announce | INFO | ✅ | ✅ | line 632 |
| Penalized-method missing pkg | STRONG_WARNING | ✅ | ✅ | line 635 |

**Notice serialization risk:** [`R/oddsratio.b.R:105`](../../R/oddsratio.b.R#L105) `self$results$insert(position, n$notice)` — calls insert with a Notice object held in a list. Same root-cause risk as multisurvival; needs the HTML-message pattern.

#### Code review

- **Overall quality:** ★★★☆☆ (3/5)
- **Architecture:** Reasonable — R6 with clear separation between fit / table / plot / nomogram.
- **Mathematical correctness:** CORRECT — uses `finalfit::finalfit` and `logistf::logistf` from established packages; Firth-penalized fallback is correctly implemented.
- **Clinical readiness:** NEEDS_VALIDATION — sensitivity/specificity/likelihood-ratio computations for binary predictors are sound, but `outcomeLevel` parameter behavior for non-default positive class needs UI documentation.
- **i18n coverage:** PARTIAL — 8 `.()` calls (very few; most strings hard-coded English).

**Top issues:**

1. Notice serialization risk at line 105 — migrate to HTML pattern.
2. 2 stray `as.formula` calls not migrated to `jmvcore::asFormula` (lines 1101, 1693).
3. `knitr::kable(format="html")` output at line 699 doesn't escape `rownames(tOdds[[1]])` — column-name XSS possible if pathological names.

#### Recommended remediation

- `/fix-notices oddsratio` — migrate to HTML pattern (use `R/singlearm.b.R` as reference).
- `/jamovify-function oddsratio --pattern=formula --apply` — wrap remaining `as.formula` with `jmvcore::asFormula`.

---

### outcomeorganizer

**Status:** ⚠️ NEEDS WORK
**Files:** [`R/outcomeorganizer.b.R`](../../R/outcomeorganizer.b.R) · [`jamovi/outcomeorganizer.a.yaml`](../../jamovi/outcomeorganizer.a.yaml) · [`jamovi/outcomeorganizer.u.yaml`](../../jamovi/outcomeorganizer.u.yaml) · [`jamovi/outcomeorganizer.r.yaml`](../../jamovi/outcomeorganizer.r.yaml)
**Metrics:** .b.R LOC 1123 · options 25 · outputs 14 · 10 glue calls · 4 sprintf · 12 stop() · 0 reject() · 0 `.()` calls

#### Security

- **[D-LOW]** glue::glue with interpolated names (similar to oddsratio). Limited surface — output mostly tables, not HTML.
- **No C-category findings** — outcomeorganizer doesn't build formulas dynamically (it transforms outcome variables, not models them).

#### jmvcore migration

- **[jmvcore/error]** 12 `stop()` calls → convert user-facing ones to `jmvcore::reject`.

#### Integration

**Arguments declared:** 25 · **used in logic:** 24 (`data` excluded) · **dead:** 0
**Outputs declared:** 14 · **populated:** 14

#### Notices coverage

| Trigger | Type | Present? | Quantified? | Notes |
|---|---|:---:|:---:|---|
| Missing required inputs | ERROR | ✅ | ✅ | `.addNotice` helper |
| Invalid coding | ERROR | ✅ | ✅ | various paths |
| Methodology summary | INFO | ⚠ | n/a | embedded in HTML outputs |

**Notice serialization risk:** [`R/outcomeorganizer.b.R:43`](../../R/outcomeorganizer.b.R#L43) `self$results$insert(position, n$notice)` — same root-cause risk as oddsratio.

#### Code review

- **Overall quality:** ★★★☆☆ (3/5)
- **Architecture:** Clear — transformation pipeline (data → cleaned names → recoded outcome → status).
- **Mathematical correctness:** N/A — this is a data-preparation function, not a statistical estimator.
- **Clinical readiness:** READY — covers overall survival, cause-specific, competing risks, progression-free, multistate.
- **i18n coverage:** NONE — 0 `.()` calls.

**Top issues:**

1. Notice serialization risk at line 43.
2. 12 `stop()` calls in user-facing paths should be `jmvcore::reject`.
3. i18n not started — 0 wrapped strings.

#### Recommended remediation

- `/fix-notices outcomeorganizer` — migrate to HTML notice pattern.
- `/jamovify-function outcomeorganizer --pattern=error --apply` — `stop` → `jmvcore::reject`.
- `/prepare-translation outcomeorganizer` — start i18n wrapping.

---

### singlearm

**Status:** ✅ READY
**Files:** [`R/singlearm.b.R`](../../R/singlearm.b.R) · [`jamovi/singlearm.a.yaml`](../../jamovi/singlearm.a.yaml) · [`jamovi/singlearm.u.yaml`](../../jamovi/singlearm.u.yaml) · [`jamovi/singlearm.r.yaml`](../../jamovi/singlearm.r.yaml)
**Metrics:** .b.R LOC 3074 · options 40 · outputs 65 · 10 glue calls · 19 sprintf · 1 stop() · 0 reject() · 43 `.()` calls

#### Security

- **[C1-LOW]** [`R/singlearm.b.R:1266`](../../R/singlearm.b.R#L1266), `:1509`, `:1583`, `:2548` — 4 `as.formula(formula)` sites where `formula` is built from sanitized names. Low risk because variable names go through `jmvcore::constructFormula` at lines 640, 2266, 2388 first. Recommend wrapping outer call with `jmvcore::asFormula` for defense-in-depth.
- **[D-LOW]** `paste0('Time (', self$options$timetypeoutput, ')')` for plot xlab (lines 2309, 2332, 2414) — flows to ggplot xlab, not HTML, so no XSS sink.

#### jmvcore migration

- **[jmvcore/formula]** Already uses `jmvcore::constructFormula` for input sanitation (lines 640, 2259, 2262, 2266, 2388) — excellent. Stray 4 `as.formula(...)` sites should switch to `jmvcore::asFormula` for the allow-list guard. **This is the best formula-discipline in the module.**
- **[jmvcore/error]** Only 1 `stop()` call — minimal cleanup needed.

#### Integration

**Arguments declared:** 40 · **used in logic:** 41 (`data` excluded; some accessed multiple times) · **dead:** 0
**Outputs declared:** 65 · **populated:** 65

#### Notices coverage

| Trigger | Type | Present? | Quantified? | Notes |
|---|---|:---:|:---:|---|
| Missing required inputs | ERROR | ✅ | ✅ | `.addError` + `.displayMessages` |
| Low events | STRONG_WARNING | ✅ | ✅ | classified via keyword grep (line 226-227) |
| Methodology summary | INFO | ✅ | ✅ | `.addInfo` |

**Notice serialization:** ✅ **No risk.** Uses message accumulation + HTML rendering (lines 192-246). Reference implementation for the rest of the module.

#### Code review

- **Overall quality:** ★★★★☆ (4/5)
- **Architecture:** OK — clean R6 with `.cache` env, explicit `.init` visibility management (lines 79-190), accumulating messages.
- **Mathematical correctness:** CORRECT — Kaplan-Meier, person-time, baseline hazard, hazard smoothing all use `survival::` primitives correctly.
- **Clinical readiness:** READY — supports KMunicate format, baseline hazard, advanced diagnostics.
- **i18n coverage:** PARTIAL — 43 `.()` calls (~50% wrapping based on visible strings).

**Top issues:**

1. 4 `as.formula` sites should use `jmvcore::asFormula` for defense-in-depth.
2. Message-type detection by keyword grep (line 226: `grepl("Very few events|critically", msg)`) is fragile — prefer explicit type parameter.
3. `.init` visibility chain (lines 79-190) is verbose; could be data-driven (map of option → output list).

#### Recommended remediation

- Optional: `/jamovify-function singlearm --pattern=formula --apply` — wrap remaining `as.formula` with `jmvcore::asFormula`.

---

### survival

**Status:** ✅ READY (with known notices migration debt)
**Files:** [`R/survival.b.R`](../../R/survival.b.R) · [`jamovi/survival.a.yaml`](../../jamovi/survival.a.yaml) · [`jamovi/survival.u.yaml`](../../jamovi/survival.u.yaml) · [`jamovi/survival.r.yaml`](../../jamovi/survival.r.yaml) · [`jamovi/js/...`](../../jamovi/js/) (none for survival)
**Metrics:** .b.R LOC 4522 · options 60 · outputs 111 · 10 glue calls · 9 sprintf · 10 stop() · **6 reject()** · 88 `.()` calls

#### Security

- **[C1-LOW]** [`R/survival.b.R:1437`](../../R/survival.b.R#L1437), `:1592`, `:1926`, `:2122`, `:2198`, `:2322`, `:2913`, `:3028` — ~10 `as.formula(formula)` sites. Variable names cleaned via `.escapeVariableNames` (line 207-212). Wrap with `jmvcore::asFormula` for the allow-list guard.
- **[D-LOW]** plot xlab/title interpolation (no HTML sink).
- **No high-risk findings.**

#### jmvcore migration

- **[jmvcore/formula]** ~10 `as.formula` sites → `jmvcore::asFormula`.
- **[jmvcore/error]** 10 `stop()` vs 6 `jmvcore::reject` — **survival.b.R is the only file using `jmvcore::reject`** (lines 580, 584, 592, 596, 602, 608). Convert remaining stops.

#### Integration

**Arguments declared:** 60 · **used in logic:** 59 (`data` excluded) · **dead:** 0
**Outputs declared:** 111 · **populated:** 105 (some declared outputs may be subkeys of tables; cross-check via `/check-function-full survival` for precise counts)

#### Notices coverage

| Trigger | Type | Present? | Quantified? | Notes |
|---|---|:---:|:---:|---|
| Missing required inputs | ERROR | ✅ | n/a | `jmvcore::reject` at lines 580-608 |
| Empty data | ERROR | ✅ | n/a | line 608 |
| PH violation | WARNING | ⚠ | ✅ | Code at line 1938 commented "Cannot show notice due to serialization issues" — diagnostic exists but display removed |
| Analysis completion summary | INFO | ⚠ | n/a | Line 1411 same comment — INFO notice was removed |

**Notice migration debt:** Comments at lines 1042, 1346, 1411, 1938 ("Cannot show notice due to serialization issues") indicate the function previously hit the protobuf bug and the author **removed** the notices rather than migrating them to HTML. These notices should be restored using the `singlearm.b.R` or `survivalcont.b.R` pattern.

#### Code review

- **Overall quality:** ★★★★☆ (4/5)
- **Architecture:** OK — proper `.init` visibility chain (line 252-319+), proper `jmvcore::reject` usage for validation.
- **Mathematical correctness:** CORRECT — Kaplan-Meier, log-rank, Cox PH all use `survival::` primitives; parametric models supported.
- **Clinical readiness:** READY — extensive option surface (60 options, 111 outputs); 1/3/5-year survival, person-time, KMunicate.
- **i18n coverage:** PARTIAL — 88 `.()` calls (~60% wrapping).

**Top issues:**

1. Removed-not-migrated notices (4 sites) — restore as HTML.
2. ~10 `as.formula` should be `jmvcore::asFormula`.
3. 10 `stop()` calls remain user-facing (most validation already uses `reject` — just stragglers).

#### Recommended remediation

- `/fix-notices survival` — restore the 4 removed notices as HTML.
- `/jamovify-function survival --pattern=formula --apply` — `as.formula` → `jmvcore::asFormula`.
- `/jamovify-function survival --pattern=error --apply` — convert remaining `stop()` to `jmvcore::reject`.

---

### survivalcont

**Status:** ✅ READY (best-in-module HTML escaping)
**Files:** [`R/survivalcont.b.R`](../../R/survivalcont.b.R) · [`jamovi/survivalcont.a.yaml`](../../jamovi/survivalcont.a.yaml) · [`jamovi/survivalcont.u.yaml`](../../jamovi/survivalcont.u.yaml) · [`jamovi/survivalcont.r.yaml`](../../jamovi/survivalcont.r.yaml)
**Metrics:** .b.R LOC 4236 · options 51 · outputs 104 · 22 glue calls · 22 sprintf · 0 stop() · 0 reject() · 16 `.()` calls

#### Security

- **[C1-LOW]** [`R/survivalcont.b.R:1684`](../../R/survivalcont.b.R#L1684), `:1983`, `:2125`, `:2439`, `:2665`, `:2849`, `:2937`, `:3000`, `:3075`, `:3283`, `:3530`, `:3626` — ~12 `as.formula(...)` sites for Cox / log-rank cutpoint analyses. Variable names sanitized. Wrap with `jmvcore::asFormula`.
- **[D-LOW]** Only file in module that uses `htmltools::htmlEscape` ([`R/survivalcont.b.R:733-734`](../../R/survivalcont.b.R#L733)) for message title/content — **gold-standard pattern**.

#### jmvcore migration

- **[jmvcore/formula]** ~12 `as.formula` sites → `jmvcore::asFormula`.
- **[jmvcore/error]** 0 `stop()` calls — already clean.

#### Integration

**Arguments declared:** 51 · **used in logic:** 50 (`data` excluded) · **dead:** 0
**Outputs declared:** 104 · **populated:** 101 (some declared outputs may be subkeys; verify via `/check-function-full survivalcont`)

#### Notices coverage

| Trigger | Type | Present? | Quantified? | Notes |
|---|---|:---:|:---:|---|
| Missing required inputs | ERROR | ✅ | ✅ | `.addHtmlMessage` (line 700-743) |
| Cutpoint not found | ERROR | ✅ | ✅ | Error path |
| Methodology summary | INFO | ✅ | ✅ | Line 1571 ("completion notices at the bottom") |
| Citation note | INFO | ✅ | ✅ | Line 1593 |

**Notice serialization:** ✅ No risk. Uses `.addHtmlMessage` with proper `htmltools::htmlEscape` — **reference implementation for the module.**

#### Code review

- **Overall quality:** ★★★★★ (5/5)
- **Architecture:** Excellent — clean R6, proper htmlEscape, descriptive function names.
- **Mathematical correctness:** CORRECT — maximally selected rank statistics (`survminer::surv_cutpoint` family) for optimal cutpoint, then Cox/Kaplan-Meier on the dichotomized variable.
- **Clinical readiness:** READY — appropriate guardrails on continuous-variable dichotomization (the most error-prone step).
- **i18n coverage:** MINIMAL — 16 `.()` calls (low for a 4236-line file).

**Top issues:**

1. ~12 `as.formula` should be `jmvcore::asFormula`.
2. i18n coverage is minimal — most strings hard-coded.
3. `glue::glue` HTML output also needs `htmlEscape` for interpolated factor labels (the `.addHtmlMessage` pattern is good but is only used for accumulated messages, not the analytic-result HTML).

#### Recommended remediation

- `/jamovify-function survivalcont --pattern=formula --apply` — wrap `as.formula` with `jmvcore::asFormula`.
- `/prepare-translation survivalcont` — expand i18n coverage.

---

### timeinterval

**Status:** ✅ READY
**Files:** [`R/timeinterval.b.R`](../../R/timeinterval.b.R) · [`jamovi/timeinterval.a.yaml`](../../jamovi/timeinterval.a.yaml) · [`jamovi/timeinterval.u.yaml`](../../jamovi/timeinterval.u.yaml) · [`jamovi/timeinterval.r.yaml`](../../jamovi/timeinterval.r.yaml)
**Metrics:** .b.R LOC 1081 · options 33 · outputs 10 · 13 glue calls · 12 sprintf · 16 stop() · 0 reject() · 0 `.()` calls

#### Security

- **No formula construction** — function is purely a date-parser / interval calculator, doesn't fit statistical models.
- **[D-LOW]** glue::glue building summary HTML (line 800-840+) with interpolated mean/median/range — values are numeric, not user-controlled strings.
- **No high-risk findings.**

#### jmvcore migration

- **[jmvcore/error]** 16 `stop()` calls. Many are inside helpers (`.validateInputData` at line 74-145 uses early `return(list(valid=FALSE, error=...))` instead of `stop` — good pattern). The remaining `stop()` calls in main path should become `jmvcore::reject`.

#### Integration

**Arguments declared:** 33 · **used in logic:** 16 unique `self$options$` accesses — **17 declared options may be cosmetic / not affecting logic**. Some are likely UI groups (`Label`-style) that are referenced but not by the simple `self$options$X` pattern. Recommend `/check-function-full timeinterval` for precise dead-option enumeration.

**Outputs declared:** 10 · **populated:** 10

#### Notices coverage

| Trigger | Type | Present? | Quantified? | Notes |
|---|---|:---:|:---:|---|
| Missing required inputs | ERROR | ✅ | ✅ | Via `add_message` helper (line 848-852) |
| Critically small sample (n<10) | STRONG_WARNING | ✅ | ✅ | line 848-849 |
| Small sample (n<20) | WARNING | ✅ | ✅ | line 850-851 |
| Methodology summary | INFO | ✅ | ✅ | line 856-858 |
| No valid intervals | ERROR | ✅ | n/a | line 862-873 (HTML notice path) |

**Notice serialization:** ✅ No risk — uses inline HTML setContent and an `add_message` helper.

#### Code review

- **Overall quality:** ★★★★☆ (4/5)
- **Architecture:** Clean — validators return status objects rather than throwing; explicit data-quality assessment pipeline.
- **Mathematical correctness:** CORRECT — calendar-aware vs standardized time-basis (line 804), confidence intervals, person-time aggregation are sound.
- **Clinical readiness:** READY — small-sample guardrails (n<10 strong warning, n<20 warning), landmark analysis support, negative-interval detection.
- **i18n coverage:** NONE — 0 `.()` calls. All strings hard-coded English.

**Top issues:**

1. Apparent option-count gap (33 declared vs 16 used) — likely UI labels/groups, but worth verifying.
2. i18n not started.
3. 16 `stop()` calls in main paths should be `jmvcore::reject` for jamovi UX.

#### Recommended remediation

- `/check-function-full timeinterval` — verify the 17-option gap (likely labels, not dead).
- `/jamovify-function timeinterval --pattern=error --apply` — convert main-path `stop()` to `jmvcore::reject`.
- `/prepare-translation timeinterval` — start i18n wrapping.

---

### ORPHANED — survivalPower.events.js

**Status:** ❌ ORPHANED-IMPLEMENTATION

[`jamovi/js/survivalPower.events.js`](../../jamovi/js/survivalPower.events.js) exists (4853 bytes, event handlers for clinical presets like `oncology_phase3`, `cardio_prevention`, `biomarker_study`), but there is no matching analysis:

- No entry for `survivalPower` in [`jamovi/0000.yaml`](../../jamovi/0000.yaml)
- No `jamovi/survivalPower.a.yaml`
- No `R/survivalPower.b.R`
- Only [`R/survivalPower_distributions.R`](../../R/survivalPower_distributions.R) exists (helper code with a roxygen comment that says "Enhanced distribution support for survivalPower function")

The function appears to have been moved to a different module (`meddecide`?) but the JS asset and distributions helper were left behind. **Recommendation:** delete `jamovi/js/survivalPower.events.js` and `R/survivalPower_distributions.R` if confirmed orphaned, or restore the `.a.yaml`/`.b.R`/`.r.yaml`/`.u.yaml` if the analysis is intended to live in jsurvival.

---

## Cross-Cutting Issues

### C1 — Formula injection via `as.formula(...)` without `jmvcore::asFormula` guard

**Severity:** MEDIUM (reachability: column names go through `jmvcore::constructFormula` or `.escapeVariableNames` in most paths, but the outer call lacks the allow-list parse-time guard introduced in jmvcore for jamovi 2.7.27+).

**Affected functions:** `multisurvival` (~10 sites), `survival` (~10 sites), `survivalcont` (~12 sites), `singlearm` (~4 sites), `oddsratio` (~2 sites).

**Reference fix pattern:** wrap the outer `as.formula(<string>)` with `jmvcore::asFormula(<string>)`. `singlearm` already uses `jmvcore::constructFormula` to build the string — just swap the outer call.

**Run per function:** `/jamovify-function <name> --pattern=formula --apply`

### D — HTML XSS via unescaped variable names / factor labels in `setContent`

**Severity:** LOW–MEDIUM. The known sink is `setContent()` with `paste0`/`sprintf`/`glue::glue` interpolation. Reachability requires column names containing HTML metacharacters, which is unusual but allowed by jamovi.

**Affected functions:** `multisurvival`, `oddsratio`, `singlearm`, `outcomeorganizer`, `datetimeconverter` (uses partial `gsub` escaping).

**Reference fix pattern:** `survivalcont.b.R:733-734` — `htmltools::htmlEscape(title), htmltools::htmlEscape(message)`. Wrap any interpolated string-typed value before pasting into HTML.

**Run per function:** `/security-audit-function <name>` (full audit with approval gates).

### Notice serialization risk — `self$results$insert(N, notice)` with `jmvcore::Notice`

**Severity:** CRITICAL (per `CLAUDE.md` — causes "attempt to apply non-function" protobuf serialization errors).

**Affected functions:** `multisurvival` (20 sites including 5 `insert(999, …)`), `oddsratio` (1 wrapper site), `outcomeorganizer` (1 wrapper site). `survival.b.R` previously hit this and removed 4 notices instead of migrating.

**Reference fix patterns:**

- `R/singlearm.b.R:192-246` — message accumulation (`.addError` / `.addWarning` / `.addInfo`) + `.displayMessages` rendering 3 HTML outputs (errors / warnings / info).
- `R/survivalcont.b.R:700-743` — `.addHtmlMessage(type, title, message)` with `htmltools::htmlEscape` per call.

**Run per function:** `/fix-notices <name>` (uses `R/waterfall.b.R` from main ClinicoPath module as the canonical example).

### Stop() vs jmvcore::reject() — error UX gap

**Severity:** MEDIUM (functional but poor jamovi UX).

**Stats:** 71 `stop()` total vs 6 `jmvcore::reject()` (all in `survival.b.R`).

**Per function:** multisurvival 25 · timeinterval 16 · outcomeorganizer 12 · survival 10 · oddsratio 5 · datetimeconverter 2 · singlearm 1 · survivalcont 0.

**Run per function:** `/jamovify-function <name> --pattern=error --apply`

### i18n setup fragility

**Severity:** LOW (functional but suggests build-time issues).

`multisurvival.b.R:2-3` has:
```r
if (!exists(".")) {
  . <- function(x) x  # Fallback identity function for testing
}
```

This workaround exists because `.()` from jmvcore isn't reliably loaded during testing. **Fix:** ensure `NAMESPACE` has `importFrom(jmvcore, .)` and remove the workaround.

Also: `jamovi/i18n/` directory does not exist. 332 `.()` wraps in source have no catalog destination.

**Run per function:** `/prepare-translation <name>`

### Orphaned JavaScript / helper files

- `jamovi/js/survivalPower.events.js` — no matching analysis.
- `R/survivalPower_distributions.R` — only referenced by the orphaned JS's docstring.

**Recommendation:** confirm with the maintainer whether `survivalPower` belongs in jsurvival (then create the `.a.yaml`/`.b.R`/`.r.yaml`/`.u.yaml`) or has been moved to another module (then delete the orphans).

### Duplicate top-level helpers

`.escapeVariableNames` is defined identically in both `R/multisurvival.b.R:7-12` and `R/survival.b.R:207-212`. Move to `R/utils.R` (which already exists) and remove duplicates.

---

## Remediation Playbook

**Priority 1 — Notice serialization (CRITICAL, blocks reliable jamovi rendering):**

- `/fix-notices multisurvival`
- `/fix-notices oddsratio`
- `/fix-notices outcomeorganizer`
- `/fix-notices survival` (restore the 4 removed notices)

**Priority 2 — Security hardening (HIGH—MEDIUM-tier):**

- `/security-audit-function multisurvival`
- `/security-audit-function oddsratio`
- `/security-audit-function singlearm`
- `/security-audit-function survival`
- `/security-audit-function survivalcont`

**Priority 3 — jmvcore migration (module-wide hygiene):**

- `/jamovify-function multisurvival --apply`
- `/jamovify-function survival --apply`
- `/jamovify-function survivalcont --apply`
- `/jamovify-function singlearm --pattern=formula --apply`
- `/jamovify-function oddsratio --pattern=formula --apply`
- `/jamovify-function outcomeorganizer --pattern=error --apply`
- `/jamovify-function timeinterval --pattern=error --apply`
- `/jamovify-function datetimeconverter --pattern=error --apply`

**Priority 4 — Statistical / clinical validation (deeper review):**

- `/review-function multisurvival` (largest mathematical surface)
- `/review-function survival`
- `/review-function survivalcont` (cutpoint dichotomization safety)
- `/review-function oddsratio` (Firth-penalized, diagnostic metrics)

**Priority 5 — Internationalization:**

- `/prepare-translation datetimeconverter`
- `/prepare-translation outcomeorganizer`
- `/prepare-translation timeinterval`
- `/prepare-translation oddsratio`
- `/prepare-translation survivalcont`
- Complete coverage on multisurvival / survival / singlearm (~50–60% currently)
- Fix `NAMESPACE` to ensure `importFrom(jmvcore, .)` so the workaround in `multisurvival.b.R:2-3` is no longer needed
- Create `jamovi/i18n/en.po` + `jamovi/i18n/tr.po` (currently missing)

**Priority 6 — Cleanup:**

- Confirm and delete (or restore) the `survivalPower` orphans: `jamovi/js/survivalPower.events.js` and `R/survivalPower_distributions.R`.
- Consolidate duplicate `.escapeVariableNames` into `R/utils.R`.
- Delete `# EXPERIMENTAL:` commented-out blocks in `multisurvival.b.R` (lines 2330-2380 and others) — preserve in a feature branch if needed.
- Resolve "Cannot show notice due to serialization issues" TODO comments in `survival.b.R` (lines 1042, 1346, 1411, 1938) by restoring as HTML.

---

## Appendix: Guides Referenced

- `references/security-patterns.md` — validated categories A, B, C1/C3, D, E, F, H1, H2, I across all 8 functions.
- `references/jmvcore-migration.md` — validated formula / error / na pattern groups; source/term groups not extensively triggered.
- `references/integration-checks.md` — argument/output crosswalk passed for all 8 functions; placeholder check passed (every function exercises `self$data` and `self$options`).
- `references/notices-checklist.md` — clinical thresholds confirmed for survival (events count), oddsratio (EPV), survivalcont (small-cell warnings); gaps in datetimeconverter / timeinterval / outcomeorganizer are domain-appropriate (these are data-prep, not estimators).
- `references/code-review-checks.md` — i18n area triggered consistently; mathematical/statistical correctness marked NOT_EVALUATED in depth (per audit scope; use `/review-function` for per-function deep review).

---

*Generated by the `audit-module` skill on 2026-05-14 13:43. Re-run with `--profile deep` to add R6 / R-package hygiene checks and vignette cross-references, or `--functions multisurvival,survival` for a targeted re-audit after remediation.*
