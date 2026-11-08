# jsurvival release qualification

The adjusted-curve model mismatch identified on 2026-11-08 has been repaired.
See `docs/audit/ADJUSTED_CURVES_FIX_20261108.md` for the repair evidence and the earlier
`docs/audit/MODULE_AUDIT_REPORT_20261108-RELEASE.md` for the original assessment.
The review directory is gitignored; retain both reports with the submission records.
The interaction-forest repair and follow-up evidence are recorded in
`docs/audit/REMAINING_FIXES_20261108.md`. Retain all reports with the submission records.
Complete the remaining desktop and website acceptance coverage before submission.

Nine GUI-configured examples are now in `data/` and registered in the maintained
datasets section of `jamovi/0000.yaml`. See `GUI_EXAMPLES.md` for saved arguments,
90 archive/arithmetic checks, tested options and the remaining DateTime component
preview issue. Rebuild the candidate to bundle these new files and catalog entries.
The example pass does not close the full desktop acceptance matrix below.

## Before submission

1. Verify the repaired Survival Analysis adjusted curves on the final candidate.
   The curves and table share one fitted model, retaining baseline strata, age strata
   or both. Keep the independent baseline-hazard probability regressions and restored
   drawing-state checks green. Recalculate figures from earlier candidates.
2. Verify the repaired interaction forests on the final candidate. Both styles show
   every fitted coefficient, its conditioning values, confidence interval and caption.
   Restore the caption layout with drawing state. A conditional main-effect HR is not
   an overall effect; an interaction coefficient is a ratio of HRs, not a subgroup HR.
3. Make statistical repairs in the canonical ClinicoPath source and synchronize the
   sibling through the maintained distribution tooling. Verify parity after generation;
   do not hand-edit `.h.R`, `.Rd`, `NAMESPACE` or analysis registry entries in
   `0000.yaml`. Its module-owned datasets section is maintained with the dataset
   registration helper and preserved by `prepare()`.
4. Rerun the shipped suite, affected numerical tests and the independent oracle on the
   final source. Run `R CMD build` and `R CMD check --no-manual --no-vignettes` on its tarball.
   Vignettes are excluded from the R tarball by this repository's `.Rbuildignore`, so
   render the documentation separately. The 2026-11-08 online build could not resolve
   `cloud.r-project.org`; retain a successful online build as a submission check.
   Distinguish executed examples from deliberately
   disabled illustrative chunks.
5. Rebuild the candidate after any source repair. Record its SHA-256, platform and
   jamovi series; repeat archive/source and bundled-engine checks on that exact file.
   A matching version number alone does not identify an artifact.
6. In the desktop app, test the final candidate with empty, valid and invalid inputs;
   change options and data; save and reopen `.omv`; resize plots; check light/dark
   themes; inspect copied tables and PNG/PDF exports. Record the app version and
   candidate digest. Engine serialization checks do not complete desktop acceptance.
7. Recalculate older saved adjusted curves, interaction forests and DateTime parsing
   tables; cached output can still contain the former labels/percentages.
8. Check metadata, NEWS, citations, dataset descriptions and bundled licenses again.
   Update website sources and confirm a normal online site build on the release checkout.
   Do not describe numerical agreement as external clinical validation.

## Evidence after follow-up repairs (2026-11-08)

- Forest regression before repair: **6 PASS / 10 FAIL**. Repaired regression: **34 PASS**;
  a further **12 PASS** cover caption-layout restoration. No errors, warnings or skips.
- DateTime denominator regression: original **3 PASS / 9 FAIL**; repaired **12 PASS**.
  The table now names its common denominator and contains finite percentages.
- Full shipped suite: **1,141 PASS**, no failures/errors/warnings; two existing skips
  for helpers absent from jsurvival.
- Independent cross-analysis oracle against the final archive: **86/86 PASS**.
- Built-package check with tests and normal examples enabled: **Status: OK**
  (`--no-manual --no-vignettes`; standard example skip markers still apply).
- Installed-namespace smoke: zero findings over **1,252** functions/methods.
- Static release gate: zero blocking findings and zero advisories.
- Candidate/source identity: **1,252** matching functions/methods; archive CRC passed.
- Actual-archive engine: **35/35 adjusted-curve checks** and **34/34 forest checks**,
  including restored PNG identity, coefficient/CI/label agreement and style changes.
  DateTime native lifecycle and invalid/recovery checks: **12/12 PASS**.
- English/Turkish runtime catalogs agree with the final archive. README and the
  changed executable/reporting and clinician/interactions articles rendered.
- Desktop verification and its limits are recorded in the follow-up report. This
  evidence does not replace the complete matrix in item 6 or external clinical validation.

Candidate SHA-256: `7e66df987fddee16f9ee527481930625c11d212bffbebfa9f29f83ac839a156a`.
These results close R1, R2 and the DateTime presentation finding for the tested behavior; submission acceptance remains a
separate decision. No statistical defaults or clinical cutoffs changed.

## Library submission

Once the remaining work is complete, follow the current
[official jamovi distribution instructions](https://dev.jamovi.org/tutorial/tuts0111-distributing-modules/).
Provide the source repository and release identifier to the jamovi team. A sideloaded
`.jmo` is specific to its build platform and jamovi series; the library team builds
the supported variants. No submission or external message was sent by this review.
