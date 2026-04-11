# TODO

## granovagg.1w
- [issue #149] Add guardrails for two-group unequal sample sizes so the t-test branch  can stack data correctly; include regression tests.
- Create `tests/testthat/test-granovagg-1w.R` verifying group summaries and F-statistic against a known dataset (e.g., `poison`).
- Migrate every `aes_string()` use in `R/granovagg.1w.R` to tidy-eval `aes()` with `.data[[...]]` or `.data$...` lookups; verify plots render identically.
- Replace the lone `%>%` pipeline in `GetSummary()` with the native `|>` pipe (and `dplyr::group_by()`/`summarise()` calls using explicit `.data` pronouns) to reduce dependencies on magrittr syntax.
- [issue #175] Align the plotting orientation with the documented X/Y definitions (or vice versa) and update the help file accordingly.

## granovagg.ds
- Assert that the incoming data has exactly two numeric columns after optional reversal, and that `conf.level` lies in (0,1).
- Add `tests/testthat/test-granovagg-ds.R` that checks the printed summary matrix for `anorexia.sub` and snapshots the CI output.
- Swap the `southwest.padding` / `northeast.padding` multipliers in `PadViewingWindow()` so each argument adjusts the intended edge of the viewing window.
- Replace all `aes_string()` calls with tidy-eval `aes()` usage.
- Convert the `%<>%` assignment in `GetTtest()`/`EnsureDataIsADataFrame()` to idiomatic base R (or `|>`), eliminating reliance on magrittr compound operators.
- [issue #175] Ensure column 1 maps to X and column 2 to Y unless `revc = TRUE`, and cross-check docs.
- [issue #168] Resolve the “Length of logical index vector” failure by tracing where a logical vector is recycled; add a test reproducing the mpg example.

## granovagg.contr
- Assert that contrast matrices have zero-sum columns and dimensions compatible with the response vector; fail fast using `assertthat`.
- Add `tests/testthat/test-granovagg-contr.R` to confirm weighted means/effect sizes for the `arousal` demo and capture console outputs.
- Recompute the pooled standard deviation in `GetGroupSummary()` using a true pooled variance formula (weighted by group sizes) instead of `mean(standard.deviation)^0.5`.
- Replace `tidyr::gather()` with `tidyr::pivot_longer()` in the summary-plot pipeline.
- Convert `aes_string()` calls to tidy-eval `aes()`.
- Replace all `%>%` pipelines (contrast summaries, pooled SD computation, linear-model summary formatting) with base `|>` piping and explicit helper calls.
- [issue #67] Restore user control over jitter width by honoring the `jj` argument again (or adding a new parameter) and documenting its effect.
- [issue #68] Allow per-contrast x-axis labels or overrides while keeping the automatic naming default.
- [issue #49] Preserve original group names in the summary plot instead of renaming columns to numerals.

## Cross-cutting
- Introduce `tests/testthat/setup.R` (if needed) to load datasets and set a deterministic seed for jitter-related unit tests.
- Ensure `DESCRIPTION` lists `testthat` (Suggests) and `assertthat` (Imports) once the above changes land.
- Once `%>%`/`%<>%` usages are removed, drop the `magrittr` import from `DESCRIPTION`/`NAMESPACE`.
- [issue #158] Implement optional suppression/formatting of tabular output across all exported functions with consistent parameter names.
- [issue #70] Add formula/modeled interfaces so users can pass `y ~ group` along with a `data=` argument similar to `lm()`.
- [issue #43] Adopt an overplot detection flow that avoids the current `owp$summary` → `owp$params` → `owp$overplot` dependency.
- [issue #147] Replace partial argument matching with explicit parameter names (e.g., `jitter(stats.vc, amount = ammt)`).
- [issue #95] Re-introduce proper math typesetting when roxygen permits.
- [issue #88] Write a package vignette showing ggplot customization.
- [issue #176] Add `devtools::check_win_devel()` to CI/release scripts and run `usethis::use_dev_version(push = TRUE)` after CRAN acceptance.
