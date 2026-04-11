# TODO

## granovagg.contr
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
- Audit remaining `geom_line()`/`geom_hline()` usages (e.g., in granovagg.1w) to ensure they set `linewidth` instead of the deprecated `size` aesthetic before ggplot2 3.5.
