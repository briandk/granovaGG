# granovaGG (development version)
-   granovagg.contr once again honors the `jj` jitter argument, letting values
    below 1 act as direct widths while retaining the historical percent-based
    behavior for larger values; the demo now includes an example showing how to
    widen the jitter window when contrast panels overplot.
-   Added the "Customizing granovaGG Graphics with ggplot2" vignette (issue
    #88) to demonstrate how to theme and extend each plot type.
-   Audited the remaining line-based geoms in granovagg.1w/ds so they now set
    `linewidth` instead of the deprecated `size` aesthetic, eliminating the
    ggplot2 3.4 warnings.
-   Dropped the unused `magrittr` dependency (and roxygen import) now that all
    pipelines rely on base syntax.
-   Clarified dependency metadata by explicitly importing `assertthat (>= 0.2.1)`
    and keeping `testthat` in Suggests so the new regression tests have their
    required packages declared.
-   Added `tests/testthat/setup.R` to seed jittered geoms and preload the common
    demo datasets so visual regression tests remain deterministic.
-   The granovagg.contr summary plot now preserves the original group labels
    from the input data (or contrast rows), only falling back to numeric
    indices when no names exist.
-   Added an `xlab` override to granovagg.contr so each contrast panel can adopt
    a custom axis label (matched either by position or column name) while
    retaining the automatic `Contrast <name>` default for unspecified entries.
-   Updated the contrast plots to draw horizontal reference lines and effect
    connectors with `linewidth` rather than the deprecated `size` aesthetic,
    silencing the ggplot2 3.4 warnings emitted during testing.
-   granovagg.1w now asserts numeric inputs and matching group labels up front, failing fast when arguments are malformed.
-   Fixed the two-group summary branch so unequal sample sizes no longer break the t-test output, and added regression coverage for the scenario.
-   Migrated all `aes_string()` calls in granovagg.1w to tidy-eval `aes()` usage with `.data` pronouns, reducing reliance on deprecated evaluation helpers.
-   Corrected the y-range expansion calc to use `owp$range.expansion$vertical.range.expansion`, preventing collapsed axes.
-   Added `print.group.summary`, `print.model.summary`, and `summary.table.format` parameters to granovagg.1w so tabular output can be suppressed or rendered with formatted tables; added regression tests for both behaviors.
-   The ggplot returned by granovagg.1w now carries `group.summary`/`model.summary` attributes, and new regression tests verify those summaries (and the F-statistic) against the `poison` dataset.
-   granovagg.contr now fails fast when contrast matrices do not sum to zero or the data length is incompatible with the group count.
-   Added `tests/testthat/test-granovagg-contr.R`, which exercises the `arousal` demo to ensure weighted means, effect sizes, and console summaries stay consistent.
-   granovagg.contr now computes pooled standard deviations using the proper weighted variance formula and exposes the summary data via the returned plot list for testing.
-   Replaced the remaining `tidyr::gather()` usage in granovagg.contr with `pivot_longer()` as part of modernizing the summary plot pipeline.
-   Converted all `aes_string()` usage in granovagg.contr to tidy-eval `aes()` calls with `.data` pronouns.
-   Replaced the remaining `%>%` pipelines in granovagg.contr with base `|>` flows for cleaner dependency management.
-   granovagg.ds now checks for numeric two-column input after any reversal and validates `conf.level` lies in (0,1); added tests covering both conditions.
-   Added `tests/testthat/test-granovagg-ds-summary.R` to snapshot the anorexia.sub summary output, ensuring reported means, confidence intervals, and t-statistic stay stable.
-   Removed the last magrittr `%<>%` usage in granovagg.ds by switching `GetTtest()` to a base assignment, continuing the migration toward native pipes.
-   `PadViewingWindow()` now applies `southwest.padding` and `northeast.padding` to the intended edges; regression tests confirm the coordinate ranges respect the user inputs.
-   Converted every `aes_string()` call in granovagg.ds to tidy-eval `aes()` with `.data` pronouns, further modernizing the plotting code.
-   Added a regression test using `ggplot2::mpg` to ensure granovagg.ds handles tibble inputs without triggering the historical “Length of logical index vector” error (issue #168).

# granovaGG 1.4.1

## granovaGG 1.4.1

-   Change Software License to the MIT License (previously it was AGPL v2)
-   Update `DESCRIPTION` including `Authors@R` to reflect [best practices](https://r-pkgs.org/description.html#sec-description-authors-at-r)
-   Minor behind-the-scenes housekeeping changes

## granovaGG 1.4.0

MAJOR CHANGES

-   Fixed a typo on an alpha parameter in granovagg.ds that was giving unneeded grief.
    Many thanks to Jeremy Gray (@jeremycg) for raising the issue

-   Fix bugs related to drawing CI Band, crossbow, trails, and shadows in granovagg.ds

MINOR CHANGES

-   Stop importing datasets from MASS; instead put them directly in the package and document them locally

-   Remove calls to unit(), which will help address issues like #164

## granovaGG 1.2

MINOR CHANGES

-   granovaGG now uses ggplot2 v0.92's theming system (<https://github.com/wch/ggplot2/wiki/New-theme-system>)

-   geom_rug_alt() has been removed; we use the "sides" argument of geom_rug instead

## granovaGG 1.1

MAJOR CHANGES

-   granovaGG is compatible with both ggplot2 0.8.9 and ggplot2 0.9.0

-   granovaGG now imports most of its dependencies through a NAMESPACE file, so most dependent packages are no longer loaded into the global namespace

MINOR CHANGES

-   granovagg.ds provides printed summary output

-   granovagg.ds handles axis reversal and visual padding arguments

-   granovagg.1w provides better printed summary output, including linear model information

-   Users can now suppress printed squares in granovagg.1w

-   Printed squares in granovagg.1w now change color if F \> F_critical; previously they changed color if F \> 1.

## granovaGG 1.0

NEW FEATURES

-   ggplot2 implementations of three of the four graphical analysis of variance functions:

    -   .1w - elemental graphic for the one-way analysis of variance contr -
    -   .elemental graphic for observing contrast effects in one-way analysis of
    -   .variance ds - elemental graphic for dependent sample assessment

-   Since the core functions can return ggplot2 objects (or a list of them, in the case of .contr) you can use the extensibility of ggplot2's grammar of graphics to customize your plots

-   geom_rug_alt - an alternate ggplot2 geom for creating rugs whose tufts appear on the top and right of a graph, instead of the default bottom and left sides produced by geom_rug
