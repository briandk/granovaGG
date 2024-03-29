# granovaGG (development version)

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
