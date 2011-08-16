## Welcome
You're at the development home for the Graphical Analysis of Variance (granova) package for [R]!

## What does this package do?
The package `granova` is designed to produce statistical graphics driven by the fundamental questions of analysis of variance. The graphics `granova` creates can offer far more visual information than a traditional tabular model summary or significance test. For an example of how granova can enhance analysis, see [Pruzek and Helmreich (2009)].

## How is this package different from granova on CRAN?
This github repository serves as the home of active development on `granova`. It's where we're trying out new things, like re-engineering some of the graphics to take advantage of [ggplot2] graphics.

If you're looking for the current stable, base-graphics version of granova, you'll want to head to [CRAN][granovaClassic]. If you're looking to test-drive the latest [ggplot2][ggplot2] implementations of some `granova` functions, you're in the right place.

## Can I see what some sample graphics look like?
Absolutely. To see examples of some of the [ggplot2][ggplot2]-based `granova` functions, check out:

1.  A [presentation][Feb2011Presentation] Brian Danielak gave at the DC UseR group in February, 2011.
2.  A [presentation][2011July14Presentation] on some of the latest updates to `granovagg.1w` and `granova.contr.ggplot`

## How can I install the development version of granova on my system?
There are at least two ways to do this:

1.  [Install Using Git][gitGranovaInstall]
2.  Instead of Step 1 in the [directions above][gitGranovaInstall], just download the latest version by [clicking the Downloads button][gitDownload]. 

## How do I remove the experimental version?
There are at least two ways to do this:

1.   If you want to go back to [granova 2.0][granovaClassic], just use R's package manager to re-install that package from CRAN.
2.   If you want to remove `granova` entirely, run: 

    `remove.packages("granova", lib = .libPaths())`
    
## How can I provide feedback?
*  If you want to request something, or report a bug, use the [Issue Tracker][issueTracker]
*  If you'd like to contact us directly, we'd love to hear from you:
  *  [Robert M. Pruzek](mailto:rpruzek@uamail.albany.edu)
  *  [James E. Helmreich](mailto:James.Helmreich@marist.edu)
  *  [Brian A. Danielak](mailto:briandk@umd.edu)
  *  [William E.J. Doane](mailto:wil@drdoane.com)



[R]: http://www.r-project.org
[Pruzek and Helmreich (2009)]: http://www.amstat.org/publications/jse/v17n1/helmreich.html
[granovaClassic]: http://cran.r-project.org/web/packages/granova/index.html
[ggplot2]: http://cran.r-project.org/web/packages/ggplot2/index.html
[Feb2011Presentation]: http://www.google.com/url?q=http%3A%2F%2Fdl.dropbox.com%2Fu%2F382638%2FBrian-Danielak-granova.pdf&sa=D&sntz=1&usg=AFQjCNGAu0dsFF_GaDjVzLv52fqRScVDSA
[2011July14Presentation]:http://dl.dropbox.com/u/382638/DanielakGranovaRevision20110714.pdf
[gitGranovaInstall]: http://cl.ly/090m3t2g0a1c25111p2n
[gitDownload]: http://cl.ly/1x0y402p3e1p413Z172N
[issueTracker]: https://github.com/briandk/granova/issues