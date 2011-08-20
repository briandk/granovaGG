## Welcome
You're at the development home for the Graphical Analysis of Variance using ggplot2 (granovaGG) package for [R]!

## What does this package do?
The package granovaGG is designed to produce statistical graphics driven by the fundamental questions of analysis of variance. The graphics granovaGG creates can offer far more visual information than a traditional tabular model summary or significance test. For an example of how granovaGG can enhance analysis, see [Pruzek and Helmreich (2009)].

## Isn't there already a granova package on CRAN?
Yes.

## So what's the difference between granova and granovaGG?
There are several, actually.

First, the current version of [granova][granovaClassic] on CRAN uses R's base graphics to produce plots. `granovaGG`, on the other hand, uses an R package called [ggplot2][ggplot2], which offers a much greater degree of expressiveness and extensibility.

Second, 3-D functionality for creating an elemental graphic for 2-way ANOVA is only in granova; granovaGG doesn't have it.

In short, `granovaGG` offers ggplot2-based versions of three of the four core functions in `granova`:

  *  `granova.1w`
  *  `granova.contr`
  *  `granova.ds` 

## Do I have to choose between granova and granovaGG?
Absolutely not. You can have your granova cake with granovaGG icing!

The packages are currently designed to work side-by-side, which means you can load them both and get the most out of their respective strengths.

And, if you already have legacy granova code but would like to use the new ggplot2-based functions in granovaGG, the transition is easy: just add two letters to your function call.

So, suppose you have some code that uses three classic functions:

```r
granova.contr(arousal, contrasts = contrasts22)
granova.ds(blood_lead)
granova.1w(poison$SurvTime, group = poison$Group, ylab = "Survival Time")
```

You can pipe the same code through granovaGG adding a "gg" after the "granova" part of the function call:

```r
granovagg.contr(arousal, contrasts = contrasts22)
granovagg.ds(blood_lead)
granovagg.1w(poison$SurvTime, group = poison$Group, ylab = "Survival Time")
```

## Can I see what some sample graphics look like?
Absolutely. To see examples of granovaGG output, check out:

1.  A [presentation][Feb2011Presentation] Brian Danielak gave at the DC UseR group in February, 2011.
2.  A [presentation][2011July14Presentation] on some of the latest updates to `granovagg.1w` and `granovagg.contr`

## How can I install the development version of granovaGG on my system?
There are three ways to get granovaGG. Which way you choose depends on how adventurous you are and how much stability you want. *Choose your path wisely:*

### I've got icewater in my veins I want bleeding-edge code
First, make sure you've got Hadley Wickham's excellent [devtools][devtools] package installed. If you haven't got it, you can get it in R:

```r
    install.packages(pkgs="devtools", dependencies=TRUE)
```

Then, use the `install_github()` function to fetch our spooky experimental code from the development branch

```r
    library("devtools")
    install_github(repo="granovaGG", username="briandk", branch="dev")
```
        
Lastly, sign the imaginary waiver that says we're not responsible if granovaGG steals your cat *And, don't forget to [Report your bugs][issueTracker] and [share stories of inspiration on the wiki][wiki]*
        
### I want a relatively stable release, but I don't have time to wait for CRAN
First, make sure you've got Hadley Wickham's excellent [devtools][devtools] package installed. If you haven't got it, you can get it in R:

```r
    install.packages(pkgs="devtools", dependencies=TRUE)
```    
Then, use the `install_github()` function to fetch our hardy code from the master branch:

```r
    library("devtools")
    install_github(repo="granovaGG", username="briandk", branch="master")
```

### I want a stable, official release from CRAN
You'll have to wait a bit. We haven't yet submitted to CRAN, but we hope you'll come back and see us again soon :-)

## How do I remove the experimental version?

If you want to remove granovaGG entirely, run: 

```r
    remove.packages("granovaGG", lib = .libPaths())
```

*If you're removing `granovaGG` because of something buggy, be sure to [report it][issueTracker] so we can get right on fixing it.*
    
## How can I provide feedback?
*  If you want to request something, or report a bug, use the [Issue Tracker][issueTracker]
*  If you'd like to contact us directly, we'd love to hear from you:
    *  [Robert M. Pruzek](mailto:rpruzek@uamail.albany.edu)
    *  [James E. Helmreich](mailto:James.Helmreich@marist.edu)
    *  [Brian A. Danielak](mailto:briandk@umd.edu)
    *  [William E.J. Doane](mailto:wil@drdoane.com)
    *  [Jason Bryer](mailto:jason@bryer.org)



[R]: http://www.r-project.org
[Pruzek and Helmreich (2009)]: http://www.amstat.org/publications/jse/v17n1/helmreich.html
[granovaClassic]: http://cran.r-project.org/web/packages/granova/index.html
[ggplot2]: http://cran.r-project.org/web/packages/ggplot2/index.html
[Feb2011Presentation]: http://www.google.com/url?q=http%3A%2F%2Fdl.dropbox.com%2Fu%2F382638%2FBrian-Danielak-granova.pdf&sa=D&sntz=1&usg=AFQjCNGAu0dsFF_GaDjVzLv52fqRScVDSA
[2011July14Presentation]:http://dl.dropbox.com/u/382638/DanielakGranovaRevision20110714.pdf
[gitGranovaInstall]: http://cl.ly/090m3t2g0a1c25111p2n
[gitDownload]: http://cl.ly/1x0y402p3e1p413Z172N
[issueTracker]: https://github.com/briandk/granovaGG/issues
[wiki]: https://github.com/briandk/granovaGG/wiki
[devtools]: http://cran.r-project.org/web/packages/devtools/index.html