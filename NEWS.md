granovaGG 1.4.0
===============

## Minor Changes

- Added a NEWS.md file. (Hello, world!)
- Remove calls to unit(), which will help address issues like #164
- Stop importing datasets from MASS; instead put them directly in the package and document them locally

## Bug Fixes

- [Fixed a typo on an `alpha` parameter][1] in `granovagg.ds` that was giving unneeded grief. Many thanks to Jeremy Gray ([@jeremycg](https://github.com/jeremycg)) for [raising the issue][2]
- Fix bugs related to drawing CI Band, crossbow, trails, and shadows in granovagg.ds

[1]: https://github.com/briandk/granovaGG/pull/160
[2]: https://stackoverflow.com/questions/32482088/matching-error-unknown-parameters-lpha?noredirect=1#32482088
