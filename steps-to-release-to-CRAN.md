# Checking

1. Use `devtools` to run checks in the current version of R
2. Run against `R-devel` by firing up the vagrant VM (`vagrant up` at the terminal). Make sure the Vagrantfile and bootstrap.sh provisioning script are for the latest Long Term Support (LTS) release of Ubuntu. If any bootstrap.sh packages aren't compatible with the latest LTS version, fix that.
3. **When all checks pass**, bump the version number and date. See below for *how to commit the version bump*.

# Bumping Versions and Dates

Use the `--no-verify` option to bypass our ad-hoc versioning system (which uses pre-commit hooks to append a datestamp to the version):

```bash
$ git commit --no-verify -m "Bump Version number and datestamp"
```

# Submitting to CRAN

```r
package_directory = # wherever the package is
library(devtools)
release(
    pkg = package_directory,
    check = TRUE
)
```

# Tagging the release once CRAN accepts it

```bash
$ git tag [version number]
$ git push origin [version number]
```
