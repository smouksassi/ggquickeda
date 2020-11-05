## Test environments
* local Windows 10 install, R 4.0.3
* ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is an update adding several shiny app ui and functionality enhancements and one new vignettes.
* some Notes about unused dependencies from the previous builds on some platforms are false positives since these are used in the shiny app code and are required for it.
* Sometimes a Note about depending on many packages appear and is a choice and a risk I am willing to take, as this package requires all of them to work properly.
* One Note about unavailable URL is for the new vignette and it will work once the package is on CRAN

## Reverse dependencies

There is no listed dependencies.

---


