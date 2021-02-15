## Test environments
* local Windows 10 install, R 4.0.3
* ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is an update adding several shiny app ui and functionality enhancements.
* some Notes about unused dependencies from the previous builds on some platforms are false positives since these are used in the shiny app code and are required for it.
* Note about depending on many packages appear and is a choice and a risk I am willing to take, as this package requires all of them to work properly.
* An attempt to reduce the size of the package was made by reducing the readme/vignettes' pngs.

## Reverse dependencies

There is no listed dependencies.

---


