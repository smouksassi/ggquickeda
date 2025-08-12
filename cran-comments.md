## Test environments
* local Windows 11 install, R 4.5.1
* github actions windows-latest (release)
* github actions macOS-latest (release)
* github actions ubuntu-latest (devel, release and oldrel-1)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note
* This is a significant update fixing several bugs and updating/merging plotting functions.
* Notes on "All declared Imports should be used" are false positives since these are used in the shiny app code and are required for it to work properly.
* Notes "Importing from so many (37) packages" is a choice and a risk I am willing to take, as this package requires all of them to work properly.
* Notes on some examples taking > 5s is necessary since ggplot2 rendering of complex plots is long

## Reverse dependencies

There is no listed dependencies.

---


