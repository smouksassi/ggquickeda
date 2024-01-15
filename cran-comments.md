## Test environments
* local Windows 10 install, R 4.3.2
* github actions windows-latest (release)
* github actions macOS-latest (release)
* github actions ubuntu-latest (devel, release and oldrel-1)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 3 notes
* This is a re-submission after further trimming down the examples to a minimum
* This is a significant update fixing several bugs and adding three plotting functions.
* Notes about unused declared imports from the previous/current builds are false positives since these are used in the shiny app code and are required for it to work properly.
* Notes about depending on many packages appear and is a choice and a risk I am willing to take, as this package requires all of them to work properly.
* Notes on examples taking > 5s is necessary since ggplot2 rendering of complex plots is long

## Reverse dependencies

There is no listed dependencies.

---


