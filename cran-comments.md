## Test environments
* local Windows 10 install, R 4.2.2
* github actions windows-latest (release)
* github actions macOS-latest (release)
* github actions ubuntu-20.04 (release)
* github actions ubuntu-20.04 (devel)
* win-builder (devel and release)


## R CMD check results

0 errors | 0 warnings | 2 notes

* The package size is now 4917 kb after compressing the vignette pngs even more.l
* This is a small update adding user experience improvement for the user to be able to save the state of the shiny app.
* some Notes about unused dependencies from the previous builds on some platforms are false positives since these are used in the shiny app code and are required for it.
* Note about depending on many packages appear and is a choice and a risk I am willing to take, as this package requires all of them to work properly.
* An attempt to reduce the size of the package was made by further reducing the readme/vignettes' pngs.

## Reverse dependencies

There is no listed dependencies.

---


