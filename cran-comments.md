## Test environments
* local Windows 10 install, R 3.6.2
* ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a minor update adding a new UI for gradientcolor input
and fixing a bug in risktable when the selection was empty.
* some Notes about dependencies from the previous builds on some platforms are false positives since these are used in the shiny app code and are required for it.
* depending on many packages is a choice and risk I am willing to take as this package requires all of them to work properly.



## Reverse dependencies

There is no listed dependencies.

---


