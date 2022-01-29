This is a documentation/maintenance update only (see NEWS.md).

The standard devtools::check(manual = TRUE, cran = TRUE) gave:

- 0 errors, 0 notes and the 1 (usual) warning about qpdf.

Additionally checked with  

- check_win_devel()
- check_rhub()

with these (seemingly unproblematic) findings:

- checking for detritus in the temp directory ... NOTE Found the following files/directories: 'lastMiKTeXException'

