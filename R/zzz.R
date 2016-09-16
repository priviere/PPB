# to print welcome message when loading the package
.onLoad <- function(...) {
packageStartupMessage("
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Thanks for using PPBformations :-)
!!! To cite PPBstats, type citation(\"PPBformations\")
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
"
)
}
