#' @importFrom madrat vcat

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The mrplayground package is used for archiving only. DO NOT INCLUDE IT IN YOUR WORK! If you need a function from it, move it to another package first!")
}

.onLoad <- function(libname, pkgname){
  madrat::setConfig(packages=c(madrat::getConfig("packages"),pkgname), .cfgchecks=FALSE, .verbose=FALSE)
}

#create an own warning function which redirects calls to vcat (package internal)
warning <- function(...) vcat(0,...)

# create a own stop function which redirects calls to stop (package internal)
stop <- function(...) vcat(-1,...)

# create an own cat function which redirects calls to cat (package internal)
cat <- function(...) vcat(1,...)