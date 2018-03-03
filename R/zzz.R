# zzz.R - DESC
# /zzz.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


.onAttach <- function(libname, pkgname) {
  
  # FIND OS-based path to pella binary
  path <- paste0(
    system.file(paste0("bin/", get_os()), package="pella", mustWork=TRUE),
    .Platform$path.sep, Sys.getenv("PATH"))
  
  # SET path
  Sys.setenv(PATH=path)
}

# get_os {{{
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(unname(os))
} # }}}
