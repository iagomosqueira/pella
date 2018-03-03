# admb.R - DESC
# pella/R/admb.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# writeADMB {{{

writeADMB <- function(x, file, append=FALSE) {

  cat('#', names(x[1]),'\n', file=file, append=append)
  cat(x[[1]], '\n', file=file, append=TRUE)
  
  if (length(x) > 1)
  for (i in 2:length(x)) {
    cat('#', names(x[i]),'\n', file=file, append=TRUE)
    cat(x[[i]], '\n', file=file, append=TRUE)
  }
  invisible(TRUE)
}
# }}}
