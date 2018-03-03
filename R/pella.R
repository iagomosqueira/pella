# pella.R - DESC
# pella/R/pella.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# setPella {{{
setPella <- function(x, dir=tempfile("tmpdir"), lav=FALSE) {
  
  # DELETE tmp dir if it exists
  if (dir.exists(dir))
    unlink(dir)

  # CREATE dir
  dir.create(dir)
  
  # SET options
  dgts <- options()$digits
  options(digits=22)

  # cpue
  idx <- as.data.frame(x@indices, drop=TRUE)
  names(idx)[2:3] <- c('index','name')
  idx <- transform(idx, name=as.numeric(name))
  idx <- idx[!is.na(idx$index),]

  # params
  nms <- c("r", "k", "p","b0")
  if (length(unique(idx$name)) > 0)
    nmIdx <- paste(c('q','sigma'), rep(unique(idx$name), each=2), sep='')
  else
    nmIdx <- c('q','sigma')

  # control
  ctl  <-  x@control[nms,]
  ctl[, 2:4]  <-  ctl[,c(2,4,3)]
  
  flp2rows <- function(x, names=dimnames(x)[[2]])
    lapply(as(x, 'list'), setNames, nm=names)

  ctl <- flp2rows(ctl)

  # CREATE CTL file
  fctl <- file.path(dir, "pella.ctl")

  mpb:::writeADMB(ctl, fctl, append=FALSE)
  
  cat('# q ####################\n', file=fctl, append=TRUE)

  ctl <- x@control[nmIdx[grep('q',nmIdx)],,1]
  ctl[, 2:4, 1] <- ctl[, c(2,4,3), 1]
 
  flp2cols <- function(x, names=dimnames(x)[[2]])
    lapply(lapply(as(ctl, 'list'), setNames, nm=names), as.list)

  ctl <- flp2cols(ctl, names=c('phase','lower','upper','guess'))
  mpb:::writeADMB(unlist(ctl), fctl, append=TRUE)
  
  cat('# sigma ################\n', file=fctl,append=TRUE)
  ctl <- x@control[nmIdx[grep('s',nmIdx)],,1]
  ctl[, 2:4, 1] <- ctl[, c(2,4,3), 1]
 
  ctl <- flp2cols(ctl, names=c('phase','lower','upper','guess'))
  mpb:::writeADMB(unlist(ctl), fctl, append=TRUE)

  # CREATE OBJ file
  mpb:::writeADMB(ifelse(lav, 0, 1), file.path(dir, "pella.obj"), append=FALSE)

  # CREATE PRR file
  prr <- x@priors[c(nms,c('msy','bmsy','fmsy'),nmIdx),] 
  prr <- flp2rows(FLPar(prr), names=dimnames(prr)[[2]])
  mpb:::writeADMB(prr, file.path(dir, "pella.prr"), append=FALSE)
 
  # CREATE REF file
  if (is.na(x@ref["yr"])) 
    x@ref["yr"]=as.integer(range(x)["maxyear"] - 
    (range(x)["minyear"] + range(x)["maxyear"]) / 2)
  
  mpb:::writeADMB(x@ref, file.path(dir, "pella.ref"), append=FALSE)

  # WRITE data
  ctc <- as.list(model.frame(FLQuants("catch"=x@catch), drop=TRUE))
  ctc <- c(nYrs=length(ctc[[1]]), ctc)
  res <- c(ctc, c(nIdxYrs=dim(idx)[1], nIdx=length(unique(idx$name)), idx))

  mpb:::writeADMB(res, file.path(dir, "pella.dat"), append=FALSE)

  invisible(TRUE)

  # vcov
  vcov(x) <- FLPar(array(NA, dim=c(dim(params(x))[1], dim(params(x))[1],1), 
    dimnames=list(params=dimnames(params(x))[[1]],
    params=dimnames(params(x))[[1]],iter=1)))
  
  options(digits=dgts)
  return(x)} # }}}

# callPella {{{
callPella <- function(dir, args="") {
  cur <- getwd()
  setwd(dir)
  on.exit(setwd(cur))
  return(system2("pella", args=args, stdout=TRUE, stderr = TRUE))
} # }}}

# readPella {{{

readPella <- function(dir) {

  # rep file
  frep <- file.path(dir, "pella.rep")
  t1 <- read.table(frep, skip=18, header=TRUE)

  # params
  t2  <- unlist(c(read.table(frep, nrows=4)))
  tq <- unlist(c(read.table(frep, nrows=1, skip=8)))
  ts <- unlist(c(read.table(frep, nrows=1, skip=10)))
  
  names(t2) <- c('r','k','b0','p')
  names(tq) <- paste0("q", seq_len(length(tq)))
  names(ts) <- paste0("sigma", seq_len(length(ts)))

  pars <- FLPar(c(t2, tq, ts),
    units=c("", "t", "", "", rep("", length(tq) + length(ts))))
  
  stk <- FLQuant(unlist(c(t1[,'stock'])), units="t", dimnames=list(year=t1[,"year"])) 
  
  msg <- try(t3 <- t(array(read.table(file.path(dir, "lls.txt"), sep="\n"))))

  # TODO: ll
# if (!(any(is(msg) == "try-error"))) {
#   
#   t3 <- t3[,length(t3)]
#   t3 <- as.numeric(unlist(strsplit(str_trim(t3), " ")))
#
#    ll <- FLPar(array(t(array(t3,c(length(s.),5))),
#      dim=c(5,length(s.),1),
#      dimnames=list(params=c("ll","rss","sigma","n","q"),
#      index =seq(length(s.)),
#      iter=1)))[-5,]
#    }

  return(list(stock=stk, params=pars))
  
} # }}}
