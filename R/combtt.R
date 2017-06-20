combtt <-
function(...){
  ttlist <- list(...)
  if(is.null(names(ttlist))){names(ttlist) <- as.character(match.call(expand.dots = TRUE))[-1]}
  
  cllist <- unlist(lapply(ttlist, class))
  if(any(cllist!="tukeytrend")){stop("All elements must be objects of class 'tukeytrend'.")}

  MNAM <- names(ttlist) 
  MMM <- list()
  MLF <- list()
  MI <- character()
  IC <- list()
  DF <- numeric()
  
  for(i in seq(along.with=ttlist))
  {
    NAMi <- paste(MNAM[i], ttlist[[i]]$modelinfo, names(ttlist[[i]]$mmm), sep=".")
    MMMi <- ttlist[[i]]$mmm
    MLFi <- ttlist[[i]]$mlf
    DFi <- ttlist[[i]]$df
    names(DFi) <- names(MMMi) <- names(MLFi) <- NAMi
    MMM <- c(MMM, MMMi)
    class(MMM) <- "mmm"
    MLF <- c(MLF, MLFi)
    class(MLF) <- "mlf"
    DF <- c(DF, DFi)
    MI <- c(MI, ttlist[[i]]$modelinfo)
    IC <- c(IC, ttlist[[i]]$initcall)
  }
  
  if(length(unique(names(MMM))) < length(names(MMM))){names(DF) <- names(MLF) <- names(MMM) <- make.unique(names(MMM))}

  return(list(mmm=MMM, mlf=MLF, df=DF, modelinfo=MI, initcall=IC))

}
