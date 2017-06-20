dosescalett2mlf <-
function(object, mmm, ctype=NULL)
{
DAT <- object$data
TNAM <- object$transnam
SCA <- object$scaling

MLF <- as.list(paste(TNAM, " = 0", sep=""))
names(MLF) <- TNAM

if(any(SCA %in% c("ari", "ord", "log", "arilog"))){
  wtreat <- which(SCA %in% c("ari", "ord", "log", "arilog"))
  for(i in wtreat){
   # MLF[[i]] <- chrlinfct2cmat(mmm[[i]], linfct=MLF[[i]])
    MLF[[i]] <- glht(model=mmm[[i]], linfct=MLF[[i]])$linfct
  }
}

if("treat" %in% SCA){
  if(is.null(ctype)){CTYPE <- "Dunnett"}else{CTYPE <- ctype}
  wtreat <- which(SCA=="treat")
  MCPL <- list(CTYPE)
  names(MCPL) <- TNAM[wtreat]

 # MLF[[wtreat]] <- do.call("mcp", MCPL)
  # MLF[[wtreat]] <- multcomp:::mcp2matrix(model=mmm[[wtreat]], linfct=do.call("mcp", MCPL))$K
  MLF[[wtreat]] <- glht(model=mmm[[wtreat]], linfct=do.call("mcp", MCPL))$linfct
}

if("highvslow" %in% SCA){
  if(is.null(ctype)){CTYPE <- "Dunnett"}else{CTYPE <- ctype}
  whl <- which(SCA=="highvslow")
  MCPL <- list(CTYPE)
  names(MCPL) <- TNAM[whl]
 # MLF[[whl]] <- multcomp:::mcp2matrix(model=mmm[[whl]], linfct=do.call("mcp", MCPL))$K
  MLF[[whl]] <- glht(model=mmm[[whl]], linfct=do.call("mcp", MCPL))$linfct
}
  

class(MLF)<-"mlf"
return(MLF)
}
