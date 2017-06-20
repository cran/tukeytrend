tukeytrendformula <-
function(formula, data, model="lm", dose, 
            scaling=c("ari", "ord", "log", "arilog", "treat", "treatHL"), ctype=NULL,
            ddf = c("residual", "KR", "PB"), d0shift=1, ...){
  margs <- list(...)
  TX <- dosescalett(data=data, dose=dose, scaling=unique(scaling), d0shift=d0shift)
  TNAM <- TX$transnam
  TDAT<- TX$data
  SCAL <- TX$scaling
  MLIST <- list()
  
  for(i in seq(along.with=SCAL)){
    NFORMI <- as.formula(paste(". ~ . - ", dose, " + ", TNAM[i], sep=""))
    FORMI <- update(old=formula, new=NFORMI)
    MARGSI <- c(list(formula=FORMI, data=TDAT), margs, na.action="na.exclude")
    MLIST[[i]] <- do.call(model, args=MARGSI)
  }
  names(MLIST) <- TNAM
 
  if(any(class(MLIST[[1]]) == "lmerMod")){
    switch(ddf,
           "residual" = {DF <- unlist(lapply(MLIST, df.residual))},
           "PB" = {warning("Option 'PB' is not available for models fitted using lmer; option 'KR' will be used instead.");
             DF <- dfKRmerModtt(mlist=MLIST, effv=TNAM)},
           "KR" = {DF <- dfKRmerModtt(mlist=MLIST, effv=TNAM)})
    MLF <- dosescalett2mlf(object=TX, mmm=MLIST, ctype=ctype)
    MLIST <- lapply(MLIST, lmer2lm)
    MMM <- list2mmm(MLIST)  
    
  }else{
    if(any(class(MLIST[[1]]) == "lme")){
      switch(ddf,
             "residual" = {DF <- unlist(lapply(MLIST, df.residual.lme))},
             "KR" = {warning("Option 'KR' is not available for models fitted using lme; option 'PB' will be used instead.");
               DF <- dfClmett(mlist=MLIST, effv=TNAM)},
             "PB" = {DF <- dfClmett(mlist=MLIST, effv=TNAM)})
      MLF <- dosescalett2mlf(object=TX, mmm=MLIST, ctype=ctype)
      MLIST <- lapply(MLIST, lmer2lm)
      MMM <- list2mmm(MLIST) 
    }else{
      
    
    MMM <- list2mmm(MLIST)  
    MLF <- dosescalett2mlf(object=TX, mmm=MMM, ctype=ctype)
    DF <- unlist(lapply(MLIST, df.residual))
  }}
  
  MODINFO <- getmodelinfo(MLIST[[1]])
  OUT <- c(list(mmm=MMM, mlf=MLF, df=DF), MODINFO)
  class(OUT) <- "tukeytrend"
  return(OUT)
}
