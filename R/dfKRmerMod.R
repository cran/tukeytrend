dfKRmerMod <-
function(model, effect){
 if(!is.character(effect) & length(effect)!=1){stop("Effect must be a single character string")}
  formred <- paste(". ~ . -", effect, sep="")
  datnax <- droplevels(na.omit(model.frame(model)))
  model <- update(model, data=datnax)
  modelred <- update(model, formula.=formred, data=datnax)
  
  dfKR <- getKR(KRmodcomp(largeModel=model, smallModel=modelred), name="ddf")
  return(dfKR)
}
