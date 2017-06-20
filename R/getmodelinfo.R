getmodelinfo <-
function(object){
  if(any(class(object) %in% c("lm", "glm", "coxph", "lme"))){CALL <- object$call}else{
    if(any(class(object)=="lmerMod")){ CALL <- object@call}else{
      stop(paste("Fitted model should be one of 'lm', 'glm', or 'merMod'; current object has class", class(object), sep=" "))
    }}
  cCALL <- as.character(CALL)
  cCALL
  cCALL[2] <- strsplit(cCALL[2], split="[ ~ ]")[[1]][1]
  
  INFO <- paste(cCALL[1:2], collapse=".")
  return(list(modelinfo=INFO, initcall=CALL))
}
