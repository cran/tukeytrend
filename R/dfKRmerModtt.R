dfKRmerModtt <-
function(mlist, effv){
  DF <- numeric(length=length(effv))
  for(i in seq(along.with=effv))
  {DF[i] <- dfKRmerMod(model=mlist[[i]], effect=effv[i])}
  return(DF)
}
