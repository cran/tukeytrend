dfClmett <-
function(mlist, effv){

  DF <- numeric(length=length(effv))
  for(i in seq(along.with=effv))
  {DF[i] <- dfClme(object=mlist[[i]], effect=effv[i])}
  return(DF)
}
