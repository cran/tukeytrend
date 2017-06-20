asglht <-
function(object, df="mean", ...)
{
  dargs <- list(...)
  args <- list()
  args$model <- object$mmm
  args$linfct <- object$mlf

  if(is.null(df)){args$df <- NULL}else{
  if(is.numeric(df) | is.integer(df)){
  if(length(df)!=1){warning("df should be a single integer value")}  
    args$df <- df
  }else{
    args$df <- floor(do.call(what=df, args=list(object$df, na.rm=TRUE)))}}

  do.call(what="glht", args=c(args,dargs))
}
