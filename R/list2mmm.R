list2mmm <-
function(ret) 
{
  if (is.null(names(ret))) 
    names(ret) <- as.character(match.call(expand.dots = TRUE))[-1]
  class(ret) <- "mmm"
  ret
}
