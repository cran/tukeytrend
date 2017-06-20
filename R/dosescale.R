dosescale <-
function(x, scaling=c("ari", "ord", "log", "arilog", "highvslow", "treat"), xu, wd0=NULL, d0shift=1)
{
  SCA <- match.arg(scaling)
  
  dintpol <- function(xu){log(xu[2]) - d0shift*((xu[2]-xu[1])/(xu[3]-xu[2])) * (log(xu[3]) - log(xu[2]))}
  
  switch(SCA,
         ari={XOUT <- x},
         ord={XOUT <- as.integer(factor(x))-1},
         log={XOUT <- log(x); if(!is.null(wd0)){XOUT[x %in% wd0] <- NA}},
         arilog={XOUT <- log(x); if(!is.null(wd0)){XOUT[x %in% wd0] <- dintpol(xu)}},
         highvslow={xuHL <- c(xu[1], xu[length(xu)]); XT <- x; XT[x %in% wd0] <- xu[1]; XT[!x %in% xuHL] <- NA; XOUT <- droplevels(factor(XT)) },
         treat={XT <- x; XT[x %in% wd0] <- xu[1]; XOUT <- factor(XT, levels=xu);
         tf <- table(XOUT); if(any(tf<2)){warning(paste("Some levels have less than 2 observations after coercing 'dose' to a factor:", paste(levels(XOUT)[which(tf<2)], collapse=","), sep=""))}
         }
           )
  return(XOUT)
  
}
