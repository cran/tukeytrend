dosescalett <-
function(data, dose, scaling=c("ari", "ord", "log", "arilog"), sep="", d0shift=1){

namdat <- colnames(data)
if(length(dose)!=1){stop("argument 'dose' should be a single character string")}
if(!dose %in% namdat){stop( paste("Specified dose variable", dose, "can not be found in data"))}

x <- data[, dose]
xu <- sort(unique(x))

if(any(xu < 0)){stop(paste("There is at least one negative value in the dosage variable (",dose, "): ", paste(xu[which(xu<0)], collapse=", "), sep=""))}

if(any(scaling == "arilog") && all(xu>0)){warning("There is no dose = 0; re-scaling options 'log' and 'arilog' are the same; d0shift is ignored")}

# is dose = 0 in the data

d0 <- xu < 10*.Machine$double.eps
if(any(d0)){isd0 <- TRUE; wd0 <- xu[d0]; 
if(length(wd0)>1){xu[xu %in% wd0] <- 0; xu <- unique(xu)
  warning(paste("In the dosage variable, there is more than one value very close to 0:", paste(wd0, collapse=", "), 
                "; these values have been coerced to 0."))}}else{isd0 <- FALSE; wd0 <- NULL}

if(any(d0shift<0)){stop("d0shift must be positive numbers")}

if(is.null(d0shift) || (length(d0shift)==1 & d0shift==1)){
  SCAL <- scaling
  DIV <- rep(1, times=length(SCAL))
  DIVN <- rep("", times=length(SCAL))
  }else{
  warilog <- which(scaling=="arilog")
  rscal <- rep(1, times=length(scaling))
  rscal[warilog] <- length(d0shift)
  SCAL <- rep(scaling, times=rscal)
  DIV <- rep(1, times=length(SCAL))
  DIVN <- rep("", times=length(SCAL))
  DIV[which(SCAL=="arilog")] <- d0shift
  DIVN[which(SCAL=="arilog")] <- as.character(d0shift)# format(d0shift)
}

datlist <- list()

for(i in seq(along.with=SCAL)){
  datlist[[i]] <- dosescale(x=x, scaling=SCAL[i], xu=xu, wd0=wd0, d0shift=DIV[i])
}

transnam <- paste(dose, SCAL, DIVN, sep="")
names(datlist) <- transnam
datappend <- as.data.frame(datlist)

return(list(data=cbind(data, datappend), transnam=transnam, scaling=SCAL))
}
