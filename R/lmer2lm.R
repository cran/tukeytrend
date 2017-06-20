lmer2lm <- function(lmerFit)
{
  if (inherits(lmerFit, "lmerMod"))
  {
    yVec <- getME(lmerFit, "y")
    Zmat2 <- getME(lmerFit, "Zt")
    # Vinv2 <- (as.numeric(VarCorr(lmerFit)[[reName]]))*(t(Zmat2)%*%Zmat2)+
    # diag(attr(VarCorr(lmerFit), "sc")^2, length(yVec))
    Vinv2 <- Reduce("+",
                    mapply(function(x,y){x * (y^2)},
                           lapply(getME(lmerFit, "Ztlist"), function(x){t(x)%*%x}),
                           lapply(VarCorr(lmerFit), function(x){attr(x, "stddev")}))) +
      diag(attr(VarCorr(lmerFit), "sc")^2, length(yVec))
    # Vsqrt2 <- solve(chol(Vinv2))
    Xmat2 <- getME(lmerFit, "X")
  ##  print(colnames(Xmat2)) ##FS
    naRows <- attr(lmerFit@frame, "na.action")
  }
  if (inherits(lmerFit, "lme"))
  {
    yVec <- getResponse(lmerFit) # without missing values
    fullData <- as.data.frame(lmerFit[["data"]]) # with missing values
    if (inherits(lmerFit, "glmmPQL")) # removing additional columns in the data set in case of PQL fits
    {
      ncolFD <- ncol(fullData)
      fullData <- fullData[, -c(ncolFD, ncolFD - 1, ncolFD - 2)]
    }
   # require(mgcv, quietly = TRUE) # for using extract.lme.cov2()
    Vinv2 <- bdiag((extract.lme.cov2(lmerFit, fullData))[["V"]])
    # constructing block diagonal variance-covariance matrix
    Xmat2 <- model.matrix(lmerFit, fullData) # without missing values
    naRows <- as.vector(lmerFit[["na.action"]]) # rows with missing values
  }
  Vsqrt2 <- solve(chol(Vinv2))
  totalRows <- length(yVec) + length(naRows)
  XmatNew2 <- matrix(0, totalRows, ncol(Xmat2))
  colnames(XmatNew2) <- colnames(Xmat2)
  respNew2 <- rep(NA, totalRows)
  
  if (length(naRows) > 0)
  {
    XmatNew2[-naRows, ] <- as.matrix(t(Vsqrt2) %*% Xmat2)
    respNew2[-naRows] <- as.matrix(t(Vsqrt2) %*% yVec)
  } else {
    XmatNew2 <- as.matrix(t(Vsqrt2) %*% Xmat2)
    respNew2 <- as.matrix(t(Vsqrt2) %*% yVec)
  }
  X <- XmatNew2
  fit <- lm(respNew2 ~ X - 1, na.action = na.exclude)
  #fit <- lm.fit(x=X, y=respNew2)
  #return(update(fit, na.action=na.exclude))
  cnam <- colnames(fit$model$X)
  names(fit$coefficients) <- cnam
  colnames(fit$qr$qr) <- cnam
  names(fit$effects)[1:length(cnam)] <- cnam
  return(fit)
}
