\name{combtt}
\alias{combtt}
\title{
Combine several pobjects of class 'tukeytrend'
}
\description{
Concatenate several objects of class 'tukeytrend', for example to perform inference for multiple marginal models with different endpoints or including different covariates.
}
\usage{
combtt(...)
}

\arguments{
  \item{\dots}{names of tukeytrend objects (for multiple endpoinst, or with different covariates), separated by comma
}
}

\value{
Same structure as  output of tuketrendfit, see \code{\link{tukeytrendfit}}, \code{\link{tukeytrendformula}}
}

\examples{

# Simulated data for normal and binomial response

dat <- data.frame(y = rnorm(20,0,1), 
 succ=rbinom(n=20, size=10, prob=0.3), 
 dose=rep(c(0,1,2,5,10), each=4))

# linear models with 3 different 
# scalings of the dose variable

fitn <- lm(y~dose, data=dat)
ttn <- tukeytrendfit(fitn, dose="dose",
 scaling=c("ari", "ord", "arilog"))

# generalized linear models with 3 
# different scalings of the dose variable
fitb <- glm(cbind(succ, 10-succ)~dose, data=dat, family=binomial)
ttb <- tukeytrendfit(fitb, dose="dose", 
 scaling=c("ari", "ord", "arilog"))

# concatenate the normal and binomial models
ttnb <- combtt(ttn, ttb)

# joint inference for the 6 regression slopes
summary(asglht(ttnb))

}

\keyword{misc}
