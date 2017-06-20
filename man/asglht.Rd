\name{asglht}
\alias{asglht}

\title{
Coerce object of class 'tukeytrend' to 'glht'
}

\description{
Transfers output of functions tukeytrendfit and tukeytrendformula to glht.
}

\usage{
asglht(object, df = "mean", ...)
}

\arguments{
  \item{object}{
 an object of class 'tukeytrend' as results from functions \code{\link{tukeytrendfit}}, or \code{\link{tukeytrendformula}}
}
  \item{df}{defines whether and how/which degrees of freedom are passed to \code{glht} for the computation of multivariate quantile. Can be a single character string, that names a function (\code{"mean", "min", ...}) to summarize the individual models' degrees of freedom to a single degree of freedom, defaults to computing the mean df of all the input models. Setting \code{df=NULL} invokes that no degree of freedom is passed to \code{glht}, i.e. asymptotic inference by use of multivariate normal quantiles. You may also specify a single integer df in this argument, which is then passed to \code{glht.}
}
  \item{\dots}{
further arguments to be passed to \code{glht}, e.g. \code{alternative}
}
}
\details{
Passes the elements \code{$mmm} from tukeytrend output to \code{glht}, argument \code{model}, and \code{$mlf} to argument \code{linfct}, computes summary statistics for the vector of degrees of freedom in tukeytrend and passes it to \code{glht}, argument \code{df}. All other arguments are passed via \code{...} to \code{glht}; be aware that mis-spelled arguments may be lost in \code{glht}.

}
\value{
An object of class \code{glht}.
}

\examples{
data(litter, package="multcomp")
dl <- litter
dl$dosen <- as.numeric(as.character(dl$dose))
fit <- lm(weight ~ dosen + number, data=dl)
ttlitter <- tukeytrendfit(fit, dose="dosen", 
 scaling=c("ari", "ord", "log"))

# instead of transferring individual elements from the
# tukeytrend output into the arguments of glht , ...
# comp1 <- glht(model=ttlitter$mmm, linfct=ttlitter$mlf,
# df=round(mean(ttlitter$df)), alternative="less")
# summary(comp1)

# ... you can just use:
comp2 <- asglht(ttlitter, alternative="less")
summary(comp2)


}

\keyword{ misc }
