\name{lmer2lm}
\alias{lmer2lm}

\title{
Coerce objects of class lmerMod or lme to lm
}
\description{
Coerce fitted mixed effect models of class lmerMod or lme to an lm-like object using a pseudo data approach (Ritz et al., 2017)
}
\usage{
lmer2lm(lmerFit)
}
\arguments{
  \item{lmerFit}{
 a fitted object of class \code{"lmerMod"} or \code{"lme"}, as can be obtained from packages \code{"lme4"} and \code{"nlme"}
}
}
\details{
The returned object resembles a fitted model of class "lm" such that its asymptotic representation corresponds to that of the originally fitted mixed model, see Ritz et al. (2017) for details. 
}

\references{
Christian Ritz, Rikke Pilmann Laursen and Camilla Trab Damsgaard (2017):
Simultaneous inference for multilevel linear mixed models - with an application to a large-scale school meal study: Appl. Statist. 66, Part 2, pp. 295-311.
}
\author{
Christian Ritz
}

\keyword{ misc }

