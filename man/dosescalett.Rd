\name{dosescalett}
\alias{dosescale}
\alias{dosescalett}

\title{
Rescaling dose variables acc. to Tukey et al. (1985)
}
\description{
Add rescaled dose variables to a data.frame.
}
\usage{
dosescalett(data, dose, 
scaling = c("ari", "ord", "log", "arilog"),
sep = "", d0shift = 1)
}

\arguments{
  \item{data}{
a data.frame
}
  \item{dose}{
a single charcater string, naming the (numeric) variable in \code{data} that is to be rescaled
}
  \item{scaling}{
A vector of character strings, naming the options for rescaling the variable specified in \code{dose}: \code{"ari"}: no rescaling, \code{"ord"}: ranks of dose levels, \code{"log"}: log-transformed dose levels, \code{"arilog"}: log-transformned dose levels, with interpolated dose score for 0, \code{"high.vs.low"}: dose coerced to a factor, and only highest and lowest dose level retained, all others set NA, \code{"treat"}: dose coerced to a factor, all levels retained with the option to apply multiple contrast tests to the treatment levels
}
  \item{sep}{
an optional separator for the names of the rescaled variables
}
  \item{d0shift}{
an optional factor, that is multiplied with the interpolated dose score for \code{dose = 0} in option \code{scaling="arilog"}; ignored in all other options for rescaling
}
}

\value{
A list containing 
 \item{data}{the input data.frame, with rescaled variables added as colummns}
\item{transnam}{a vector of names of the re-scaled variables}
\item{scaling}{the scaling options, as input}
}

\keyword{ misc }
