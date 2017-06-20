\name{tukeytrendfit}
\alias{tukeytrendfit}

\title{
Fit multiple marginal models by updating a given fitted model object with re-scaled dose variable.
}
\description{
Wrapper function to re-fit a given model after different rescalings of a single dose variable. The refitted models are combined into a list that is suitable as input to the multiple marginal model function of package multcomp, mmm.
}

\usage{
tukeytrendfit(fit, dose, 
scaling = c("ari", "ord", "log", "arilog", "highvslow", "treat"),
ctype = NULL, ddf = c("residual", "KR", "PB"), d0shift = 1)
}

\arguments{
  \item{fit}{
A fitted model object, currently, the classes \code{lm}, \code{glm}, \code{lmer}, and \code{nlme} are supported. The left hand side of the models formula must contain a single (numeric) variable that is treated as a dose variable in Tukeys trend test. It may contain further variables that are not changed when refitting the model.
}
  \item{dose}{
A single character string, naming a numeric variable in the models formula. This variable is rescaled acc. to the options in \code{scaling} and the model in \code{fit} is then refitted
with the rescaled \code{dose} variable.
}
  \item{scaling}{
A vector of character strings, naming the options for rescaling the variable specified in \code{dose}: \code{"ari"}: no rescaling, \code{"ord"}: ranks of dose levels, \code{"log"}: log-transformed dose levels, \code{"arilog"}: log-transformnd dose levels, with interpolated dose score for 0, \code{"high.vs.low"}: dose coerced to a factor, and only highest and lowest dose level retained, all others set NA, \code{"treat"}: dose coerced to a factor, all levels retained with the option to apply multiple contrast tests to the treatment levels
}
  \item{ctype}{
optional character string naming a contrast type for multiple comparisons between dose levels, when \code{scaling="treat"}. Options are \code{"Dunnett"}, \code{"William"} etc., see \code{?contrMat} in package \code{multcomp}. Argument \code{ctype} is ignored if \code{scaling} does not involve option \code{"treat"}.
}
  \item{ddf}{
single character string, defining the option for the degree of freedom in inference after model fitting. By default, \code{"residual"} degrees of freedom will be used for all models. \code{"KR"}: For models of class \code{"lmerMod"} (fitted by \code{"lmer"} from package \code{"lme4"}), Kenward-Roger degrees of freedom can be computed (based on methods from package \code{"pbkrtest"}); \code{"PB"}: For models of class \code{"lme"} (fitted by \code{"lme"} from package \code{"nlme"}), containment degrees of freedom as defined by Pinheiro and Bates can be extracted.
}
  \item{d0shift}{
an optional factor, that is multiplied with the interpolated dose score for \code{dose = 0} in option \code{scaling="arilog"}; ignored in all other options for rescaling
}
}

\value{
A list with elements
 \item{mmm}{a list of fitted models, after rescaling the \code{dose} variable}
 \item{mlf}{a list of matrices defining a linear functions of model parameters for each model in \code{mmm}, defining the parameter of interest for inference in function \code{mlf} and \code{glht}}
  \item{df}{a vector of degrees of freedom, one for each model in \code{mmm}}
  and information of the model typ and call of the initial model
}

\references{
Tukey JW, Ciminera JL, Heyse JF (1985). Testing the statistical certainty of a response to increasing doses of a drug. Biometrics 41(1), 295-301.

Pipper CB, Ritz C, Bisgaard H (2012). A versatile methode for confirmatory evaluation of the effects of a covariate in multiple models. JRSSC - Applied Statistics 61, 315-326.
}
\author{
Frank Schaarschmidt and Christian Ritz (providing internal functions to interface objects of class \code{"lmerMod"} and \code{"lme"})
}


\seealso{
Functions \code{glht} and \code{mmm} in package \code{multcomp}; 
\code{\link{tukeytrendformula}} for starting with a model formula and data.frame;
\code{\link{combtt}} for concatenating several objects resulting from this function, e.g. for multiple endpoints or with different covariates; 
\code{\link{asglht}} for direct conversion to a glht-object
}

\examples{

data(litter, package="multcomp")

# variable of interest is 'dose', but
# we may want to include 'number' as a covariate

dl <- litter
dl$dosen <- as.numeric(as.character(dl$dose))

fit <- lm(weight ~ dosen + number, data=dl)

# as it is unclear on which scale of dosage, 
# dose-response-shape is met best, we may try different
# re-scalings of the dose variable, including Dunnett- type multiple contrast test

ttlitter <- tukeytrendfit(fit, dose="dosen", 
 scaling=c("ari", "ord", "log", "treat"), ctype="Dunnett")

summary(asglht(ttlitter))


}

\keyword{htest}
