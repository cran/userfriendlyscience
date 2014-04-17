\name{scaleReliability}
\alias{scaleReliability}
\title{
scale.ic
}
\description{
The scale.ic function computes a number of measures to assess scale reliability and internal consistency.
}
\usage{
scaleReliability(dat=NULL, items = 'all', digits = 2, ci = TRUE,
                 conf.level=.95, silent=FALSE, samples=1000, bootstrapSeed = NULL,
                 omega.psych = FALSE)
}
\arguments{
  \item{dat}{
  A dataframe containing the items in the scale. All variables in this
  dataframe will be used if items = 'all'.
}
  \item{items}{
  If not 'all', this should be a character vector with the names of the
  variables in the dataframe that represent items in the scale.
}
  \item{digits}{
  Number of digits to use in the presentation of the results.
}
  \item{ci}{
  Whether to compute confidence intervals as well (this can take a while!).
}
  \item{conf.level}{
  The confidence of the confidence intervals.
}
  \item{silent}{
  If computing confidence intervals, the user is warned that it may take a while, unless silent=TRUE.
}
  \item{samples}{
  The number of samples ot compute for the bootstrapping of the confidence intervals.
}
  \item{bootstrapSeed}{
  The seed to use for the bootstrapping - setting this seed makes it possible to replicate the exact same intervals,
  which is useful for publications.
}
  \item{omega.psych}{
  Whether to also compute omega using the function 'omega' in the 'psych' package. This method differs from the one used by the MBESS package, and usually (perhaps always) results in higher estimates for omega.
}
}
\details{

This function is basically a wrapper for functions from the psych,
GPArotation, ltm, and MBESS packages that compute measures of
reliability and internal consistency. For backwards compatibility,
in addition to scaleReliability, scale.ic can still be used (but this
is obsolete and therefore strongly discouraged).

}
\value{

An object with the input and several output variables. Most notably:
  \item{input}{Input specified when calling the function}
  \item{intermediate}{Intermediate values and objects computed to get to the final results}
  \item{output}{Values of reliability / internal consistency measures, with as most notable elements:}
  \item{output$dat}{A dataframe with the most important outcomes}
  \item{output$omega}{Point estimate for omega}
  \item{output$glb}{Point estimate for the Greatest Lower Bound}
  \item{output$alpha}{Point estimate for Cronbach's alpha}
  \item{output$omega.ci}{Confidence interval for omega}
  \item{output$alpha.ci}{Confidence interval for Cronbach's alpha}
}
\examples{

\dontrun{
### This will prompt the user to select an SPSS file
scaleReliability();
}

### Load data from simulated dataset testRetestSimData (which
### satisfies essential tau-equivalence).
data(testRetestSimData);

### Select some items in the first measurement
exampleData <- testRetestSimData[2:6];

### Use all items
scaleReliability(dat=exampleData, ci=FALSE);

### Use a selection of two variables
scaleReliability(dat=exampleData, items=c('t0_item2', 't0_item4'), ci=FALSE);

### Note that 'ci = FALSE' can be omitted to compute confidence intervals
### for scales of three items or more.

}
\keyword{ utilities }
\keyword{ univar }