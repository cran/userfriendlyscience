\name{descr}
\alias{descr}
\alias{descriptives}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
descr (or descriptives)
}
\description{
This function provides a number of descriptives about your data, similar to
what SPSS's DESCRIPTIVES (often called with DESCR) does.
}
\usage{
descr(x, digits = 4, errorOnFactor = FALSE,
      include = c("central tendency", "spread", "range",
                  "distribution shape", "sample size"),
      maxModes = 1,
      t = FALSE, conf.level=.95,
      quantileType = 2);
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The vector for which to return descriptives.
}
  \item{digits}{
  The number of digits to round the results to when showing them.
}
  \item{errorOnFactor}{
  Whether to show an error when the vector is a factor, or just show the
  frequencies instead.
}
  \item{include}{
  Which elements to include when showing the results.
}
  \item{maxModes}{
  Maximum number of modes to display: displays "multi" if more than this number of modes if found.
}
  \item{t}{
  Whether to transpose the dataframes when printing them to the screen (this is easier for users relying on screen readers).
}
  \item{conf.level}{
  Confidence of confidence interval around the mean in the central tendency measures.
}
  \item{quantileType}{
  The type of quantiles to be used to compute the interquartile range (IQR). See \code{\link{quantile}} for more information.
}
}
\details{
Note that R (of course) has many similar functions, such as \code{\link{summary}},
\code{\link{describe}} in the excellent \code{\link{psych}} package.

The Hartigans' Dip Test may be unfamiliar to users; it is a measure of uni- vs. multidimensionality, computed by \code{\link{dip.test}} from the \code{\link{dip.test}} package. Depending on the sample size, values over .025 can be seen as mildly indicative of multimodality, while values over .05 probably warrant closer inspection (the p-value can be obtained using \code{\link{dip.test}}; also see Table 1 of Hartigan & Hartigan (1985) for an indication as to critical values).
}
\value{
A list of dataframes with the requested values. 
}
\references{
Hartigan, J. A.; Hartigan, P. M. The Dip Test of Unimodality. Ann. Statist. 13 (1985), no. 1, 70--84. doi:10.1214/aos/1176346577. http://projecteuclid.org/euclid.aos/1176346577.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\seealso{
\code{\link{summary}}, \code{\link{describe}}
}
\examples{
descr(mtcars$mpg);
}
\keyword{ univariate }
