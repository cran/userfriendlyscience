\name{normalityAssessment}
\alias{normalityAssessment}
\title{
normalityAssessment
}
\description{
This function can be used to assess whether a variable and the sampling distribution of its mean have an approximately normal distribution.
}
\usage{
normalityAssessment(sampleVector, samples = 5000, digits=3,
                    samplingDistColor = "#2222CC",
                    normalColor = "#00CC00",
                    samplingDistLineSize = 2,
                    normalLineSize = 1,
                    xLabel.sampleDist = NULL,
                    yLabel.sampleDist = NULL,
                    xLabel.samplingDist = NULL,
                    yLabel.samplingDist = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sampleVector}{
  Numeric vector containing the sample data.
}
  \item{samples}{
  Number of samples to use when constructing sampling distribution.
}
  \item{digits}{
  Number of digits to use when printing results.
}
  \item{samplingDistColor}{
  Color to use when drawing the sampling distribution.
}
  \item{normalColor}{
  Color to use when drawing the standard normal curve.
}
  \item{samplingDistLineSize}{
  Size of the line used to draw the sampling distribution.
}
  \item{normalLineSize}{
  Size of the line used to draw the standard normal distribution.
}
  \item{xLabel.sampleDist}{
  Label of x axis of the distribution of the sample.
}
  \item{yLabel.sampleDist}{
  Label of y axis of the distribution of the sample.
}
  \item{xLabel.samplingDist}{
  Label of x axis of the sampling distribution.
}
  \item{yLabel.samplingDist}{
  Label of y axis of the sampling distribution.
}
}
\details{

This function provides a number of normality tests and draws histograms
of the sample data and the sampling distribution of the mean (most
statistical tests assume the latter is normal, rather than the first;
normality of the sample data guarantees normality of the sampling
distribution of the mean, but if the sample size is sufficiently large,
the sampling distribution of the mean is approximately normal even when
the sample data are not normally distributed). Note that for the sampling
distribution, the degrees of freedom are usually so huge that the normality
tests, negligible deviations from normality will already result in
very small p-values.

}
\value{

An object with several results, the most notably of which are:
  \item{plot.sampleDist}{Histogram of sample distribution}
  \item{sw.sampleDist}{Shapiro-Wilk normality test of sample distribution}
  \item{ad.sampleDist}{Anderson-Darling normality test of sample distribution}
  \item{ks.sampleDist}{Kolmogorov-Smirnof normality test of sample distribution}
  \item{kurtosis.sampleDist}{Kurtosis for sample distribution}
  \item{skewness.sampleDist}{Skewness for sample distribution}
  \item{plot.samplingDist}{Histogram of sampling distribution}
  \item{sw.samplingDist}{Shapiro-Wilk normality test of sampling distribution}
  \item{ad.samplingDist}{Anderson-Darling normality test of sampling distribution}
  \item{ks.samplingDist}{Kolmogorov-Smirnof normality test of sampling distribution}
  \item{kurtosis.samplingDist}{Kurtosis for sampling distribution}
  \item{skewness.samplingDist}{Skewness for sampling distribution}
}
\examples{

normalityAssessment(rnorm(35));

}
\keyword{ utilities }
