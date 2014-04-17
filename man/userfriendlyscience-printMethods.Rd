\name{userfriendlysciencePrintMethods}
\alias{userfriendlysciencePrintMethods}
\alias{print.dlvPlot}
\alias{print.freq}
\alias{print.meanConfInt}
\alias{print.meanDiff}
\alias{print.meanDiff.multi}
\alias{print.normalityAssessment}
\alias{print.oddsratio}
\alias{print.powerHist}
\alias{print.rMatrix}
\alias{print.scaleDiagnosis}
\alias{print.scaleReliability}
\alias{print.sdConfInt}
\alias{print.testRetestAlpha}
\title{
userfriendlyscience print methods
}
\description{
These methods print the userfriendlyscience objects.
}
\usage{
\method{print}{dlvPlot}(x, ...) 
\method{print}{freq}(x, digits=x$input$digits, nsmall=x$input$nsmall,
transposed=x$input$transposed, ...)
\method{print}{meanConfInt}(x, digits=2, ...)
\method{print}{meanDiff}(x, digits=x$digits,powerDigits=x$digits + 2, ...)
\method{print}{meanDiff.multi}(x, digits=x$digits,
powerDigits=x$digits + 2, ...)
\method{print}{normalityAssessment}(x, ...)
\method{print}{oddsratio}(x, digits=x$input$digits, ...)
\method{print}{powerHist}(x, ...)
\method{print}{rMatrix}(x, digits=x$digits, output=x$output,
env.LaTeX = x$env.LaTeX,
pboxWidthMultiplier = x$pboxWidthMultiplier,
colNames = x$colNames, pval=x$pval, ...)
\method{print}{scaleDiagnosis}(x, ...)
\method{print}{scaleReliability}(x, digits=x$input$digits, ...)
\method{print}{sdConfInt}(x, digits=2, ...)
\method{print}{testRetestAlpha}(x, ...)
}
\arguments{
  \item{x}{
The object to print
}
  \item{digits}{
The number of significant digits to print.
}
  \item{nsmall}{
The minimum number of digits to the right of the decimal point in formatting real/complex numbers in non-scientific formats. Allowed values are 0 <= nsmall <= 20.
}
  \item{transposed}{
Whether the frequency object should be printed transposed (this can be useful for blind users).
}
  \item{powerDigits}{
Number of digits to use when printing the power.
}
  \item{output, env.LaTeX, pboxWidthMultiplier, colNames, pval}{
Output to produce; see ?rMatrix for details.
}
  \item{...}{
Addition arguments that are passed on to the print functions when it's called.
}
}
\value{
These printing methods return nothing, but print stuff.
}
\examples{

print.sdConfInt(sdConfInt(sd=4, n=20));

}
\keyword{ utilities }