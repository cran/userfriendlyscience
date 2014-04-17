\name{convert}
\alias{convert}
\alias{convert.r.to.t}
\alias{convert.t.to.r}
\alias{convert.t.to.p}
\alias{convert.f.to.p}
\alias{convert.chisq.to.p}
\title{
convert functions
}
\description{
These are a number of functions to convert statistics and effect size measures from/to each other.
}
\usage{
convert.r.to.t(r, n)
convert.t.to.r(t, n)
convert.t.to.p(t, df)
convert.f.to.p(f, df1, df2, lower.tail=FALSE)
convert.chisq.to.p(chisq, df, lower.tail=FALSE)
}
\arguments{
  \item{r, t, f, chisq}{
  THe value of the relevant statistic
}
  \item{n}{
  The number of observations that the r or t value is based on.
}
  \item{df, df1, df2}{
  The degrees of freedrom for that statistic (for F, the first one is the numerator (i.e. the effect), and the second one the denominator (i.e. the error term).
}
  \item{lower.tail}{
  For the F and chisquare distributions, whether to get the probability of the lower or upper tail.
}
}
\value{

The converted value as a numeric value.

}
\examples{

convert.t.to.r(t=-6.46, n=200);
convert.r.to.t(r=-.41, n=200);

### Compute some p-values
convert.t.to.p(4.2, 197);
convert.chisq.to.p(5.2, 3);
convert.f.to.p(8.93, 3, 644);

}
\keyword{ utilities }
