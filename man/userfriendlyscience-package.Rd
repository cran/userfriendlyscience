\name{userfriendlyscience}
\alias{userfriendlyscience}
\alias{userfriendlyscience-package}
\docType{package}
\title{
Userfriendlyscience
}
\description{
This package contains a number of functions that serve
  two goals: first, make R more accessible to people migrating from
  SPSS by adding a number of functions that behave roughly like their
  SPSS equivalents; and second, make a number of slightly more
  advanced functions more userfriendly to relatively novice users.
}
\details{
\tabular{ll}{
Package: \tab userfriendlyscience\cr
Type: \tab Package\cr
Version: \tab 0.1.1\cr
Date: \tab 2014-02-25\cr
License: \tab GPL (>= 2)\cr
}
The package contains a variety of functions designed to make life easier. These functions are geared towards researchers in psychology.
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
}
\examples{
### Generate two factor vectors
treatment <- factor(c(rep(0, 33), rep(1, 45), rep(0, 63), rep(1, 21)),
                    levels=c(0,1), labels=c("no", "yes"));
survival <- factor(c(rep(0, 78), rep(1, 84)),
                   levels=c(0, 1), labels=c("no", "yes"));

### Compute and display odds ratio
oddsratio(treatment, survival);

### Or present a table
oddsratio(table(treatment, survival));
}
\keyword{ package }