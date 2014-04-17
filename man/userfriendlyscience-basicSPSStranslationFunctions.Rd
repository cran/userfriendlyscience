\name{basicSPSStranslationFunctions}
\alias{basicSPSStranslationFunctions}
\alias{getData}
\alias{mediaan}
\alias{modus}
\title{
Basic SPSS translation functions
}
\description{
Basic functons to make working with R easier for SPSS users
}
\usage{
getData(filename=NULL, errorMessage =
        "Specified file does not exist or does not have the extension .sav!",
        use.value.labels=TRUE, to.data.frame=TRUE, ...)
mediaan(vector)
modus(vector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
  It is possible to specify a path and filename to load here. If not specified, the default R file selection dialogue is shown
}
  \item{errorMessage}{
  The error message that is shown if the file does not exist or does not have the right extension.
}
  \item{use.value.labels}{
  See read.spss in package foreign.
  }
  \item{to.data.frame}{
  See read.spss in package foreign.
  }
  \item{...}{
  Additional options, passed on to read.spss in package foreign.
  }
  \item{vector}{
  For mediaan and modus, the vector for which to find the median or mode.
  }
}
\value{

getData returns the imported dataframe, with the filename from which it was read stored in the 'filename' attribute.

mediaan returns the median, or, in the case of a factor where the median is in between two categories, both categories.

modus returns the mode.

}
\examples{

### Open a dialogue to read an SPSS file
### (wrapped in 'if (interactive())' to prevent execution during
###  the testing of the examples when building the package)
if (interactive()) {
  getData();
}

### Get a median and a mode
mediaan(c(1,2,2,3,4,4,5,6,6,6,7));
modus(c(1,2,2,3,4,4,5,6,6,6,7));

}
\keyword{ utilities }
\keyword{ file }
\keyword{ univar }
