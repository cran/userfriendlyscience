\name{sysrev}
\alias{sysrev}
\alias{sysrev.read.bibtex}
\alias{sysrev.export}
\title{
sysrev functions
}
\description{
These functions are meant for use in systematic reviews.
}
\usage{
sysrev.read.bibtex(filename, encoding="unknown")
sysrev.export(libraryObject, filename, drop=NULL, keep=NULL,
              sep="\t", row.names = FALSE, ...)
}
\arguments{
  \item{filename}{
  The path and filename to read from or write to
}
  \item{encoding}{
  When reading bibtex files, you might want to specify the encoding (passed on to 'readLines').
}
  \item{libraryObject}{
  An object, created with sysrev.read.bibtex, with references.
}
  \item{drop}{
  Character vector with names of fields to drop when exporting (rest is kept). Cannot be used at the same time as 'keep'.
}
  \item{keep}{
  Character vector with names of fields to keep when exporting (rest is dropped). Cannot be used at the same time as 'drop'.
}
  \item{sep}{
  Separator to use when writing exported file (defaults to tab).
}
  \item{row.names}{
  Whether to write rownames in the export (passed on to write.table).
}
  \item{...}{
  Additional arguments passed on to write.table().
}
}
\details{

sysrev.read.bibtex and sysrev.export and for importing bibtex files, and after manipulation, exporting them to a 'spreadsheet format'. t.to.p, f.to.p, and chisq.to.p make it slilghtly easier to compute p-values from reported statistics.

}
\value{

sysrev.read.bibtex returns a BibTeX Library object (a collection of references).

sysrev.export returns nothing.

t.to.p, f.to.p and chisq.to.p return a p-value.

}
\examples{

\dontrun{
bibTexObject <- sysrev.read.bibtex("BibTex library.bib");
sysrev.export(bibTexObject, "references in table.csv");
}

}
\keyword{ utilities }
