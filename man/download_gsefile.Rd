% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_gsefile.R
\name{download_gsefile}
\alias{download_gsefile}
\title{Download files from Entrez GEO repository}
\usage{
download_gsefile(gsefile, dest = ".", verbose = FALSE)
}
\arguments{
\item{gsefile}{GEO file name, starting with Accession number, a character string.}

\item{dest}{Path to local folder where downloaded files will be stored, a character string. Defaults to current directory.}

\item{verbose}{Defaults to FALSE.}
}
\description{
Downloads supplementary or MINiML formatted family files from Entrez GEO repository.
}
\examples{
\dontrun{
c("GSE100206_family.xml.tgz",
"GSE100206_exoNormal_circRNA_RPM.txt.gz") \%>\%
sapply(download_gsefile)
}

}
