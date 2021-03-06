% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entrez_docsums.R
\name{entrez_docsums}
\alias{entrez_docsums}
\title{Query entrez database}
\usage{
entrez_docsums(query = NULL, uid = NULL, db = "gds", retmax = 500,
  ...)
}
\arguments{
\item{query}{A GEO query string (optional).}

\item{uid}{Character vector of UIDs (optional).}

\item{db}{Entrez database. For example "gds" == GEO.}

\item{retmax}{Maximum number of records to return, default is 500.}

\item{...}{Further arguments to esearch and esummary APIs.}
}
\value{
a data_frame. Returns document summaries for query Ids.
}
\description{
Run entrez database query. Returns document summaries for query Ids. Wrapper around entrez esearch and esummary tools.
}
\examples{
\dontrun{
query <- "expression profiling by high throughput sequencing[DataSet Type]"
qres <- entrez_docsums(query = query, db = 'gds', retmax = 10)
}
}
