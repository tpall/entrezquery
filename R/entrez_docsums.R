
# get_GEO_Ids -------------------------------------------------------------

#' @title Get UID vector
#' @description Runs entrez search using esearch API and returns UIDs.
#' @param query A GEO query string
#' @param db Entrez database. For example "gds" == GEO.
#' @param retmax Maximum number of records to return, default is 500.
#' @param ... Further arguments to esearch API.
#' @return A vector of UIDs.
#' @export
#'
get_ids <- function(query, db, retmax = 500, ...){
  esearch <- "esearch.fcgi"
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
  qres <- httr::GET(file.path(base_url, esearch),
                    query = list(db = db,
                                 term = query,
                                 retmax = retmax, ...))
  rescont <- httr::content(qres)

  nids <- xml2::xml_find_first(rescont, xpath = "//Count")
  nids <- xml2::xml_contents(nids)
  nids <- xml2::xml_double(nids)

  if(nids==0){
    stop("Query found no results", call.=FALSE)
  } else {
    message(sprintf("Query found %s results, retrieving max %s.", nids, retmax))
  }

  rescont <- xml2::xml_find_all(rescont, xpath = "//Id")
  xml2::xml_text(rescont)
}

# get_GEO_DocSums ---------------------------------------------------------

#' @title Get entrez document summaries for UIDs.
#' @description Returns entrez document summaries for UIDs using esummary API.
#' @param ids Character vector of UIDs.
#' @param db Entrez database, defaults to "gds" == GEO.
#' @param ... Further arguments to esummary API.
#' @return A list of document summaries of class "xml_document" "xml_node"
#' @export
#'
get_docsums <- function(ids, db, ...){
  esummary <- "esummary.fcgi"
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

  # Split UIDs into chunks of size max 500
  chunkize <-  function(d, chunksize) split(d, ceiling(seq_along(d)/chunksize))
  UID_chunks <- chunkize(ids, 500)

  get_qsums <- function(uid) {
    httr::GET(file.path(base_url, esummary),
              query = list(db = db,
                           id = paste(uid, collapse = ","), ...))
  }

  qsums <- lapply(UID_chunks, get_qsums)
  lapply(qsums, httr::content)
}


# extract_gds_docsums -----------------------------------------------------

#' @title Extract GEO DocSums into tibble
#' @description Extracts entrez esummary API results from XML into tibble.
#' @param xmldoc A GEO query result contents, list of document summaries of class "xml_document" "xml_node"
#' @return A tibble of GEO document summaries
#'
extract_docsums <- function(xmldoc){

  ## Parse xml
  d <- XML::xmlParse(xmldoc)
  d <- XML::xmlRoot(d)

  ## Extract colnames
  DocSum <- d[1]$DocSum
  items <- XML::xmlSApply(DocSum, XML::xmlGetAttr, name = "Name")
  items <- unlist(items)
  items <- c("Id", items)
  items <- unname(items)

  ## Extract data and assign colnames
  d <- XML::xmlSApply(d, function(x) XML::getChildrenStrings(x))
  d <- dplyr::as_data_frame(t(d))
  colnames(d) <- items
  return(d)
}

#' @title Query entrez database.
#' @description Run entrez database query. Wrapper around entrez esearch and esummary tools. Returns document summaries for query Ids.
#' @inheritParams get_ids
#' @param ... Further arguments to esearch and esummary APIs.
#' @examples
#' \dontrun{
#' query <- "expression profiling by high throughput sequencing[DataSet Type]"
#' qres <- entrez_docsums(query, db = 'gds', retmax = 10)
#' }
#' @return a data_frame. Returns document summaries for query Ids.
#' @export
#'
entrez_docsums <- function(query, db = "gds", retmax = 500, ...) {

  ## get Ids
  ids <- get_ids(query, db = db, retmax = retmax, ...)

  ## Get query summaries
  sumcont <- get_docsums(ids = ids, db = db, ...)

  ## Extract document summaries.
  ## Gateway error in previous step may fuck up this step!!!,
  ## if this occurs, please just rerun.
  docsums <- lapply(sumcont, extract_docsums)
  docsums <- dplyr::bind_rows(docsums)
  return(docsums)
}

