
#' @title Get UID vector
#'
#' @description Runs entrez search using esearch API and returns UIDs.
#'
#' @param query A GEO query string.
#' @param db Entrez database. For example "gds" == GEO.
#' @param retmax Maximum number of records to return, default is 500.
#' @param ... Further arguments to esearch API.
#'
#' @examples
#' \dontrun{
#' query <- 'expression profiling by high throughput sequencing[DataSet Type]'
#' ids <- get_ids(query, db = 'gds', retmax = 10)
#' }
#'
#' @return A vector of UIDs.
#'
#' @importFrom httr GET content
#' @importFrom xml2 xml_find_first xml_contents xml_double xml_find_all xml_text
#'
#' @export
#'
get_ids <- function(query, db, retmax = 500, ...) {

  ## Construct API url
  esearch <- "esearch.fcgi"
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
  url <- file.path(base_url, esearch)

  ## Run GET request
  qres <- httr::GET(url, query = list(db = db,
                                      term = query,
                                      retmax = retmax, ...))

  ## Extract html contents
  rescont <- httr::content(qres)

  ## Extract UIDs
  nids <- xml2::xml_find_first(rescont, xpath = "//Count")
  nids <- xml2::xml_contents(nids)
  nids <- xml2::xml_double(nids)

  if (nids == 0) {
    stop("Query found no results", call. = FALSE)
  } else {
    message(sprintf("Query found %s results, retrieving max %s", nids, retmax))
  }

  rescont <- xml2::xml_find_all(rescont, xpath = "//Id")
  xml2::xml_text(rescont)
}

#' @title Run GET request on Entrez database with UIDs.
#'
#' @description Runs entrez query using esummary API, returns html response.
#'
#' @param uid Character vector of UIDs.
#' @param db Entrez database, defaults to "gds" == GEO.
#' @param wait The time interval to wait after request, in seconds.
#' @param ... Further arguments to esummary API.
#'
#' @importFrom httr GET
#'
get_qsums <- function(uid, db, wait = 0, ...) {

  ## Construct esummary API url
  esummary <- "esummary.fcgi"
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
  url <- file.path(base_url, esummary)

  ## Create query id string
  uid_string <- paste(uid, collapse = ",")

  ## Run GET request
  r <- httr::GET(url, query = list(db = db, id = uid_string, ...))

  ## Use wait when running multiple requests and hit server limit
  Sys.sleep(wait)
  return(r)
}

#' @title Get entrez document summaries for UIDs
#'
#' @description Returns entrez document summaries for UIDs using esummary API.
#'
#' @inheritParams get_qsums
#' @param chunksize number of uids simultaneously queried, integer. Longer vectors will be split into smaller chunks.
#' @return A list of document summaries of class "xml_document" "xml_node".
#'
#' @importFrom httr content
#' @export
#'
get_docsums <- function(uid, db, chunksize = 100, ...) {

  # Split UIDs into chunks of size max chunksize
  UID_chunks <- split(uid, seq_along(uid) %/% chunksize)

  ## Run query chunkwise
  qsums <- lapply(UID_chunks, get_qsums, db = db, ...)

  ## Extract html content from query results
  lapply(qsums, httr::content)
}

#' @title Extract GEO DocSums into tibble
#'
#' @description Extracts entrez esummary API results from XML into tibble.
#'
#' @param xmldoc A GEO query result contents, list of document summaries of class "xml_document" "xml_node".
#'
#' @return A tibble of GEO document summaries.
#' @importFrom XML xmlParse xmlRoot xmlSApply getChildrenStrings
#' @importFrom dplyr as_data_frame
#'
extract_docsums <- function(xmldoc) {

  ## Parse xml
  children <- xml2::xml_children(xmldoc)
  grandchildren <- purrr::map(children, xml2::xml_children)
  values <- purrr::map(grandchildren, xml2::xml_text)
  attrs <- purrr::map(grandchildren, xml2::xml_attr, attr = "Name")
  names <- purrr::map(grandchildren, xml2::xml_name)
  attrs <- purrr::map2(attrs, names, ~{i <- which(is.na(.x)); .x[i] <- .y[i]; .x})
  values_with_names <- purrr::map2(values, attrs, ~{names(.x) <- .y; .x})
  dataframes <- purrr::map(values_with_names, ~dplyr::as_data_frame(as.list(.x)))
  dplyr::bind_rows(dataframes)
}

#' @title Query entrez database
#'
#' @description Run entrez database query. Returns document summaries for query Ids. Wrapper around entrez esearch and esummary tools.
#'
#' @param query A GEO query string (optional).
#' @param uid Character vector of UIDs (optional).
#' @param db Entrez database. For example "gds" == GEO.
#' @param retmax Maximum number of records to return, default is 500.
#' @param ... Further arguments to esearch and esummary APIs.
#'
#' @examples
#' \dontrun{
#' query <- "expression profiling by high throughput sequencing[DataSet Type]"
#' qres <- entrez_docsums(query = query, db = 'gds', retmax = 10)
#' }
#' @return a data_frame. Returns document summaries for query Ids.
#'
#' @importFrom dplyr bind_rows
#'
#' @export
#'
entrez_docsums <- function(query = NULL, uid = NULL, db = "gds", retmax = 500, ...) {

  if (is.null(query) && is.null(uid)) {
    stop("Either GEO query string or UID must be specified")
  }

  if (!is.null(query) && !is.null(uid)) {
    stop("Only one of GEO query string or UID must be specified")
  }

  # get Ids
  if (!is.null(query)) {
  uid <- get_ids(query, db = db, retmax = retmax, ...)
  }

  # Get query summaries
  sumcont <- get_docsums(uid = uid, db = db, ...)

  # Extract document summaries.
  # Error in previous step may fuck up this step!,
  # if this occurs, please just rerun.
  safely_extract_docsums <- purrr::safely(extract_docsums)
  docsums <- purrr::map(sumcont, safely_extract_docsums)
  docsums <- purrr::map(docsums, "result")

  ## Check if any of the chunks has failed
  fails <- purrr::map_lgl(docsums, is.null)
  if (any(fails)) warning("Some of the document summaries failed to retrieve.")

  ## return good chunks
  good_docsums <- docsums[which(!fails)]
  dplyr::bind_rows(good_docsums)
}

