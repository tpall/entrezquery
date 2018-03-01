
#' Return current year when clock time is provided from entrez GEO ftp.
#' @description Entrez database shows clock time for files submitted on the current year.
#' @param x character string of clock time e.g. '6:21' or year.
#' @return character string with current year.
this_year <- function(x) {

  if (stringr::str_detect(x, ":")) {
    x <- format(Sys.Date(), "%Y")
  }

  return(x)
}

#' Returns supplementary file names from Entrez GEO
#'
#' @description Replaces GEOquery package getDirlisting function, seems to be faster.
#' @param r object of class 'response'.
#' @return a data frame with columns date, size and suppfile.
#' Date is the supplementary file date, size is filesize in bytes.
#' Suppfile is the supplementary file name.
#'
#' @examples \notrun{
#'
#' ## Download supplementary file name
#' url <-  "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE81nnn/GSE81555//suppl/"
#' r <- httr::GET(url)
#' suppfiles <- get_dirlist(r)
#'
#' ## Download series matrix file name
#' url <-  "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE81nnn/GSE81555//miniml/"
#' r <- httr::GET(url)
#' matrixfile <- get_dirlist(r)
#'
#' }
#'
#' @import dplyr
#' @import httr
#' @import readr
#' @import tidyr
#' @import lubridate
#' @import purrr
#' @import stringr
#'
#' @export
#'
get_dirlist <- function(r){

  if (class(r) != "response") stop("not html response", call. = FALSE)

  cont <- httr::content(r, as = "text", encoding = "UTF-8")
  tb <- readr::read_delim(cont, "\n", col_names = FALSE)
  tb <- tidyr::separate(tb, "X1", paste0("C", 1:9), "[[:space:]]+")
  tb <- dplyr::mutate(tb, year = purrr::map_chr(C8, this_year),
                      month = map_int(C6, ~which(stringr::str_detect(month.abb, .x))),
                      date = lubridate::dmy(paste(C7, month, year, sep = "-")))
  tb <- dplyr::select(tb, date, C5, C9)
  colnames(tb) <- c("date", "size", stringr::str_extract(r$url, "miniml|suppl"))
  tb
}

#' @title Download file from Entrez GEO repository
#'
#' @description Downloads supplementary file or MINiML formatted family file from Entrez GEO repository.
#'
#' @param ftplink FTPLink supplied by document summary table via \{entrez_docsums}, a character string.
#' @param slug Subfolder name where to look for files. suppl specifies supplementary files folder and miniml specifies MINiML series family files. Defaults to suppl.
#' @param filename File name to download, a character string.
#' @param destdir Path to local folder where downloaded files will be stored, a character string.
#' @param verbose Print filename to console, a boolean. Defaults to TRUE.
#'
#' @export
#'
geo_supp_dwnl <- function(ftplink, slug = c("suppl", "miniml"), filename, destdir, verbose = TRUE) {

  if (verbose) message(filename)

  slug <- match.arg(slug)

  dest <- file.path(destdir, filename)

  if (file.exists(dest)) stop("File exists")

  fp <- file.path(ftplink, slug, filename)

  download.file(fp, dest)
}
