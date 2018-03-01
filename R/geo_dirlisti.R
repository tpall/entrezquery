
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
#' @param cont a character vector of files at Entrez GEO ftp site.
#' @return a data frame with columns date, size and suppfile.
#' Date is the supplementary file date, size is filesize in bytes.
#' Suppfile is the supplementary file name.
#'
#' @examples \notrun{
#'
#' ## Download supplementary file name
#' url <-  "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE81nnn/GSE81555/suppl/"
#' r <- httr::GET(url)
#' cont <- httr::content(r, as = "text", encoding = "UTF-8")
#' suppfiles <- get_dirlist(cont)
#'
#' ## Download series matrix file name
#' url <-  "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE81nnn/GSE81555/miniml/"
#' r <- httr::GET(url)
#' cont <- httr::content(r, as = "text", encoding = "UTF-8")
#' matrixfile <- get_dirlist(cont)
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
munge_dirlist <- function(cont) {

  tb <- readr::read_delim(cont, "\n", col_names = FALSE)
  tb <- tidyr::separate(tb, "X1", paste0("C", 1:9), "[[:space:]]+")
  tb <- dplyr::mutate(tb, year = purrr::map_chr(C8, this_year),
                      month = purrr::map_int(C6, ~which(stringr::str_detect(month.abb, .x))),
                      date = lubridate::dmy(paste(C7, month, year, sep = "-")))
  tb <- dplyr::select(tb, date, C5, C9)
  colnames(tb) <- c("date", "size", "file")
  tb
}

#' @title Download file from Entrez GEO repository
#'
#' @description Downloads supplementary file or MINiML formatted family file from Entrez GEO repository.
#'
#' @param Accession GEO Accession, a charcter string.
#' @param slug Subfolder name where to look for files. suppl specifies supplementary files folder and miniml specifies MINiML series family files. Defaults to suppl.
#' @param filename File name to download, a character string. Defaults to NULL in which case MINiML series family file(s) will be downloaded.
#' @param destdir Path to local folder where downloaded files will be stored, a character string. Defaults to current directory.
#' @param verbose Print filename to console, a boolean. Defaults to TRUE.
#'
#' @export
#'
geo_supp_dwnl <- function(Accession, filename = NULL, destdir = ".", verbose = TRUE) {

  slug <- "suppl"

  if (is.null(filename)) {
    filename <- paste0(Accession, "_family.xml.tgz")
    slug <- "miniml"
  }

  if (verbose) message(filename)

  destdir <- file.path(destdir, slug)

  if (!dir.exists(destdir)) dir.create(destdir)

  dest <- file.path(destdir, filename)

  if (file.exists(dest)) stop("File exists")

  ftplink <- file.path("ftp://ftp.ncbi.nlm.nih.gov/geo/series",
                       sub("[0-9]{3}$", "nnn", Accession),
                       Accession)

  fp <- file.path(ftplink, slug, filename)

  download.file(fp, dest)
}


library(crul)
library(dplyr)

resp <- get_dirlist("GSE100206")

get_dirlist <- function(Accession) {
  ftplink <- file.path("ftp://ftp.ncbi.nlm.nih.gov/geo/series",
                       sub("[0-9]{3}$", "nnn", Accession),
                       Accession)
  cc <- Async$new(urls = file.path(ftplink, c("miniml/", "suppl/")))
  res <- cc$get()
  purrr::map(res, ~.x$parse("UTF-8")) %>%
    purrr::map(munge_dirlist) %>%
    purrr::set_names(c("miniml", "suppl")) %>%
    dplyr::bind_rows(.id = "type")
}


download_dirlist <- function(gsefiles, dest = ".", verbose = TRUE) {

  Accession <- unique(stringr::str_extract(gsefiles, "GSE[0-9]+"))

  if (length(Accession) != 1) stop("Supply one Accession")

  ftplink <- file.path("ftp://ftp.ncbi.nlm.nih.gov/geo/series",
                       sub("[0-9]{3}$", "nnn", Accession),
                       Accession)

  filepath <- if_else(stringr::str_detect(gsefiles, "family"),
                      file.path("miniml", gsefiles),
                      file.path("suppl", gsefiles))

  localfiles <- file.exists(file.path(dest, filepath))
  filepath <- filepath[!localfiles]

  if (identical(filepath, character(0))) stop("Files exist")

  dirs <- unique(dirname(filepath))
  dest <- file.path(dest, dirs)

  if (!any(dir.exists(dest))) {
    sapply(dest[!dir.exists(dest)], dir.create, recursive = TRUE)
    }

  cc <- Async$new(urls = file.path(ftplink, filepath))
  cc$get(disk = filepath, verbose = verbose)
}
