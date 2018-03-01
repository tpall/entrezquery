
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
#'
#' @param x a character vector of files at Entrez GEO ftp site.
#'
#' @return a data frame with columns date, size and suppfile.
#' Date is the supplementary file date, size is filesize in bytes.
#' Suppfile is the supplementary file name.
#'
#' @examples \dontrun{
#'
#' ## Download supplementary file name
#' url <-  "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE81nnn/GSE81555/suppl/"
#' r <- httr::GET(url)
#' cont <- httr::content(r, as = "text", encoding = "UTF-8")
#' suppl_filename <- munge_dirlist(cont)
#'
#' ## Download series matrix file name
#' url <-  "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE81nnn/GSE81555/miniml/"
#' r <- httr::GET(url)
#' cont <- httr::content(r, as = "text", encoding = "UTF-8")
#' miniml_filename <- munge_dirlist(cont)
#'
#' }
#'
#' @importFrom readr read_delim
#' @importFrom tidyr separate
#' @importFrom dplyr mutate select
#' @importFrom purrr map_chr map_int
#' @importFrom stringr str_detect
#' @importFrom lubridate dmy
#'
#' @export
#'
munge_dirlist <- function(x) {

  tb <- readr::read_delim(x, "\n", col_names = FALSE)
  tb <- tidyr::separate(tb, "X1", paste0("C", 1:9), "[[:space:]]+")
  tb <- dplyr::mutate(tb, year = purrr::map_chr(C8, this_year),
                      month = purrr::map_int(C6, ~which(stringr::str_detect(month.abb, .x))),
                      date = lubridate::dmy(paste(C7, month, year, sep = "-")))
  tb <- dplyr::select(tb, date, C5, C9)
  colnames(tb) <- c("date", "size", "file")
  tb
}

#' @title Download files from Entrez GEO repository
#'
#' @description Downloads supplementary or MINiML formatted family files from Entrez GEO repository.
#'
#' @param gsefiles GEO file names, starting with Accession number, a character vector.
#' @param dest Path to local folder where downloaded files will be stored, a character string. Defaults to current directory.
#' @param verbose Defaults to FALSE.
#'
#' @examples \dontrun{
#' c("GSE100206_family.xml.tgz",
#' "GSE100206_exoNormal_circRNA_RPM.txt.gz") %>%
#' download_gsefiles()
#' }
#'
#' @import crul
#' @importFrom dplyr if_else
#' @importFrom stringr str_extract str_detect
#'
#' @export
#'
download_gsefiles <- function(gsefiles, dest = ".", verbose = FALSE) {

  # Extract GEO Accession from filenames
  Accession <- unique(stringr::str_extract(gsefiles, "GSE[0-9]+"))

  if (length(Accession) != 1) stop("Supply one Accession")

  # Compose ftp link
  ftplink <- file.path("ftp://ftp.ncbi.nlm.nih.gov/geo/series",
                       gsub("\\d{3}$", "nnn", Accession),
                       Accession)

  # Update file paths with subdir names
  filepath <- dplyr::if_else(stringr::str_detect(gsefiles, "family.soft.gz$"),
                      file.path("soft", gsefiles),
                      file.path("suppl", gsefiles))

  # Test if files exist locally
  localfiles <- file.exists(file.path(dest, filepath))
  filepath <- filepath[!localfiles]

  if (identical(filepath, character(0))) stop("Files exist")

  # Test if dest dir is present and create if not
  dirs <- unique(dirname(filepath))
  destdirs <- file.path(dest, dirs)

  if (!any(dir.exists(destdirs))) {
    sapply(destdirs[!dir.exists(destdirs)], dir.create, recursive = TRUE)
  }

  # Create connection and get files to dest dir
  cc <- Async$new(urls = file.path(ftplink, filepath))
  cc$get(disk = file.path(dest, filepath), verbose = verbose)
}


#' @title Download file list from Entrez GEO repository ftp directory
#'
#' @description Downloads list of supplementary or MINiML formatted family file names from Entrez GEO repository.
#'
#' @param Accession GEO Accession, a charcter string.
#'
#' @import crul
#' @importFrom purrr map set_names
#' @importFrom dplyr bind_rows %>%
#'
#' @examples
#' resp <- get_dirlist("GSE100206")
#' resp
#'
#' @export
#'
get_dirlist <- function(Accession) {
  ftplink <- file.path("ftp://ftp.ncbi.nlm.nih.gov/geo/series",
                       sub("[0-9]{3}$", "nnn", Accession),
                       Accession)
  cc <- Async$new(urls = file.path(ftplink, c("soft/", "suppl/")))
  res <- cc$get()
  purrr::map(res, ~.x$parse("UTF-8")) %>%
    purrr::map(munge_dirlist) %>%
    purrr::set_names(c("soft", "suppl")) %>%
    dplyr::bind_rows(.id = "type")
}

