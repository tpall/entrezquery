#' @title Download files from Entrez GEO repository
#'
#' @description Downloads supplementary or MINiML formatted family files from Entrez GEO repository.
#'
#' @param gsefile GEO file name, starting with Accession number, a character string.
#' @param dest Path to local folder where downloaded files will be stored, a character string. Defaults to current directory.
#' @param verbose Defaults to FALSE.
#'
#' @examples \dontrun{
#' c("GSE100206_family.xml.tgz",
#' "GSE100206_exoNormal_circRNA_RPM.txt.gz") %>%
#' sapply(download_gsefile)
#' }
#'
#' @import crul
#' @importFrom dplyr if_else
#' @importFrom stringr str_extract str_detect
#'
#' @export
#'
download_gsefile <- function(gsefile, dest = ".", verbose = FALSE) {

  # Extract GEO Accession from filename
  Accession <- stringr::str_extract(gsefile, "GSE[0-9]+")

  # Compose ftp link
  ftplink <- file.path("ftp://ftp.ncbi.nlm.nih.gov/geo/series",
                       gsub("\\d{3}$", "nnn", Accession),
                       Accession)

  # Update file paths with subdir names
  filepath <- dplyr::if_else(stringr::str_detect(gsefile, "family.soft.gz$"),
                             file.path("soft", gsefile),
                             file.path("suppl", gsefile))

  # Test if file exist locally
  localfiles <- file.exists(file.path(dest, filepath))

  if (localfiles) stop("Files exist")

  # Test if dest dir is present and create if not
  dir <- dirname(filepath)
  destdir <- file.path(dest, dir)

  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
  }

  # Create connection and get files to dest dir
  x <- HttpClient$new(url = file.path(ftplink, filepath))
  x$get(disk = file.path(dest, filepath), verbose = verbose)
  closeAllConnections()
}
