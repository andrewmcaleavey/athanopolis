#' Clean variable names of a data frame
#'
#' @param .data The data to clean
#' @param unique Logical. Should unique variable names be permitted?
#'
#' @return A data frame with clean names
#' @export
#'
#' @examples
#'  clean_names(
#'    c(
#'    "  a", "a  ",
#'    "a %", "a", "$a", "$$$a", "GDP ($)", "GDP (us$)",
#'    "a (#)", "a & b", "#", "$",
#'    "a_cnt",
#'    "Aa&Bb", "camelCasePhrases",
#'    "AlphaBetaGamma", "Alpha       Beta", "Beta  !!! Gamma",
#'    "a + b", "a - b", "a * b"
#'    )
#'  )
#'
#'  library(dplyr)
#'  df <- read.csv("data-raw/source_data") %>%
#'    clean_names()
#'
#' @source https://drdoane.com/clean-consistent-column-names/
clean_names <- function(.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data

  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)

  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))

  n <- gsub("(^_+|_+$)", "", n)

  n <- gsub("_+", "_", n)

  if (unique) n <- make.unique(n, sep = "_")

  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}

