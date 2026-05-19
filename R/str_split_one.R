#' Split a string
#'
#' @param string A character vector with, at most, one element.
#' @inheritParams stringr::str_split
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' str_split_one(x, pattern = ",")
#' str_split_one(x, pattern = ",", n = 2)
#'
#' y <- "192.168.0.1"
#' str_split_one(y, pattern = stringr::fixed("."))
str_split_one <- function(string, pattern, n = Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}


#' Split a string on commas
#'
#' @param string A string to split
#'
#' @return A character vector
#'
#' @export
str_split_comma <- function(string) {
  stringr::str_split(string, pattern = ",")[[1]]
}


#' Split a string and convert pieces to numbers
#'
#' @param string A string to split
#' @param pattern The pattern to split on
#'
#' @return A numeric vector
#'
#' @export
str_split_nums <- function(string, pattern) {
  pieces <- stringr::str_split(string, pattern = pattern)[[1]]

  nums <- suppressWarnings(as.numeric(pieces))

  if (any(is.na(nums))) {
    warning("NAs in the vector")
  }

  nums
}
