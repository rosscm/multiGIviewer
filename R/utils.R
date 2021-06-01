#' Repeat tags$br
#'
#' @param times the number of br to return
#' @return the number of br specified in times
#' @examples
#' rep_br(5)
#' @importFrom shiny HTML
#'
#' @noRd
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)
not_null <- Negate(is.null)
not_na <- Negate(is.na)


#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#' @importFrom shiny column
#'
#' @noRd
col_12 <- function(...){
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...){
  column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...){
  column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...){
  column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...){
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...){
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...){
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...){
  column(1, ...)
}
