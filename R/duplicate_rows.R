#' Dupliquer le nombre de lignes
#'
#' Utile pour avoir
#'
#' @param df a data.frame
#' @param from variable date
#' @param to variable date
#' @param .data a strin
#'
#' @export
#'
#' @examples
#' X <- data.frame(id = 1:5,
#'   from = Sys.Date() + 1:5,
#'   to = Sys.Date() + c(1,3,5,8,10))
#' duplicate_rows(X, from, to)

duplicate_rows <- function(df, from, to, .new_name = ".date"){
  x_from <- dplyr::pull(df, {{from}})
  x_to <- dplyr::pull(df, {{to}})
  if( !(lubridate::is.Date(x_from) & lubridate::is.Date(x_to))  ){
    stop( sprintf("%s and %s variables must be both dates !",
                  as.character(substitute(from)) ,
                  as.character(substitute(to)) ))
  }

  .dates <- purrr::map2_dfr(
    .x = x_from, .y = x_to,
    .f = function(.x, .y, ...){
      tibble::tibble( !!.new_name := base::seq.Date(.x, .y, 1))})

  dplyr::slice(df, base::rep( 1:base::NROW(df), x_to - x_from + 1)) %>%
    dplyr::bind_cols( .dates ) %>%
    base::return()
}
