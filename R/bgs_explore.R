#' bgs_explore
#'
#' Quick way to explore underlying BGStats data in all its chaotic energy.
#'
#' @param x List object as read from read_bgs(...)
#'
#' @return Tibble with variable names and their total occurrences.
#' @export
#'

bgs_explore <- function(x) {

  x |>
    unlist() |>
    names() |>
    tibble::as_tibble_col(column_name = "variable") |>
    dplyr::count(variable,
                 name = "count")

}
