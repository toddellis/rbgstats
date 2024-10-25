#' read_bgs
#'
#' Reads JSON files as exported from the BGStats app.
#'
#' @param x .json file path.
#'
#' @return list object storing all the bells and whistles from a .json BGStats object.
#' @export

read_bgs <- function(x) {

  x |>
    readLines() |>
    paste(collapse = "") |>
    rjson::fromJSON() |>
    ## Because these files are an absolute mess of poor structure.
    suppressWarnings()

}
