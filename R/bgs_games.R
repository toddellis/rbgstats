#' bgs_games
#'
#' Extracts game information and flags expansions.
#'
#' @param x List object as read from read_bgs(...)
#'
#' @return Tibble with player information.
#' @export
#'

bgs_games <- function(x) {

  get_element <- function(x, string) {
      unlist(x)[stringr::str_equal(names(unlist(x)), string)]
    }

  tibble::tibble(game_id = get_element(x$games,
                                       "id"),
                 bgg_id = get_element(x$games,
                                      "bggId"),
                 bg_year = get_element(x$games,
                                       "bggYear"),
                 bg_name = get_element(x$games,
                                       "bggName"),
                 flag_base = get_element(x$games,
                                         "isBaseGame")) |>
    dplyr::mutate(dplyr::across(.cols = c(game_id, bgg_id, bg_year, flag_base),
                                .fns = ~ as.numeric(.x)))
}
