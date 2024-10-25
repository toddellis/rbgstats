#' bgs_players
#'
#' Extracts player information and flags AI players.
#'
#' @param x List object as read from read_bgs(...)
#'
#' @return Tibble with player information.
#' @export
#'

bgs_players <- function(x) {

  x$players |>
    dplyr::bind_rows() |>
    dplyr::transmute(player_id = id,
                     player_name = stringr::str_trim(name),
                     bgg_user = dplyr::if_else(stringr::str_trim(bggUsername) == "",
                                               NA_character_,
                                               stringr::str_trim(bggUsername)),
                     # flag_ai = stringr::str_extract(metaData, "\"isNpc\":[0-1]") |> ## Alternative means.
                     #   stringr::str_extract("[0-1]{1}") |>
                     #   as.numeric(),
                     flag_ai = as.numeric(stringr::str_detect(player_name, "\U0001f916"))) |>
    dplyr::arrange(player_id)

}
