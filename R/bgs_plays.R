#' bgs_plays
#'
#' Extracts full play history
#'
#' @param x List object as read from read_bgs(...)
#' @param idx_solo Possible vector to
#'
#' @return Tibble with full games history.
#' @export
#'

bgs_plays <- function(x,
                      idx_solo = NULL) {

  get_element <- function(x, string) {
    unlist(x)[stringr::str_equal(names(unlist(x)), string)]
  }

  .ls_games <-
    bgs_games(x) |>
    dplyr::select(-flag_base)

  .output <-
    list()

  .pb <-
    utils::txtProgressBar(max = length(x$plays),
                          style = 3)

  for (i in 1:length(x$plays)) {

    .game_id <-
      get_element(x$plays[i],
                  "gameRefId")[[1]] |>
      as.numeric()

    .game_duration <-
      get_element(x$plays[i],
                  "durationMin")[[1]] |>
      as.numeric() / 60

    .game_date <-
      get_element(x$plays[i],
                  "playDate")[[1]] |>
      lubridate::as_datetime()

    .players.score <-
      as.numeric(get_element(x$plays[i],
                             "playerScores.score"))

    .players <-
      tibble::tibble(player_id = get_element(x$plays[i],
                                             "playerScores.playerRefId") |>
                       as.numeric(),
                     player_score = if (length(.players.score) > 0) .players.score else NA_real_,
                     flag_win = as.numeric(as.logical(get_element(x$plays[i],
                                                       "playerScores.winner")))) |>
      dplyr::left_join(bgs_players(x),
                       by = dplyr::join_by(player_id))

    .top_score <-
      .players |>
      dplyr::filter(flag_win == 1) |>
      dplyr::pull(player_score) |>
      mean(na.rm = TRUE)

    .output[[i]] <-
      tibble::tibble(game_id = .game_id) |>
      dplyr::inner_join(.ls_games,
                        by = dplyr::join_by(game_id)) |>
      dplyr::mutate(play_dt = .game_date,
                    duration_hrs = .game_duration,
                    player_count = nrow(.players),
                    winners = .players |> dplyr::filter(flag_win == 1) |> dplyr::pull(player_name) |> paste(collapse = ", "),
                    top_score = if (!is.nan(.top_score)) .top_score else NA_real_) |>
      dplyr::mutate(winners = dplyr::if_else(winners == "",
                                             NA_character_,
                                             winners))

    if (!is.null(idx_solo) &
        is.numeric(idx_solo)) {

      .flag_solo <-
        .players |>
        dplyr::filter(!player_id %in% c(
          idx_solo
        ),
        flag_ai == 0) |>
        nrow() == 0

      .output[[i]] <-
        dplyr::mutate(.output[[i]],
                      flag_solo = as.numeric(.flag_solo))

    }

    utils::setTxtProgressBar(.pb, i)

    if (i == length(x$plays)) {

      .output <-
        dplyr::bind_rows(.output)

    }

  }

  return(.output)


}
