#' ggbgs
#'
#' Plots boardgame history.
#'
#' @param x List object as read from read_bgs(...)
#' @param y Whether to plot play count (n) or number of hours played (hrs)
#' @param yr_min Minimum year
#' @param yr_max Maximum year
#' @param origin Starting month for x axis.
#' @param dt Note to summarise by month or year
#' @param prop Logical on whether to plot y axis as raw values or percentages.
#' @param pal Colours to use for plotting
#'
#' @return Tibble with player information.
#' @export
#'


ggbgs <- function(x,
                  y = c("hrs", "n"),
                  game = NULL,
                  yr_min = NULL,
                  yr_max = NULL,
                  origin = 1,
                  dt = c("month", "year"),
                  prop = FALSE,
                  pal = c("#313131", "#F28F1C")) {

  .y <-
    match.arg(y)

  .dt <-
    match.arg(dt)

  .df <-
    x |>
    dplyr::mutate(label_dt = lubridate::floor_date(play_dt,
                                                   unit = "months"))
  if (!is.null(yr_min)) {

    if (is.null(yr_max)) {
      yr_max = lubridate::year(Sys.Date()) + 1
    }
    .df <-
      .df |>
      dplyr::filter(label_dt >= lubridate::as_date(glue::glue("{yr_min}-{origin}-01")) &
                      label_dt < lubridate::as_date(glue::glue("{yr_max}-{origin}-01")))
  }
  if (.dt == "year") {

    .df <-
      .df |>
      dplyr::mutate(label_dt = lubridate::quarter(label_dt,
                                                  fiscal_start = origin,
                                                  type = "year.quarter") |>
                      round(digits = 0))

  }

  if (!origin %in% c(1:12)) {
    stop("Set origin month to between 1 and 12.")
  }



  if (!is.null(game)) {

    .df <-
      .df |>
      dplyr::mutate(flag_game = dplyr::if_else(bg_name == game,
                                               bg_name,
                                               "Other games") |>
                      factor(levels = c("Other games",
                                        game)))

  } else {

    .df <-
      .df |>
      dplyr::mutate(flag_game = dplyr::if_else(flag_solo == 1,
                                               "Solo",
                                               "Multiplayer"))
  }

  if (.y == "hrs") {
    .df <-
      .df |>
      dplyr::summarise(y = sum(duration_hrs, na.rm = TRUE),
                       .by = c(label_dt, flag_game))
  } else if (.y == "n") {
    .df <-
      .df |>
      dplyr::summarise(y = dplyr::n(),
                       .by = c(label_dt, flag_game))
  }

  p <-
    .df |>
    ggplot2::ggplot(ggplot2::aes(x = label_dt,
                                 y = y)) +
    ggplot2::scale_fill_manual(values = pal) + ##c("#313131", "#F28F1C") "#6E3A93",
    ggplot2::coord_cartesian(expand = 0) +
    ggplot2::labs(x = NULL,
                  y = if (.y == "hrs") "Hours played" else "Total plays",
                  fill = NULL) +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom",
                   axis.line.y = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_line(colour = "grey40"),
                   axis.ticks = ggplot2::element_line(colour = "grey40"))

  if (.dt == "month") {
    p <-
      p +
      ggplot2::scale_x_datetime(date_breaks = "month",
                                labels = ~ dplyr::if_else(lubridate::month(.x) == 1,
                                                          glue::glue("{lubridate::month(.x, label = TRUE, abbr = TRUE)} '{stringr::str_sub(lubridate::year(.x), 3, 4)}"),
                                                          glue::glue("{lubridate::month(.x, label = TRUE, abbr = TRUE)}")))
  } else {
    p <-
      p +
      ggplot2::scale_x_continuous(breaks = c(yr_min:yr_max),
                                  labels = ~ if (origin == 1) round(.x, digits = 0) else glue::glue("{.x-1}-{origin} - {.x}-{origin-1}"))

  }

  if (prop) {
    p +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::geom_col(ggplot2::aes(fill = flag_game),
                        position = "fill",
                        alpha = 0.7)
  } else {
    p +
      ggplot2::scale_y_continuous(labels = NULL) +
      ggplot2::geom_col(ggplot2::aes(fill = flag_game),
                        alpha = 0.7)
  }

}
