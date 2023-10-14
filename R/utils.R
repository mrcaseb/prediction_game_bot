remove_vig_power <- function(probs, overround_limit = 1e-5, verbose = FALSE, sum_to = 1){
  # see http://dx.doi.org/10.11648/j.ajss.20170506.12
  n <- length(probs)
  pi <- sum(probs) / sum_to # booksum
  error <- abs(pi-1) # overround
  # to check how many iterations were necessary
  if(isTRUE(verbose)) cli::cli_alert_info("overround: {.val {error}}")
  if(error <= overround_limit) return(probs)
  k <- log(n) / log(n / pi)
  new_probs <- probs ^ k
  remove_vig_power(new_probs, overround_limit = overround_limit, verbose = verbose, sum_to = sum_to)
}

implied_prob <- function(..., wager = NULL, tax = FALSE, overround_limit = 1e-5, verbose = FALSE, sum_to = 1) {
  odds <- list(...) |> unlist()
  data.frame("odd" = odds) |>
    dplyr::mutate(
      implied = dplyr::if_else(odd >= 0, 100 / (odd + 100), -odd / (-odd + 100), missing = 0.5),
      prob_vig_removed = remove_vig_power(implied, overround_limit = overround_limit, verbose = verbose, sum_to = sum_to),
      decimal = ifelse(odd >= 0, 1 + odd / 100, 1 - 100 / odd)
    ) |>
    dplyr::select(odd, implied, prob_vig_removed, decimal)
}

fetch_pinnacle_moneylines <- function(){
  matchups <-
    pinnacler::pinnacle_get_raw_matchups() |>
    pinnacler::pinnacle_parse_matchups()

  lines <-
    pinnacler::pinnacle_get_raw_lines() |>
    pinnacler::pinnacle_parse_lines()

  game_lines <- lines |>
    dplyr::filter(!is.na(designation), period == "GAME") |>
    dplyr::filter(type == "moneyline") |>
    dplyr::select(matchup_id, price, designation, period)

  corresponding_matchups <- matchups |>
    dplyr::filter(id %in% game_lines$matchup_id) |>
    dplyr::select(
      start_time, matchup_id = id, designation = participants_alignment,
      name = participants_name
    )

  moneylines <- game_lines |>
    dplyr::left_join(corresponding_matchups, by = c("matchup_id", "designation")) |>
    dplyr::select(-period) |>
    tidyr::pivot_wider(
      names_from = "designation",
      values_from = c(price, start_time, name)
    ) |>
    dplyr::mutate(
      game_data = pinnacler::pinnacle_guess_nflverse_game_id(
        start_time = start_time_home,
        home_team_name = name_home,
        away_team_name = name_away
      )
    ) |>
    tidyr::unnest_wider(game_data) |>
    dplyr::select(
      game_id,
      pinnacle_away_money_line = price_away,
      pinnacle_home_money_line = price_home
    )

  moneylines
}

`%na%` <- function(x, y) ifelse(is.na(x), y, x)
