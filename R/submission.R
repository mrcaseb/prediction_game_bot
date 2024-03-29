get_ids_to_predict <- function(){
  httr2::request("https://nflgamedata.com/predict/picks.php") |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    xml2::xml_find_all(".//input") |>
    xml2::xml_attr("id")
}

compute_sebs_predictions <- function(game_ids_to_predict, pred_model){
  pinnacle_odds <- purrr::possibly(
    .f = fetch_pinnacle_moneylines,
    otherwise = data.frame(
      game_id = game_ids_to_predict,
      pinnacle_away_money_line = NA,
      pinnacle_home_money_line = NA
    ),
    quiet = FALSE
  )()
  predictions <- nflreadr::load_schedules() |>
    dplyr::filter(game_id %in% game_ids_to_predict) |>
    dplyr::select(game_id, home_team, away_team, home_moneyline, away_moneyline, result) |>
    dplyr::left_join(
      pinnacle_odds, by = "game_id"
    ) |>
    dplyr::mutate(
      home_moneyline = pinnacle_home_money_line %na% home_moneyline,
      away_moneyline = pinnacle_away_money_line %na% away_moneyline,
      home_odd = implied_prob(home_moneyline)$implied,
      away_odd = implied_prob(away_moneyline)$implied,
      pinnacle_home_money_line = NULL,
      pinnacle_away_money_line = NULL
    ) |>
    nflreadr::clean_homeaway() |>
    dplyr::mutate(
      non_vig = implied_prob(team_moneyline)$prob_vig_removed,
      .by = game_id
    ) |>
    dplyr::filter(location == "home") |>
    dplyr::mutate(
      home_non_vig = round(100 * non_vig, 0),
      sebs_pred = predict(pred_model, home_non_vig) |> round(0)
    ) |>
    dplyr::select(-non_vig)
  cli::cli_alert_info("We'll use this for predictions:")
  print(predictions)
  predictions
}

submit_prediction <- function(game_id_to_predict, home_prediction, home_team){
  cli::cli_progress_step(
    msg = "Set {.val {game_id_to_predict}} to {.val {home_prediction}}% {.val {home_team}} win.",
    msg_failed = "Invalid Authorization cookie or Game ID"
  )
  resp <- httr2::request("https://nflgamedata.com/predict/set_prediction.php") |>
    httr2::req_method("POST") |>
    httr2::req_url_query(
      game_id = game_id_to_predict,
      prediction = home_prediction
    ) |>
    httr2::req_headers(cookie = Sys.getenv("SEBS_GOOGLE_PREDICTION_GAME_COOKIE")) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  # let's be nice to Lee's server
  Sys.sleep(1)
  if (!grepl("^SUCCESS", resp)) stop("submission failed")
}
