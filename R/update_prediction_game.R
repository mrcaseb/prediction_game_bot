get_ids_to_predict <- function(){
  httr2::request("https://nflgamedata.com/predict/picks.php") |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    xml2::xml_find_all(".//input") |>
    xml2::xml_attr("id")
}

compute_sebs_predictions <- function(game_ids_to_predict){
  predictions <- nflreadr::load_schedules() |>
    dplyr::filter(game_id %in% game_ids_to_predict) |>
    dplyr::select(game_id, home_team, home_moneyline, away_moneyline) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      home_odd = mrcaseb::implied_prob(home_moneyline)$implied,
      away_odd = mrcaseb::implied_prob(away_moneyline)$implied,
      non_vig = list(mrcaseb::implied_prob(dplyr::c_across(c(home_moneyline, away_moneyline)))$prob_vig_removed)
    ) |>
    dplyr::mutate(
      home_non_vig = round(100 * non_vig[[1]], 0),
      away_non_vig = round(100 * non_vig[[2]], 0),
      sebs_pred = dplyr::case_when(
        home_non_vig > 50 ~ home_non_vig + 3,
        home_non_vig < 50 ~ home_non_vig - 3,
        TRUE ~ home_non_vig
      )
    ) |>
    dplyr::select(-non_vig)
  cli::cli_alert_info("We'll use this for predictions:")
  print(predictions)
  predictions
}

submit_prediction <- function(game_id_to_predict, home_prediction, home_team){
  cli::cli_progress_step("Set {.val {game_id_to_predict}} to {.val {home_prediction}}% {.val {home_team}} win.")
  cookie <- paste0(
    "PHPSESSID=", Sys.getenv("PHPSESSID"),
    "; ",
    "nflgamedata_twitter_guid=", Sys.getenv("NFLGAMEDATA_TWITTER_GUID")
  )
  httr2::request("https://nflgamedata.com/predict/set_prediction.php") |>
    httr2::req_method("POST") |>
    httr2::req_url_query(
      game_id = game_id_to_predict,
      prediction = home_prediction
    ) |>
    httr2::req_headers(cookie = cookie) |>
    httr2::req_perform()
  # let's be nice to Lee's server
  Sys.sleep(1)
}

ids <- get_ids_to_predict()
sebs_preds <- compute_sebs_predictions(ids)

purrr::pwalk(
  list(
    game_id_to_predict = sebs_preds$game_id,
    home_prediction = sebs_preds$sebs_pred,
    home_team = sebs_preds$home_team
  ),
  submit_prediction
)
