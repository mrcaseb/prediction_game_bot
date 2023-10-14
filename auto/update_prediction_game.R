ids <- get_ids_to_predict()
prediction_game_model <- readRDS("auto/prediction_game_model.rds")
sebs_preds <- compute_sebs_predictions(ids, pred_model = prediction_game_model)

purrr::pwalk(
  list(
    game_id_to_predict = sebs_preds$game_id,
    home_prediction = sebs_preds$sebs_pred,
    home_team = sebs_preds$team
  ),
  purrr::possibly(submit_prediction, quiet = TRUE)
)
