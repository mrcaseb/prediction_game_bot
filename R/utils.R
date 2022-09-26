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
