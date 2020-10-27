##' Rank forecasts by calculating an average across targets
##'
##' @param x a data frame with scored forecasts
##' @return a data frame with ranked forecasts, and mean/sd of the weighted interval score
##' @importFrom dplyr filter group_by mutate ungroup filter summarise
##' @author Sebastian Funk
rank_forecasts <- function(x) {
  ranked_forecasts <- x %>%
    group_by(value_type, ensemble_type, model) %>%
    mutate(min_date = min(creation_date)) %>%
    group_by(value_type) %>%
    mutate(max_min_date = max(min_date)) %>%
    ungroup() %>%
    filter(creation_date >= max_min_date) %>%
    group_by(ensemble_type, model, value_type, 
             geography, creation_date, value_date) %>%
    summarise(score = mean(score), .groups = "drop") %>%
    group_by(ensemble_type, model) %>%
    summarise(mean = mean(score),
              sd = sd(score),
              .groups = "drop") %>%
    ungroup() %>%
    arrange(mean)

  return(ranked_forecasts)
}
