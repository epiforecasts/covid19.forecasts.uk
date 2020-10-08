##' Score forecasts according to the weighted interval score and coverage
##'
##' Calculates the weighted interval score for each value/data pair, and sets a
##' flag whether the data is inside (1) or outside the given interval
##' @param forecast a data frame of forecasts
##' @param data a data frame of data with the same \code{value_date},
##' \code{geography}, \code{value_type} and \code{value_desc} entries as
##' \code{forecast}
##' @return a data frame of forecasts with additional columns \code{score} (WIS)
##' and \code{inside} (whether the data are inside or outside the given interval
##' @importFrom dplyr left_join filter select mutate rename
##' @importFrom tidyr spread
##' @importFrom scoringutils interval_score
##' @author Sebastian Funk
score_forecasts <- function(forecast, data) {

  ## Score forecasts
  interval_scored <- forecast %>%
    left_join(data, by = c("value_date", "geography",
                           "value_type", "value_desc")) %>%
    rename(value = value.x, data = value.y) %>%
    mutate(interval = round(2 * abs(quantile - 0.5), 2),
           ## 0.5 centile defined as "upper" here -> will have to duplicate to
           ## "lower" as for the 0% centile 0.5 is the upper and lower
           ## boundary
           boundary = if_else(quantile < 0.5, "lower", "upper"),
           boundary = factor(boundary, levels = c("lower", "upper"))) %>%
    filter(!is.na(data)) %>%
    select(-quantile) %>%
    spread(boundary, value) %>%
    mutate(lower = if_else(is.na(lower), upper, lower)) %>%
    mutate(score = interval_score(data, lower, upper, 100 * interval, weigh = TRUE),
           inside = as.integer(data >= lower & data <= upper))

  return(interval_scored)
}


