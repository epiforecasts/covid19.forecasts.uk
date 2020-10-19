##' Create null model forecasts
##'
##' Uses a truncated normal distribution fitted to past data points under the
##' assumption of no change. Contains uk-specific code on truncation at the time
##' of data availability 
##'
##' @param forecasts existing forecast data frame
##' @param data data to create null model
##' @return data frame with null model forecasts
##' @importFrom dplyr arrange mutate group_by ungroup filter
##' @importFrom tidyr complete expand_grid nest unnest
##' @importFrom purrr map
##' @author Sebastian Funk
fit_null_model <- function(forecasts, data) {

  creation_dates <- forecasts %>%
    arrange(creation_date) %>%
    .$creation_date %>%
    unique

  use_data <- data %>%
    mutate(truncation = if_else(value_type == "death_inc_line", 4, 0),
           truncation = if_else(value_type == "hospital_inc" &
                                geography == "Wales", 2, truncation),
           truncation = if_else(value_type == "hospital_inc" &
                                geography == "Scotland", 4, truncation),
           truncation = if_else(value_type == "hospital_inc" &
                                geography == "Northern Ireland", 9, truncation)) %>%
    group_by(geography, value_type, value_desc, truncation) %>%
    tidyr::complete(value_date =
                      seq(min(value_date), max(value_date), by = "day"),
                    fill = list(value = 0)) %>%
  ungroup()

  null_model_forecast_df <-
    function(x, creation_date, horizon = 22, truncation = 0)
  {
    quant <-
      null_model_forecast_quantiles(x$value, horizon = horizon,
                                    truncation = truncation)
    df <- 
      bind_rows(replicate(horizon + truncation, quant, simplify = FALSE)) %>%
      mutate(value_date = creation_date - truncation +
               seq(1, horizon + truncation),
             model = "null") %>%
      gather(quantile, value, starts_with("0")) %>%
      mutate(value = round(value),
             quantile = as.numeric(quantile))
  }

  fc <- use_data %>%
    group_by(geography, value_desc, value_type, truncation) %>%
    complete(value_date = seq(min(value_date), max(value_date), by = "day"),
             fill = list(value = 0)) %>%
    ungroup() %>%
    expand_grid(creation_date = creation_dates) %>%
    filter(value_date <= creation_date) %>%
    group_by(creation_date, geography, value_desc, value_type, truncation) %>%
    nest() %>%
    mutate(forecast = map(data, null_model_forecast_df,
                          creation_date = creation_date,
                          truncation = truncation)) %>%
    unnest(forecast) %>%
    ungroup() %>%
    select(model, geography, value_type, value_desc, creation_date, value_date,
           quantile, value)
  
  return(fc)
}

##' Makes a null model forecast based on a vector of values
##'
##' Null model assumes last value stays
##' @param values vector of values
##' @param horizon forecast horizon
##' @param truncation truncation of data series
##' @importFrom msm dtnorm qtnorm
##' @return quantiles of null model predictive distribution
##' @author Sebastian Funk
null_model_forecast_quantiles <- function(values, horizon, truncation = 0)
{
  if (truncation > 0) {
    values <- head(values, -truncation)
  }

  tnorm_unc_fit <- function(x, mean, true) {
    return(-sum(dtnorm(x = true, mean = mean, sd = x, lower = 0, log = TRUE)))
  }

  quantiles <- seq(0.05, 0.95, by = 0.05)
  ts <- tail(values, min(horizon, length(values)))

  if (length(ts) > 1) {
    ## null model
    null_ts <- rep(last(ts), horizon + truncation)

    interval <- c(0, max(abs(diff(ts))))

    if (diff(interval) > 0 ) {
      tnorm_sigma <-
        optimise(tnorm_unc_fit, interval = interval,
                 mean = head(ts, -1),
                 true = tail(ts, -1))

      quant <- round(qtnorm(p = quantiles, mean = last(ts),
                            sd = tnorm_sigma$minimum, lower = 0))
    } else {
      quant <- rep(last(ts), length(quantiles))
    }
  } else {
    quant <- rep(last(ts), length(quantiles))
  }

  names(quant) <- quantiles

  return(quant)
}

