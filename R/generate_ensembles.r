##' Generate quantile regression ensembles and an equal-weighted ensemble
##'
##' Produces variants of QRA where the weights are optimised per quantile or
##' not, with or without intercept, and normalised to sum up to 1 or not.
##' @param forecasts data frame of forecasts
##' @param data data from of validation data
##' @param all_dates whether ensembles are to be generated for all creation
##' dates; otherwise will just do the latest
##' @param max_history maximum history to consider in QRA
##' @param ... any options to pass to the \link{qra} function.
##' @importFrom tidyr spread nest unnest gather expand_grid
##' @importFrom dplyr mutate select
##' @importFrom furrr future_map
##' @importFrom future plan multisession
##' @return a data frame with the ensembles
##' @author Sebastian Funk
generate_ensembles <- function(forecasts, data, all_dates = TRUE, max_history = 5, ...) {

  data_present <- data %>%
    select(value_date, geography, value_type)

  ## fill quantiles
  complete_forecasts <- forecasts %>%
    spread(quantile, value) %>%
    nest(data = matches("^[0-9]")) %>%
    mutate(extrapolated = map(data, estimate_quantiles)) %>%
    unnest(extrapolated) %>%
    select(-data) %>%
    gather(quantile, value, matches("^[0-9]")) %>%
    mutate(quantile = as.numeric(quantile)) %>%
    inner_join(data_present, by = c("value_date", "geography", "value_type"))

  estimate_weights <- function(x, ...) {
    creation_date <- x$creation_date
    geo_pool <- x$geo_pool
    norm <- x$norm
    per_quantile_weights <- x$per_quantile_weights
    history_weeks <- x$history_weeks

    name <- paste("QRA",
                  if_else(norm == 1, "Norm", "NoNorm"),
                  if_else(geo_pool == 1, "GeoPool", "NoGeoPool"),
                  if_else(per_quantile_weights == 1, "PQ", "NoPQ"),
                  if_else(per_quantile_weights == 2, "Int", "NoInt"),
                  history_weeks)

    message(date(), " ", creation_date, " ", name)
    pool <- "horizon" ## integrate over all horizons
    if (geo_pool) pool <- c(pool, "geography")

    qe <-
      qra(complete_forecasts,
          data, target_date = as.Date(creation_date),
          pool = pool,
          enforce_normalisation = (norm == 1),
          per_quantile_weights = (per_quantile_weights == 1),
          intercept = (per_quantile_weights == 2),
          history = history_weeks, ...)

    if (!is.null(qe$weights)) {
      qe$ensemble <- qe$ensemble %>%
        mutate(model = name)
    }

    x$weights <- list(qe$weights)
    x$ensemble <- list(qe$ensemble)

    return(x)
  }

  creation_dates <- unique(forecasts$creation_date)
  if (!all_dates) creation_dates <- last(creation_dates)

  future::plan(future::multisession)

  ## check all combinations of: normalise, intercept, per quantile wieghts,
  ## number of weeks of history to consider
  qra_weights <- expand_grid(creation_date = creation_dates,
                         geo_pool = c(0, 1),
                         norm = c(0, 1),
                         per_quantile_weights = c(0, 1, 2), ## 2: intercept
                         history_weeks = 1:max_history) %>%
    mutate(id = 1:n()) %>%
    nest(data = c(-id)) %>%
    mutate(qra = furrr::future_map(data, estimate_weights, .progress = TRUE)) %>%
    select(-id, -data) %>%
    unnest(qra)

  ewq_weights_mean <-
    tibble(ensemble =
             list(complete_forecasts %>%
                  group_by_at(vars(-model, -value)) %>%
                  summarise(value = mean(value), .groups = "drop") %>%
                  ungroup() %>%
                  mutate(model = "EWQmean")))

  ewq_weights_median <-
    tibble(ensemble =
             list(complete_forecasts %>%
                  group_by_at(vars(-model, -value)) %>%
                  summarise(value = median(value), .groups = "drop") %>%
                  ungroup() %>%
                  mutate(model = "EWQmedian")))

  weights <- qra_weights %>%
    select(ensemble) %>%
    bind_rows(ewq_weights_mean) %>%
    bind_rows(ewq_weights_median) %>%
    unnest(ensemble)

  return(weights)
}
