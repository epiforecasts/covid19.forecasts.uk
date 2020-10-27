##' Creates a table of forecast scores
##'
##' @param forecasts data frame with scored forecasts
##' @param file filename, if to be saved into a file
##' @param coverage_levels coverage levels for assessing calibration
##' @param horizons forecast horizons to assess
##' @param common_creation_dates if TRUE, ensure only creation dates are assessed
##' @param format table format
##' @param ... any further parameters to pass to \code{kable}
##' @return table
##' @importFrom dplyr mutate filter group_by ungroup summarise rowwise select bind_rows arrange
##' @importFrom tidyr nest gather unite spread
##' @importFrom kableExtra kable kable_styling group_rows add_header_above save_kable
##' @author Sebastian Funk
scores_table <- function(forecasts, file,
                         coverage_levels = c(0.5, 0.9), horizons = c(7, 14),
                         common_creation_dates = FALSE,
                         format = "latex", ...) {

  scores <- list()

  if (common_creation_dates) {
    forecasts <- forecasts %>%
      mutate(nmodels = length(unique(model))) %>%
      ## ensure common creation_dates
      group_by(creation_date, value_desc) %>%
      filter(length(unique(model)) == nmodels) %>%
      ungroup() %>%
      select(-nmodels)
  }

  forecasts <- forecasts %>%
    mutate(horizon = as.integer(value_date - creation_date)) %>%
    filter(horizon %in% horizons) %>%
    group_by(model, value_desc) %>%
    mutate(n = length(unique(creation_date))) %>%
    ungroup() %>%
    filter(n > 2)

  scores[["calibration"]] <- forecasts %>%
    mutate(horizon = as.integer(value_date - creation_date)) %>%
    filter(interval %in% coverage_levels) %>%
    group_by(model, n, value_type, value_desc, horizon, interval) %>%
    summarise(coverage = mean(inside), .groups = "drop") %>%
    ungroup()

  bias_score <- function(x) {
    if (any(x$inside)) {
      min_int <- x %>%
        filter(inside == 1) %>%
        .$interval %>%
        min
    } else {
      min_int <- 1
    }

    median <- x %>%
      filter(interval == 0) %>%
      .$lower

    data <- x %>%
      .$data %>%
      unique

    return(((2 * (data < median) - 1) * min_int))
  }

  scores[["bias"]] <- forecasts %>%
    group_by(model, n, geography, value_date, creation_date, value_type, value_desc,
             horizon) %>%
    nest() %>%
    rowwise() %>%
    mutate(bias = bias_score(data)) %>%
    group_by(model, n, value_type, value_desc, horizon) %>%
    summarise(bias = mean(bias), .groups = "drop") %>%
    ungroup()

  scores[["sharpness"]] <- forecasts %>%
    mutate(sharpness = interval / 2 * (upper - lower)) %>%
    group_by(model, n, geography, value_date, creation_date,
             value_type, value_desc, horizon) %>%
    summarise(sharpness = sum(sharpness), .groups = "drop") %>%
    group_by(model, n, value_type, value_desc, horizon) %>%
    summarise(sharpness = mean(sharpness), .groups = "drop") %>%
    ungroup()

  scores[["wis"]] <- forecasts %>%
    group_by(model, n, geography, value_date, creation_date,
             value_type, value_desc, horizon) %>%
    summarise(n = unique(n), score = mean(score), .groups = "drop") %>%
    ungroup() %>%
    group_by(model, n, value_type, value_desc, horizon) %>%
    summarise(score = mean(score), .groups = "drop") %>%
    ungroup()

  scores[["mae"]] <- forecasts %>%
    filter(!is.na(ae)) %>%
    group_by(model, n, value_type, value_desc, horizon) %>%
    summarise(mae = mean(ae), .groups = "drop") %>%
    ungroup()

  tab <- lapply(scores, function(x) {
    x %>%
      select(-value_type) %>%
      gather(key, value, ncol(.))
  })
  tab[["calibration"]] <- tab[["calibration"]] %>%
    unite("key", key, interval)
  score_tab <- expand.grid(c("n", paste("coverage", coverage_levels, sep = "_"),
                             "bias", "sharpness", "score", "mae"),
                           c("7", "14"))
  levels <- sprintf('%s_%s', score_tab[, 2], score_tab[, 1])

  ptab <- bind_rows(tab) %>%
    unite("key", horizon, key) %>%
    mutate(key = factor(key, levels = levels)) %>%
    mutate(value = signif(value, 2),
           value = round(value, 2)) %>%
    spread(key, value) %>%
    arrange(value_desc, model)

  desc <- unique(ptab$value_desc)
  nmet <- (ncol(ptab) - 3) / 2

  col.names <-
    c("Model", "n", rep(c("Cov 0.5", "Cov 0.9", "Bias", "Sharp", "WIS", "MAE"),
                   times = 2))

  k <- kable(ptab %>% select(-value_desc),
             format,
             col.names = col.names,
             align=c('l|', 'r|', rep('r', nmet), '|r', rep('r', nmet - 1)),
             booktabs = TRUE,
             ...) %>%
    kable_styling(font_size = 5,
                  latex_options=c("hold_position"))

  per_group <- nrow(ptab) / length(desc)

  for (i in seq_along(desc)) {
    k <- k %>%
      group_rows(desc[i], (i - 1) * per_group + 1, i * length(per_group),
                 indent = FALSE)
  }

  k <-
    add_header_above(k, c(" " = 2,
                          "7 days ahead" = nmet, "14 days ahead" = nmet))


  if (!missing(file)) {
    save_kable(k, file, latex_header_includes = c("\\usepackage{fullpage}"))
  } else {
    print(k)
  }
}

