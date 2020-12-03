##' Plot performance against the null model
##'
##' @param forecasts a list of data frames with scored forecasts
##' @param data data frame with data
##' @param null data frame with scored null model
##' @param horizons forecast horizons to consider
##' @importFrom dplyr bind_rows mutate filter group_by ungroup summarise select
##' @importFrom tidyr gather spread
##' @importFrom ggplot2 aes geom_vline geom_hline geom_point geom_linerange scale_x_discrete scale_colour_brewer xlab ylab facet_wrap expand_limits coord_flip theme position_dodge
##' @importFrom cowplot theme_cowplot
##' @importFrom binom binom.confint
##' @return ggplot object
##' @author Sebastian Funk
null_model_plot <- function(forecasts, null, horizons = c(7, 14)) {

  model_names <- lapply(forecasts, function(x) {
    c(rev(sort(as.character(unique(x$model)))), "")
  })

  model_names <- unlist(model_names)
  model_names <- model_names[-length(model_names)]

  comp_scores <- bind_rows(forecasts) %>%
    bind_rows(null) %>%
    mutate(horizon = as.integer(value_date - creation_date)) %>%
    filter(horizon %in% horizons)

  comp_sc <- list()
  for (cmodel in setdiff(model_names, "")) {
    comp_sc[[cmodel]] <- comp_scores %>%
      filter(model %in% c(cmodel, "null")) %>%
      mutate(model = factor(model)) %>%
      group_by(geography, value_desc, creation_date,
               value_date, horizon, interval) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n == 2) %>%
      group_by(geography, value_desc, creation_date,
               value_date, horizon, model) %>%
      summarise(WIS = mean(score), MAE = mean(ae, na.rm = TRUE), .groups = "drop") %>%
      mutate(model = if_else(model == cmodel, "submitted", as.character(model))) %>%
      gather(metric, score, WIS, MAE) %>%
      spread(model, score) %>%
      mutate(model = cmodel)
  }

  better_null <- comp_sc %>%
    bind_rows() %>%
    filter(!is.na(submitted)) %>%
    group_by(model, value_desc, horizon, metric) %>%
    summarise(x = sum(submitted < null), n = n(), .groups = "drop")

  better_null_confint <-
    binom.confint(better_null$x, better_null$n, method = "exact")

  better_null <- better_null %>%
    cbind(better_null_confint %>% select(mean, lower, upper)) %>%
    mutate(horizon = paste(horizon, "days")) %>%
    filter(n > 10) %>%
    mutate(model =
             factor(model,
                    levels = intersect(model_names, c("", unique(model)))))

  p <- ggplot(better_null %>% filter(metric == "WIS"),
              aes(x = model, y = mean, ymin = lower, ymax = upper,
                  colour =
                    factor(horizon, levels = c("7 days", "14 days")))) +
    geom_vline(xintercept = "", linetype = "dashed") +
    geom_hline(yintercept = 0.5, linetype = "dotted") +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_linerange(position = position_dodge(width = 0.5)) +
    scale_x_discrete(drop = FALSE) +
    scale_colour_brewer("Forecast horizon", palette = "Dark2") +
    xlab("") + ylab("Proportion of forecasts improved over null model") +
    facet_wrap( ~ value_desc, nrow = 2) +
    expand_limits(y = c(0, 1)) +
    cowplot::theme_cowplot() +
    coord_flip() +
    theme(legend.position = "bottom")

  return(p)
}
