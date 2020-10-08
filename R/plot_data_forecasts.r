##' Plot data and forecasts
##'
##' @param forecasts data frame with forecasts
##' @param horizons data frame with data
##' @importFrom dplyr bind_rows filter mutate inner_join rename group_by count between
##' @importFrom tidyr pivot_wider nest unnest
##' @importFrom ggplot2 ggplot geom_bar expand_limits facet_wrap xlab ylab ggtitle aes geom_line scale_y_continuous theme scale_colour_brewer scale_fill_brewer geom_point geom_linerange
##' @importFrom cowplot theme_cowplot
##' @importFrom scales comma
##' @return ggplot object
##' @author Sebastian Funk
plot_data_forecasts <- function(forecasts, data, horizon = 7, uncertainty = TRUE) {

  compare_ensemble <- forecasts %>%
    mutate(quantile = round(quantile, 2)) %>%
    mutate(model = factor(model)) %>%
    pivot_wider(names_from = "quantile", values_from = "value") %>%
    mutate(value = `0.5`,
           min = `0.05`,
           max = `0.95`,
           model = factor(model))

  plot_forecasts <- compare_ensemble %>%
    filter(value_date == creation_date + horizon)

  data_present <- data %>%
    inner_join(plot_forecasts %>%
               group_by(value_type) %>%
               mutate(max_date = max(value_date)) %>%
               ungroup() %>%
               select(value_type, max_date) %>%
               distinct(),
               by = c("value_type")) %>%
    filter(value_date <= max_date)

  forecasts_data <- plot_forecasts %>%
    right_join(data_present,
               by = c("value_date", "value_type", "value_desc")) %>%
    rename(value = value.x, data = value.y) %>%
    mutate(value = if_else(value > 2 * data, NA_real_, value)) %>%
    mutate(max = if_else(max > 2 * data, 2 * data, max)) %>%
    filter(!is.na(model))

  p <- ggplot(forecasts_data, mapping = aes(x = value_date, y = value)) +
    geom_line(data = data_present)

  if (uncertainty) {
    p <- p + geom_point(aes(colour = model), alpha = 0.5) +
      geom_linerange(aes(ymin = min, ymax = max, colour = model), alpha = 0.5)
  } else {
    p <- p + geom_point(aes(fill = model), pch = 21)
  }
  p <- p +
    facet_wrap(~ value_desc, scales = "free_y", nrow = 2) +
    xlab("") + ylab("") +
    scale_y_continuous(labels = comma) +
    theme_cowplot() +
    theme(legend.position = "bottom")

  if (length(unique(plot_forecasts$model)) > 9) {
    p <- p +
      scale_colour_brewer("", palette = "Paired") +
      scale_fill_brewer("", palette = "Paired")
  } else {
    p <- p +
      scale_colour_brewer("", palette = "Set1") +
      scale_fill_brewer("", palette = "Set1")
  }


  return(p)
}
