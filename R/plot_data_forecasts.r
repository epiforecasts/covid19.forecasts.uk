##' Plot data and forecasts
##'
##' @param forecasts data frame with forecasts
##' @param horizons data frame with data
##' @importFrom dplyr bind_rows filter mutate inner_join rename group_by count between
##' @importFrom tidyr pivot_wider nest unnest
##' @importFrom ggplot2 ggplot geom_bar expand_limits facet_wrap xlab ylab ggtitle aes geom_line scale_y_continuous theme scale_colour_brewer scale_colour_manual scale_fill_brewer scale_fill_manual geom_point geom_linerange
##' @importFrom cowplot theme_cowplot
##' @importFrom scales comma
##' @importFrom RColorBrewer brewer.pal
##' @return ggplot object
##' @author Sebastian Funk
plot_data_forecasts <- function(forecasts, data, horizon = 7, uncertainty = TRUE, scales = "free_y") {

  compare_forecasts <- forecasts %>%
    mutate(quantile = round(quantile, 2)) %>%
    mutate(model = factor(model)) %>%
    pivot_wider(names_from = "quantile", values_from = "value") %>%
    mutate(value = `0.5`,
           min = `0.05`,
           max = `0.95`,
           model = factor(model)) %>%
    filter(geography %in% levels(data$geography)) %>%
    mutate(geography = factor(geography, levels = levels(data$geography)))

  plot_forecasts <- compare_forecasts %>%
    filter(value_date == creation_date + horizon)

  data_present <- data %>%
    inner_join(plot_forecasts %>%
               group_by(value_type, geography) %>%
               mutate(max_date = max(value_date)) %>%
               ungroup() %>%
               select(value_type, geography, max_date) %>%
               distinct(),
               by = c("value_type", "geography")) %>%
    filter(value_date <= max_date) %>%
    unite("geography_value_desc", geography, value_desc, sep = ": ",
          remove = FALSE) %>%
    mutate(nvt = length(unique(value_type)),
           geography_value_desc =
             if_else(nvt == 1, as.character(geography), geography_value_desc)) %>%
    select(-nvt, -value_type)

  forecasts_data <- plot_forecasts %>%
    inner_join(data_present,
               by = c("value_date", "geography", "value_desc")) %>%
    rename(value = value.x, data = value.y) %>%
    group_by(geography_value_desc) %>%
    mutate(max_data = max(data)) %>%
    ungroup() %>%
    mutate(value = if_else(value > 2 * max_data, NA_real_, value),
           max = if_else(max > 2 * max_data, 2 * max_data, max),
           min = if_else(min > 2 * max_data, 2 * max_data, min))

  ncol <- forecasts_data %>%
    .$geography_value_desc %>%
    unique() %>%
    length() %>%
    sqrt() %>%
    ceiling()

  p <- ggplot(forecasts_data, mapping = aes(x = value_date, y = value)) +
    geom_line(data = data_present)

  if (uncertainty) {
    p <- p + geom_linerange(aes(ymin = min, ymax = max, colour = model), alpha = 0.5) +
      geom_point(aes(fill = model), pch = 21, alpha = 0.5)
  } else {
    p <- p + geom_point(aes(fill = model), pch = 21)
  }
  p <- p +
    facet_wrap(~ geography_value_desc, scales = scales, ncol = ncol) +
    xlab("") + ylab("") +
    scale_y_continuous(labels = comma) +
    theme_cowplot() +
    theme(legend.position = "bottom")

  if (length(unique(plot_forecasts$model)) > 9) {
    palette <- brewer.pal(n = length(unique(plot_forecasts$model)) + 1, "Paired")
    ## remove yellow
    palette <- palette[-11]
    p <- p +
      scale_colour_manual("", values = palette) +
      scale_fill_manual("", values = palette)
  } else {
    p <- p +
      scale_colour_brewer("", palette = "Set1") +
      scale_fill_brewer("", palette = "Set1")
  }

  return(p)
}
