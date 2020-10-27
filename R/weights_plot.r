##' Plot model weights from an ensemble
##'
##' @param weights a data frame containing the models and weights
##' @param plot_quantile the quantile to plot the weights for
##' @return a ggplot object
##' @importFrom ggplot2 ggplot geom_bar facet_wrap expand_limits scale_colour_brewer scale_fill_brewer scale_y_continuous theme guides aes guide_legend xlab coord_flip
##' @importFrom cowplot theme_cowplot
##' @importFrom lubridate floor_date wday
##' @author Sebastian Funk
weights_plot <- function(weights) {

  nmodels <- length(unique(weights$model))
  nvalues <- length(unique(weights$value_type))

  ## one set of weights per week - starting on Fridays
  weekly_weights <- weights %>%
    mutate(creation_week = floor_date(creation_date, "week", week_start = 5) + 5,
           creation_day = wday(creation_date, week_start = 5)) %>%
    group_by(creation_week) %>%
    filter(creation_day == max(creation_day)) %>%
    ungroup()

  plot <-
    ggplot(weekly_weights,
           aes(x = creation_week, y = weight, fill = model)) +
    geom_bar(stat = "identity", position = "stack", colour = "black") +
    facet_wrap( ~ value_desc, nrow = round(sqrt(nvalues))) +
    expand_limits(y = c(0, 1)) +
    cowplot::theme_cowplot() +
    ggplot2::scale_colour_brewer("", palette = "Paired") +
    ggplot2::scale_fill_brewer("", palette = "Paired") +
    ggplot2::scale_y_continuous("Weight") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = guide_legend(nrow = ceiling(nmodels / 3), byrow = TRUE)) +
    xlab("Week of forecast")

  return(plot)
}
