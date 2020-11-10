##' Formats a table of ranked QRA ensembles
##'
##' @param ensembles data frame with ranked ensembles
##' @param file filename, if to be saved into a file
##' @param format table format
##' @param ... any further parameters to pass to \code{kable}
##' @return table
##' @importFrom dplyr mutate arrange recode_factor
##' @importFrom tidyr separate
##' @importFrom kableExtra kable kable_styling save_kable
##' @author Sebastian Funk
qra_rank_table <- function(ranked_ensembles, file,
                       format = "latex", ...) {
  df <- ranked_ensembles %>%
    separate(model, c("ensemble_type", "sum to 1",
                      "by quantile", "intercept", "history (weeks)")) %>%
    mutate(`sum to 1` = recode_factor(`sum to 1`, NoNorm = "no", Norm = "yes"),
           `by quantile` =
             recode_factor(`by quantile`, NoPQ = "no", PQ = "yes"),
           intercept = recode_factor(intercept, Int = "yes", NoInt = "no"),
           mean = round(mean, 1),
           sd = round(sd, 1)) %>%
    select(`history (weeks)`, `sum to 1`,
           intercept, `by quantile`,
           mean, sd) %>%
    arrange(mean)

  k <- kable(df,
             format,
             align=c('rrrrrrr'),
             booktabs = TRUE) %>%
    kable_styling(font_size = 4,
                  latex_options=c("hold_position"))

  if (!missing(file)) {
    save_kable(k, file, latex_header_includes = c("\\usepackage{fullpage}"))
  } else {
    print(k)
  }
}

