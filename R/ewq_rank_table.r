##' Formats a table of ranked QRA ensembles
##'
##' @param ensembles data frame with ranked ensembles
##' @param file filename, if to be saved into a file
##' @param format table format
##' @param ... any further parameters to pass to \code{kable}
##' @return table
##' @importFrom dplyr mutate select
##' @importFrom kableExtra kable kable_styling save_kable
##' @author Sebastian Funk
ewq_rank_table <- function(ranked_ensembles, file,
                       format = "latex", ...) {
  df <- ranked_ensembles %>%
    mutate(combination = substr(model, 4, nchar(model)),
           mean = round(mean, 1),
           sd = round(sd, 1)) %>%
    select(combination, mean, sd)

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

