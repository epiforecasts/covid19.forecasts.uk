##' UK forecasts in quantile format
##'
##' Contains Covid-19 forecasts made between 31 March and 13 July 2020, by geography (NHS region and UK nation), value_type (code for different targets), value_desc (description of the traget), creation_date (the date on which the forecast was made), value_date (the predicted date), quantile (the predictive quantile) and value (the forecast at the given quantile).
##'
##' @name uk_forecasts
##' @docType data
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
NULL

##' UK ensemble forecasts in quantile format
##'
##' Contains ensembles of Covid-19 forecasts made between 31 March and 13 July 2020, by geography (NHS region and UK nation), value_type (code for different targets), value_desc (description of the traget), creation_date (the date on which the forecast was made), value_date (the predicted date), quantile (the predictive quantile) and value (the forecast at the given quantile); ensembles have been created as equally weighted quantiles (EWQ) and using a quantile regression average (QRA), using the `generate_ensembles` function and encompassing a variety of model variants.
##'
##' @name ensembles
##' @docType data
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
NULL

##' QRA weights
##'
##' Contains the weights of individual models for each quantile as estimated in
##' the different variants (qra_model) of quantile regression averaging (QRA).
##'
##' @name qra_weights
##' @docType data
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
NULL

##' NHSE regions
##'
##' A character vector containing the names of National Health Service England
##' (NHSE) regions.
##'
##' @name nhse_regions
##' @docType data
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
NULL

##' Covid-19 UK data
##'
##' Contains the weights of individual models for each quantile as estimated in
##' the different variants (qra_model) of quantile regression averaging (QRA).
##'
##' @name covid_uk_data
##' @docType data
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
NULL
