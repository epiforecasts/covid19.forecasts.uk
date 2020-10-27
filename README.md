
<!-- README.md is generated from README.Rmd. Please edit that file -->

Short-term forecasts to inform the response to the Covid-19 epidemic in the UK
==============================================================================

This repository contains the data and code for our manuscript:

> Funk S, Abbott S, Atkins B, Baguelin M, Baillie JK, Birrell P, Blake
> J, Bosse NI, Burton J, Carruthers J, Davies NG, De Angelis D, Dyson L,
> Edmunds WJ, Eggo RM, Ferguson NM, Gaythorpe K, Gorsich E,
> Guyver-Fletcher G, Hellewell J, Hill EM, Holmes A, House TA, Jewell C,
> Jit M, Jombart T, Joshi I, Keeling MJ, Kendall E, Knock E, Kucharski
> AJ, Lythgoe KA, Meakin SR, Munday JD, Openshaw PJM, Overton CE, Pagani
> F, Pearson J, Pellis L, Scarabel F, Semple MG, Sherratt K, Tang M,
> Tildesley MJ, Van Leeuwen E, Whittles LK, CMMID COVID-19 Working
> Group, Imperial College COVID-19 Response Team, ISARIC4C
> Investigators. *Short-term forecasts to inform the response to the
> Covid-19 epidemic in the UK*.
> <!-- MedRxiv:, online at <https://doi.org/> -->

### How to download or install

You can download the compendium as a zip from from this URL:
<a href="https://github.com/sbfnk/covid19.forecasts.uk/archive/master.zip" class="uri">https://github.com/sbfnk/covid19.forecasts.uk/archive/master.zip</a>

Or you can install this compendium as an R package,
`covid19.forecatss.uk`, from GitHub with:

    # install.packages("devtools")
    remotes::install_github("sbfnk/covid19.forecasts.uk")

### Included data sets

The package includes a publicly available data set of new
hospitalisations and hospital bed occupancy with Covid-19 by NHS England
region and devolved nation, and deaths by date of death in England.

    data(covid_uk_data)

All the individual model forecasts are included also included.

    data(uk_forecasts)

Lastly, the forecast ensembles described in the paper, and the weights
of the quantile regression average, are included:

    data(ensembles)
    data(qra_weights)

These can be re-created from the individual model forecasts using

    ensembles_and_weights <- generate_ensembles(uk_forecasts, data)
    ensembles <- ensembles_and_weights$ensembles
    qra_weights <- ensembles_and_weights$qra_weights

To extract the best-performing equally-weighted quantiles (between mean
and median) and QRA model, run

    scored_ensembles <- ensembles %>%
      score_forecasts(data) %>%
      mutate(ensemble_type = substr(model, 1, 3))
    ranked_ensembles <- scored_ensembles %>%
      filter(geography %in% nhse_regions) %>%
      rank_forecasts()
    best_ensembles <- ranked_ensembles %>%
      group_by(ensemble_type) %>%
      filter(mean == min(mean)) %>%
      ungroup()

For later use, the package also includes a vector with the name of the
National Health Service (NHS) England regions,

    data(nhse_regions)

### Table and figures

To generate the tables and figures from the paper, we will need the
`dplyr` and `cowplot` packages:

    library("dplyr")
    library("tidyr")
    library("purrr")
    library("cowplot")

To generate Figures 1, run

    england_data <- covid_uk_data %>% 
      filter(geography %in% c(nhse_regions, "England")) %>% 
      group_by(value_type, value_desc, value_date) %>% 
      summarise(value = sum(value), .groups = "drop")
    england_forecasts <- uk_forecasts %>%
      filter(geography == "England")
    p <- plot_data_forecasts(england_forecasts, england_data)
    save_plot("figure1.png", p, base_height = 7)

To generate Figure 2, run

    complete_forecasts <- uk_forecasts %>%
        pivot_wider(names_from = "quantile", values_from = "value") %>%
        nest(data = matches("^[0-9]")) %>%
        mutate(extrapolated = purrr::map(data, estimate_quantiles)) %>%
        unnest(extrapolated) %>%
        select(-data) %>%
        pivot_longer(names_to = "quantile", values_to = "value", matches("^[0-9]")) %>%
        mutate(quantile = as.numeric(quantile))
    scored_forecasts <- complete_forecasts %>%
      score_forecasts(covid_uk_data)
    scored_selected_ensembles <- scored_ensembles %>%
      filter(model %in% best_ensembles$model) %>%
      mutate(model = ensemble_type, .keep = "unused")
    scored_null <- fit_null_model(uk_forecasts, covid_uk_data) %>%
      score_forecasts(covid_uk_data)
    p <- null_model_plot(list(scored_selected_ensembles, scored_forecasts), 
                         covid_uk_data, scored_null)
    save_plot("figure2.png", p, base_asp = 1.7, base_height = 7)

Because data on ICU bed occupancy were not available at the time of
writing the manuscript, this data is not included in the package and the
corresponding panel in Figure 2 missing if using the command above.

To generate Table 1, run

    scores_table(selected_scored_ensembles, file = "table1.pdf", common_creation_dates = TRUE)

To generate Figure 3, run

    selected_england_ensembles <- ensembles %>%
      filter(geography == "England",
             model %in% best_ensembles$model) %>%
      mutate(model = substr(model, 1, 3))
    p <- plot_data_forecasts(selected_england_ensembles, england_data)
    save_plot("figure3.png", p, base_height = 7)

To generate Figure 4, run

    data(qra_weights) 
    plot_weights <- qra_weights %>%
      filter(qra_model %in% best_ensembles$model,
             geography == "England", quantile == 0.5)
    p <- weights_plot(plot_weights)
    save_plot("figure4.png", p, base_asp = 1.7, base_height = 7)

To generate Supplementary Table S1, run

    scores_table(scored_forecasts, file = "si_table1.pdf")

To generate Supplementary Table S2, run

    qra_rank_table(ranked_ensembles %>% filter(ensemble_type == "QRA"),
                   file = "si_table2.pdf")

To generate Supplementary Table S3, run

    ewq_rank_table(ranked_ensembles %>% filter(ensemble_type == "EWQ"),
                   file = "si_table3.pdf")
