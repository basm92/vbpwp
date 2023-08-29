# new_results
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category != 'social')

fiscal <- data |> filter(category == 'fiscal') |>
    mutate(expected_inheritance = expected_inheritance/1000000,
           vermogen_tax = vermogen_tax/1000)


# First stage 
fs1 <- feols(ihs(wealth_defl) ~ expected_inheritance | party_granular + law, cluster=~b1_nummer, data = fiscal)
# IV Blank
iv1 <- feols(vote ~ 1 | law + party_granular | ihs(wealth_defl) ~ expected_inheritance, cluster=~b1_nummer, data = fiscal)
# First stage - full controls
fs2 <- feols(ihs(wealth_defl) ~ expected_inheritance + vote_share + turnout + vote_share_nc + soc_share + soc_dum + income_tax + dsle + vermogen_tax +  catholic_pct + hervormd_pct| party_granular + law, cluster=~b1_nummer, data=fiscal)
# IV - full controls
iv2 <- feols(vote ~ vote_share + turnout + vote_share_nc + soc_share + soc_dum + income_tax + dsle + vermogen_tax +  catholic_pct + hervormd_pct | party_granular + law | ihs(wealth_defl) ~ expected_inheritance, cluster=~b1_nummer, data = fiscal)
# First stage - select controls
fs3 <- feols(ihs(wealth_defl) ~ expected_inheritance + turnout +  vote_share_nc + vermogen_tax + industry + services + catholic_pct + dsle | party_granular + law, cluster=~b1_nummer, data = fiscal)
# IV - select controls
iv3 <- feols(vote ~ turnout +  vote_share_nc + vermogen_tax + industry + services + catholic_pct + dsle | party_granular + law | ihs(wealth_defl) ~ expected_inheritance, cluster=~b1_nummer, data=fiscal)


fstats <- map_chr(list(iv1, iv2, iv3), ~ fitstat(.x, 'ivwald1') |> pluck(1) |> pluck(1) |> round(2) |> as.character())
sr <- map_chr(list(iv2, iv3), ~ compute_delta(iv1, .x, 'fit_ihs(wealth_defl)', 0.75) |> round(2) |> as.character())
description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
    "Clustering", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "First-Stage Wald Stat.", "", fstats[1], "", fstats[2], "", fstats[3],
    "Selection Ratio", "-", "-", "-", sr[1], "-", sr[2])

notes <- "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise. Robust standard errors clustered at the politician-level in parentheses. Personal Wealth is defined as ihs(Wealth at Time of Vote), and instrumented by Expected Inheritance."
knitr::opts_current$set(label = "ivresults_fisc_inherit")
ivres <- list(fs1, iv1, fs2, iv2, fs3, iv3)
modelsummary(ivres, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_fisc_inherit.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms") |>
    kableExtra::add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 3))) |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/iv_results_fisc_inherit.tex")

