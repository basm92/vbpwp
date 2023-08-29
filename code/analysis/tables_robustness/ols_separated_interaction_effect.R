# new_results
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category != 'social', party_granular != 'neutral')

fiscal <- data |> filter(category == 'fiscal')
electoral <- data |> filter(category == 'electoral')

# First, main model pooled
m1 <- feols(vote ~ ihs(wealth_defl):party_granular | party_granular + law, cluster=~b1_nummer, data=data)
# Suffrage (First three panels)
m2 <- feols(vote ~ ihs(wealth_defl):party_granular + industry + services + catholic_pct + hervormd_pct | party_granular + law, cluster=~b1_nummer, data=electoral)
m3 <- feols(vote ~ ihs(wealth_defl):party_granular + industry + services + catholic_pct + hervormd_pct + 
                income_tax + vermogen_tax + total_pers_taxes + amount_of_strikes | party_granular + law, cluster=~b1_nummer, data=electoral)
m4 <- feols(vote ~ ihs(wealth_defl):party_granular + industry + services + catholic_pct + hervormd_pct + 
                income_tax + vermogen_tax + total_pers_taxes + tenure + 
                soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
            | party_granular + law, cluster=~b1_nummer, data = electoral)
# Fiscal (Second three panels)
m5 <- feols(vote ~ ihs(wealth_defl):party_granular + industry + services + catholic_pct + hervormd_pct | party_granular + law, cluster=~b1_nummer, data=fiscal)
m6 <- feols(vote ~ ihs(wealth_defl):party_granular + industry + services + catholic_pct + hervormd_pct + 
                income_tax + vermogen_tax + total_pers_taxes + amount_of_strikes | party_granular + law, cluster=~b1_nummer, data=fiscal)
m7 <- feols(vote ~ ihs(wealth_defl):party_granular + industry + services + catholic_pct + hervormd_pct + 
                income_tax + vermogen_tax + total_pers_taxes + tenure + 
                soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
            | party_granular + law, cluster=~b1_nummer, data = fiscal)

description <- tribble(
    ~term, ~m1, ~m2, ~m3, ~m4, ~m5, ~m6, ~m7,
    "Clustering", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

knitr::opts_current$set(label = "ols_separated_interaction_effect")
notes <- "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise. Personal Wealth is defined as ihs(Wealth at Time of Vote). Robust standard errors clustered at the politician-level in parentheses."
ols_separated <- list(m1, m2, m3, m4, m5, m6, m7)
modelsummary(ols_separated, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_separated_interaction_effect.tex",
             add_rows = description,
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage and Fiscal Legislation") |>
    kableExtra::add_header_above(c(" " = 1, "Pooled" = 1, "Suffrage Extension" = 3, "Fiscal Legislation" = 3))|>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::footnote(general_title="Note:", general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/ols_separated_interaction_effect.tex")
