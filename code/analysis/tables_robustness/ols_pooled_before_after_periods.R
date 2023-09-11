# new_results
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category != 'social', party_granular != 'neutral')

data_before <- data |> 
    filter(date < dmy("01-01-1897"))

data_after <- data |> 
    filter(date > dmy("01-01-1897"))


## Before 1897 (Radical Suffrage Extension)
# Baseline
model1 <- feols(vote ~ ihs(wealth_defl) | party_granular + law, cluster=~b1_nummer, data = data_before)
# Basic district characteristics
model2 <- feols(vote ~ ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct | party_granular + law, cluster=~b1_nummer, data = data_before)
# All available controls (district + electoral)
model3 <- feols(vote ~ ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct + 
                    income_tax + vermogen_tax + total_pers_taxes + tenure + 
                    soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
                | party_granular + law, cluster=~b1_nummer, data = data_before)
## After 1897 (Radical Suffrage Extension)
# Baseline
model4 <- feols(vote ~ ihs(wealth_defl) | party_granular + law, cluster=~b1_nummer, data = data_after)
# Basic district characteristics
model5 <- feols(vote ~ ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct | party_granular + law, cluster=~b1_nummer, data = data_after)
# All available controls (district + electoral)
model6 <- feols(vote ~ ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct + 
                    income_tax + vermogen_tax + total_pers_taxes + tenure + 
                    soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
                | party_granular + law, cluster=~b1_nummer, data = data_after)


sr <- map_chr(list(model2, model3), ~ compute_delta(model1, .x, 'ihs(wealth_defl)', 0.75) |> round(2) |> as.character())
sr2 <- map_chr(list(model5, model6), ~ compute_delta(model4, .x, 'ihs(wealth_defl)', 0.75) |> round(2) |> as.character())

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
    "Clustering", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "Selection Ratio", "-", sr[1], sr[2], "-", sr2[1], sr2[2])

knitr::opts_current$set(label = "ols_pooled_before_after")
notes <- "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise. Personal Wealth is defined as ihs(Wealth at Time of Vote). Robust standard errors clustered at the politician-level in parentheses."
ols_pooled <- list(model1, model2, model3, model4, model5, model6)
modelsummary(ols_pooled, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra", 
             output="./tables/ols_pooled_before_after.tex",
             add_rows = description,
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage and Fiscal Legislation") |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::add_header_above(c(" " = 1, "Before 1897" = 3, "After 1897" = 3)) |>
    kableExtra::footnote(general_title="Note: ", general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/ols_pooled_before_after.tex")
