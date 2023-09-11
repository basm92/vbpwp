## Results of placebo test with government intervention
# new_results
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category == 'social', party_granular != 'neutral') |>
    mutate(expected_inheritance = expected_inheritance/100000)



# OLS1: single
ols1 <- feols(vote ~ ihs(wealth_defl) | party_granular + law, cluster=~b1_nummer, data = data)

# OLS2: Controls
ols2 <- feols(vote ~ ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct + 
                  income_tax + vermogen_tax + total_pers_taxes + tenure + 
                  soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle | party_granular + law, data = data)
# IV1: First stage 
fs1 <- feols(ihs(wealth_defl) ~ father_politician | party_granular + law, cluster=~b1_nummer, data = data)
# IV1: Blank
iv1 <- feols(vote ~ 1 | law + party_granular | ihs(wealth_defl) ~ father_politician, cluster=~b1_nummer, data = data)
# First stage2: full controls
fs3 <- feols(ihs(wealth_defl) ~ father_politician + industry + services + catholic_pct + hervormd_pct + 
                 income_tax + vermogen_tax + total_pers_taxes + tenure + 
                 soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle | party_granular + law, cluster=~b1_nummer, data = data)
# IV2: full controls
iv3 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct + 
                 income_tax + vermogen_tax + total_pers_taxes + tenure + 
                 soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
             | party_granular + law | ihs(wealth_defl) ~ father_politician, cluster=~b1_nummer, data = data)

fstats <- map_chr(list(iv1, iv3), ~ fitstat(.x, 'ivwald1') |> pluck(1) |> pluck(1) |> round(2) |> as.character())
sr <- map_chr(list(iv3), ~ compute_delta(iv1, .x, 'fit_ihs(wealth_defl)', 0.75) |> round(2) |> as.character())
description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
    "Clustering", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "First-Stage Wald Stat.", "", "", "", fstats[1], "", fstats[2],
    "Selection Ratio", "-", "-", "-", "", "-", sr[1])

notes <- "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise. Robust standard errors clustered at the politician-level in parentheses. Personal Wealth is defined as ihs(Wealth at Time of Vote), and if applicable, instrumented by Father Politician."
knitr::opts_current$set(label = "govtint_results_fp")
ivres <- list(ols1, ols2, fs1, iv1, fs3, iv3)
modelsummary(ivres, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/govtint_results_fp.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Gov't Intervention") |>
    kableExtra::add_header_above(c(" " = 1, "OLS" = 2, "Personal Wealth"=1, "Vote"=1, "Personal Wealth"=1, "Vote"=1)) |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/govtint_results_fp.tex")

