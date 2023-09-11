# new_results
#iv_results_fisc_by_instrument_timing
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category != 'social', party_granular != 'neutral')

fiscal <- data |> filter(category == 'fiscal')

## IV for Wealth at Time Vote
# IV Blank
iv1 <- feols(vote ~ 1 | law + party_granular | ihs(wealth_defl) ~ father_politician, cluster=~b1_nummer, data = fiscal)
# IV - select controls
iv2 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct | party_granular + law | ihs(wealth_defl) ~ father_politician, cluster=~b1_nummer, data=fiscal)
# IV - full controls
iv3 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct + 
                 income_tax + vermogen_tax + total_pers_taxes + tenure + 
                 soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
             | party_granular + law | ihs(wealth_defl) ~ father_politician, cluster=~b1_nummer, data = fiscal)
## IV for Wealth at Time Death
iv4 <- feols(vote ~ 1 | law + party_granular | ihs(w_deflated) ~ father_politician, cluster=~b1_nummer, data = fiscal)
# IV - select controls
iv5 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct | party_granular + law | ihs(w_deflated) ~ father_politician, cluster=~b1_nummer, data=fiscal)
# IV - full controls
iv6 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct + 
                 income_tax + vermogen_tax + total_pers_taxes + tenure + 
                 soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
             | party_granular + law | ihs(w_deflated) ~ father_politician, cluster=~b1_nummer, data = fiscal)

fstats <- map_chr(list(iv1, iv2, iv3, iv4, iv5, iv6), ~ fitstat(.x, 'ivwald1') |> pluck(1) |> pluck(1) |> round(2) |> as.character())
sr <- map_chr(list(iv2, iv3), ~ compute_delta(iv1, .x, 'fit_ihs(wealth_defl)', 0.75) |> round(2) |> as.character())
sr2 <- map_chr(list(iv5, iv6), ~ compute_delta(iv4, .x, 'fit_ihs(w_deflated)', 0.75) |> round(2) |> as.character())
description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
    "Clustering", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "First-Stage Wald Stat.", fstats[1], fstats[2], fstats[3], fstats[4], fstats[5], fstats[6],
    "Selection Ratio", "-", sr[1], sr[2], "-", sr2[1], sr2[2])

notes <- "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise. Robust standard errors clustered at the politician-level in parentheses. Personal Wealth is defined as ihs(Wealth at Time of Vote) and ihs(Wealth at Time of Death) respectively, and instrumented by Father's profession."
knitr::opts_current$set(label = "ivresults_fisc_timing")
ivres <- list(iv1, iv2, iv3, iv4, iv5, iv6)
modelsummary(ivres, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_fisc_timing.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms") |>
    kableExtra::add_header_above(c("Endog. Var.:" = 1, "Wealth at Time Vote" = 3, "Wealth at Death" = 3)) |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/iv_results_fisc_timing.tex")
