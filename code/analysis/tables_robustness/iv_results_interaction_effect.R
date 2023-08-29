# new_results
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category != 'social', party_granular != 'neutral', party_granular != 'socialist')

electoral <- data |> filter(category == 'electoral')
fiscal <- data |> filter(category == 'fiscal')
# IV Blank
iv1 <- feols(vote ~ 1 | law + party_granular | ihs(wealth_defl):party_granular ~ father_politician:party_granular, cluster=~b1_nummer, data = electoral)
# IV - select controls
iv2 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct | party_granular + law | ihs(wealth_defl):party_granular ~ father_politician:party_granular, cluster=~b1_nummer, data = electoral)
# IV - full controls
iv3 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct + 
                 income_tax + vermogen_tax + total_pers_taxes + tenure + 
                 soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
             | party_granular + law | ihs(wealth_defl):party_granular ~ father_politician:party_granular, cluster=~b1_nummer, data = electoral)
# IV Blank
iv4 <- feols(vote ~ 1 | law + party_granular | ihs(wealth_defl):party_granular ~ father_politician:party_granular, cluster=~b1_nummer, data = fiscal)
# IV - select controls
iv5 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct | party_granular + law | ihs(wealth_defl):party_granular ~ father_politician:party_granular, cluster=~b1_nummer, data=fiscal)
# IV - full controls
iv6 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct + 
                 income_tax + vermogen_tax + total_pers_taxes + tenure + 
                 soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
             | party_granular + law | ihs(wealth_defl):party_granular ~ father_politician:party_granular, cluster=~b1_nummer, data = fiscal)



fstatscath <- map_chr(list(iv1, iv2, iv3, iv4, iv5, iv6), ~ fitstat(.x, 'ivwald1') |> pluck(1) |> pluck(1) |> round(2) |> as.character())
fstatslib <- map_chr(list(iv1, iv2, iv3, iv4, iv5, iv6), ~ fitstat(.x, 'ivwald1') |> pluck(2) |> pluck(1) |> round(2) |> as.character())
fstatsprot <- map_chr(list(iv1, iv2, iv3, iv4, iv5, iv6), ~ fitstat(.x, 'ivwald1') |> pluck(3) |> pluck(1) |> round(2) |> as.character())

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
    "Clustering", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "First-Stage Wald Stat. (Cath)", fstatscath[1], fstatscath[2], fstatscath[3], fstatscath[4], fstatscath[5], fstatscath[6],
    "First-Stage Wald Stat. (Lib.)",fstatslib[1], fstatslib[2], fstatslib[3], fstatslib[4], fstatslib[5], fstatslib[6],
    "First-Stage Wald Stat. (Prot.)", fstatsprot[1], fstatsprot[2], fstatsprot[3], fstatsprot[4], fstatsprot[5], fstatsprot[6])

notes <- "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise. Robust standard errors clustered at the politician-level in parentheses. Personal Wealth is defined as ihs(Wealth at Time of Vote), and its interaction with party is instrumented by the interaction Father's profession interacted by party."
knitr::opts_current$set(label = "ivresults_interaction_effect")
ivres <- list(iv1, iv2, iv3, iv4, iv5, iv6)
modelsummary(ivres, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_interaction_effect.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms") |>
    kableExtra::add_header_above(c(" " = 1, "Suffrage" = 3, "Fiscal" = 3)) |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/iv_results_interaction_effect.tex")
