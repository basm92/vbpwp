# new_results
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category != 'social', party_granular != 'neutral') |> 
    mutate(harnas2 = if_else(year(death_date) - year(date) < 3, 1, 0),
           harnas5 = if_else(year(death_date) - year(date) < 6, 1, 0))

fiscal <- data |> filter(category == 'fiscal')
electoral <- data |> filter(category == 'electoral')

# Two basic regressions for selection ratio
suffrage_basic <-  feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) | party_granular + law, cluster=~b1_nummer, data=electoral)
fiscal_basic <- feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) | party_granular + law, cluster=~b1_nummer, data=fiscal)

# Pooled model
# First, main model split up
m1 <- feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) | party_granular + law, cluster=~b1_nummer, data=data)
# Suffrage (First three panels)
m2 <- feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct | party_granular + law, cluster=~b1_nummer, data=electoral)
m3 <- feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct + 
                income_tax + vermogen_tax + total_pers_taxes + amount_of_strikes | party_granular + law, cluster=~b1_nummer, data=electoral)
m4 <- feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct + 
                income_tax + vermogen_tax + total_pers_taxes + tenure + 
                soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
            | party_granular + law, cluster=~b1_nummer, data = electoral)
# Fiscal (Second three panels)
m5 <- feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct | party_granular + law, cluster=~b1_nummer, data=fiscal)
m6 <- feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct + 
                income_tax + vermogen_tax + total_pers_taxes + amount_of_strikes | party_granular + law, cluster=~b1_nummer, data=fiscal)
m7 <- feols(vote ~ ihs(wealth_defl):harnas2 + harnas2 + ihs(wealth_defl) + industry + services + catholic_pct + hervormd_pct + 
                income_tax + vermogen_tax + total_pers_taxes + tenure + 
                soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
            | party_granular + law, cluster=~b1_nummer, data = fiscal)

# Selection Ratios
sr_el <-  map_chr(list(m2, m3, m4), ~ compute_delta(suffrage_basic, .x, 'ihs(wealth_defl)', 0.75) |> round(2) |> as.character())
sr_fisc <-  map_chr(list(m5, m6, m7), ~ compute_delta(fiscal_basic, .x, 'ihs(wealth_defl)', 0.75) |> round(2) |> as.character())


description <- tribble(
    ~term, ~m1, ~m2, ~m3, ~m4, ~m5, ~m6, ~m7,
    "Clustering", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "Selection Ratio", "-", sr_el[1], sr_el[2], sr_el[3], sr_fisc[1], sr_fisc[2], sr_fisc[3]
)

notes <- "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise. Personal Wealth is defined as ihs(Wealth at Time of Vote). Robust standard errors clustered at the politician-level in parentheses."
knitr::opts_current$set(label = "ols_endogeneity")
ols_endogeneity <- list(m1, m2, m3, m4, m5, m6, m7)
modelsummary(ols_endogeneity, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_endogeneity.tex",
             add_rows = description,
             title = "Endogeneity Test for Suffrage Extension and Fiscal Legislation") |> 
    kableExtra::add_header_above(c(" " = 1, "Pooled" = 1, "Suffrage" = 3, "Fiscal" = 3)) |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::footnote(general_title="Note: ", general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/ols_endogeneity.tex")
