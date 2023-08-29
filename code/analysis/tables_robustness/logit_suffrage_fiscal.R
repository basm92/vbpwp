# new_results
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category != 'social', party_granular != 'neutral')
suffrage <- data |> filter(category == 'electoral')
fiscal <- data |> filter(category == 'fiscal')

# logit results - suffrage
library(survival)
model1 <- clogit(formula = vote ~ ihs(wealth_defl) + strata(law) + strata(party_simple), data = suffrage)
model2 <- update(model1, . ~ . + industry + services + catholic_pct + hervormd_pct)
model3 <- update(model2, . ~ . + 
                     income_tax + vermogen_tax + total_pers_taxes + tenure +
                     soc_share + soc_dum +  vote_share + vote_share_nc + turnout + amount_of_strikes + dsle)
model4 <- clogit(formula = vote ~ ihs(wealth_defl) + strata(law) + strata(party_simple), data = fiscal)
model5 <- update(model4, . ~ . + industry + services + catholic_pct + hervormd_pct)
model6 <- update(model5, . ~ . + 
                     income_tax + vermogen_tax + total_pers_taxes + tenure +
                     soc_share + soc_dum +  vote_share + vote_share_nc + turnout + amount_of_strikes + dsle)

modelz <- list(model1, model2, model3, model4, model5, model6)

gma <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "r2.nagelkerke","Nagelkerke $R^2$", 2)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, 
    "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
knitr::opts_current$set(label = "logit_suffrage_fiscal")
notes <- "Standard errors in parentheses. Results for lower house voting outcomes. The dependent variable, Vote, is defined as 1 if the politician is in favor of the reform, 0 otherwise."
modelsummary(modelz, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gma,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/logit_suffrage_fiscal.tex",
             add_rows = description,
             title = "Logit Analysis of Suffrage Extension and Fiscal Legislation") |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::add_header_above(c(" " = 1, "Suffrage" = 3, "Fiscal" = 3)) |>
    kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/logit_suffrage_fiscal.tex")