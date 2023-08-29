# new_results
library(tidyverse); library(fixest); library(modelsummary)
source("./code/analysis/compute_delta.R")
source('./code/analysis/regression_settings.R')
coefconvert <- c(coefconvert, 
                 "party_granularliberal" = "Liberal", 
                 "party_granularprotestant" = "Protestant",
                 "party_granularsocialist" = "Socialist"
                 )

data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category != 'social', party_granular != 'neutral') |> 
    mutate(observed = if_else(!is.na(wealth_defl), 1, 0),
           harnas2 = if_else(year(death_date) - year(date) < 3, 1, 0),
           birth_date = year(birth_date))

electoral <- data |>
    filter(category == "electoral")
fiscal <- data |>
    filter(category == "fiscal")

m1 <- feols(observed ~ party_granular | law, cluster=~b1_nummer, data=data)
m2 <- feols(observed ~ party_granular +  harnas2 + vote_share + vote_share_nc + turnout + dsle + soc_dum + soc_share+ birth_date | law, cluster=~b1_nummer, data=data)
m3 <- feols(observed ~ party_granular +  catholic_pct + hervormd_pct + agriculture + services + income_tax + vermogen_tax + total_pers_taxes + tenure + harnas2 + vote_share + vote_share_nc + turnout + dsle + soc_dum + soc_share+ birth_date | law, cluster=~b1_nummer, data=data)
# suffrage
m4 <- feols(observed ~ party_granular +  harnas2 + vote_share + vote_share_nc + turnout + dsle + soc_dum + soc_share+ birth_date | law, cluster=~b1_nummer, data=electoral)
m5 <- feols(observed ~ party_granular +  catholic_pct + hervormd_pct + agriculture + services + income_tax + vermogen_tax + total_pers_taxes + tenure + harnas2 + vote_share + vote_share_nc + turnout + dsle + soc_dum + soc_share+ birth_date | law, cluster=~b1_nummer, data=electoral)
# fiscal
m6 <- feols(observed ~ party_granular +  harnas2 + vote_share + vote_share_nc + turnout + dsle + soc_dum + soc_share+ birth_date | law, cluster=~b1_nummer, data=fiscal)
m7 <- feols(observed ~ party_granular +  catholic_pct + hervormd_pct + agriculture + services + income_tax + vermogen_tax + total_pers_taxes + tenure + harnas2 + vote_share + vote_share_nc + turnout + dsle + soc_dum + soc_share+ birth_date | law, cluster=~b1_nummer, data=fiscal)




description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, 
    "Clustering", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician", "Politician",
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
notes <- "The reference party is Catholic. Standard errors are clustered at the politician level. The dependent variable is 1 if wealth observed, 0 otherwise."
selection <- list(m1, m2, m3, m4, m5, m6, m7)
knitr::opts_current$set(label = "ols_selection")
modelsummary(selection, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_selection.tex",
             add_rows = description,
             title = "Selection Equations for Suffrage Extension and Fiscal Legislation") |>
    kableExtra::add_header_above(c(" " = 1, "Pooled" = 3, "Suffrage" = 2, "Fiscal" = 2)) |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
    kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
    kableExtra::save_kable("./tables/ols_selection.tex")
