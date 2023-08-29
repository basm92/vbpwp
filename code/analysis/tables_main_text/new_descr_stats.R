# new_descriptive_stats
library(tidyverse); library(modelsummary)
# couple of helpers
# Table with dissent
## Dissent function
dissent <- function(x) {
    mean_value <- mean(x, na.rm = TRUE)
    min(length(x[x < mean_value])/length(x), length(x[x >= mean_value])/length(x))
} 
## median in words function
custom_median <- function(x){
    step1 <- median(x, na.rm = TRUE)
    out <- if_else(step1 == 1, "Pro", if_else(step1 == 0.5, "None", "Con"))
    
    return(out)
}

## accepted or rejected
Status <- function(x){
    step1 <- mean(x, na.rm = TRUE)
    out <- if_else(step1 > 0.5, "Accepted", "Rejected")
    
    return(out)
}

# read in data
dataset <- read_csv2("data/analysis/dataset_final.csv") |>
    filter(category != 'social')

df_dissent <- dataset |> mutate(category = case_when(category ==  "electoral" ~ "Electoral Law", 
                                       category == "fiscal" ~ "Fiscal Law"),
                  year_law = as.character(year(date)),
                  party_simple = case_when(party_simple == "confessional" ~ "Confessional",
                                           party_simple == "liberal" ~ "Liberal",
                                           party_simple == "socialist" ~ "Socialist")) |>
    filter(party_simple != "neutral")


notes <- c("Dissent is defined as the percentage of politicians of each faction having voted against the party line.",
              "Party Line is defined as the median vote per party: 'Pro' if in favor, 'Con' if against, 'None' if no discerible party line (equally split), and '-' if N.A.",
              "Kieswet - Electoral Law, Inkomstenbelasting - Income Tax, Successiewet - Inheritance Tax")

knitr::opts_current$set(label = "descriptivestats_dissent")

modelsummary::datasummary(data = df_dissent, 
                          (`Category` = category)*(`Law` = law)*(`Year` = year_law) ~  N*DropEmpty() + (` `=vote*(`Pct. In Favor`=Mean))*DropEmpty() +
                              (` `=vote*Status)*DropEmpty() + 
                              (vote * (`Party Line` = custom_median) + #*Arguments(fmt="%.0f") + #* Arguments(fmt="%.0f") 
                                   vote * (`Dissent` = dissent))*DropEmpty(empty="-") * party_simple ,
                          sparse_header = TRUE,
                          notes = notes, 
                          title = "Dissent in Voting Behavior in Key Laws",
                          out = "kableExtra",
                          output = "./tables/descriptivestats_dissent.tex") |>
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"), font_size=10) |>
    kableExtra::save_kable("./tables/descriptivestats_dissent.tex")


## Now the "regular" descriptive statistics

nn <- function(x, fmt = 0){
    
    if(length(x[!is.na(x)]) > 0){
        length(x[!is.na(x)]) %>%
            format(digits = fmt)
    } else{
        return("-")
    }
    
}

df_datasum <- dataset |> 
    mutate(catholic = if_else(party_granular == "catholic", 1, 0),
           protestant = if_else(party_granular == "protestant", 1, 0),
           socialist = if_else(party_granular == "socialist", 1, 0),
           liberal = if_else(party_granular == "liberal", 1, 0),
           category = case_when(category == "fiscal" ~ "Fiscal",
                                category == "electoral" ~ "Electoral")
           )

notes <- "All wealth numbers deflated to 1900, and displayed in units of 1000 guilders. Wealth at time vote represents the wealth of politician $i$ at the time of voting for a particular law. Socialist dummy indicates whether a socialist participated in the last election of politician $i$'s district. Seniority indicates the days since a politician became an MP. Father politician indicates whether father of politician $i$ was a politician."
knitr::opts_current$set(label = "descriptives_all")
modelsummary::datasummary(data = df_datasum,
                          formula = 
                              (`Vote`=vote) +
                              (`Wealth (Time Vote)`=wealth_defl/1000) +
                              (`Wealth (Time Vote), Rebalanced`=defw_rb/1000) +
                              (`Catholic`=catholic) +
                              (`Protestant`=protestant) +
                              (`Socialist`=socialist) +
                              (`Liberal`=liberal) +
                              (`% District in Agriculture` =agriculture) + 
                              (`% District in Industry` = industry) + 
                              (`% District in Services` = services) + 
                              (`Share of District Income Tax` = income_tax/100) +
                              (`Share of District Wealth Tax` = vermogen_tax/1000) + 
                              (`District Total Personal Tax Income`=total_pers_taxes/1000) +
                              (`No. of Strikes`=amount_of_strikes) +
                              (`% Catholic`=catholic_pct) +
                              (`% Hervormd`=hervormd_pct) +
                              (`% Gereformeerd`=geref_pct) +
                              (`Vote Share`=vote_share) + 
                              (`Socialist Dummy`=soc_dum) + 
                              (`Socialist Vote Share`=soc_share) + 
                              (`Days Since Last Election`=dsle) + 
                              (`Turnout`=turnout) + 
                              (`Vote Share Nearest Competitor`=vote_share_nc) +
                              (`Seniority`=tenure) +
                              (`Father Politician`=father_politician) +
                              (`Expected Inheritance`=expected_inheritance/1000) ~ category*(Mean+Median+SD+N),
                          title="Descriptive Statistics",
                          out = "kableExtra",
                          output="./tables/descriptivestats_all.tex")  |>
    kableExtra::group_rows("Panel A: Dependent and Main Indep. Vars", 1, 3) |>
    kableExtra::group_rows("Panel B: Party Affiliation", 4, 7) |>
    kableExtra::group_rows("Panel C: District Characteristics", 8, 17) |>
    kableExtra::group_rows("Panel D: Electoral Characteristics", 18, 24) |>
    kableExtra::group_rows("Panel E: IV-Related Variables", 25, 26) |>
    kableExtra::kable_styling(latex_options = c("hold_position","scale_down")) |>
    kableExtra::footnote(general_title = "Note:", 
             footnote_as_chunk = TRUE,
             threeparttable = TRUE,
             general=notes) |>
    kableExtra::save_kable("./tables/descriptivestats_all.tex") 
                          
                          
   