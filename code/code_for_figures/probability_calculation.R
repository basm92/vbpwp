# probability_calculation

## Two functions, one for scaling the probabilities
## One for giving wealth bonuses and simulating counterfactuals

# And two models - OLS and IV
library(poibin); library(tidyverse); library(latex2exp); library(survival)
data <- read_csv2('./data/analysis/dataset_final.csv') |>
    filter(category == 'fiscal', party_granular != 'neutral')

ihs <- function(x) { log(x + sqrt(x^2 + 1))}

# winsorize the probabilities
winsorize <- function(x){
    x[x< 0.05] <- 0.05
    x[x > 0.95] <- 0.95
    return(x)
}

# calculate the acceptance prob from a vector of probabilities
calculate_acceptance <- function(preds){
    n <- length(preds)
    k <- dplyr::if_else(n %% 2 == 0, n/2 + 1, round(n/2, 0))
    
    return(1 - poibin::ppoibin(kk = k, pp = preds))
}

# An IHS IV Model
iv1 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct | party_granular + law | ihs(wealth_defl) ~ father_politician, cluster=~b1_nummer, data=data)
# A Log IV Model
iv2 <- feols(vote ~ industry + services + catholic_pct + hervormd_pct + 
                 income_tax + vermogen_tax + total_pers_taxes + tenure + 
                 soc_share + soc_dum + vote_share + vote_share_nc + turnout + amount_of_strikes + dsle
             | party_granular + law | log(1+wealth_defl) ~ father_politician, cluster=~b1_nummer, data = data)

## Part 1: Ihs IV Model
# create a list of data.frames with different wealths
check <- map(1:10, .f = ~ data |> mutate(wealth_defl = .x * wealth_defl))
# create the predictions for each of these data.frames
predictions <- map(check, .f = ~ winsorize(predict(iv1, newdata = .x)))
# from each of these probabilities, put them back in the data.frame
new_data <- map2(check, predictions, .f = ~ .x |> mutate(predictions = .y))
# now, in each of these new_data, calculate the probabilities of passage for each law
out <- map2(.x = new_data, .y = 1:10, .f = ~ .x |> 
        group_by(law) |> 
        filter(!is.na(predictions)) |> 
        summarize(acceptance = calculate_acceptance(predictions), alpha = .y))

df <- bind_rows(out)

p1 <- df |> ggplot(aes(x = alpha, y = acceptance, group = law, shape = law)) + 
    geom_line() + 
    geom_point() +
    xlab(TeX("Scaled Wealth $\\alpha W$")) +
    ylab(TeX("P($Vote=1 | \\alpha W, X$)")) +
    scale_shape_discrete(name="Law") + 
    theme_bw() + 
    theme(legend.position = "none") + 
    ggtitle("Panel A: Ihs(Wealth)")

## Part 2: Log IV Model
check <- map(1:10, .f = ~ data |> mutate(wealth_defl = .x * wealth_defl))
# create the predictions for each of these data.frames
predictions <- map(check, .f = ~ winsorize(predict(iv2, newdata = .x)))
# from each of these probabilities, put them back in the data.frame
new_data <- map2(check, predictions, .f = ~ .x |> mutate(predictions = .y))
# now, in each of these new_data, calculate the probabilities of passage for each law
out <- map2(.x = new_data, .y = 1:10, .f = ~ .x |> 
                group_by(law) |> 
                filter(!is.na(predictions)) |> 
                summarize(acceptance = calculate_acceptance(predictions), alpha = .y))

df <- bind_rows(out)

p2 <- df |> ggplot(aes(x = alpha, y = acceptance, group = law, shape = law)) + 
    geom_line() + 
    geom_point() +
    xlab(TeX("Scaled Wealth $\\alpha W$")) +
    ylab(TeX("P($Vote=1 | \\alpha W, X$)")) +
    scale_shape_discrete(name="Law") +
    theme_bw() +
    theme(legend.title = element_text(size=8), legend.text=element_text(size=8)) + 
    ggtitle("Panel B: Log(Wealth)")

fig <- cowplot::plot_grid(p1, p2, ncol = 2, rel_widths = c(0.42, 0.58))
cowplot::save_plot(filename = "./figures/interpretation.pdf", fig, base_width= 10, base_height = 5)
