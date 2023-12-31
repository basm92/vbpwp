## Figures for the tax rates of the income tax and the inheritance tax
library(tidyverse)

### Panel 1: Income tax

#### First, determine the marginal rates in a function
## 1893
tax_amount_1893 <- function(income){
    dplyr::case_when(between(income, 0, 649) ~ 0,
                     between(income, 650, 699) ~ 1.25,
                     between(income, 700, 749) ~ 2,
                     between(income, 750, 799) ~ 2.75,
                     between(income, 800, 849) ~ 3.50,
                     between(income, 850, 899) ~ 4.25,
                     between(income, 900, 949) ~ 5,
                     between(income, 950, 999) ~ 5.75,
                     between(income, 1000, 1049) ~ 6.50,
                     between(income, 1050, 1099) ~ 7.25,
                     between(income, 1100, 1149) ~ 8, 
                     between(income, 1150, 1199) ~ 8.75,
                     between(income, 1200, 1249) ~ 9.50,
                     between(income, 1250, 1299) ~ 10.25,
                     between(income, 1300, 1349) ~ 11,
                     between(income, 1350, 1399) ~ 12, 
                     between(income, 1400, 1449) ~ 13,
                     between(income, 1450, 1499) ~ 14,
                     between(income, 1500, 1999) ~ 15 + 2*((income - 1500)/100),
                     between(income, 2000, 9999) ~ 25 + 3*((income - 2000)/100),
                     between(income, 10000, 19999) ~ 265 + 4*((income - 10000)/100),
                     income >= 20000 ~ 665 + 5*((income - 20000)/100)
    )
}

tax_rate_1893 <- function(income){ tax_amount_1893(income) / income}

## 1914
tax_amount_1914 <- function(income){
    dplyr::case_when(between(income, 0, 649) ~ 0,
                     between(income, 650, 699) ~ 1,
                     between(income, 700, 749) ~ 2,
                     between(income, 750, 799) ~ 3,
                     between(income, 800, 849) ~ 4,
                     between(income, 850, 899) ~ 5,
                     between(income, 900, 949) ~ 6,
                     between(income, 950, 999) ~ 7,
                     between(income, 1000, 1049) ~ 8,
                     between(income, 1050, 1099) ~ 9,
                     between(income, 1100, 1149) ~ 10, 
                     between(income, 1150, 1199) ~ 11,
                     between(income, 1200, 1249) ~ 12,
                     between(income, 1250, 1299) ~ 13,
                     between(income, 1300, 1349) ~ 14,
                     between(income, 1350, 1399) ~ 15, 
                     between(income, 1400, 1449) ~ 16,
                     between(income, 1450, 1499) ~ 17,
                     between(income, 1500, 1549) ~ 18,
                     between(income, 1550, 1599) ~ 19,
                     between(income, 1600, 1999) ~ 20 + 2.5*((income - 1600)/100),
                     between(income, 2000, 4999) ~ 30 + 3.5*((income - 2000)/100),
                     between(income, 5000, 9999) ~ 135 + 4*((income - 5000)/100),
                     between(income, 10000, 14999) ~ 335 + 4.5*((income - 10000)/100),
                     between(income, 15000, 19999) ~ 560 + 5*((income - 15000)/100),
                     between(income, 20000, 24999) ~ 810 + 5.5*((income - 20000)/100),
                     income >= 25000 ~ 1085 + 6*((income - 25000)/100)
    )
}

tax_rate_1914 <- function(income){ tax_amount_1914(income) / income}

#### Then, determine the effective rate in a function

p1 <- data.frame(income = seq(0, 30000, 100)) %>%
    mutate(tax_rate1893 = tax_rate_1893(income),
           tax_rate1914 = tax_rate_1914(income)) %>%
    pivot_longer(c(tax_rate1893, tax_rate1914)) %>%
    mutate(name = if_else(name == "tax_rate1893", "Income Tax 1893", "Income Tax 1914")) %>%
    rename("Law" = "name") %>%
    ggplot(aes(x = income, y = value, group = Law, lty = Law)) + 
    geom_line() + ylab("Effective Tax Rate") + xlab("Yearly Taxable Income") + theme_bw() + 
    theme(legend.position = c(0.22, 0.87)) 

p1
# legend in graph

## p1_alt 
p1_alt <- tribble(~year, ~tariff_2500, ~tariff_5000, ~tariff_25000,
        "01-01-1870", 0.00, 0.00, 0.00,
        "31-12-1893", 0.00, 0.00, 0.00,
        "01-01-1894", tax_rate_1893(2500), tax_rate_1893(5000), tax_rate_1893(25000),
        "31-12-1913", tax_rate_1893(2500), tax_rate_1893(5000), tax_rate_1893(25000),
        "01-01-1914", tax_rate_1914(2500), tax_rate_1914(5000), tax_rate_1914(25000),
        "31-12-1925", tax_rate_1914(2500), tax_rate_1914(5000), tax_rate_1914(25000)
) %>%
    mutate(year = lubridate::dmy(year)) %>%
    pivot_longer(contains("tariff")) %>%
    mutate(name = case_when(name == "tariff_2500" ~ "2,500",
                            name == "tariff_5000" ~ "5,000",
                            name == "tariff_25000" ~ "25,000"
    )) %>%
    mutate(name = factor(name, levels = c("2,500", "5,000", "25,000"))) %>%
    rename("Gross Income" = name) %>%
    ggplot(aes(x = year, y = value, group = `Gross Income`, lty = `Gross Income`)) + theme_bw() + 
    geom_line(size = 1, alpha = 0.5) + ylab("Tax Rate (% Gross Income)") +
    xlab("Year") +
    theme(legend.position = c(0.22, 0.85)) + ylim(0,0.075)

### Inheritance Tax

### Make a graph, x axis = year, y axis = rate, groups = different inheritances (lineal descendants, lineal ascendants, brothers/sisters)
p2 <- tribble(~ year, ~ tariff_linealdescendants_50k, ~ tariff_linealdescendants_150k, ~tariff_linealdescendants_300k,
        "01-01-1870", 0.00, 0.00, 0.00,
        "31-12-1877", 0.00, 0.00, 0.00,
        "01-01-1878", 0.01, 0.01, 0.01,
        "31-12-1910", 0.01, 0.01, 0.01,
        "01-01-1911", 0.0175, 0.02, 0.025,
        "31-12-1916", 0.0175, 0.02, 0.025, 
        "01-01-1917", 0.045, 0.05, 0.055,
        "31-12-1920", 0.045, 0.05, 0.055,
        "01-01-1921", 0.065, 0.07, 0.075,
        "31-12-1925", 0.065, 0.07, 0.075) %>%
    mutate(year = lubridate::dmy(year)) %>%
    pivot_longer(contains("tariff")) %>%
    mutate(name = case_when(name == "tariff_linealdescendants_300k" ~ "300,000",
                            name == "tariff_linealdescendants_150k" ~ "150,000",
                            name == "tariff_linealdescendants_50k" ~ "50,000"
                            )) %>%
    mutate(name = fct_reorder(name, as.numeric(str_extract(name, "\\d{3}")))) %>%
    rename("Net Wealth" = name) %>%
    ggplot(aes(x = year, y = value, group = `Net Wealth`, lty = `Net Wealth`)) + theme_bw() + 
    geom_line(size = 1, alpha = 0.5) + ylab("Tax Rate (% Net Wealth)") +
    xlab("Year") +
    theme(legend.position = c(0.15, 0.85)) + ylim(0, 0.075)


p2 

plot <- cowplot::plot_grid(p1_alt, p2, nrow = 1)

cowplot::save_plot("./figures/tax_rates.pdf", plot, base_height = 5, base_width = 8)
