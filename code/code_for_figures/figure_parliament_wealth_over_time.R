# read wealth and lh
library(readxl); library(tidyverse); library(janitor); library(cowplot)
data <- read_csv2('./data/analysis/dataset_final.csv')

lh_parliaments <- read_csv("./data/pdc/lh_parliaments.csv") %>%
    clean_names() |>
    select(-1) |>
    slice(1, .by=c(parliament, b1_nummer))

lh_parliaments <- left_join(lh_parliaments, data,
                            by = "b1_nummer") |>
    mutate(year_of_vote = year(date)) |> 
    separate(parliament, into=c('from', 'to'), sep ='-', remove = F) |> 
    filter(year_of_vote < to) |> 
    slice(1, .by = c(parliament, b1_nummer))

meanmedw_lh <- lh_parliaments %>%
    group_by(parliament) %>%
    summarize(p50 = median(wealth_defl, na.rm = T),
              p25 = quantile(wealth_defl, 0.25, na.rm = T),
              p75 = quantile(wealth_defl, 0.75, na.rm = T),
              count = sum(!is.na(wealth_defl)))

p1 <- meanmedw_lh %>%
    pivot_longer(c(p25, p50, p75),
                 names_to = "Statistic", 
                 values_to = "Wealth") %>%
    ggplot(aes(x = parliament, 
               y = Wealth, 
               group = Statistic, 
               linetype = Statistic)) + 
    geom_line() + 
    geom_point() +
    theme_minimal() +
    xlab("Parliament") +
    ylab("Wealth (defl. 1900 guilders)") +
        theme(axis.text.x = element_text(angle = 45), 
          text = element_text(size=13),
          legend.position = c(0.9, 0.8),
          #panel.border = element_rect(colour = "black", fill=NA),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")
    ) +
    ggtitle("Distribution of Lower House Wealth over time", subtitle = "Estimated wealth at time of incumbency") + 
    guides(linetype=guide_legend(title="Quantile")) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) #,
                     #  limits=c(0,12e5)) +
    
p2 <- meanmedw_lh2 %>%
    pivot_longer(c(p25, p50, p75),
                 names_to = "Statistic", 
                 values_to = "Wealth") %>%
    ggplot(aes(x = parliament, 
               y = log(Wealth), 
               group = Statistic, 
               linetype = Statistic)) + 
    geom_line() + 
    geom_point() +
    theme_minimal() +
    xlab("Parliament") +
    ylab("Log(Wealth) (deflated 1900 guilders)")  +
    theme(axis.text.x = element_text(angle = 45), 
          text = element_text(size=13),
          #legend.position = c(0.9, 0.8),
          #panel.border = element_rect(colour = "black", fill=NA),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")
    ) +
    ggtitle("Distribution of Lower House Wealth over time") +
    guides(linetype=guide_legend(title="Quantile")) + 
    scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks=c(8,9,10,11, 12, 13)) 

ggplot2::ggsave("./figures/wealth_parl.pdf", p1, width = 8, height = 5)
