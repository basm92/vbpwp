library(tidyverse)
hdng <- read_delim('./data/district/HDNG_v4.txt')
source("./data/x_helpers_step4.R")
# use the district information to match the districts to municipalities
## and given that, find municipality-level variables of interest, and
## aggregate them to the district level

# variables of interest:
## labor force decomposition
## district tax revenue
## share of tax-liability indvs. in distr. (total taxes in each district)
### promillage aangeslagenen inkomstenbelastingen, beroepsbevolking 30+ 
### percentage aangeslagenen vermogensbelastingen, inkomstenbelasting 30+
## no. of strikes in year t-1
## religious decomposition, hervormd, gereformeerd, catholic

# first, prepare the map between district and hdng:
## merge these to get the district -> gemeentenaam -> amco (hdng identifier) match
dm <- read_csv('./data/district/district_municipality_time_table.csv')
key <- read_delim('./data/district/key_elections_hdng_amco.csv')
mapping <- dm |> left_join(key, by = c('gemeentenaam' = 'name_elections'))

data <- read_delim('./data/voting/voting_behavior_b1_nummer_district.csv')

find_district_vars <- function(row){
  
  if (is.na(row$district)) {
    # Return default values or skip processing
    return(data.frame(dis = NA))
  }
  
  year <- row$date |> year()
  distr <- row$district |> strip_roman()
  municipalities <- find_municipalities(distr, year)
  
  # get the required metrics
  years <- c(1889, 1899, 1930)
  hdng_target_year <- years[which.min(abs(year-years))]
  # get the confined hdng database
  limited_hdng <- hdng |> filter(is.element(amco, municipalities))
  industry <- extract_lf_data(limited_hdng, hdng_target_year, 'industry')
  services <- extract_lf_data(limited_hdng, hdng_target_year, 'services')
  agriculture <- extract_lf_data(limited_hdng, hdng_target_year, 'agriculture')
  # get the labor force data
  df <- bind_cols(industry, services, agriculture) |> 
    rowwise() |> 
    mutate(industry = sum_industry/sum(across(everything())),
              services = sum_services/sum(across(everything())),
              agriculture = sum_agriculture/sum(across(everything())))
  # get the total pers. taxes data
  years <- c(1859, 1869, 1870, 1879, 1889)
  hdng_target_year <- years[which.min(abs(year-years))]
  df2 <- extract_tax_data(limited_hdng, hdng_target_year)
  
  # get the inkomens and vermogens tax data
  df3 <- limited_hdng |> 
    filter(str_detect(information, 'vermogensbelastingen')) |> 
    slice_min(year) |>
    summarize(vermogen_tax = sum(value, na.rm=TRUE))
  df4 <- limited_hdng |> 
    filter(str_detect(information, 'inkomstenbelastingen')) |> 
    slice_min(year) |>
    summarize(income_tax = sum(value, na.rm=TRUE))
  
  # get the religious decomposition
  years <- c(1869, 1879, 1899, 1909, 1920, 1930)
  target_year_hdng <- years[which.min(abs(year-years))]
  df5 <- get_religious_decomposition(limited_hdng, target_year_hdng)
  
  bind_cols(df, df2, df3, df4, df5)
}


# now, do it for all observations (tomorrow)
out <- data |>
  rowwise() |> 
  mutate(dis = list(find_district_vars(cur_data())))
  
out <- out |> unnest_wider(dis)

# write to csv
out <- out |>
  select(-dis)

write_csv2(out, './data/voting/vot_beh_b1_district_data.csv')
