# add strikes
library(tidyverse); library(Hmisc) #warning, this package overrides dplyr's summarize
source('./code/x_helpers_step4.R')
data <- read_csv2('./data/voting/vot_beh_wealth_instr.csv')
# strikes url
#download.file("https://datasets.iisg.amsterdam/api/access/datafile/9841",
#              destfile = "./data/strikes/strikes.mdb")

data_strikes <- Hmisc::mdb.get('./data/strikes/strikes.mdb')
strikes <- data_strikes$tblActies
places <- data_strikes$tblPlaats |>
  mutate(ID = as.integer(ID)) |> 
  select(ID, PLAATS, Amsterdam.Code)
place_key <- data_strikes$tblActies_Plaats

# filter strikes within reasonable years
strikes_db <- strikes |> 
  as_tibble() |>
  left_join(place_key, by = c("ID" = "ActieID")) |> 
  filter(between(JAAR, 1850, 1950)) |> 
  left_join(places, by = c("PlaatsID" = "ID"))

#find an overview of number of strikes per year
strikes_db <- strikes_db |> 
  group_by(Amsterdam.Code, JAAR) |> 
  count() |>
  ungroup()

# retrieve the municipality-mapping and incorporate the strikes in year-1
## merge these to get the district -> gemeentenaam -> amco (hdng identifier) match
dm <- read_csv('./data/district/district_municipality_time_table.csv')
key <- read_delim('./data/district/key_elections_hdng_amco.csv')
mapping <- dm |> left_join(key, by = c('gemeentenaam' = 'name_elections'))


find_number_of_strikes <- function(row){
  
  year <- row$date |> year()
  district <- row$district |> strip_roman()
  
  if(is.na(district)){
    return(NA)
  }
  municipalities <- find_municipalities(district, year)
  
  strikes_db |> 
    filter(is.element(Amsterdam.Code, municipalities),
           between(JAAR, year-1, year)) |> 
    dplyr::summarize(amount_of_strikes = sum(n)) |>
    pull(amount_of_strikes)
  
}

# write this variable
data <- data |> 
  rowwise() |> 
  mutate(amount_of_strikes = list(find_number_of_strikes(cur_data())))

data <- data |> 
  unnest(amount_of_strikes)

#write to csv
write_csv2(data, "./data/analysis/dataset_final.csv")
