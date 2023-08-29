# match votes to pdc - give everyone a pdc number
# on the basis of a data.frame with the voting outcomes
library(tidyverse); library(stringdist)


# helper: find the match
find_b1_nummer <- function(row){
  date <- row$date
  name <- row$politician
  
  candidates <- pdc_tk |> 
    filter(begin_periode < date, 
           einde_periode > date)
  
  distances <- stringdist(name, candidates$full_name, method='jaccard')
  index <- which.min(distances)
  score <- min(distances)
  
  match_name <- candidates$achternaam[index]
  match_b1_nummer <- candidates$b1_nummer[index]
  
  return(c(match_name, match_b1_nummer, score))
}

# import the data
files <- c('fiscal.csv', 'social.csv', 'suffrage.csv')
data <- map(files, ~ read_delim(paste0('./data/voting/', .x))) |>
  bind_rows()

# import the pdc data
pdc_tk <- readxl::read_xlsx('./data/pdc/tk_1815tot1950uu.xlsx') |>
  janitor::clean_names() |>
  mutate(begin_periode = parse_date(begin_periode),
         einde_periode = parse_date(einde_periode),
         full_name = if_else(!is.na(prepositie), str_c(voorletters, ' ', prepositie, ' ', achternaam),
                             str_c(voorletters, ' ', achternaam)))

# apply to each function a matching thing
data <- data |>
  rowwise() |>
  mutate(test = list(find_b1_nummer(cur_data())))

data <- data |> unnest_wider(test, names_sep = 'out')

# clean the variable names and make categories
data <- data |>
  rename(b1_nummer = testout2, pdc_naam = testout1) |>
  select(-testout3) |>
  relocate(c(b1_nummer, politician)) |>
  mutate(category = case_when(
    str_detect(law, 'Inkomsten') ~ "fiscal",
    str_detect(law, "Successie") ~ "fiscal",
    str_detect(law, "Kieswet") ~ "electoral",
    TRUE ~ "social")
  )

# write to csv
write_csv2(data, './data/voting/voting_behavior_b1_nummer.csv')
