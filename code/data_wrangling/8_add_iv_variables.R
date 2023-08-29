#8_add_iv_variables
library(tidyverse)
data <- read_csv2('./data/voting/voth_wealth.csv')

instrumental <- readxl::read_xlsx('./data/wealth/instrumental_variable_est.xlsx') 
ihs <- function(x) log(x + sqrt(x^2 + 1))

instrumental <- readxl::read_xlsx('./data/wealth/instrumental_variable_est.xlsx') |> 
  select(polid,
         dod_father,
         dod_mother,
         hoeveel_broers_zussen,
         father_profession,
         wealth_father,
         wealth_mother,
         wealth_misc) |> 
  mutate(wealth_father = as.numeric(wealth_father),
         hoeveel_broers_zussen = if_else(!is.na(as.numeric(hoeveel_broers_zussen)), as.numeric(hoeveel_broers_zussen), 0),
         expected_inheritance_rough = case_when(
           !is.na(wealth_mother) & !is.na(wealth_father) ~ wealth_father + wealth_mother,
           is.na(wealth_father) & !is.na(wealth_mother) ~ wealth_mother*2,
           is.na(wealth_mother) & !is.na(wealth_father) ~ wealth_father*2,
           is.na(wealth_mother) & is.na(wealth_father) ~ wealth_misc*2),
    expected_inheritance = expected_inheritance_rough / (1+hoeveel_broers_zussen)) |>
  mutate(father_politician = if_else(father_profession == "Politicus", 1, 0)) |>
  select(polid, dod_father, dod_mother, expected_inheritance_rough, expected_inheritance, father_politician) |> 
  distinct(polid, .keep_all = T)

data <- data |> 
  left_join(instrumental, by = c('b1_nummer' = 'polid'))

# potentially deflate the expected inheritance
deflator <- readxl::read_xlsx("./data/roroe/rateofreturnoneveryting_data.xlsx", sheet = "Data") |>
  filter(iso == "NLD") |> 
  select(year, cpi) |>
  mutate(cpi_1900 = (cpi / 5.817804))

deflate <- function(row){
  dad <- row$dod_father |> ymd() |> year()
  mom <- row$dod_mother |> ymd() |> year()
  eh <- row$expected_inheritance
  
  if(is.na(dad) & is.na(mom)){
    dad <- 1900
    mom <- 1900
  } else if(is.na(dad)){
    dad <- mom
  } else if(is.na(mom)){
    mom <- dad
  }
  
  cpi <- deflator |> 
    filter(year == round((dad+mom)/2, digits=0)) |> 
    select(cpi_1900) |> 
    pull()
  
  if(length(cpi) == 0){
    cpi <- 1
  }
  
  return(eh / cpi)
  
}

data <- data |> 
  rowwise() |> 
  mutate(deflated_eh = list(deflate(cur_data()))) |> 
  unnest(deflated_eh) |>
  select(-c(dod_father, dod_mother))

# write to csv
write_csv2(data, "./data/voting/vot_beh_wealth_instr.csv")

