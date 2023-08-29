# party_classification
## two classifications: one 4-way (cath, prot, lib, soc), one 3-way (conf, lib, soc)
# also variables of interest:
## Age of vote
## Age of first entrance
## Tenure (Time active)
## death date (for harnas)
data <- read_csv2('./data/voting/vot_beh_b1_district_data.csv')
tk <- readxl::read_xlsx("./data/pdc/tk_1815tot1950uu.xlsx") |>
  janitor::clean_names() |> 
  select(b1_nummer, contains('partij'))

tk2 <-  readxl::read_xlsx('./data/pdc/tk_1815tot1950uu.xlsx', sheet = 2) |>
  janitor::clean_names() |>
  separate(datum, into = c("from", "to"), sep = "/") |>
  mutate(from = dmy(from),
         to = dmy(to))

data <- data |> 
  left_join(tk, by = 'b1_nummer')

# implement the two party divisions
data <- data |>
  mutate(
    party_granular =  case_when(
      stringr::str_detect(partij_en_fractie_s, 'liberaal|Liberaal|Lib|lib|Thorb|Takk') ~ "liberal",
      stringr::str_detect(partij_en_fractie_s, "RK|Rooms|Katholiek|katholiek|kath|conservatief (katholiek)|RKSP|Schaep|Bahl") ~ "catholic",
      stringr::str_detect(partij_en_fractie_s, "Prot|ARP|CHU|Anti-Rev|anti|cons|AR|Christ|christ|a.r.|onafhankelijk c.h.") ~ "protestant",
      stringr::str_detect(partij_en_fractie_s, "VDB|Vrije Demo|Socia|Soc|Comm|SDAP|Arbeid|Radi|vrijz|Vrijz|CPN|CPH") ~ "socialist",
      stringr::str_detect(partij_en_fractie_s, "neutral|Neutral") ~ "neutral",
      TRUE ~ NA_character_
    )
  )

data <- data |>
  mutate(
    party_simple = case_when(
      stringr::str_detect(partij_en_fractie_s, 'liberaal|Liberaal|Lib|lib|Thorb|Takk') ~ "liberal",
      stringr::str_detect(partij_en_fractie_s, "RK|Rooms|Katholiek|katholiek|kath|conservatief (katholiek)|RKSP|Schaep|Bahl|Prot|ARP|CHU|Anti-Rev|anti|cons|AR|Christ|christ|a.r.|onafhankelijk c.h.") ~ "confessional",
      stringr::str_detect(partij_en_fractie_s, "VDB|Vrije Demo|Socia|Soc|Comm|SDAP|Arbeid|Radi|vrijz|Vrijz|CPN|CPH") ~ "socialist",
      stringr::str_detect(partij_en_fractie_s, "neutral|Neutral") ~ "neutral",
      TRUE ~ NA_character_
    )
  )

data <- data |>
  select(-contains("partij"))

# now implement the other variables:
find_other <- function(row) {
  date <- row$date
  b1_num <- row$b1_nummer
  
  birth_date <- tk2 |> 
    filter(rubriek == "3010", b1_nummer == b1_num) |> 
    select(from) |> 
    pull()
  
  start_date <- tk2 |>
    filter(b1_nummer == b1_num, str_detect(waarde, "Tweede Kamer")) |>
    slice_min(from, n = 1) |>
    select(from) |>
    pull()
  
  death_date <- tk2 |>
    filter(rubriek == "3020", b1_nummer == b1_num) |>
    slice_head(n=1) |>
    select(from) |>
    pull()
  
  # now the variables which I really want
  data.frame(birth_date = birth_date, 
             start_date = start_date,
             tenure = date - start_date, 
             death_date = death_date)
}


data <- data |> 
  rowwise() |> 
  mutate(pdc = list(find_other(cur_data())))

data <- data |>
  unnest_wider(pdc)

data <- data |>
  unnest(c(birth_date, start_date, tenure, death_date))

write_csv2(data, './data/voting/vot_beh_b1_district_data_party.csv')
