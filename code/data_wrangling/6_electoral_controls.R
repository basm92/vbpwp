## find electoral controls
library(tidyverse)
data <- read_csv2('./data/voting/vot_beh_b1_district_data_party.csv')
# import the key to go to the election dataset
map_to_elec <- read_csv('./data/handmade_matches_elections_pdc.csv') |> 
  select(b1_nummer, name_in_elec) |>
  group_by(b1_nummer) |>
  summarize(name_in_elec_combined = paste(unique(name_in_elec), collapse = "|"))


# get the elections data
elections <- read_delim('./data/elections/election_results_details_new.csv') |>
  janitor::clean_names() |>
  mutate(verkiezingdatum = parse_date(verkiezingdatum, format = "%d/%m/%Y"),
         aantal_stemmen = parse_number(aantal_stemmen),
         aantal_zetels = parse_number(aantal_zetels),
         aantal_stembriefjes = parse_number(aantal_stembriefjes),
         opkomst = parse_number(opkomst),
         year = as.numeric(str_extract(verkiezingdatum, "\\d{4}")),
         across(where(is.character), str_squish))

# merge the elections with whom is a socialist to compute socialist share
ext_rec <-read_csv2('./data/extended_recommendations.csv')
elections <- elections |>
  left_join(ext_rec, by = 'naam')
  
# merge the elections data to the map and see how many missing (should be zero)
data <- data |> 
  left_join(map_to_elec, by = 'b1_nummer')


# extract the election data
## variables of interest:
### turnout
### vote_share
### dummy socialist
### perc. socialist vote
### days since last election
### nearest_competitor_margin

find_election_data <- function(row){
  date <- row$date
  name <- row$name_in_elec_combined |> str_split("\\|")
  name <- name[[1]]

  if (is.na(row$district) | is.na(name)) {
    # Return default values or skip processing
    return(data.frame(dis = NA))
  }
  
  # find the last (unique) general election in which the candidate partook
  last_election <- elections |>
    filter(type == "algemeen" | type == "periodiek" | type == "tussentijds", 
           naam == name,
           verkiezingdatum < date) |>
    slice_max(verkiezingdatum, n=1) |>
    select(district, verkiezingdatum, type) 
  
  if(nrow(last_election) == 0){
    last_election <- elections |>
      filter(naam == name,
             verkiezingdatum < date) |>
      slice_max(verkiezingdatum, n=1) |>
      select(district, verkiezingdatum, type) 
  }
  
  if (nrow(last_election) == 0) {
    # Return default values or skip processing
    return(data.frame(dis = NA))
  }
  
  last_election_distr <- pull(last_election, 1)
  last_election_date <- pull(last_election, 2)[1]
  last_election_type <- pull(last_election, 3)[1]
  # in case of more districts, find the district where the politician got the highest votes
  if(length(last_election_distr) > 1){
    intm <- elections |> filter(
      is.element(district, last_election_distr),
      verkiezingdatum == last_election_date,
      is.element(naam, name)) |>
      mutate(vs = aantal_stemmen/aantal_stembriefjes) |> 
      slice_max(vs, n=1) 
    
    kind <- intm |> 
      select(type) |>
      pull()
    
    last_election_distr <- intm |>
      select(district) |> 
      pull()
    
    le <- elections |> filter(
      district == last_election_distr,
      verkiezingdatum == last_election_date,
      type == kind)
    
  } else {
  # all obs in that last election
  le <- elections |> filter(
    district == last_election_distr,
    verkiezingdatum == last_election_date,
    type == last_election_type)
  }
  # turnout
  turnout <- le |> 
    reframe(turnout = aantal_stembriefjes/omvang_electoraat) |> 
    distinct()
  # filter the data.frame to find information about this election
  total_votes <- le |>
    summarize(tot_votes = sum(aantal_stemmen, na.rm=T)) |>
    pull()
  # vote share
  vote_share <- le |> 
    filter(is.element(naam, name)) |>
    summarize(vote_share = aantal_stemmen/total_votes)
  # socialist
  socialist <- le |>
    summarize(soc_share = sum(aantal_stemmen[rec_soc == 1], na.rm=T),
              soc_dum = if_else(soc_share > 0, 1, 0)) 
  ### days since last election
  dsle <- data.frame(dsle = date - last_election_date) |>
    distinct()
  ### nearest competitor margin
  num <- le |> filter(is.element(naam, name)) |> select(number) |> pull()
  vsnc <- le |>
    filter(number == num+1) |>
    summarize(vote_share_nc = aantal_stemmen/total_votes)
  if(nrow(vsnc) == 0){
    vsnc <- data.frame(vote_share_nc = NA)
  }

  return(bind_cols(turnout, vote_share, socialist, dsle, vsnc))
  
}

# implement the row-based function
data <- data |>
  rowwise() |>
  mutate(elec = list(find_election_data(cur_data())))

data <- data |>
  unnest_wider(elec)

data <- data |> 
  select(-dis)

## write this to csv
write_csv2(data, './data/voting/voting_b1_dis_elec.csv')
