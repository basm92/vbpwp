# use the b1_nummer data.frame to find which district a politician represents at a point in time
data <- read_delim('./data/voting/voting_behavior_b1_nummer.csv')
# import the pdc data
pdc_tk <- readxl::read_xlsx('./data/pdc/tk_1815tot1950uu.xlsx', sheet = 2) |>
  janitor::clean_names() |>
  separate(datum, into = c("from", "to"), sep = "/") |>
  mutate(from = dmy(from),
         to = dmy(to))

# helper: extract the years and evaluate whether the actual year is in between them
# Example input string
extract_cor <- function(input_string, year) {
  # Regular expression to match year ranges in the input string
  regex <- "\\d{4}-?(\\d{0,4})?"
  # Extract year ranges from the input string
  year_ranges <- str_extract_all(input_string, regex)[[1]]
  
  # Find the substring that contains the year variable G
  for (i in 1:length(year_ranges)) {
    range <- str_split(year_ranges[i], "-")[[1]]
    if (as.numeric(range[1]) <= year & (length(range) == 1 | year <= as.numeric(range[2]))) {
      substring <- str_trim(str_split(input_string, ",")[[1]][i])
      break
    }
  }
  return(substring)
}

# helper: find the match
find_district <- function(row){
  date <- row$date
  b1_no <- row$b1_nummer
  year <- date |> year() 
  
  if(year <= 1917) {
  
  district_raw <- pdc_tk |>
    filter(b1_nummer == b1_no,
           from < date, 
           to > date,
           str_detect(waarde, 'Tweede Kamer')) |>
    filter(!is.na(toelichting)) |>
    slice(1) |>
    select(toelichting) |>
    pull()
  
  # need the following in the case it fails (after 1918)
  if(length(district_raw) == 0){
    district_raw <- " "
  }

  # clean up the string
  # if there is no comma, just remove the years
  if(!str_detect(district_raw, ",")){
  out <- district_raw |>
    str_remove_all('voor het kiesdistrict ') |>
    str_remove_all('in ') |>
    str_remove('^[0-9]{4}-[0-9]{4}') |>
    str_squish()
  
  } else {
    # otherwise, try to find out which one of the conditions applies
    interm <- district_raw |>
      str_remove_all('voor het kiesdistrict ') |>
      str_remove_all('in ') 
    
    out <- extract_cor(interm, year)
  }

  return(out)
  } else {
    
    return(" ")
  }
  
  
}

data <- data |>
  rowwise() |>
  mutate(test = list(find_district(cur_data())))

# do some short post-processing to finalize:
data <- data |>
  rename(district = test) |>
  mutate(district = str_remove(district, "\\d{4}-?(\\d{0,4})?") |> 
           str_remove('september 1918') |>
           str_squish() |>
           str_replace("In Zutphen", "Zutphen") |>
           str_replace("van Amsterdam IV", "Amsterdam IV") |>
           str_replace("vanaf Weststellingwerf", "Weststellingwerf"))

# write to csv
write_csv2(data, './data/voting/voting_behavior_b1_nummer_district.csv')
