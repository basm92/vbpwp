# x_helpers_step4
library(tidyverse)
# helper: find the municipalities
find_municipalities <- function(distr, year){
  # years within the mapping
  years <- c(1848, 1850, 1888, 1897)
  years <- years[years <= year]
  target_year <- years[which.min(abs(year-years))]
  
  if(distr == "'s-Gravenhage"){
    distr <- "Den Haag"
  }
  if(distr == "'s-Hertogenbosch"){
    distr <- "Den Bosch"
  }
  # get the correct municipalities
  municipalities <- mapping |>
    filter(district == distr, year == target_year, belong_to_district == 1) |>
    select(amco) |> 
    pull()
  
  municipalities <- municipalities[!is.na(municipalities)]
  return(municipalities)
}

# helper: strip of roman numerals to identify municipality
strip_roman <- function(x) {
  sub(" \\b[XVIMCD]+$", "", x)
}
# helper: extract the correct labor force data
extract_lf_data <- function(df, ty, category) {
  # df = limited_hdng, ty = hdng_target_year
  industry <- c("aardewerk", "bouwbedrijven", "chemische nijverheid",
                "diamentbewerking", "houtbewerking", "kunstnijverheid", 
                "metaalbewerking", "landbouw", "leder", "mijnen en veenderijen", "papier",
                "textiel", "verlichting", "voedings- en genotmiddelen")
  services <- c("drukkersbedrijven", "huiselijke diensten", "godsdienst",
                "kleding en reiniging", "krediet- en bankwezen",
                "losse werklieden", "onderwijs", "verkeerswezen",
                "vrije beroepen", "verzekeringswezen", "handel")
  agriculture <- c("landbouw", "visserij en jacht")
  
  output_col_name <- paste0("sum_", category)
  
  category_vector <- switch(category, 
                            "industry" = industry,
                            "services" = services,
                            "agriculture" = agriculture)
  df |> 
    filter(year == ty, 
           str_detect(description, str_c(category_vector, collapse="|")),
           information == 'totaal',
           !is.na(sex)) |>
    summarize({{ output_col_name }} := sum(value, na.rm=T)) |>
    select({{ output_col_name }})
  
}

# helper: extract the tax (totaal personele belastingen data)
extract_tax_data <- function(df, y){
  
  df |> 
    filter(str_detect(information, 'totaal personele belastingen'), 
           year == y) |>
    summarize(total_pers_taxes = sum(value, na.rm=T))
}

# helper: extract the religious decomposition:

get_religious_decomposition <- function(df, y){
  
  interim <- df |>
    filter(description == 'Aantal gelovigen', year == y) 
  if(is.element(y, c(1899, 1909))) {
    interim |>
      filter(sex == "_T") |> 
    summarize(catholics = sum(value[information == 'Rooms-Katholieken'], na.rm=T),
              geref = sum(value[str_detect(information, "Gereformeerd")], na.rm=T),
              hervormd = sum(value[information == 'Nederlands Hervormden'], na.rm=T)) |>
      rowwise() |>
      mutate(catholic_pct = catholics/sum(catholics, geref, hervormd, na.rm=T),
             geref_pct = geref/sum(catholics, geref, hervormd, na.rm=T),
             hervormd_pct = hervormd/sum(catholics, geref, hervormd, na.rm=T))
  } else {
  
  interim |>
    filter(is.element(sex, c("M", "V"))) |>
    summarize(catholics = sum(value[information == 'Rooms-Katholieken'], na.rm=T),
              geref = sum(value[str_detect(information, "Gereformeerd")], na.rm=T),
              hervormd = sum(value[information == 'Nederlands Hervormden'], na.rm=T)) |>
    rowwise() |>
    mutate(catholic_pct = catholics/sum(catholics, geref, hervormd, na.rm=T),
           geref_pct = geref/sum(catholics, geref, hervormd, na.rm=T),
           hervormd_pct = hervormd/sum(catholics, geref, hervormd, na.rm=T))
  }
}
