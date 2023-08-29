# step 1: import all data together
# on the basis of the raw voting files
library(tidyverse)
files <- list.files('./data/voting/electoral_law/', recursive = T)

suffrage <- purrr::map(files, ~ paste0('./data/voting/electoral_law/', .x) |> source())
suffrage <- bind_rows(kieswet1872, kieswet1887, kieswet1892, kieswet1896, kieswet1918)
write_csv2(suffrage, "./data/voting/suffrage.csv")

files <- list.files('../data/voting/fiscal/', recursive = T)
fiscal <- purrr::map(files, ~ paste0('../data/voting/fiscal/', .x) |> source())
fiscal <- bind_rows(inkomstenbelasting1872, inkomstenbelasting1893, inkomstenbelasting1914,
                    successiewet1878, successiewet1911, successiewet1916, successiewet1921)
write_csv2(fiscal, './data/voting/fiscal.csv')


files <- list.files('./data/voting/social_redistribution/', recursive = T)
social <- purrr::map(files, ~ paste0('../data/voting/social_redistribution/', .x) |> source())
social <- bind_rows(amendementrutgers, antistakingswet1903, arbeidscontractwet1907, arbeidswet1919,
                    hogeronderwijswet1904, kinderwet1874, leerplichtwet1901, ongevallenwet1901, 
                    staatspensioen1916, wetouderdominvaliditeit1913, woningwet1901, ziektewet1913)
write_csv2(social, './data/voting/social.csv')
