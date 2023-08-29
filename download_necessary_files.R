# 1. Download HDNG
# Install and load the necessary packages
if (!require("dataverse")) {
  install.packages("dataverse")
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
}

library(dataverse); library(tidyverse)

# Specify the instructions to download the HDNG file
hdng <- dataverse::get_file_by_name(filename='HDNG_v4.txt',
                            dataset = 'hdl:10622/RPBVK4', 
                            server='http://datasets.iisg.amsterdam/')

# Write the file to disk in the appropriate directory
writeBin(hdng, './data/district/HDNG_v4.txt')


# 2. Download Strikes
strikes <- dataverse::get_file_by_name(filename='Stakingen Nederland_1372_2019 (1).mdb',
                            dataset = 'hdl:10622/APNT4U',
                            server='http://datasets.iisg.amsterdam/')

# Write the file to disk in the appropriate directory
writeBin(strikes, './data/strikes/strikes.mdb')
