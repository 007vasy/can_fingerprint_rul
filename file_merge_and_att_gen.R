# file concat and att gen for the six forklift
library(readr)
library(purrr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

#PAKS3 (batman)
setwd("/home/vassb/fingerprint_data/ansgar_att_six_forklift/")
export_location="/home/vassb/fingerprint_data/ansgar_att_six_forklift_merged/"

wd_filenames = as.data.frame(list.files(pattern = '\\.csv$'),col.names = 'filenames') %>%
  transform(filenames = as.character(filenames)) %>%
  mutate(truck_cat = function(x = filenames){
    if(str_detect(x,'Daimler F 00150'))
      return('Daimler F 00150')
    else if(str_detect(x,'Daimler F 00166'))
      return('Daimler F 00166')
    else if(str_detect(x,'Daimler F 00192'))
      return('Daimler F 00192')
    else if(str_detect(x,'Imperial_D_00125'))
      return('Imperial_D_00125')
    else if(str_detect(x,'IMRl_F_00214'))
      return('IMRl_F_00214')
    else if(str_detect(x,'Daimler F 00166'))
      return('Schenker_C_01115')
    }
  ) %>%
  groupby(truck_cat) %>%
  arrange(filenames)
print(wd_filenames)

#for(i in i:)
