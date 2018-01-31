# file concat and att gen for the six forklift
library(readr)
library(purrr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

#PAKS3 (batman)
setwd("/media/vasy/Data/Doksik/projekts/AITIA/reduced_can_fp/")
export_location="/media/vasy/Data/Doksik/projekts/AITIA/reduced_can_fp/merged_machines/"

truck_category <- function(x){
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
  else if(str_detect(x,'Schenker_C_01115'))
    return('Schenker_C_01115')
  else
    return(NA)
}

wd_filenames = as.data.frame(list.files(pattern = '\\.csv$'))
names(wd_filenames) =  'filenames'

for(i in 1:length(wd_filenames$filenames)){
  wd_filenames$truck_cat[i]=truck_category(wd_filenames$filenames[i])
}
 wd_filenames$truck_cat = as.factor(wd_filenames$truck_cat)

cat_files = group_by(wd_filenames,truck_cat) %>%
  arrange(.by_group = TRUE)
print(cat_files)

#for(i in i:)
