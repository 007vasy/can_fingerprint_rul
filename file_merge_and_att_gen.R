# file concat and att gen for the six forklift
library(readr)
library(purrr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

#PC
#setwd("/media/vasy/Data/Doksik/projekts/AITIA/reduced_can_fp/")
#export_location="/media/vasy/Data/Doksik/projekts/AITIA/reduced_can_fp/merged_machines/"
#PAKS3 (batman)
setwd("/home/vassb/fingerprint_data/ansgar_att_six_forklift_att/")
export_location="/home/vassb/fingerprint_data/ansgar_att_six_forklift_att_merged/"

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
  arrange(.by_group = TRUE) %>%
  ungroup()
print(cat_files)

for(truck in levels(cat_files$truck_cat)){
  w_files = filter(cat_files,truck_cat == truck) %>%
    select(filenames) %>%
    droplevels()
  w_files$filenames = factor(w_files$filenames)
  flag = TRUE
  for(file in levels(w_files$filenames)){
    if(flag){
      flag = FALSE
      write_csv(as.data.frame(read_csv(file)),path = paste(export_location,truck,"_att_merged.csv",sep=""))
      #print(truck)
      #print(file)
    }
    else{
      write_csv(as.data.frame(read_csv(file,skip = 1)),path = paste(export_location,truck,"_att_merged.csv",sep=""),append = TRUE)
      #print(truck)
      #print(file)
    }
  }
}