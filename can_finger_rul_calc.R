#RUL calc Bence Vass
library(R.matlab)
library(dplyr)
library(purrr)
library(plyr)
library(lubridate)
library(zoo)
library(readr)
library(ggplot2)
library(stringr)
library(RcppRoll)

# \item{elapsed time} 
# \item{traveled distance} 
# \item{count in various speed and torque state}
# \item{changing x direction}
# \item{changing y direction}
# \item{is there weight counter (max 3600 in a hour)}
# \item{steering wheel degree change derived by time aggregated by average}

# \item{join the tire measurement and the hourly aggregated attributes by timestamp}
# \item{interpolate from the tire measurements to the hourly aggregations}
# \item{compute the tire diameter change on all the given measurement points}
# \item{calculate the average diameter change by hourly aggregations} with that the tire diameter change by hourly attribute change is given
# \item{produce the attributes hourly for a new measurement}
# \item{calculate the tire diameter change for all attributes based on proportionality}
# \item{compute the mean tire diameter change by hour}
# \item{summarize the tire diameter change from last know diameter}
# \item{if the summary is greater than a given constant, the tire should be changed}
# \item{compute reaming useful life on the last hours abrasion rate}
# \end{enumerate}

# setwd("/home/vasy/RStudioProjects/still_github/rds_files/")
# export_location="/home/vasy/RStudioProjects/still_github/rul_calc/"

#PAKS3 (batman)
setwd("/home/vassb/box_window_att_files/")
export_location="/home/vassb/box_window_att_files/"

Filename = "Part 0* Schenker_C_01115_MultiTimeChannel_att"

# rds_1_I = readRDS("Part 01 Schenker_C_01115_MultiTimeChannel_only_att.rds")
# rds_2_I = readRDS("Part 02 Schenker_C_01115_MultiTimeChannel_only_att.rds")
# rds_3_I = readRDS("Part 03 Schenker_C_01115_MultiTimeChannel_only_att.rds")
# rds_4_I = readRDS("Part 04 Schenker_C_01115_MultiTimeChannel_only_att.rds")

double_the_size_in_time <- function(double_me){
  #colname could be useful
  double_me_temp = double_me
  double_me_temp$date_time = double_me_temp$date_time + 1 + (last(double_me$date_time) - double_me$date_time[1])
  return(bind_rows(double_me,double_me_temp))
}

double_me_wrapper <- function(df_with_date_time_col,doubling_times){
  #colname could be useful
  if(doubling_times >= 6)
  {
    return(df_with_date_time_col)
  } else if (doubling_times>1) {
    return(double_the_size_in_time(double_me_wrapper(df_with_date_time_col,doubling_times-1)))
  }
  else
  {  
    return(double_the_size_in_time(df_with_date_time_col))
  }
}

# Filename = "Part 0* IMRl_F_00214_MultiTimeChannel_att"
# 
# rds_1 = readRDS("Part 01 IMRl_F_00214_MultiTimeChannel_att.rds")
# rds_2 = readRDS("Part 02 IMRl_F_00214_MultiTimeChannel_att.rds")

# Filename = "Part 0* Imperial_D_00125_MultiTimeChannel_att"
# 
# rds_1 = readRDS("Part 01 Imperial_D_00125_MultiTimeChannel_att.rds")
# rds_2 = readRDS("Part 02 Imperial_D_00125_MultiTimeChannel_att.rds")
# rds_3 = readRDS("Part 03 Imperial_D_00125_MultiTimeChannel_att.rds")
# rds_4 = readRDS("Part 04 Imperial_D_00125_MultiTimeChannel_att.rds")

rds = readRDS("Part 01 Imperial_D_00125_MultiTimeChannel_only_att.rds") %>%
   bind_rows(readRDS("Part 02 Imperial_D_00125_MultiTimeChannel_only_att.rds")) %>%
   bind_rows(readRDS("Part 03 Imperial_D_00125_MultiTimeChannel_only_att.rds")) %>%
   bind_rows(readRDS("Part 04 Imperial_D_00125_MultiTimeChannel_only_att.rds"))


tire_meas = read_csv("Still_tire_measurement_201710 - Summary.csv")

#POXCT time class
tire_meas$`measurement date` = as.POSIXct(ymd(tire_meas$`measurement date`))

tire_meas_curr = tire_meas %>%
  select(`measurement date`,`FL 4`,`FL 3`,`FL 2`,`FL 1`,`FR 1`,`FR 2`,`FR 3`,`FR 4`,`BL 3`,`BL 2`,`BL 1`,`BR 1`,`BR 2`,`BR 3`) %>%
  filter(str_detect(Filename,`Measurement file name`))

rds_temp = double_the_size_in_time(rds)

rds_RUL = double_me_wrapper(rds,4)

# rds_2_time = rds_2_time %>%
#   mutate(
#     FL_4 = NA,
#     FL_3 = NA,
#     FL_2 = NA,
#     FL_1 = NA,
#     
#     FR_1 = NA,
#     FR_2 = NA,
#     FR_3 = NA,
#     FR_4 = NA,
#     
#     BL_3 = NA,
#     BL_2 = NA,
#     BL_1 = NA,
#     
#     BR_1 = NA,
#     BR_2 = NA,
#     BR_3 = NA
#   )

rds_RUL = rds_RUL %>% 
  left_join(tire_meas_curr, by = c("date_time" = `measurement date`))
  select(date_time,`FL 4`,`FL 3`,`FL 2`,`FL 1`,`FR 1`,`FR 2`,`FR 3`,`FR 4`,`BL 3`,`BL 2`,`BL 1`,`BR 1`,`BR 2`,`BR 3`) %>% 

#NA cols to the measurement

#rds_3_time = double_me_wrapper(rds,3)
# 
# rds_1_II = rds_1_I
# rds_2_II = rds_2_I
# rds_3_II = rds_3_I
# rds_4_II = rds_4_I
# 
# rds_1_II$date_time = rds_1_II$date_time + 1 + (last(rds_4_I$date_time)-rds_1_I$date_time[1])
# rds_2_II$date_time = rds_2_II$date_time + 1 + (last(rds_4_I$date_time)-rds_1_I$date_time[1])
# rds_3_II$date_time = rds_3_II$date_time + 1 + (last(rds_4_I$date_time)-rds_1_I$date_time[1])
# rds_4_II$date_time = rds_4_II$date_time + 1 + (last(rds_4_I$date_time)-rds_1_I$date_time[1])
# 
# rds_1_III = rds_1_II
# rds_2_III = rds_2_II
# rds_3_III = rds_3_II
# rds_4_III = rds_4_II
# 
# rds_1_III$date_time = rds_1_III$date_time + 1 + (last(rds_4_I$date_time)-rds_1_I$date_time[1])
# rds_2_III$date_time = rds_2_III$date_time + 1 + (last(rds_4_I$date_time)-rds_1_I$date_time[1])
# rds_3_III$date_time = rds_3_III$date_time + 1 + (last(rds_4_I$date_time)-rds_1_I$date_time[1])
# rds_4_III$date_time = rds_4_III$date_time + 1 + (last(rds_4_I$date_time)-rds_1_I$date_time[1])

head(rds_1_I$date_time,4)
tail(rds_1_I$date_time,4)

head(rds_2_I$date_time,4)
tail(rds_2_I$date_time,4)

head(rds_3_I$date_time,4)
tail(rds_3_I$date_time,4)

head(rds_4_I$date_time,4)
tail(rds_4_I$date_time,4)

tire_meas_curr

#rds_1$date_time[1] + 1 + (last(rds_1$date_time)-rds_1$date_time[1])

#rds_1$date_time[1] + 1 + (last(rds_4$date_time)-rds_1$date_time[1])
