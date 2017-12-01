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

setwd("/home/vasy/RStudioProjects/still_github/rds_files/")
export_location="/home/vasy/RStudioProjects/still_github/rul_calc/"

Filename = "Part 0* Schenker_C_01115_MultiTimeChannel_att"

rds_1_I = readRDS("Part 01 Schenker_C_01115_MultiTimeChannel_att.rds")
rds_2_I = readRDS("Part 02 Schenker_C_01115_MultiTimeChannel_att.rds")
rds_3_I = readRDS("Part 03 Schenker_C_01115_MultiTimeChannel_att.rds")
rds_4_I = readRDS("Part 04 Schenker_C_01115_MultiTimeChannel_att.rds")



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

# rds = readRDS("Part 01 Imperial_D_00125_MultiTimeChannel_att.rds") %>%
#   bind_rows(readRDS("Part 02 Imperial_D_00125_MultiTimeChannel_att.rds")) %>%
#   bind_rows(readRDS("Part 03 Imperial_D_00125_MultiTimeChannel_att.rds")) %>%
#   bind_rows(readRDS("Part 04 Imperial_D_00125_MultiTimeChannel_att.rds"))

tire_meas = read_csv("/home/vasy/RStudioProjects/still_github/new_still_files_201710/tobatman_newstill/Still_tire_measurement_201710 - Summary.csv")

tire_meas_curr = tire_meas %>%
  filter(str_detect(Filename,`Measurement file name`))



head(rds_1$date_time,4)
tail(rds_1$date_time,4)

head(rds_2$date_time,4)
tail(rds_2$date_time,4)

head(rds_3$date_time,4)
tail(rds_3$date_time,4)

head(rds_4$date_time,4)
tail(rds_4$date_time,4)

tire_meas_curr

rds_1$date_time[1] + 1 + (last(rds_1$date_time)-rds_1$date_time[1])

rds_1$date_time[1] + 1 + (last(rds_4$date_time)-rds_1$date_time[1])
