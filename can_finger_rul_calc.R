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

setwd("/home/vasy/RStudioProjects/still_github/rds_files/")
export_location="/home/vasy/RStudioProjects/still_github/rul_calc/"

Filename = "Part 0* Schenker_C_01115_MultiTimeChannel_att"

rds_1 = readRDS("Part 01 Schenker_C_01115_MultiTimeChannel_att.rds")
rds_2 = readRDS("Part 02 Schenker_C_01115_MultiTimeChannel_att.rds")
rds_3 = readRDS("Part 03 Schenker_C_01115_MultiTimeChannel_att.rds")
rds_4 = readRDS("Part 04 Schenker_C_01115_MultiTimeChannel_att.rds")

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

min(rds_1$date_time)
max(rds_4$date_time)