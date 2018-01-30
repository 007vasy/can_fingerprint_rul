#Still CAN fingerprint dec.20 big files 6 forklift- Vass Bence
#Attribute gen for Ansgar
library(R.matlab)
library(readr)
library(purrr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(iotools)
library(bigtabulate)
library(bigmemory)


#contans
is.weight_limit = 50
d_wheel = 0.467 # from catalog

#resolution for factor matrix
reso_m = 7 #must be odd!!!

if(reso_m%%2!=1)
  print("Must be odd!")
print("asd1!")
speed_max = 5000
torque_max = 80

smoothing = 1 # window size

#where the files to be boxshorted

#PAKS3 (batman)
setwd("/home/vassb/fingerprint_data/")
export_location="/home/vassb/fingerprint_data/ansgar_att_six_forklift/"

#PC
# setwd("/home/vasy/RStudioProjects/still_github/RStudio_wd_Can_fp/")
# export_location="/home/vasy/RStudioProjects/still_github/cleaned_files/"

#Cut of ".mat" for classification categories
wd_filenames = list.files(pattern = '\\.mat$')
fp_df = data.frame()
for(i in 1:length(wd_filenames)){
  wd_filenames[i]=gsub(".mat","",c(wd_filenames[i]),fixed = TRUE)
}

# for(file_name in list.files())
# {
#   print(file_name)
# }
temp_list = list()
#loop through files in wd
for(file_name_i in wd_filenames)
{
  print(paste("file:",file_name_i))
  #temp_list = readMat(paste(file_name_i,".mat",sep=""))

  fp_df <- chunk.apply(paste(file_name_i,".mat",sep=""),
                                # A function to process each of the chunk
                                function(chunk) {
                                  # Turn the chunk into a data frame
                                  for(colName in names(chunk)) {
                                    print(names(chunk[[colName]]))
                                   # temp_list$colName=rbind(temp_list$colName,dstrsplit(chunk[[colName]], col_types = rep("numeric", 2), sep = ",") %>%
                                   #    mutate(V1 = round(V1)) %>%
                                   #    group_by(V1) %>%
                                   #    summarise_all(mean,na.rm = FALSE) %>%
                                   #    as.list()
                                   #)
                                  }
                                  join_all(temp_list, by='V1', type = 'outer') %>%
                                  as.data.frame()
                                  
                                  # Return the column sums
                                  
                                },
                                # Maximum chunk size in bytes
                                CH.MAX.SIZE = 1e5)
  # 2 processors read and process data
  #parallel = 2)
  
  #glimpse(temp_list)
  #print(names(temp_list))
  
  #all timestamp possibilites for boxshort (max calculated /file)
  # fp_df = data.frame(
  #   0:(
  #     max(
  #       temp_list$UE.DAC.6.Auslenkung.Z.prop.....................................[,1]
  #     )
  #     + 1
  #   )
  # )
  # names(fp_df) = "time_ID"
  # 

  #print(names(fp_df))
  #
  
  #cleaning solution
  df_fp_tidy = fp_df %>% 
    #drop meaningless values  
    #rearrenge columns to properly rename them  
  # TODO TIME ID S for python
  select(
      time_ID_s,
      A5.Sekunde.....................................................,
      A4.Minute......................................................,
      A3.Stunde......................................................,
      A2.Tag.........................................................,
      A1.Monat.......................................................,
      A0.Jahr........................................................,
      SR.DAC.3.Drehzahl.Lenkrad......................................,
      SR.DAC.2.Lenkwinkel............................................,
      SR.DAC.1.Drehzahl.PM...........................................,
      SR.DAC.0.Drehmoment.PM.........................................,
      UE.DAC.6.Auslenkung.Z.prop.....................................,
      UE.DAC.5.Auslenkung.Y.prop.....................................,
      UE.DAC.4.Auslenkung.X.prop.....................................,
      UE.DAC.7.Auslenkung.W.prop.....................................,
      SR.DAC.7.Auslenkung.Zusatz.2...................................,
      SR.DAC.6.Auslenkung.Zusatz.1...................................,
      SR.DAC.5.Auslenkung.Neigen.....................................,
      SR.DAC.4.Auslenkung.Heben......................................,
      UE.DAC.3.Drehzahl.FM.2.........................................,
      UE.DAC.2.Drehzahl.FM.1.........................................,
      UE.DAC.1.Drehmoment.FM.2.......................................,
      UE.DAC.0.Drehmoment.FM.1.......................................
    )
  
  #name w_columns, short column names 
  names(df_fp_tidy) = c("time_ID_s",
                        "Second_s",
                        "Minute_m",
                        "Hour_h",
                        "Day_d",
                        "Month_mo",
                        "Year_y",
                        "Speed_Steering_wheel_RPM",
                        "Steering_angle_angle",
                        "Speed_pump_motor_RPM",
                        "Torque_pump_motor_Nm",
                        "Crash_Z_0.01g",
                        "Crash_Y_0.01g",
                        "Crash_X_0.01g",
                        "Crash_W_0.01g",
                        "Lever_position_Add2_mV_base_4000mV",
                        "Lever_position_Add1_mV_base_4000mV",
                        "Lever_position_tilting_mV_base_4000mV",
                        "Lever_position_lifting_mV_base_4000mV",
                        "Speed_Drivemotor_2_RPM",
                        "Speed_Drivemotor_1_RPM",
                        "Torque_Drivemotor_2_Nm",
                        "Torque_Drivemotor_1_Nm")
  
    #correct time related values (no value after decimal needed)
    mutate(
      Second_s = floor(Second_s),
      Minute_m = floor(Minute_m),
      Hour_h = floor(Hour_h),
      Day_d = floor(Day_d),
      Month_mo = floor(Month_mo),
      Year_y = floor(Year_y)) %>%

    #date time convert with lubridate separeted  (time_ID leave separated, with the lubridate package it can be merged)
    mutate(date = ymd(paste(Year_y,Month_mo,Day_d)),time = hms(paste(Hour_h,Minute_m,Second_s))) %>%
    #drop redundant values
    select(
      -Second_s,
      -Minute_m,
      -Hour_h,
      -Day_d,
      -Month_mo,
      -Year_y
    ) %>%
    #mutate fingerprint type
    #shorten the dataframe, drop not usefull data
    mutate(date_time = as.POSIXct(ymd_hms(paste(date,hms(time),sep = ",") ))) 
  
  #save in export location
  #csv
  # write.csv(df_fp_tidy_no_na,file=paste(export_location,file_name_i,".csv",sep=""))
  # print(paste(file_name_i,".csv is saved to: ",export_location,sep = ""))
  # warnings()
  
  #RDS
  saveRDS(df_fp_tidy_no_na,file=paste(export_location,file_name_i,".rds",sep=""))
  print(paste(file_name_i,".rds is saved to: ",export_location,sep = ""))
  
  ##################################################################################################################################################################################################################
  #windowing and attributes
  ##################################################################################################################################################################################################################
  
  #check direction change
  direction_check <- function(value,next_value){
    return(sign(value) != sign(next_value))
  }
  
  #categorise speed and drivemotor profiles, to future comparison using not linear intervall search not binary search but modulo calculation
  drivemotor_category_modulo_calc <- function(value_to_cat,real_scale_max,resolution_m){
    return(
      #cut down decimals, for more resolution, increase reso_m
      floor(
        (
          #(rescale to positive region)  #calc binning
          (value_to_cat+real_scale_max)/(2*real_scale_max/resolution_m)
          #calc the correct binning
        )%%resolution_m
      )
    )
  }
  
  factor_variations <- function(resolution_m){
    
    temp_m = expand.grid(0:resolution_m,0:resolution_m)
    
    for(i in 1:(resolution_m+1)**2){
      if(i == 1)
        factor_variations_string = paste(temp_m[i,1],temp_m[i,2],sep = ",")
      else
        factor_variations_string = c(factor_variations_string,paste(temp_m[i,1],temp_m[i,2],sep = ","))
    }
    return(temp_string)
  }
  
  #is.weight on the truck
  tdf_attributes = mutate(
    df_fp_tidy_no_na,

    is.weight = Lever_position_lifting_mV_base_4000mV > is.weight_limit #contans from plots
  ) %>%
    
    #smoohting used in direction and derivatives
    
    #changing x and y direction
    mutate(
      speed_1_direction_changed = direction_check(Speed_Drivemotor_1_RPM,lag(Speed_Drivemotor_1_RPM,n=smoothing,default = 0)),
      speed_2_direction_changed = direction_check(Speed_Drivemotor_2_RPM,lag(Speed_Drivemotor_2_RPM,n=smoothing,default = 0)),
      torque_1_direction_changed = direction_check(Torque_Drivemotor_1_Nm,lag(Torque_Drivemotor_1_Nm,n=smoothing,default = 0)),
      torque_2_direction_changed = direction_check(Torque_Drivemotor_2_Nm,lag(Torque_Drivemotor_2_Nm,n=smoothing,default = 0)),
      is.changed_y_direction = direction_check(Steering_angle_angle,lag(Steering_angle_angle,n=smoothing,default = 0)),
      is.y_direction_0 = Steering_angle_angle == 0
    ) %>%
    
    #speed torque matrix
    
    
    
    #make all factor variations for comparsion
    
    mutate(
      tdf_attributes, 
      speed_1_modulo_factor = drivemotor_category_modulo_calc(Speed_Drivemotor_1_RPM,speed_max,reso_m), 
      speed_2_modulo_factor = drivemotor_category_modulo_calc(Speed_Drivemotor_2_RPM,speed_max,reso_m),
      torque_1_modulo_factor = drivemotor_category_modulo_calc(Torque_Drivemotor_1_Nm,torque_max,reso_m),
      torque_2_modulo_factor = drivemotor_category_modulo_calc(Torque_Drivemotor_2_Nm,torque_max,reso_m),
      
      speed_torque_1_factor = paste(speed_1_modulo_factor,torque_1_modulo_factor,sep=","),
      speed_torque_2_factor = paste(speed_2_modulo_factor,torque_2_modulo_factor,sep=","),
      
      speed_torque_1_factor = as.factor(speed_torque_1_factor,levels = factor_variations(reso_m)),
      speed_torque_2_factor = as.factor(speed_torque_2_factor,levels = factor_variations(reso_m)),
      
      driving_or_standing = Speed_Drivemotor_1_RPM == 0 & Speed_Drivemotor_2_RPM == 0 & Torque_Drivemotor_1_Nm == 0 & Torque_Drivemotor_1_Nm == 0,
      
      is.speed_torque_factor_equal = speed_torque_1_factor == speed_torque_2_factor
    ) %>%
    
  #savaRDS to attributes
  saveRDS(tdf_attributes,file=paste(export_location,file_name_i,"_att.rds",sep=""))
  print(paste(file_name_i,"_att.rds is saved to: ",export_location,sep = ""))
  
  only_att = tdf_attributes %>%
    select(
      date_time,
      Speed_Drivemotor_2_RPM,
      Speed_Drivemotor_1_RPM,
      Torque_Drivemotor_2_Nm,
      Torque_Drivemotor_1_Nm,
      Steering_angle_angle, 
      
      #events
      speed_torque_1_factor,
      speed_torque_2_factor,
      is.y_direction_0,
      #TODO from Ansgar Simulink
      is.weight,
      driving_or_standing
    )

  saveRDS(only_att,file=paste(export_location,file_name_i,"_only_att.rds",sep=""))
  print(paste(file_name_i,"_only_att.rds is saved to: ",export_location,sep = ""))
}

