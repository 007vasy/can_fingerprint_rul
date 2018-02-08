# att_merged visualization

library(readr)
library(purrr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(plot3D)
library(rgl)


hist3D_fancy<- function(x, y, break.func = c("Sturges", "scott", "FD"), xlab = NULL,ylab=NULL, breaks = NULL,
                        colvar = NULL, col="white", clab=NULL, phi = 45, theta = 50, ...){
  
  # Compute the number of classes for a histogram
  break.func <- break.func [1]
  if(is.null(breaks)){
    x.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(x),
                       scott = nclass.scott(x),
                       FD = nclass.FD(x))
    y.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(y),
                       scott = nclass.scott(y),
                       FD = nclass.FD(y))
  } else x.breaks <- y.breaks <- breaks
  
  # Cut x and y variables in bins for counting
  x.bin <- seq(min(x), max(x), length.out = x.breaks)
  y.bin <- seq(min(y), max(y), length.out = y.breaks)
  xy <- table(cut(x, x.bin), cut(y, y.bin))
  z <- xy
  
  xmid <- 0.5*(x.bin[-1] + x.bin[-length(x.bin)])
  ymid <- 0.5*(y.bin[-1] + y.bin[-length(y.bin)])
  
  oldmar <- par("mar")
  par (mar = par("mar") + c(0, 0, 0, 2))
  hist3D(x = xmid, y = ymid, z = xy, ...,
         zlim = c(-max(z)/2, max(z)), zlab = "counts", bty= "g", 
         phi = phi, theta = theta,
         shade = 0.2, col = col, border = "black",
         d = 1, ticktype = "detailed",xlab=xlab,ylab=ylab)
  
  scatter3D(x, y,
            z = rep(-max(z)/2, length.out = length(x)),
            colvar = colvar, col = gg.col(100),
            add = TRUE, pch = 18, clab = clab,
            colkey = list(length = 0.5, width = 0.5,
                          dist = 0.05, cex.axis = 0.8, cex.clab = 0.8)
  )
  par(mar = oldmar)
}

setwd("/home/vassb/fingerprint_data/ansgar_att_six_forklift_att_merged/")
export_location="/home/vassb/fingerprint_data/ansgar_att_six_forklift_att_merged/"

file_list = list.files(pattern = '\\.csv$')

for(file in file_list)
{
  temp_df = as.data.frame(read_csv(file))
  temp_df_com = temp_df[complete.cases(temp_df), ]
  
  temp_df_com = temp_df_com  %>%
    mutate(
      steering_cat = case_when(
        abs(Steering_angle_angle) <=  30 ~ 1,
        abs(Steering_angle_angle) <=  60 ~ 2,
        TRUE ~ 3
      )
    )
    for(wl_wol in c(0,1)){
      for(s_type in 1:max(temp_df_com$steering_cat)){
        plot_df = temp_df_com %>%
          filter(steering_cat == s_type, is.weight == wl_wol )
        #Dynamic
        hist3D_fancy(x=plot_df$Speed_Drivemotor_1_RPM,y = plot_df$Torque_Drivemotor_1_Nm, xlab = "RPM", ylab ="Nm", breaks = 8,main = paste("Dynamic_",gsub(".csv","",file,fixed = TRUE),"_s_type:",s_type,"_wl_wol_",wl_wol,"_3Dplot.png",sep=""))
        png(filename = paste(export_location,"Dynamic_",gsub(".csv","",file,fixed = TRUE),"_s_type:",s_type,"_wl_wol_",wl_wol,"_3Dplot.png",sep=""))
      }
      plot_df = temp_df_com %>%
        filter(is.weight == wl_wol)
      #Stress
      hist3D_fancy(x=as.numeric(plot_df$date_time),y = plot_df$Torque_Drivemotor_1_Nm, breaks = 8, xlab = "RPM", ylab ="Nm", breaks = 8, main = paste("Stress_",gsub(".csv","",file,fixed = TRUE),"_wl_wol_",wl_wol,"_3Dplot.png",sep=""))
      png(filename = paste(export_location,"Stress_",gsub(".csv","",file,fixed = TRUE),"_wl_wol_",wl_wol,"_3Dplot.png",sep=""))
      
      #Travelling
      hist3D_fancy(x=as.numeric(plot_df$date_time),y = plot_df$Speed_Drivemotor_1_RPM, breaks = 8, xlab = "RPM", ylab ="Nm", breaks = 8, main = paste("Travelling_",gsub(".csv","",file,fixed = TRUE),"_wl_wol_",wl_wol,"_3Dplot.png",sep=""))
      png(filename = paste(export_location,"Travelling_",gsub(".csv","",file,fixed = TRUE),"_wl_wol_",wl_wol,"_3Dplot.png",sep=""))
  }
}

#PC
#IMRl_F_00214_att_merged <- as.data.frame(read_csv("/media/vasy/Data/Doksik/projekts/AITIA/can_fp_att_merged/IMRl_F_00214_att_merged.csv"))

#scatter3D(x = IMRl_F_00214_att_merged$Torque_Drivemotor_1_Nm,y=IMRl_F_00214_att_merged$Speed_Drivemotor_1_RPM, z= IMRl_F_00214_att_merged$Steering_angle_angle)
# points3D(x = IMRl_F_00214_att_merged$Torque_Drivemotor_1_Nm,y=IMRl_F_00214_att_merged$Speed_Drivemotor_1_RPM, z= IMRl_F_00214_att_merged$Steering_angle_angle)

#speed and torque 3D profile
# IMRl_F_00214_comp = IMRl_F_00214_att_merged[complete.cases(IMRl_F_00214_att_merged), ]
# #hist3D_fancy(x = IMRl_F_00214_comp$Torque_Drivemotor_1_Nm,y=IMRl_F_00214_comp$Speed_Drivemotor_1_RPM, colvar= IMRl_F_00214_comp$Steering_angle_angle, breaks = 8)
# # 
# # hist3D(x= range(IMRl_F_00214_att_merged$Torque_Drivemotor_1_Nm,finite = TRUE),y=range(IMRl_F_00214_att_merged$Speed_Drivemotor_1_RPM,finite = TRUE),z = as.matrix(IMRl_F_00214_att_merged$Torque_Drivemotor_1_Nm,IMRl_F_00214_att_merged$Speed_Drivemotor_1_RPM,IMRl_F_00214_att_merged$Steering_angle_angle))
# # 
# # tail(IMRl_F_00214_att_merged)
# # 
# # summary(IMRl_F_00214_att_merged)
# 
# #IMRl_F_00214_comp_100 = head(IMRl_F_00214_comp,100)
# #summary(IMRl_F_00214_comp_100000)
# 
# #TODO
# # double for cycle for file naming
# IMRL_steer = IMRl_F_00214_comp %>%
#   mutate(
#     steering_cat = case_when(
#         abs(Steering_angle_angle) <=  30 ~ 1,
#         abs(Steering_angle_angle) <=  60 ~ 2,
#         TRUE ~ 3
#       )
#   )
# 
# #summary(IMRL_steer)
# 
# hist3D_fancy(x=IMRL_steer$Speed_Drivemotor_1_RPM,y = IMRL_steer$Torque_Drivemotor_1_Nm, xlab = "Test", ylab ="Test", breaks = 8)
# #TODO export location and filename correction
# png(filename = paste(export_location,"3Dplot.png",sep=""))
# 
# hist3D_fancy(x=as.numeric(IMRL_steer$date_time),y = IMRL_steer$Torque_Drivemotor_1_Nm, breaks = 8)
# 
# hist3D_fancy(x=as.numeric(IMRL_steer$date_time),y = IMRL_steer$Speed_Drivemotor_1_RPM, breaks = 8)
















