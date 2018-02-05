# att_merged visualization

library(readr)
library(purrr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(plot3D)

hist3D_fancy<- function(x, y, break.func = c("Sturges", "scott", "FD"), breaks = NULL,
                        colvar = NULL, col="white", clab=NULL, phi = 5, theta = 25, ...){
  
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
         d = 1, ticktype = "detailed")
  
  scatter3D(x, y,
            z = rep(-max(z)/2, length.out = length(x)),
            colvar = colvar, col = gg.col(100),
            add = TRUE, pch = 18, clab = clab,
            colkey = list(length = 0.5, width = 0.5,
                          dist = 0.05, cex.axis = 0.8, cex.clab = 0.8)
  )
  par(mar = oldmar)
}

IMRl_F_00214_att_merged <- as.data.frame(read_csv("/media/vasy/Data/Doksik/projekts/AITIA/can_fp_att_merged/IMRl_F_00214_att_merged.csv"))

scatter3D(x = IMRl_F_00214_att_merged$Torque_Drivemotor_1_Nm,y=IMRl_F_00214_att_merged$Speed_Drivemotor_1_RPM, z= IMRl_F_00214_att_merged$Steering_angle_angle)
points3D(x = IMRl_F_00214_att_merged$Torque_Drivemotor_1_Nm,y=IMRl_F_00214_att_merged$Speed_Drivemotor_1_RPM, z= IMRl_F_00214_att_merged$Steering_angle_angle)

IMRl_F_00214_comp = IMRl_F_00214_att_merged[complete.cases(IMRl_F_00214_att_merged), ]
hist3D_fancy(x = IMRl_F_00214_comp$Torque_Drivemotor_1_Nm,y=IMRl_F_00214_comp$Speed_Drivemotor_1_RPM, colvar= IMRl_F_00214_comp$Steering_angle_angle, breaks = 8)

hist3D(x= range(IMRl_F_00214_att_merged$Torque_Drivemotor_1_Nm,finite = TRUE),y=range(IMRl_F_00214_att_merged$Speed_Drivemotor_1_RPM,finite = TRUE),z = as.matrix(IMRl_F_00214_att_merged$Torque_Drivemotor_1_Nm,IMRl_F_00214_att_merged$Speed_Drivemotor_1_RPM,IMRl_F_00214_att_merged$Steering_angle_angle))

tail(IMRl_F_00214_att_merged)

summary(IMRl_F_00214_att_merged)