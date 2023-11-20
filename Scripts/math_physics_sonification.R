## Basic Math & Physics Data Sonification Exploration ##
## June/July 2023 | Auralee Walmer ##

# Set Up #
# setwd("[/directory name here]")


## Create Data ##

# Normal Distribution
nd <- data.frame("x"=seq(1,100, by = .1), "y"=NA)
nd$y <- dnorm(nd$x, mean = 50, sd = 10)*1000
plot(nd$x,nd$y, main = "Normal Distribution", col = "blue")
nd$z_markers <- NA
z_indices <- c(which(nd$x=='2.4'),
               which(nd$x=='13.5'),
               which(nd$x=='34'),
               which(nd$x=='50'),
               which(nd$x=='66'),
               which(nd$x=='86.5'),
               which(nd$x=='97.6')
               )
nd$z_markers[z_indices] <- c(2.4, 13.5, 34, 50, 66, 86.5, 97.6)
nd$z_markers_tick <- NA
nd$z_markers_tick[z_indices] <- c(1,1,1,1,1,1,1)
nd$z_score <- NA
nd$z_score[z_indices] <- c(-3,-2,-1,0,1,2,3)
nd$mean <- NA
nd$mean[which(nd$x==50)] <- nd$y[which(nd$x==50)]


write.csv(nd, 
          "Data/normal_distribution.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

# Skewed Distribution
skewR <- data.frame("x"=seq(0,1,by=0.01),"y"=NA)
skewR$y <- dbeta(skewR$x, shape1 = 2, shape2 = 5)
plot(skewR$x,skewR$y, main = "Skewed Distr", col = "blue")
skewR$mean <- ifelse(skewR$y==skewR$y[which.min(abs(skewR$y - mean(skewR$y)))],
                     mean(skewR$y), NA
                     )
skewR$median <- ifelse(skewR$y==median(skewR$y), median(skewR$y), NA)
write.csv(skewR, 
          "Data/skewed_right_distribution.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


# Exponential Example
exp2 <- data.frame("x"=seq(-4,20, by = .1), "y"=NA)
exp2$y <- 2^(exp2$x)
plot(exp2$x,exp2$y, main = "Exponential, Y=2^x", col = "blue")
exp2$zero_tick <- NA
exp2$zero_tick[which(exp2$x==0)] <- 1
write.csv(exp2, 
          "Data/powers_of_2.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


# Powers of X?

# Logarithmic
# Logarithm Base 10 (Common Log)
log <- data.frame("x"=seq(.15, 30, by=.15),"y"=NA)
log$y <- log10(log$x)
plot(log$x,log$y, main = "Log Base 10 (Common)", col = "blue")
log$y_below_zero <- ifelse(log$y<0, log$y, NA)
log$y_above_zero <- ifelse(log$y>=0, log$y, NA)
log$past_zero_tick <- ifelse(log$x=='1.05', 1, NA)
write.csv(log, 
          "Data/logarithm_base10.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


# Abs Val
av <- data.frame("x"=seq(-50, 50, by=.5),"y"=abs(seq(-50, 50, by=.5)))
plot(av$x,av$y, main = "Absolute Value", col = "blue")
av$zero <- ifelse(av$x==av$x[which(av$x==0)], 0, NA)
write.csv(av, 
          "Data/absolute_value.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


# Gravity
grav <- data.frame("time_seconds"=seq(0,8,by=.1),
                   "velocity"=NA)
grav$velocity <- 9.81*grav$time_seconds
sec_index <- c(1,2,3,4,5,6,7,8)
which(grav$time_seconds %in% sec_index)
grav$second_ticker <- NA
grav$second_ticker[which(grav$time_seconds %in% sec_index)] <- 1
grav$free_fall_distance <- (.5)*9.81*(grav$time_seconds^2)
# initial velocity = 0, time = 8 seconds, distance in meters
grav <- grav[c(2,4,3,1)]

write.csv(grav, 
          "Data/gravity.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

# Comparison of different accelerations?
# play them sequentially
data.frame("time_seconds"=seq(0,8,by=.1),
           "velocity_accel5"=NA,
           "velocity_accelgrav"=NA,
           "velocity_accel20"=NA,
           "velocity_accel48"=NA # ~ 100 mph think of car
           )

acc = m/(s^2)
acc*(s^2) = m


## --------- 6/30 onward continuation (next meeting 7/5) ------- ##

# Data sets to create:
# y = x, y= x^2, y = x^3 (same data set sequential sonifications)
# natural log
# abs val with y split into diff columns for -x and +x
# skewed left
# exponential adjusted to extend to 30


# Skewed Left Distribution
skewL <- data.frame("x"=seq(0,1,by=0.01),"y"=NA)
skewL$y <- dbeta(skewL$x, shape1 =5, shape2 =2)
plot(skewL$x,skewL$y, main = "Skewed Distr", col = "blue")
skewL$mean <- ifelse(skewL$y==skewL$y[which.min(abs(skewL$y - mean(skewL$y)))],
                     mean(skewL$y), NA
)
skewL$median <- ifelse(skewL$y==median(skewL$y), median(skewL$y), NA)
write.csv(skewL, 
          "Data/skewed_left_distribution.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


# Exponential 2^x, with longer ranging x to 30
exp2 <- data.frame("x"=seq(-4,30, by = .1), "y"=NA)
exp2$y <- 2^(exp2$x)
plot(exp2$x,exp2$y, main = "Exponential, Y=2^x", col = "blue")
exp2$zero_tick <- NA
exp2$zero_tick[which(exp2$x==0)] <- 1
write.csv(exp2, 
          "Data/powers_of_2_xto30.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

# Powers of x
powersX <- data.frame("x"=seq(-10,10,by=0.1),"y=x"=NA, "y=x^2"=NA, "y=x^3"=NA)
powersX$y.x = powersX$x
powersX$y.x.2 = (powersX$x)^2
powersX$y.x.3 = (powersX$x)^3
write.csv(powersX, 
          "Data/powers_of_x.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


# Natural Log
nlog <- data.frame("x"=seq(.1, 50, by=.15),"y"=NA)
nlog$y <- log(nlog$x)
plot(nlog$x,nlog$y, main = "Natural Log", col = "blue")
nlog$y_below_zero <- ifelse(nlog$y<0, nlog$y, NA)
nlog$y_above_zero <- ifelse(nlog$y>=0, nlog$y, NA)
nlog$past_zero_tick <- ifelse(nlog$x=='1.05', 1, NA)
nlog <- nlog[c(1,3:5)]

write.csv(nlog, 
          "Data/natural_logarithm.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

# Abs Val pre-0 and post-0 split
av2 <- data.frame("x"=seq(-50, 50, by=.5),"y"=abs(seq(-50, 50, by=.5)))
plot(av2$x,av2$y, main = "Absolute Value", col = "blue")
av2$zero <- ifelse(av2$x==av2$x[which(av2$x==0)], 0, NA)
av2$pre0 <- ifelse(av2$x<0, av2$y, NA)
av2$post0 <- ifelse(av2$x>0, av2$y, NA)
av2 <- av2[c(1,3:5)]
write.csv(av2, 
          "Data/absolute_value_2.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


## gravity 2
grav2 <- data.frame("time_seconds"=seq(0,10,by=.1),
                   "velocity"=NA)
grav2$velocity <- 9.81*grav2$time_seconds
sec_index <- c(1,2,3,4,5,6,7,8,9,10)
grav2$second_ticker <- NA
grav2$second_ticker[which(grav2$time_seconds %in% sec_index)] <- 1
grav2$free_fall_distance <- (.5)*9.81*(grav2$time_seconds^2)
grav2$distance_negative <- grav2$free_fall_distance*(-1)
write.csv(grav2, 
          "Data/gravity2.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

## gravity on earth versus mars
grav_earthvmars <- data.frame("time_seconds"=seq(0,10,by=.1),
                              "distance_earth"=NA,
                              "distance_mars"=NA
                              )
grav_earthvmars$second_ticker <- NA
grav_earthvmars$second_ticker[which(grav_earthvmars$time_seconds %in% sec_index)] <- 1
grav_earthvmars$distance_earth <- (.5)*9.81*(grav_earthvmars$time_seconds^2)
grav_earthvmars$distance_mars <- (.5)*3.71*(grav_earthvmars$time_seconds^2)
grav_earthvmars$distance_earth <- round(grav_earthvmars$distance_earth, digits=2)
grav_earthvmars$distance_mars <- round(grav_earthvmars$distance_mars, digits=2)
grav_earthvmars$neg_dist_earth <- grav_earthvmars$distance_earth*(-1)
grav_earthvmars$neg_dist_mars <- grav_earthvmars$distance_mars*(-1)
write.csv(grav_earthvmars, 
          "Data/gravity_earthvmars.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


## NASA Data
NIRCam_NGC3132_1 <- read.csv("Data/NASA Data/NIRCam/jw02733_20230620t062646_pool_MAST_2023-07-04T1714.csv",
                             sep="|")
NIRSpec_1 <- read.csv("Data/NASA Data/NIRSpec/jw02072_20230526t220301_pool.csv",
                             sep="|")
NIRSpec_2 <- read.csv("Data/NASA Data/NIRSpec/jw02072_20230526t220301_pool 2.csv",
                      sep="|")

NIRSpec_3 <- read.csv("Data/NASA Data/NIRSpec/jw02288_20230527t003136_pool.csv",
                      sep="|")
NIRSpec_4 <- read.csv("Data/NASA Data/NIRSpec/jw01539_20230530t212510_pool.csv",
                      sep="|")
# query link:
# https://mast.stsci.edu/portal/Mashup/Clients/Mast/Portal.html?searchQuery=%7B%22service%22%3A%22CAOMFILTERED%22%2C%22inputText%22%3A%5B%7B%22paramName%22%3A%22instrument_name%22%2C%22niceName%22%3A%22instrument_name%22%2C%22values%22%3A%5B%22NIRISS%2FSOSS%22%5D%2C%22valString%22%3A%22NIRISS%2FSOSS%22%2C%22isDate%22%3Afalse%2C%22facetType%22%3A%22discrete%22%2C%22displayString%22%3A%22NIRISS%2FSOSS%22%7D%2C%7B%22paramName%22%3A%22dataproduct_type%22%2C%22niceName%22%3A%22dataproduct_type%22%2C%22values%22%3A%5B%22spectrum%22%5D%2C%22valString%22%3A%22spectrum%22%2C%22isDate%22%3Afalse%2C%22facetType%22%3A%22discrete%22%2C%22displayString%22%3A%22spectrum%22%7D%2C%7B%22paramName%22%3A%22t_obs_release%22%2C%22niceName%22%3A%22t_obs_release%22%2C%22values%22%3A%5B%7B%22min%22%3A60019.8999%2C%22max%22%3A60494.83902768%7D%5D%2C%22valString%22%3A%22%5B60019.8999%2C%2060494.83902768%5D%22%2C%22isDate%22%3Afalse%2C%22facetType%22%3A%22numeric%22%2C%22min%22%3A60019.8999%2C%22max%22%3A60494.83902768%2C%22displayString%22%3A%22%5B60019.8999%2C%2060494.83902768%5D%22%7D%5D%2C%22position%22%3A%22undefined%2C%20undefined%2C%20undefined%22%2C%22paramsService%22%3A%22Mast.Caom.Filtered%22%2C%22title%22%3A%22MAST%3A%20%20Advanced%20Search%203%22%2C%22tooltip%22%3A%22NIRISS%2FSOSS%3B%20spectrum%3B%20%5B60019.8999%2C%2060494.83902768%5D%3B%20%22%2C%22columns%22%3A%22*%22%2C%22columnsConfig%22%3A%22Mast.Caom.Cone%22%7D



