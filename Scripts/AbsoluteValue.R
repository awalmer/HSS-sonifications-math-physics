#----------------------------------------------------#
## Highcharts Sonification Studio Tutorial
## Create a data set for absolute value sonification
#----------------------------------------------------#

# Set Directory:
# setwd("[/directory name here]")

# Create Data Set:
absv <- data.frame("x"=seq(-50, 50, by=.5))
absv$negative_x <- ifelse(absv$x<=0, abs(absv$x), NA)
absv$positive_x <- ifelse(absv$x>=0, abs(absv$x), NA)
absv$zero <- ifelse(absv$x==0, 0, NA)

# Export .csv file:
#write.csv(absv, 
#          "[/directory name here/file_name_here.csv]", 
#          row.names=FALSE, fileEncoding="UTF-8", na="")
