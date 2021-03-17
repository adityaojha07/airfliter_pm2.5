install.packages("data.table")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("readxl")
install.packages("stringr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("timeSeries")
install.packages("plyr")
install.packages("ggpubr")
install.packages("forecast")

library(forecast)
library(dplyr)
library(plyr)
library(ggpubr)
library(timeSeries)
library(stringr)
library(openxlsx)
library(readxl)
library(openxlsx)
library(zoo)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)

rm(list = ls())
lc <- "C:/Users/shukl/Downloads/Book3.xlsx" #Reading file
#Cleaning the data
lines = read.xlsx(lc)
x <- data.table(lines)
# colnames extracting
col_nm <- names(x)
col_nm
c1 <- as.character(x[1])
c2 <- as.character(x[2])

c3 <- paste0(c1, c2)
c4 <- sapply(3:length(c3), function(u){
  
  if(u %in% c(3:6)){
    
   v <- paste0("inside_",c3[u])
  }
  else{
    
    v <- paste0("outside_",c3[u])
  }
    
 return(v) 
  
  
})
c5 <- c(c3[1:2], c4) # col_names
c6 <- str_replace_all(c5, " ", "_") # col_names
f <- gsub("\\)","", gsub("\\(", "" ,x = c6)) # col-names _removing (, ) pattern
g <-str_replace(f, "\\/","per") # final_col_names  
fin_clnm <- g
#settingcol_names
setnames(x, col_nm, fin_clnm)

#removing first two row
new_x <- x[-c(1:2)]

#Changing the date and time to a single column
new_x <- new_x[ ,DateNA := as.Date(DateNA, "%Y-%m-%d")]

new_x <- new_x[, TimeNA := ifelse(TimeNA %in% "0am","12am",TimeNA)]
new_x <- new_x[, TimeNA := format(strptime(new_x$TimeNA, format = "%I%p"), format = "%H:%M:%S")]
# creating date column
# h takes all date values
h <- as.Date(unlist(lapply(seq(1,3168, by = 24), function(u){
  
  v <- rep(new_x$DateNA[u],24)
  return(v)
})))

y <- copy(new_x)
y <- y[, Date :=  h]

# changing column types to numeric
#3:10 we have to change col_type
y[ ,3:9] <- y[, lapply(y[,3:9], function(u){
  v <- as.numeric(u)
  return(v)
})]
# checking structure
str(y)
#parsing date time column

y <-  y[, Date_time := as.POSIXct.default(paste(Date, TimeNA),  
                                          format ="%Y-%m-%d %H:%M:%S") ]

str(y)
#Arranging the columns in the required 
y  <- y[,c(1,2,12,3:11)] 
y <- y[, Date:= NULL]
#------
fin_data <- y[,-c(1,2)]
new_dat <- copy(fin_data)
new_dat <- new_dat[,-c(8,9)]
new_dat <- na.omit(new_dat) #final data in which we have to perform operation


#apply ing time series to inside pm2.5 data
# checking stationarity by gparph

ggplot(data = new_dat, aes(Date_time, inside_PM_2.5ugperm3Hourly_AVG))+
  geom_line(col ="red")

#variance and mean constant hence data is not staionarity

# difference one time

new_dat1 <- new_dat[,c(1,2)]
new_dat1 <- new_dat1[, Diff:= c(NA, diff(inside_PM_2.5ugperm3Hourly_AVG))]
new_dat1 <- na.omit(new_dat1)
ggplot(data = new_dat1, aes(Date_time, 
                            Diff))+  geom_line(col= 'red')
# now mean and variance is conatant hence data is stationarity

# applyig timseries arima model
attach(new_dat1)
mod1 <- auto.arima(inside_PM_2.5ugperm3Hourly_AVG)
mod1

prd <- forecast(mod1, h = 24) #next 24 hours forecast
prd


#second model linear regression

cor(new_dat$outside_PM_2.5ugperm3Hourly_AVG, new_dat$inside_PM_2.5ugperm3Hourly_AVG)
#scatter plt inside vs outside
ggplot(data = new_dat, aes(outside_PM_2.5ugperm3Hourly_AVG, inside_PM_2.5ugperm3Hourly_AVG,))+
  geom_point(col ="red")

trainingRowIndex <- 1:2100  # row indices for training data
#new_dat <- [,c(2,6)]
trainingData <- new_dat[trainingRowIndex]# model training data
testData  <- new_dat[-trainingRowIndex ]   # test data

# Build the model on training data -
lmMod <- lm(inside_PM_2.5ugperm3Hourly_AVG ~ outside_PM_2.5ugperm3Hourly_AVG, data = trainingData)  # build the model
summary(lmMod)
#R squared value is 0.20 hnece model is not good for prediction time series model is better

Pred <- predict(lmMod, testData)  # pred  test data
View(Pred)
predict(lmMod, data.frame(outside_PM_2.5ugperm3Hourly_AVG = 35)) #for given Value


#accuracy for air filter by chencking preductage redcution of particulate matter day wiese

ac_dat <- copy(new_dat)
#adding new column accuracy that shows percentage reduction
ac_dat <- ac_dat[, accuracy := (outside_PM_2.5ugperm3Hourly_AVG- inside_PM_2.5ugperm3Hourly_AVG)*100/
                   outside_PM_2.5ugperm3Hourly_AVG]

#plot consist of date vs outside and inside pm2.5 and their percenatge reduction
p <- ggplot(data = ac_dat, aes(Date_time, inside_PM_2.5ugperm3Hourly_AVG))+
  geom_line(col = "red")+ geom_line(data = ac_dat, aes(Date_time, outside_PM_2.5ugperm3Hourly_AVG),col = "blue")

p <- p + geom_line(aes(Date_time, accuracy), col = "green")  

p <- p + ggtitle("Date_Time vs PM_2.5ugperm3Hourly_AVG") +
  xlab("Date_Time")  + ylab("PM_2.5ugperm3Hourly_AVG")
p




#group by daily data of all particulate matter
daliy_dat <- dlply(ac_dat, .(date(Date_time)), function(u){
  
  
  return(u)
  
})


daliy_dat$`2020-11-24`
# or
daliy_dat["2020-11-24"]

View(daliy_dat$`2020-11-26`)

#list of day wise plot of date time vs inside, outside and reduction in pm2.5 
d_plot <- lapply(daliy_dat, function(u){
  
  p <- ggplot(data = u, aes(Date_time, inside_PM_2.5ugperm3Hourly_AVG))+
    geom_line(col = "red")+ geom_line(data = u, aes(Date_time, outside_PM_2.5ugperm3Hourly_AVG),col = "blue")
  
  p <- p + geom_line(aes(Date_time, accuracy), col = "green")  
  
  p <- p + ggtitle("Date_Time vs PM_2.5ugperm3Hourly_AVG") +
    xlab(paste("Date_Time",date(u[1,1])) ) + ylab("PM_2.5ugperm3Hourly_AVG")
  
  return(p)    
})


d_plot$`2020-11-26`
#or
d_plot["2021-02-17"]

View(daliy_dat$`2020-11-26`)
