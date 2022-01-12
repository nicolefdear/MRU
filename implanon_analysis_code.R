#Implanon study
#Created by: Nicole Dear
#Created on: 2019-04-05

install.packages("tidyverse")
install.packages("lubridate")
install.packages("praise")
install.packages("dplyr")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("ggpubr")
install.packages("gplots")
library(tidyverse)
library(lubridate)
library(praise)
library(dplyr)
library(corrplot)
library(Hmisc)
library(ggpubr)
library(gplots)

setwd("H:/Implanon")

#import dataset
a<-read.csv("ImplanonRemovalStudy_DATA_2019-04-05.csv")
names(a)

#format dates
a$date <- as.Date(a$date, format="%Y-%m-%d")
a$q14_insrt_date <- as.Date(a$q14_insrt_date, format="%Y-%m-%d")
a$q22_imp_duration_rmvdate <- as.Date(a$q22_imp_duration_rmvdate, format="%Y-%m-%d")

#date variables only
#b<-a[c(1:3,98,123:124)]

#time interval between insertion and removal (day of interview)
a$time.interval <- a$q14_insrt_date %--% a$date
a$duration_yrs<-as.duration(a$time.interval)/dyears(1)

a$duration_cat[a$duration_yrs<0.5]<-1
a$duration_cat[a$duration_yrs>=0.5&a$duration_yrs<=1]<-2
a$duration_cat[a$duration_yrs>1&a$duration_yrs<1.9]<-3
a$duration_cat[a$duration_yrs>=1.9&a$duration_yrs<2.9]<-4
a$duration_cat[a$duration_yrs>=2.9]<-5

table(a$duration_cat)

#subset dataset for only numeric variables
b <- dplyr::select_if(a, is.numeric)

#subset for numeric single response variables
names(b)
c <- b[c(3:7, 9:13, 82:85, 99, 105:108, 139:141, 143:145)]
c <- c[complete.cases(c), ]

#correlation coeffs and p-values
c.cor <- as.data.frame(cor(c))
write.csv(c.cor, "corrmatrix_implanon.csv")

c.rcorr<- rcorr(as.matrix(c))
c.rcorr
c.coeff <- c.rcorr$r
c.p <- c.rcorr$P

#plot Correlogram
c.cor <- cor(c)
corrplot(c.cor)

#plot heatmap
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = c.cor, col = palette, symm = TRUE)