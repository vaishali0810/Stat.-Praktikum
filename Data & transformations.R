rm(list=ls())
data <- readRDS("cases_GermanTemporal_2022-10-25.rds")
data <- data[, -(c(4, 7))]
data[, 5]<-as.Date(data[, 5])

dbayern <- data[data$state == "Bayern", ]

