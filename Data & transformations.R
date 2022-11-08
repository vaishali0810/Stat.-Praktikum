rm(list=ls())
data <- readRDS("cases_GermanTemporal_2022-10-25.rds")

# remove age_group_2 & reference date
data <- data[, -(c(4, 7))]
# formate date as date
data[, 5]<-as.Date(data[, 5])

#reduce data set to Bavaria
dbayern <- data[data$state == "Bayern", ]

# daten erklärung https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/45258e51f57d43efb612f700a876ae8f_0/about
