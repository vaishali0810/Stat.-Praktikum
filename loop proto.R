

districtnames <- sort(unique(dfultimate$district))
districtnames




### nachbarkreise[[1]] schon sortiert

nachbarkreise[[1]]
identical(districtnames, nachbarkreise[[1]])
neighboring <- nachbarkreise[[2]]

emptylistinz <- list()
emptylistpop <- list()

neighboring <- lapply(neighboring, function(x) {unlist(strsplit(x, ", "))})

shortdf <- dfultimate %>% select(district, week, population, inzidenz) %>% arrange(district, week)
View(shortdf)

shortdf %>% filter(district == districtnames[1])

View(nachbarkreise)

for (i in 1:96) {
  temp1 <- neighboring[[i]]
  temp1 <- unlist(temp1)
  temp2 <- 0
  temp3 <- 0
  #print(temp1)
  for (j in seq_along(temp1)) {
    tempdf <- shortdf %>% filter(district == temp1[j])
    tempdf <- as.data.frame(tempdf)
    #print(tempdf)
    temp2 <- (temp2 + tempdf[, 4]*tempdf[3, 3])
    temp3 <- (temp3 + tempdf[3, 3])
  }
  #print(temp2)
  #print(temp3)
  emptylistinz[[i]] <- temp2
  emptylistpop[[i]] <- temp3
}
emptylistinz
emptylistpop

### sum(populationLK * InzidenzLK) / sum( population LK)


for ( i in 1:96) {
  emptylistinz[[i]] <- emptylistinz[[i]] / emptylistpop [[i]]
}




vectorinz <- unlist(emptylistinz)

dfultimate <- dfultimate %>% arrange(district, week)
dfultimate$weightednbinz  <- vectorinz




unweightedlist <- list()
for (i in 1:96) {
  temp1 <- neighboring[[i]]
  temp1 <- unlist(temp1)
  temp2 <- length(temp1)
  temp3 <- 0
  #print(temp1)
  for (j in seq_along(temp1)) {
    tempdf <- shortdf %>% filter(district == temp1[j])
    tempdf <- as.data.frame(tempdf)
    temp3 <- (temp3 + tempdf[, 4])
  }
  unweightedlist[[i]] <- (temp3/temp2)
}

vectorinz2 <- unlist(unweightedlist)
dfultimate$unweightednbinz <- vectorinz2

unlist(neighboring)
all(unlist(neighboring) %in% districtnames)
unlist(neighboring[which((unlist(neighboring) %in% districtnames))])
districtnames %in% neighboring




viewtest <- cbind(districtnames, sort(unlist(neighboring)))
nblist <- unlist(neighboring)
nblist <- sort(nblist)
nblist <- unique(nblist)
nblist
vector1234 <- nblist %in% districtnames
nblist[vector1234]

listnbnew <- list()
for (i in seq_along(neighboring)) {
  listnbnew[i]<-(all(neighboring[[i]] %in% districtnames))
}

l<-as.vector(listnbnew)
 View(neighboring[[3]])

 (neighboring[[3]] %in% districtnames)





