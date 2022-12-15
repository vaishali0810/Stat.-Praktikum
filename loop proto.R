

districtnames <- unique(dfultimate$district)
districtnames






nachbarkreise[[1]]
identical(districtnames, nachbarkreise[[1]])
neighboring <- nachbarkreise[[2]]

emptylistinz <- list()
emptylistpop <- list()

neighboring <- lapply(neighboring, function(x) {unlist(strsplit(x, ", "))})

shortdf <- dfultimate %>% select(district, week, population, inzidenz)
View(shortdf)

shortdf %>% filter(district == districtnames[1])

for (i in 1:96) {
  temp1 <- neighboring[[i]]
  temp2 <- 0
  temp3 <- 0
  print(temp1)
  for (j in seq_along(temp1)) {
    tempdf <- shortdf %>% filter(district == temp1[i])
    tempdf <- as.data.frame(tempdf)
    print(tempdf)
    temp2 <- (temp2 + tempdf[, 4])
    temp3 <- (temp3 + tempdf[3, 3])
  }
  print(temp2)
  print(temp3)
  emptylistinz[[i]] <- temp2
  emptylistpop[[i]] <- temp3
}
emptylistinz
emptylistpop

whfor(i in 1:96) {
  print(is.vector(neighboring[[i]]))
}


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
  print(all(neighboring[[i]] %in% districtnames))
}










