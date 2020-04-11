data1 <- read.csv(file.choose(), header=TRUE)
plot(data1$Factors, data1$Pair.Formations, xlab="Species Factors", ylab="Pair Formations", main="Species mating" )
