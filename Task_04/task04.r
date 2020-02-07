#Making out populations 
trueMean1 <- 5 
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

#Sample each of the populations 
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)

#Compare the samples. 
boxplot(Sample1, Sample2)
#They where a bit different sample2 had sightly smaller mean 

source("http://jonsmitchell.com/code/simFxn04.R")

#add the grandparents. use head() and nrow() to look at them 
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")

#We will make the the paternal grandparents make Alan
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)

#Alan and Brenda make a child
Focus <- makeBaby(Brenda, Alan)

#We can see how many genes came from any one known ancestor of Focus 
#use grep() it is liek crtl+F 
#I think it will be about half will be from Brenda
ToMom <- length( grep("mom", Focus ) ) / length( Focus )

#Each grandparents should be a 1/4 of focus. 
ToMomMom <- length( grep( "grandma_mom", Focus ) ) / length( Focus )
ToMomDad <- length( grep( "grandpa_mom", Focus ) ) /length( Focus )
#the number was .24365 which is very close to 1/4 
ToDad <- length( grep("da", Focus ) ) / length( Focus )
ToDadMom <- length( grep( "grandma_da", Focus ) ) / length( Focus )
ToDadDad <- length( grep( "grandpa_da", Focus ) ) /length( Focus )
#Focus is equally related to her mom and dad and equally related all four grandparents 

#Let us give Focus a sibling 
Sibling_01 <- makeBaby(Brenda, Alan)

#I would guess she is 1/2 equal to her sibling 
ToSib <- length( intersect( Focus, Sibling_01 ) ) / length( Focus )
#She is about 72 pecent related to her sibling which I did not thin k it woudl be that high

#Lets make it where they have 1,000 babies
#I would guess that focus woudl be randomly related to each of them it would not be a set percent
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby( Brenda, Alan) ) ) / length( Focus ) )

quantile(ManySiblings)
mean(ManySiblings)
#It was a variation between all the siblings, ranging from about 7% to 96% with an average of 50% 
#So you can be more related to one sibling than another

plot(density(ManySiblings), main="", x=lab="proportion shared genes")

#The vartion is probably do to the amount of loci each sibiling gets from their parents it is on average 50% but it could vary


#Hardy-Weinburg Eq
#we will set allele frequency to p and calculate the expected genotype frequencies 

HWE<- function(p) {
  aa <- p^2 
  ab <- 2 * p * (1 - p)
  bb <- (1 - p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)

#plot
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
#calculate genotype genotype frequencies for a bunch of allele
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))

#plot the known allele frequency (p) against our expected genotype frequencies (GenoFreq)
lines(p, GenoFreq[, "aa"], lwd=2, col="red")
#As allele freq. increases genotype freq. increases. As the popualtion raises both freq. increase. 
#this is not showing time or geographic space

lines(p, GenoFreq[, "ab"], lwd=2, col="purple")
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")

#lets simulate a poplaution, and look at how its allele and genotype frequencies vary 
Pop <- simPop(500)

#Add to HWE plot
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")

#yes the frequency of the aa genotype match expected from Hardy-Weinberg

#Lets do another simulation with a much smaller poplaution 
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")

#get the freq. of a small poplaution will have a larger variaty becuase less people to get a closer average 

#install the learnPopGen package 
library(learnPopGen)

x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
x <- genetic.drift(C=200, nrep=5, pause=0.01)
#The time and the p value changes a lot 

#how population size effects teh time to extinction for one allele 
#first, make a bunch of populations of different size, fom 5 to 50 individuals 
PopSizes <- 5:50 

#Next, put 5 populations with each given size 
Samples <- rep(PopSizes, 5)

#Now, simulate all 230 of those populatios and get the time one of two alleles went extinct 
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))

#to fit a line (Linear Model) to data in R use the lm() function 
Line <- lm(tExt ~ Samples)

#summary(), to see fit 
summary(Line)

#To add to plot use abline()
plot(Samples, tExt)
abline(Line)

Line2 <- lm( tExt~Samples + 0)
abline(Line2)
#As the population went up it took longer for an allele to go extinct 

#Extra Credit 
install.packages("packagename")
