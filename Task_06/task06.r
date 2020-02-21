library(learnPopGen)
coalescent.plot()
#n number of haploid individuals or gene copies.
#ngen number of generations 
coalescent.plot(n=25,ngen=25,col.order="alternating")

#Questions 
#Question 1: There are 10 alleles in each simulation begains with. 
#Queston 1: To modify the number use coalescent.plot(n=20,ngen=30,col.order="alternating") and change the n 
#Question 2: I ran the test 10 times with 10 alleles and got an average of 14.6
#Question3: On average the number of offspring each haploiod has is 1 
#Question 3: There is not much variation in the number of offspring the lowest I have seen is 0 and the highest is 4, but usually averages out to 1
#Question 4: It starts out with multipe different genotypes, but only the most important ones survive weeding out the others 
#QUestion 5: No 


install.packages("coala")
install.packages("phytools")
library("coala")
library("phytools")
#Setting up a model 
#I will use a smaple of 5 individuals from 1 population. 
#Each individual will have 10 loci 
#Each locus will be 500 base pairs long. 
#And there will be two copies per individual (diloid individuals) 
#We'll also add some features 
#mutation and recombination, at fixed rates
#Finally, we'll summarize the output by showing pedigrees for each ocus, and overall diversity 
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 
                      500, ploidy = 2) +
  feat_mutation(10) +
  feat_recombination(10) +
  sumstat_trees() +
  sumstat_nucleotide_div()

#To run the simulation we can run more than one simulation for these same parameters by changing nsim
stats <- simulate(model, nsim =1)

#Each locus has a measure of genetic diversity called "pi". pi is a standard measure. 
#It is the average number of differences at a locus between any two individuals 
Diversity <- stats$pi

#Not all the numbers are the same 
#Mutation 

#Each SNP in each locus has its own ancestry tree. 
Nloci <- length(stats$trees)

#Look at the first SNP for the first locus 
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()

#Each copy of a given locus around today is given a number. The tree pattern shows the pedigree connecting the copies of this locus present today (t=0)
#Question 6: There are 5 individuals and they are inheriting two loci one from parents making 10 loci  

#We can find out the age the most recent ancestor for this SNP on this locus for these indivduals lived by looking at how deep in time the tree goes.
Agel <- max(nodeHeights(t1))

#now let's look at the first SNP of the second locus 
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()

#The most common ancestor for both are at 1
#Question 7: No they do not match 
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()

#I can compare the trees quite explictly, and see how the patterns of descent and timing differ between these two SNPs from two different loci 
compare.chronograms(t1_1, t1_2)

#Now make more comparisons
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)

#I can also leverage the power of R to compare all of the SNPs from all of the loci all at once 
for (locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n == 1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}

#Now I will plot all of the trees at once
par(mfrow=c(1,1))
densityTree(outPhy)

#Yes this code is differernt then what I predicted 

#Finally I can specify very complicated models 
#Here mutation rate varies in each of 40 simulations 
model3 <- coal_model(10, 50) +
  feat_mutation(par_prior("theta", sample.int(100, 1))) +
  sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)

mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])

plot(mean_pi)
axisPhylo()
plot(theta)
axisPhylo()
plot(mean_pi, theta)
abline(lm(mean_pi ~ theta))

#EXtra Credit
model1 <- coal_model(sample_size = c(5, 2), loci_number =1) +
  feat_migration(rate = 0.5, symmetric = TRUE) +
  feat_pop_merge(0.5, 3) +
  feat_pop_merge(0.8, 2)

