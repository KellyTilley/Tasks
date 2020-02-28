source("http://jonsmitchell.com/code/reformatData07.R")
source("http://jonsmitchell.com/code/simFxn.R")

#make a plot of each allele's frequency over time 
plot(1, 1, type="n", xlim=c(1998, 2013), ylim=c(0, 1))
s <- apply(overallFreq, 2, function(x) lines(overallFreq[,1], x, col=rgb(0, 0, 0, 0.01)))

#I will make a plot of each allele's rescaled frequency (observed - initial freq) over time 
rescaleFreq <- apply(overallFreq[,3:ncol(overallFreq)], 2, function(x) x - x[1])
plot(1, 1, type="n", xlim=c(1998, 2013), ylim=c(-0.25, 0.25))
s <- apply(rescaleFreq, 2, function(x) lines(overallFreq[,1], x, col=rgb(0, 0, 0, 0.01)))

#instead of plotting the individual allele frequencies, I will use all of this data to plot the probability of a given change in frequecy (y) by year (x) 
#Pretty much same plot as before, but instead of semitransparent see-through lines, this gives color coded probability density
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin=100)

#This smoothScatter() plot is a complete summary of how likely a given change in allele frequency is over a given period of time in this population. 
#Changing the ubin argument makes it smoother or more pixelated 
#Using simPop, I can simulate populations and stick them on this graph 
#addFit() helps this function runs a specified number (nurns) of simPop simulations with this empirical data 

smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin=100, xlab="year", ylab="change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, startT = 1997, simCol = "gray40", rescale = TRUE)

#Find the combination of n, h, and s for addFit() that produces changes comparable to what is sen in the scurb jay population 
#I can also find the use of the d_(change in frequency) and n_(number od individuls) columns useful for determining the relative strength of the different 
#evolutionary forces here. The plot below is one example of how they can be examined

plot(alleleFreqs$d_freq, alleleFreqs$d_imm, xlim=c(-0.15, 0.15), xlab="overall freq. change", ylab="freq. chage in subset")
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col='blue')
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')



