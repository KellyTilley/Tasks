#I am going to unite data and phylogenies into one grand thing
#First, I will set my working directory to your working folder 
#Now, we need to tell R that we want to use the read.tree() function

plot(tree, type="fan")

#Question 1: There are 5 tips and branch lengths are present  

#Now, let's read in the data. It's in the file svl.cvs. 
#SVL stands for "snout-vent length", and it's a standad measure of how long a vertebrate is 

data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names=1)

#Question 2: "data" has information on differents types of vertibrates and there corresponding snout-vent length.
dim(data)

#Now I am going to convert the object into a vector 
svl <- setNames(data$svl, rownames(data))

#Now, I have an evolutinary tree of these lizards, and the body size of the living species.
#Let's reconstruct the ancestral states. That is, let's estimate how large the ancestors were using the phylogeny and the data, 

Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)

#Examine the Ancestors object and the help file for fastAnc
?fastAnc

#Question 3
#Question 4

#Plot all of this togther

#First, plot the tree
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)

#Now put points instead of names at the tips. 
#The size of the points will be proportional to teh size of the lizard. 
#To do this, use tiplabels(), but tell it to plot points instead of words 
#(using the pch parameter), and to scale the points by the size od each lizard using the parameter cex. 
#Make sure the tips match the points by reorganizing the svl vector using the tip labels of the tree using square brackets.
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])

#Now add the ancestral states to the tree using nodelabels(), as each node
nodelabels(pch=16, cex=0.25*Ancestors$ace)

#Looking at the points, yu can see SOME variation in size but it's hard to tell how 
#large or small any given tip is relative to its ancestors or even to other tips.
#So let's try visualizing this data in a different way 

obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7,0.9))

#Now, I am going to add some fossils in using data from "Amber fossils demonstrate deep-time stability of Caribbean lizard communities" by Emma Sherratt et al. 2015 in PNAS

#Take the measurements and the node closest to each fossil species 
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("aliniger", "aliniger", "occultus", "christophei", "cristatellus", "occultus"), tip2=c("chlorocyanus", "coelestinus", "monticola", "cybotes", "angusticeps", "angusticeps"))

#What we'll do is: for each fossil, find what node corresponds to the Most Recent Common Ancestor (MRCA) of the pair of tips in the dataframe. 

#QUestion 5:

fossilNodes <- c()
nodeN <-c()

Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i] <- fossilData[i, "svl"]
nodeN[i] <- Node

names(fossilNodes) <- nodeN

#Now estimate the ancestral states, but this time use the fossils to contrain teh estimates at the relevant nodes 

Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)

#Question 7
#Question 8-10