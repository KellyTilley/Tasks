library("ape")
library("maps")
library("phytools")

#I am going to be making evoutionary trees. 
#This is how to input trss and plot it 
#Only this week we plot trees like this this is how they are stored

text.string <-
  "(((((((cow, pig), whale),(bat,(lemur,human))),(robin,iguana)), coelacanth),(gold_fish, trout)),shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)

#The number of nodes is what seprates two lineages reflects how distantlyrelated they are 

nodelabels(frame="circle", bg='white', cex=1)

#you can tell how distance two lineages are looking at when the most recent node (Their most recent common ancestor).
#An example, for humans and lemurs, you hit node number 21. so both humans and lemurs are descended from node 21, and no other organism are.
#Whlaes and humans are both descended from node 17, but iguanas are not, so whales and humans are closer related than either are to iguanas. 

#Question 1: sharks are closer to goldfish than humans, they are both decended from node 14

#The object created in memory when we simulate or estimate a phylogeny, or read one from input file, is a list of class "phylo". 
#*A list is just a customizable object type that can combine different objects of different types. 
#Example, a list might have a vector of real numbers (with mode "numeric") as its first element;
#and then a vector of strings (with mode "character") as its second element; and so on. 
#Assigning our tree with a special class, "phylo", is just a convenient way to tell special functions in R how to treat them as objects.

#An object in class "phylo" has at least three parts. 
#These are normally hidden, for instance, just typing the name of your "phylo" object does not give uou the structure in memory 


#Let's look at the object 
vert.tree

#Question 2: There are no branch length in this tree

str(vert.tree)

#Now let's dig into the phylo object. I will use a simpler tree to explore this 
tree<-read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree,offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)

#tip "E" is numbered 5, it is a descended from node 6.
#tip number 4 is decended from node number 9. 

#to call them all use 
tree$tip.label

#The first element is "A"  and it is listed as tip #1. That is how the ip labels are linked to the edge matrix of the tree.

#If we look at into the phylo object's edge component, we can see the structure of the phylogeny as a matrix 
tree$edge 

#Each line one the phylogeny is called an "edge". 
#Each row of the tree$edge corresponds to one of the lines (edge) of the phylogeny. 
#The first number is where the line starts, the second number is where the line ends 
#The first row shows the first line starts at number 6 and goes to number 7 

#Now we'll use a real phylogeny of Anolis lizards (yay they are cute)
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))

#This tree is different, this one has a lengths associated with each edge. 
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab=" edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))


tipsEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

#Each edge has length, and they're in order. So the first value of edge length is the length of the edge defined by teh first row of the edge matrix. 

?plot.phylo

#Question 3
plot.phylo(AnolisTree, show.tip.label = FALSE)

#Question 4
plot.phylo(AnolisTree, type="fan")

#Question 5 
plot.phylo(AnolisTree, tip.color = "red")

#Question 6-8
tipsEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
shortest <- which.min(Lengths)

plot(drop.tip(AnolisTree, shortest))


ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)

#The Line never goes down. It seems the number of lineages goes up as time goes up.

#Question 10
fit.bd(AnolisTree, rho = 0.2)

install.packages("treebase")

