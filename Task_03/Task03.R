#install.packages("paleobioDB", dep = T)
library(paleobioDB)
#Must use scientific term for taxon 
Taxon <- "Dinosauria"
#The min_ma and max_ma argumnts control the time_window that you are pulling fossils from in millions of years
MinMA <- 66 
MaxMA <- 252
fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Resoulution (Res) will define a time period of 5Ma
Res <- 5
nspeciesOverTime <- pbdb_richness(fossils, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)

#Change the plot to this 
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
#NOTE lower case l not 1
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main = Taxon)

#Get the appearance Data
newspeciesOverTime <- pbdb_orig_ext(fossils, res=5, rank="species", temporal_extent=c(MinMA, MaxMA))

#setup the plot
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))

#plot the first appearances 
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main = Taxon)

#Line for the last appearances 
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,2], col='red')

#to add a legend 
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")


#Looking through space not time 
#set color for the ocenas and the land on the map
OceanCol <- "light blue"
LandCol <- "black"

#go to colorbreer2.org to pick colors 
Cols <- c('#636363','#1c9099','#fb6a4a','#a50fl5','#dd1c77')
Cols <- "red"

#To make a map 
par(las=0)
pbdb_map_richness(fossils, col.ocean=OceanCol, col.int = LandCol, col.rich=Cols)
#COOL

#Use timescale to look at where dinosaur fossils have been found for different periods 
#For a Geological Timescale: https://www.geosociety.org/documents/gsa/timescale/timescl.pdf
#Get all the Triassic fossils
MinMA <- 201
MaxMA <- 252
triassic_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Jurassic fossils
MinMA <- 145
MaxMA <- 201
jurassic_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#Cretaceous fossil
MinMA <- 66
MaxMA <- 145
cretaceous_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#how to make a series of maps 
dev.new(height = 7.8, width = 13)
pbdb_map_richness(triassic_fossils, col.ocean=OceanCol, col.int = LandCol, col.rich=Cols)
mtext(side = 3, "Triassic (252 - 201Ma)", cex=3, line=-2) 

dev.new(height = 7.8, width = 13)
pbdb_map_richness(jurassic_fossils, col.ocean=OceanCol, col.int = LandCol, col.rich=Cols)
mtext(side = 3, "Jurassic (201 - 145Ma)", cex=3, line=-2) 

dev.new(height = 7.8, width = 13)
pbdb_map_richness(cretaceous_fossils, col.ocean=OceanCol, col.int = LandCol, col.rich=Cols)
mtext(side = 3, "Cretaceous (145 -66Ma)", cex=3, line=-2) 


#TO compare two groups
Taxon2 <- "Mammalia"
MinMA <- 66
MaxMA <- 252
fossils2 <- pbdb_occurrences(base_name = Taxon2, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

nspeciesOverTime2 <- pbdb_richness(fossils2, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)

#plot both grouos together 
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
Col_dino <- Cols[length(Cols)]
Col_mammal <- Cols[1]
LineWidth <-2 
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", col=Col_dino, lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime2)), nspeciesOverTime2[,2], col = Col_mammal, lwd=LineWidth)
legend("topleft", legend=c(Taxon, Taxon2), col=c(Col_dino, Col_mammal), bty="n", lwd=LineWidth)

#Extension
MinMA <- 0
MaxMA <- 541

# sea stars
Taxon3 <- "Echinodermata"
fossils3 <- pbdb_occurrences(base_name = Taxon3, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime3 <- pbdb_richness(fossils3, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)

# ducks
Taxon4 <- "Anseriformes"
fossils4 <- pbdb_occurrences(base_name = Taxon4, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime4 <- pbdb_richness(fossils4, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)

par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
LineWidth <-2 
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime3)), nspeciesOverTime3[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", col="blue", lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime4)), nspeciesOverTime4[,2], col = "red", lwd=LineWidth)

legend("topleft", legend=c(Taxon3, Taxon4), col=c("blue", "red"), bty="n", lwd=LineWidth)
#I hypohesis that the ducks and the seas stars was not together at the same time 
#(which I know is wrong but that is my data is showing) 
#extra credit 
library(rinat)
ducks <- get_inat_obs(query = "Mallard Duck")
head(ducks[,c(4:6)])
vp_obs <- get_inat_obs(query = "vernal pool")
head(vp_obs$species_guess)
bounds <- c(38.44047,-125,40.86652, -121.837)
duck <- get_inat_obs(query = "Mallard Duck", bounds = bounds)
m_obs <- get_inat_obs(query="Mallard Duck")
head(get_inat_obs_user(as.character(m_obs$user_login[1]),maxresults=20))[,1:5]
m_obs <- get_inat_obs(taxon_name = "Anas platyrhynchos")
duck_map <- inat_map(m_obs,plot=FALSE)
duck_map + borders("state") + theme_bw() + xlim(-125, -65) + ylim(25, 50)
