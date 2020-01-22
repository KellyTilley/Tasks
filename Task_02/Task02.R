setwd("C:\\Users\\godfr\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_02")
# This reads in the file off the internet
Data <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)

#When I do this it saves the data file to my working directory to look at in Excel 
write.csv(Data, "rawdata.csv", quote=F)

#length() is a funcation that, when applied to an object, tells you how many numbers are in it
length (Data)

#nrow() and ncol() are functions that tell you the number of rows and colums in an object 
nrow(Data)
ncol(Data)

#colnames() will list the names of the colums
colnames(Data)
#Some columns have straightforward names (like "genus"), but others are werid.
#So what kind of data are in each column? the date, time, amount, event and caregiver.
#head() will let you look at the first six rows for all columns in the file
head(Data)

#We can use square brackets, [], to acess subsets of our object. 
#Run the following lines one at a time. Then compare the output of the head () function above. 
#What does each one show? They show how to collect certain colums and rows. 
Data[1,]

Data[2,]

Data[1:3,]

Data[1:3, 4]

Data[1:5, 1:3]

#how would you find the data of the 257th observation? Data[257,]

#this is how to find which events are bottles 
Feeds <- which(Data[,9] == "bottle")

#I will store the data into a new object called berenMilk
berenMilk <- Data[Feeds,]
#head() will let you look at the first six rows for all columns in the file
head(berenMilk)
#how many rows are there in this new object? 12
#how does each row represent? year, month, day, dotw, start hour, start minute, end hur, end minute, event, value, units, caregiver

#To subset the file using the column names
Feeds <- which(Data[,"event"] == "bottle")
#Another way to subset the file using column names with $ 
Feeds <- which(Data$event == "bottle")
#I tried both and got the same results

dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse="-"))
dataID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")

#To record how many days old the son is for each observation.
#He was born on April 18th and he was weighed on April 20th 
#April 20th - April 18th should give his age as 2
Data$age <- dataID - dataID[which(Data$event == "birth")]
head(Data)

#make a copy of the object 
beren2 <- Data
#I need to reorganize that object so it sorts by date
#order() function to age column will do that 
#also, subset teh object (beren2) by that order
beren3 <- beren2[order(beren2$age),]

#save teh file as a CSV that so I can read back into R 
write.csv(beren3, "beren_new.csv", quote=F, row.names=FALSE)