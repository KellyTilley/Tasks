setwd("C:\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_02")
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

#save the file as a CSV that so I can read back into R 
write.csv(beren3, "beren_new.csv", quote=F, row.names=FALSE)



# Task 02b
#Question 1: the first hypothesis was inappropiate becuase his weight was
#not calculated and the second hypothesis the amount he drinks can be 
#before a nap making the total milk drank different from the nap time. 
setwd("C:\\Users\\godfr\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_02")
beren3 <- read.csv("beren_new.csv", stringsAsFactors = F)

Feeds <- which(beren3$event == "bottle")
#TO find out how much Beren eats at a sitting 
avgMilk <- mean(beren3$value[Feeds])

#WHat are the units for this avgMilk? Oz
#Why did I use the "Value" column? Because it tells teh oz of milk drank.
#Yes [] is important for extracting specfic data from a vector

#tapply() function takes some data (for us the Value) and 
#some treatment(for us age in days) and applies some other function to those data 
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)

varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)

#The cor() function tell me teh correlation between two sets of numbers
#?cor is the cor help page
cor(beren3$value[Feeds], beren3$age[Feeds])
#cor.test() function will conduct test for type of correlation you pick
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
#summary() will tell me the p-value and other summaries 
berenCor

#~ the function of 
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])

#use boxplot function to plot data
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab = "amount of milk consumed (oz)")

#par() function to edit details about the plot
#?par is the help page for it
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
#To add a horizontal line to indicate the average amount of milk consumed each day
abline(h=mean(totalFeed), lty=2, col='red')
#to save the graph as a PDF 
pdf("r02b-totalMilkByDay.pdf", height = 4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
#Question 2: the data is random showing no correlation 
