# Find which events are naps.
#Make a new object (beren4) that’s just beren3 subset by naps
beren4 <- which(beren3$event == "nap")
#Find the start times for each nap (remember to convert minutes appropriately!)
Starttime <- ((beren3$start_hour[beren4]) * 60) + (beren3$start_minute[beren4])
#find the end times for ech nap
endtime <- ((beren3$end_hour[beren4]) * 60) + (beren3$end_minute[beren4])
#Subtract the end and start times to get the totaltime spent sleeping for each nap
durnap <- endtime - Starttime
#find the sum total amount of time napped each day
avgnap <- mean(durnap[beren4])
varnap <- tapply(durnap, beren3$age[beren4], var)
totalnap <- tapply(durnap, beren3$age[beren4], sum)
numnap <- tapply(durnap, beren3$age[beren4], length)
#Plot the total-slept-time against the day 
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalnap)), totalnap, type="b", pch=16, xlab="total slept time", ylab="day")
abline(h=mean(totalnap), lty=2, col='red')
#perform a correlation test between the time the nap starts and its total duration.
NapCor <- cor.test(Starttime, durnap)