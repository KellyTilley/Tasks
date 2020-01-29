Nap <- which(beren3$event == "nap")

Starttime <- ((beren3$start_hour[Nap]) * 60) + (beren3$start_minute[Nap])

endtime <- ((beren3$end_hour[Nap]) * 60) + (beren3$end_minute[Nap])

durnap <- endtime - Starttime

berenANOVA <- aov(durnap ~ beren3$caregiver[Nap])

boxplot(durnap ~ beren3$caregiver[Nap], xlab= "who put beren to sleep", ylab = "Duration of nap")
