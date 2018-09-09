courier = read.delim2("C:\\Users\\rosen_000\\Dropbox\\GW\\4-Senior Year\\Programming for Analytics\\HW\\A2\\courier.txt", sep=",")
colnames(courier) = c("CourierName","Pickup Time", "Delivery Time", "Mileage", "Cost")
courier$Cost = as.numeric(courier$Cost)

#Creating aggregare Variables
TotalTime=as.vector(courier$`Pickup Time`+courier$`Delivery Time`)
courier$Total_Time = TotalTime

#Breaking data out by courier
C1 = courier[which(courier$CourierName==1),]
C2 = courier[which(courier$CourierName==2),]
C3 = courier[which(courier$CourierName==3),]


#Cost per mile will be the slope of the lm line and the y int will be the flat rate.
###Looking at C1
with(C1, plot(C1$Cost~C1$Mileage, ylab = "Cost", xlab = "Milage Travelled", main = "Cost per Mile of C1"))
Cost_per_Mile_C1 = lm(C1$Cost~C1$Mileage)
abline(Cost_per_Mile_C1, col="blue", lwd=1)
summary(Cost_per_Mile_C1)
anova(Cost_per_Mile_C1)

with(C1, plot(C1$Total_Time~C1$Mileage, ylab = "Total Time", xlab = "Milage Travelled", main = "Total minutes per mile of C1 "))
totalMinutes_per_Mile_C1 = lm(C1$Total_Time~C1$Mileage)
abline(totalMinutes_per_Mile_C1, col="blue", lwd=1)
summary(totalMinutes_per_Mile_C1)
anova(totalMinutes_per_Mile_C1)

with(C1, plot(C1$`Delivery Time`~C1$Mileage, ylab = "Total Time", xlab = "Milage Travelled", main = "Travel minutes per mile of C1 "))
travelMinutes_per_Mile_C1 = lm(C1$`Delivery Time`~C1$Mileage)
abline(travelMinutes_per_Mile_C1, col="blue", lwd=1)
summary(travelMinutes_per_Mile_C1)
anova(travelMinutes_per_Mile_C1)


with(C1, plot(C1$Cost~C1$Total_Time, ylab = "Cost", xlab = "Minutes", main = "Cost per Minute of C1"))
Cost_per_Minute_C1 = lm(C1$Cost~C1$Total_Time)
abline(Cost_per_Minute_C1, col="blue", lwd=1)
summary(Cost_per_Minute_C1)
anova(Cost_per_Minute_C1)


###Looking at C2
with(C2, plot(C2$Cost~C2$Mileage, ylab = "Cost", xlab = "Milage Travelled", main = "Cost per Mile of C2"))
Cost_per_Mile_C2 = lm(C2$Cost~C2$Mileage)
abline(Cost_per_Mile_C2, col="Red", lwd=1)
summary(Cost_per_Mile_C2)
anova(Cost_per_Mile_C2)

with(C2, plot(C2$Total_Time~C2$Mileage, ylab = "Total Time", xlab = "Milage Travelled", main = "Total minutes per mile of C2 "))
totalMinutes_per_Mile_C2 = lm(C2$Total_Time~C2$Mileage)
abline(totalMinutes_per_Mile_C2, col="Red", lwd=1)
summary(totalMinutes_per_Mile_C2)
anova(totalMinutes_per_Mile_C2)

with(C2, plot(C2$`Delivery Time`~C2$Mileage, ylab = "Total Time", xlab = "Milage Travelled", main = "Travel minutes per mile of C2 "))
travelMinutes_per_Mile_C2 = lm(C2$`Delivery Time`~C2$Mileage)
abline(travelMinutes_per_Mile_C2, col="Red", lwd=1)
summary(travelMinutes_per_Mile_C2)
anova(travelMinutes_per_Mile_C2)

with(C2, plot(C2$Cost~C2$Total_Time, ylab = "Cost", xlab = "Minutes", main = "Cost per Minute of C2"))
Cost_per_Minute_C2 = lm(C2$Cost~C2$Total_Time)
abline(Cost_per_Minute_C2, col="Red", lwd=1)
summary(Cost_per_Minute_C2)
anova(Cost_per_Minute_C2)


###Looking at C3
with(C3, plot(C3$Cost~C3$Mileage, ylab = "Cost", xlab = "Milage Travelled", main = "Cost per Mile of C3"))
Cost_per_Mile_C3 = lm(C3$Cost~C3$Mileage)
abline(Cost_per_Mile_C2, col="Green", lwd=1)
summary(Cost_per_Mile_C3)
anova(Cost_per_Mile_C3)

with(C3, plot(C3$Total_Time~C3$Mileage, ylab = "Total Time", xlab = "Milage Travelled", main = "Total minutes per mile of C3 "))
totalMinutes_per_Mile_C3 = lm(C3$Total_Time~C3$Mileage)
abline(totalMinutes_per_Mile_C3, col="Red", lwd=1)
summary(totalMinutes_per_Mile_C3)
anova(totalMinutes_per_Mile_C3)

with(C3, plot(C3$`Delivery Time`~C3$Mileage, ylab = "Total Time", xlab = "Milage Travelled", main = "Total minutes per mile of C3 "))
travelMinutes_per_Mile_C3 = lm(C3$`Delivery Time`~C3$Mileage)
abline(travelMinutes_per_Mile_C3, col="Red", lwd=1)
summary(travelMinutes_per_Mile_C3)
anova(travelMinutes_per_Mile_C3)

with(C3, plot(C3$Cost~C3$Total_Time, ylab = "Cost", xlab = "Minutes", main = "Cost per Minute of C3"))
Cost_per_Minute_C3 = lm(C3$Cost~C3$Total_Time)
abline(Cost_per_Minute_C3, col="Green", lwd=1)
summary(Cost_per_Minute_C3)
anova(Cost_per_Minute_C3)

print("Cost per Mile")
print(Cost_per_Mile_C1$coefficients)
print(Cost_per_Mile_C2$coefficients)
print(Cost_per_Mile_C3$coefficients)

print("Cost per Minute")
print(Cost_per_Minute_C1$coefficients)
print(Cost_per_Minute_C2$coefficients)
print(Cost_per_Minute_C3$coefficients)

print("Delivery Minutes per Mile")
print(Minutes_per_Mile_C1$coefficients)
print(Minutes_per_Mile_C2$coefficients)
print(Minutes_per_Mile_C3$coefficients)

print("TOtal Minutes per Mile")
print(totalMinutes_per_Mile_C1$coefficients)
print(totalMinutes_per_Mile_C2$coefficients)
print(totalMinutes_per_Mile_C3$coefficients)
