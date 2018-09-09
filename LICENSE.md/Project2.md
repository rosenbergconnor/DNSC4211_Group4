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

###########################
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
#############################
#Printing Coef.
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
############################
#Combine Plots With LM lines
with(C1, plot(courier$Cost~courier$Mileage, ylab = "Cost", xlab = "Milage Travelled", main = "Cost per Mile", col=courier$CourierName))
abline(Cost_per_Mile_C1, col="Black", lwd=1)
abline(Cost_per_Mile_C2, col="Red", lwd=1)
abline(Cost_per_Mile_C3, col="Green", lwd=1)

with(courier, plot(courier$Cost~courier$Total_Time, ylab = "Cost", xlab = "Total Delivery Time", main = "Cost per Minute", col=courier$CourierName))
abline(Cost_per_Minute_C1, col="Black", lwd=1)
abline(Cost_per_Minute_C2, col="Red", lwd=1)
abline(Cost_per_Minute_C3, col="Green", lwd=1)

with(courier, plot(courier$Total_Time~courier$Mileage, ylab = "Total Delivery Time", xlab = "Milage", main = "Total Time per Mile", col=courier$CourierName))
abline(totalMinutes_per_Mile_C1, col="Black", lwd=1)
abline(totalMinutes_per_Mile_C2, col="Red", lwd=1)
abline(totalMinutes_per_Mile_C3, col="Green", lwd=1)

with(courier, plot(courier$`Delivery Time`~courier$Mileage, ylab = "Delivery Time", xlab = "Milage", main = "Delivery Time per Mile", col=courier$CourierName))
abline(Minutes_per_Mile_C1, col="Black", lwd=1)
abline(Minutes_per_Mile_C2, col="Red", lwd=1)
abline(Minutes_per_Mile_C3, col="Green", lwd=1)

####################################3
#Plotting Average Milage of the Company's couriers
hist(courier$Mileage,breaks =10 ,main = "Histogram Plot of Mileage",xlab="Total Mileage",ylab = "Frequency",las=1,col="blue")
mean(courier$Mileage) 
abline(v=mean(courier$Mileage),col="red",lwd=2) #Plotting the mean on the histogram 

#Histograms of Arrival Times of Each Courier Service 
##C1##
hist(C1$`Pickup Time`,breaks=10,main="Histogram Plot of c1 Pickup Time",xlab="Pickup Time",ylab="Frequency",las=1)
mean(C1$`Pickup Time`) #15.52
abline(v=mean(C1$`Pickup Time`),col="green",lwd=2) #Plotting the mean on the histogram 

##C2##
hist(C2$`Pickup Time`,breaks=10,main="Histogram Plot of c2 Pickup Time",xlab="Pickup Time",ylab="Frequency",las=1,col="red")
mean(C2$`Pickup Time`) #15.4
abline(v=mean(C2$`Pickup Time`),col="blue",lwd=2) #Plotting the mean on the histogram 

##c3##
hist(C3$`Pickup Time`,breaks=10,main="Histogram Plot of c3 Pickup Time",xlab="Pickup Time", ylab="Frequency",las=1,col="green")
mean(C3$`Pickup Time`) #15.18
abline(v=mean(C3$`Pickup Time`),col="red",lwd=2) #Plotting the mean on the histogram

#Standard Deviations of Pickup Times
sd(C1$`Pickup Time`) #6.45
sd(C2$`Pickup Time`) #5.88
sd(C3$`Pickup Time`) #4.79

#Standard Deviations of Delivery Times
sd(C1$`Delivery Time`) #16.09
sd(C2$`Delivery Time`) #13.63
sd(C3$`Delivery Time`) #15.12


########################
#1)Discount Pickup times becasue they have the same mean and close SDs
#2)Looked at delivery time per mile and saw C3, Green, was significanly above C1 and C2 within the 30 mile range
#Becasue of that we eliminated C3
#SD of the delivery time has C2, Red, at the lowest making it the most reliable
#3)Looked at Cost per mile to compare C1, Black, and C2, Red, and saw that Red was cheeper than black at all points within the given range.

