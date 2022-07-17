#Deliverable 1
library(dplyr)

MechaCar_table <- read.csv(file="MechaCar_mpg.csv")

model <- lm(mpg~AWD+vehicle_length+vehicle_weight+spoiler_angle+ground_clearance,MechaCar_table) #create linear model


summary(lm(model,MechaCar_table)) #summarize linear model


#Deliverable 2
Suspension_table <- read.csv(file="Suspension_Coil.csv")

total_summary <- Suspension_table  %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI), SD_PSI=sd(PSI),  .groups = 'keep') #create summary table

lot_summary <- Suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI), SD_PSI=sd(PSI),  .groups = 'keep') #create summary table



#Deliverable 3
t.test(Suspension_table$PSI,mu=1500)

lot1 <- subset(Suspension_table, Manufacturing_Lot=="Lot1")
lot2 <- subset(Suspension_table, Manufacturing_Lot=="Lot2")
lot3 <- subset(Suspension_table, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)
