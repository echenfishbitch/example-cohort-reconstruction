###############################################
############## CWT Reconstruction #############
###############################################
library(dplyr)
library(tidyr)
setwd("~/example-cohort-reconstruction")

Escapement_SG_Bootstrap<-readRDS("CWTBootstraps.Rds") #Escapement by Year and Age using CWT expansions, for 1000 iterations
Cohort<-Escapement_SG_Bootstrap 
To_Hatchery<-read.csv("Escapement to Hatchery.csv") #Hatchery Escapement
River_Harvest<-read.csv("River Harvest.csv")
Recruits<-read.csv("Hatchery Release.csv") #Released
#Combining escapement info for each brood year
for(i in 1:1000){
Cohort[[i]]<-Cohort[[i]] %>% 
  left_join(Recruits, by = "brood_year") %>%#merging with recruits
  select(brood_year, Individuals_Released, Age1Sp, Age2Sp,Age3Sp,Age4Sp,Age5Sp) %>% #Together
  filter(brood_year != 1998 & brood_year != 1999& brood_year != 2000) %>%
  left_join(To_Hatchery, by="brood_year") %>%
  left_join(River_Harvest,by="brood_year") %>%
  select(-run_year)
Cohort[[i]][is.na(Cohort[[i]])] <- 0
}

#Impact of fishing
Fishing_Impact<-readRDS("Impact Bootstrap.Rds") #Monthly resample of impacted individuals from each cohort
for(i in 1:1000){ #For Monthly
Cohort[[i]]<-left_join(Cohort[[i]],Fishing_Impact[[i]],by="brood_year")
Cohort[[i]]<-Cohort[[i]] %>%
  select(c("brood_year","Individuals_Released","Age1Sp","Age2Sp","Age3Sp","Age4Sp","Age5Sp","Age1Hat","Age2Hat","Age3Hat","Age4Hat", "InRiver3", "InRiver4",
           "Apr3","May3","Jun3","Jul3","Aug3","Sept3","Oct3","Nov3","Apr4","May4","Jun4","Jul4","Aug4","Sept4","Oct4","Nov4"))
Cohort[[i]][is.na(Cohort[[i]])] <- 0
}
test<-CWT[[1]]

###############################################
########## *Let the fun begin * ###############
###############################################
#Age.Start of Month (includes fish that will die naturally that month 
#but not fished)
for (i in 1:1000){
# 5 Year Old Spawners (monthly)
Cohort[[i]]$Age6.2<-Cohort[[i]]$Age5Sp/(1-0.0184)
Cohort[[i]]$Age6.1<-Cohort[[i]]$Age6.2/(1-0.0184)
Cohort[[i]]$Age5.12<-Cohort[[i]]$Age6.1/(1-0.0184)
Cohort[[i]]$Age5.11<-Cohort[[i]]$Age5.12/(1-0.0184)
Cohort[[i]]$Age5.10<-Cohort[[i]]$Age5.11/(1-0.0184)
Cohort[[i]]$Age5.9<-Cohort[[i]]$Age5.10/(1-0.0184)
Cohort[[i]]$Age5.8<-Cohort[[i]]$Age5.9/(1-0.0184)
Cohort[[i]]$Age5.7<-Cohort[[i]]$Age5.8/(1-0.0184)
Cohort[[i]]$Age5.6<-Cohort[[i]]$Age5.7/(1-0.0184)
Cohort[[i]]$Age5.5<-Cohort[[i]]$Age5.6/(1-0.0184)
Cohort[[i]]$Age5.4<-Cohort[[i]]$Age5.5/(1-0.0184)
Cohort[[i]]$Age5.3<-Cohort[[i]]$Age5.4/(1-0.0184)

#4 Year Old Spawners (monthly)
Cohort[[i]]$Age5.2<-(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age5.3+Cohort[[i]]$Age4Hat+Cohort[[i]]$InRiver4)/(1-0.0184)

#Third Year at Sea. 4 year olds are caught. (monthly)
Cohort[[i]]$Age5.1<-Cohort[[i]]$Age5.2/(1-0.0184)
Cohort[[i]]$Age4.12<-Cohort[[i]]$Age5.1/(1-0.0184)
Cohort[[i]]$Age4.11<-Cohort[[i]]$Age4.12/(1-0.0184)+Cohort[[i]]$Nov4
Cohort[[i]]$Age4.10<-Cohort[[i]]$Age4.11/(1-0.0184)+Cohort[[i]]$Oct4
Cohort[[i]]$Age4.9<-Cohort[[i]]$Age4.10/(1-0.0184)+Cohort[[i]]$Sept4
Cohort[[i]]$Age4.8<-Cohort[[i]]$Age4.9/(1-0.0184)+Cohort[[i]]$Aug4
Cohort[[i]]$Age4.7<-Cohort[[i]]$Age4.8/(1-0.0184)+Cohort[[i]]$Jul4
Cohort[[i]]$Age4.6<-Cohort[[i]]$Age4.7/(1-0.0184)+Cohort[[i]]$Jun4
Cohort[[i]]$Age4.5<-Cohort[[i]]$Age4.6/(1-0.0184)+Cohort[[i]]$May4
Cohort[[i]]$Age4.4<-Cohort[[i]]$Age4.5/(1-0.0184)+Cohort[[i]]$Apr4
Cohort[[i]]$Age4.3<-Cohort[[i]]$Age4.4/(1-0.0184)

#3 Year Old Spawners and River Harvest(monthly)
Cohort[[i]]$Age4.2<-(Cohort[[i]]$Age3Sp+Cohort[[i]]$Age4.3+Cohort[[i]]$Age3Hat+Cohort[[i]]$InRiver3)/(1-0.0184)

#Second Year at Sea. 3 year olds are caught.(monthly) 
Cohort[[i]]$Age4.1<-Cohort[[i]]$Age4.2/(1-0.0184)
Cohort[[i]]$Age3.12<-Cohort[[i]]$Age4.1/(1-0.0184)
Cohort[[i]]$Age3.11<-Cohort[[i]]$Age3.12/(1-0.0184)+Cohort[[i]]$Nov3
Cohort[[i]]$Age3.10<-Cohort[[i]]$Age3.11/(1-0.0184)+Cohort[[i]]$Oct3
Cohort[[i]]$Age3.9<-Cohort[[i]]$Age3.10/(1-0.0184)+Cohort[[i]]$Sept3
Cohort[[i]]$Age3.8<-Cohort[[i]]$Age3.9/(1-0.0184)+Cohort[[i]]$Aug3
Cohort[[i]]$Age3.7<-Cohort[[i]]$Age3.8/(1-0.0184)+Cohort[[i]]$Jul3
Cohort[[i]]$Age3.6<-Cohort[[i]]$Age3.7/(1-0.0184)+Cohort[[i]]$Jun3
Cohort[[i]]$Age3.5<-Cohort[[i]]$Age3.6/(1-0.0184)+Cohort[[i]]$May3
Cohort[[i]]$Age3.4<-Cohort[[i]]$Age3.5/(1-0.0184)+Cohort[[i]]$Apr3
Cohort[[i]]$Age3.3<-Cohort[[i]]$Age3.4/(1-0.0184)

#2 Year Old Spawners and River Harvest (monthly)
Cohort[[i]]$Age3.2<-(Cohort[[i]]$Age2Sp+Cohort[[i]]$Age3.3+Cohort[[i]]$Age2Hat)/(1-0.0561)

#First Year at Sea (monthly)
Cohort[[i]]$Age3.1<-Cohort[[i]]$Age3.2/(1-0.0561)
Cohort[[i]]$Age2.12<-Cohort[[i]]$Age3.1/(1-0.0561)
Cohort[[i]]$Age2.11<-Cohort[[i]]$Age2.12/(1-0.0561)
Cohort[[i]]$Age2.10<-Cohort[[i]]$Age2.11/(1-0.0561)
Cohort[[i]]$Age2.9<-Cohort[[i]]$Age2.10/(1-0.0561)
Cohort[[i]]$Age2.8<-Cohort[[i]]$Age2.9/(1-0.0561)
Cohort[[i]]$Age2.7<-Cohort[[i]]$Age2.8/(1-0.0561)
Cohort[[i]]$Age2.6<-Cohort[[i]]$Age2.7/(1-0.0561)
Cohort[[i]]$Age2.5<-Cohort[[i]]$Age2.6/(1-0.0561)
Cohort[[i]]$Age2.4<-Cohort[[i]]$Age2.5/(1-0.0561)
Cohort[[i]]$Age2.3<-Cohort[[i]]$Age2.4/(1-0.0561)

#Outmigration Survival
Cohort[[i]]$Out_Survival<-Cohort[[i]]$Age2.3/Cohort[[i]]$Individuals_Released
#Estimating Maturation
Cohort[[i]]$Mat4<-(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$InRiver4)/(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$Age5.3+Cohort[[i]]$InRiver4)
Cohort[[i]]$Mat3<-(Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat+Cohort[[i]]$InRiver3)/(Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat+Cohort[[i]]$Age4.3+Cohort[[i]]$InRiver3)
Cohort[[i]]$Mat2<-(Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat)/(Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat+Cohort[[i]]$Age3.3)

#Estimating Impact Rate
Cohort[[i]]$Imp4<-rowSums(Cohort[[i]][,22:29])/Cohort[[i]]$Age4.3
Cohort[[i]]$Imp3<-rowSums(Cohort[[i]][,14:21])/Cohort[[i]]$Age3.3
Cohort[[i]]$Imp3_4<-Cohort[[i]]$Apr3/Cohort[[i]]$Age3.4
Cohort[[i]]$Imp3_5<-Cohort[[i]]$May3/Cohort[[i]]$Age3.5
Cohort[[i]]$Imp3_6<-Cohort[[i]]$Jun3/Cohort[[i]]$Age3.6
Cohort[[i]]$Imp3_7<-Cohort[[i]]$Jul3/Cohort[[i]]$Age3.7
Cohort[[i]]$Imp3_8<-Cohort[[i]]$Aug3/Cohort[[i]]$Age3.8
Cohort[[i]]$Imp3_9<-Cohort[[i]]$Sept3/Cohort[[i]]$Age3.9
Cohort[[i]]$Imp3_10<-Cohort[[i]]$Oct3/Cohort[[i]]$Age3.10
Cohort[[i]]$Imp3_11<-Cohort[[i]]$Nov3/Cohort[[i]]$Age3.11

Cohort[[i]]$Imp4_4<-Cohort[[i]]$Apr4/Cohort[[i]]$Age4.4
Cohort[[i]]$Imp4_5<-Cohort[[i]]$May4/Cohort[[i]]$Age4.5
Cohort[[i]]$Imp4_6<-Cohort[[i]]$Jun4/Cohort[[i]]$Age4.6
Cohort[[i]]$Imp4_7<-Cohort[[i]]$Jul4/Cohort[[i]]$Age4.7
Cohort[[i]]$Imp4_8<-Cohort[[i]]$Aug4/Cohort[[i]]$Age4.8
Cohort[[i]]$Imp4_9<-Cohort[[i]]$Sept4/Cohort[[i]]$Age4.9
Cohort[[i]]$Imp4_10<-Cohort[[i]]$Oct4/Cohort[[i]]$Age4.10
Cohort[[i]]$Imp4_11<-Cohort[[i]]$Nov4/Cohort[[i]]$Age4.11
Cohort[[i]][is.na(Cohort[[i]])]<-0 #data frame
}
saveRDS(Cohort, file = "CWT Cohort Reconstruction.Rds")
####
Maturation_Uncertainty_Bootstrap<-array(NA, c(length(Cohort[[1]]$brood_year),3,1000))
for(i in 1:1000){
   Maturation_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Mat2
   Maturation_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Mat3
   Maturation_Uncertainty_Bootstrap[,3,i]<-Cohort[[i]]$Mat4
}
Maturation_Uncertainty<-matrix(nrow = length(Cohort[[1]]$brood_year), ncol=9)
for(i in 1:length(Cohort[[1]]$brood_year)){
  for(j in 1:3){
    Maturation_Uncertainty[i,c(1+(j-1)*3,2+(j-1)*3,3+(j-1)*3)]<-quantile(Maturation_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Maturation_Uncertainty[i,2]<-mean(Maturation_Uncertainty_Bootstrap[i,1,])
    Maturation_Uncertainty[i,5]<-mean(Maturation_Uncertainty_Bootstrap[i,2,])
    Maturation_Uncertainty[i,8]<-mean(Maturation_Uncertainty_Bootstrap[i,3,])
  }
}
brood_year<-c(2001:2017)
Maturation_Uncertainty<-as.data.frame(cbind(brood_year,Maturation_Uncertainty))
names(Maturation_Uncertainty)<-c("brood_year","Mat2Lower","Mat2Mean",  "Mat2Upper", "Mat3Lower", "Mat3Mean","Mat3Upper", "Mat4Lower", "Mat4Mean","Mat4Upper")
Maturation_Uncertainty <- Maturation_Uncertainty %>%
   mutate(Source = "Hatchery_CWT")
write.csv(Maturation_Uncertainty,"Maturation_Uncertainty_CWT.csv", row.names = FALSE)

Impact_Uncertainty_Bootstrap<-array(NA, c(length(Cohort[[1]]$brood_year),3,1000))
for(i in 1:1000){
  Impact_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Imp3
  Impact_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Imp4
}
Impact_Uncertainty<-matrix(nrow = 17, ncol=6)
#5,50,95 quantile
for(i in 1:17){
  for(j in 1:2){
    Impact_Uncertainty[i,c(1+(j-1)*3,2+(j-1)*3,3+(j-1)*3)]<-quantile(Impact_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Impact_Uncertainty[i,j*j+1]<-mean(Impact_Uncertainty_Bootstrap[i,j,])
  }
}
brood_year<-c(2001:2017)
Impact_Uncertainty<-as.data.frame(cbind(brood_year,Impact_Uncertainty))
names(Impact_Uncertainty)<-c("brood_year","Imp3Lower","Imp3Mean",  "Imp3Upper", "Imp4Lower", "Imp4Mean","Imp4Upper")
write.csv(Impact_Uncertainty,"Impact_Uncertainty_CWT.csv", row.names = FALSE)
