###############################################
############## CWT Reconstruction #############
#################CWT Data Prep#################
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(lubridate)

setwd("~/example-cohort-reconstruction")
#############################################
#####        Release data           #########
#############################################
CWT_Releases<-read.csv("CWTReleased.csv")

#turning NAs into 0 for addition
CWT_Releases$cwt_1st_mark_count[is.na(CWT_Releases$cwt_1st_mark_count)] <- 0 
CWT_Releases$cwt_2nd_mark_count[is.na(CWT_Releases$cwt_2nd_mark_count)] <- 0
CWT_Releases$non_cwt_1st_mark_count[is.na(CWT_Releases$non_cwt_1st_mark_count)] <- 0
CWT_Releases$non_cwt_2nd_mark_count[is.na(CWT_Releases$non_cwt_2nd_mark_count)] <- 0

#Phi is the proportion of released fish that have CWT and Ad Clips
#Fish with CWT and Ad Clip: cwt_1st_mark_count
#Fish with CWT and No clip: cwt_2nd_mark_count
#Fish with no CWT and Ad Clip: non_cwt_1st_mark_count
#Fish with no CWT and No Clip: non_cwt_2nd_mark_count
CWT_Releases$Total_Released<-(CWT_Releases$cwt_1st_mark_count+ CWT_Releases$cwt_2nd_mark_count+ CWT_Releases$non_cwt_1st_mark_count+ CWT_Releases$non_cwt_2nd_mark_count)
CWT_Releases$Phi <- CWT_Releases$cwt_1st_mark_count/CWT_Releases$Total_Released
names(CWT_Releases)[7]<-"tag_code"
Phi_by_tag<-CWT_Releases %>% #phi for each batch
  select(tag_code, Phi)
#############################################
#####      Reading recovery data    #########
#############################################
CWT_Recoveries<-read.csv("CWTRecoveries.csv")
CWT_Recoveries<- CWT_Recoveries %>%
  select(reporting_agency,run_year,period, recovery_date, fishery, sex, tag_code, estimated_number, recovery_location_name, brood_year)

Phi_by_tag<-CWT_Releases %>% #phi for each batch
  select(tag_code, Phi)
#merging Phi batch info with recovery data
CWT_Recoveries<-left_join(CWT_Recoveries, Phi_by_tag, by="tag_code")

CWT_Recoveries <- CWT_Recoveries %>%
  mutate(Value_Expanded = 1/Phi) 

#############################################
##########Escapement to Hatchery#############
#############################################
CWT_Hatchery<-CWT_Recoveries %>%
      filter(fishery == 50) %>%
      mutate(estimated_number = ifelse(is.na(estimated_number), 1, estimated_number))%>% #assuming NAs are 1 (aka 100% sampling of fish recovered are at the hatchery)
      mutate(Individuals = estimated_number*Value_Expanded) 

EscapeToHatchery<-CWT_Hatchery %>%
  group_by(run_year, brood_year) %>%
  summarise(Escapement_to_Hatchery = sum(Value_Expanded, na.rm = TRUE)) %>%
  mutate(Age = run_year-brood_year)
#Natural Reconstruction needs long version for calculating number of escape to hatchery
EscapeToHatchery_long<-EscapeToHatchery

#For CWT Reconstruction
EscapeToHatchery<-EscapeToHatchery %>% #making each Age into its own column
  pivot_wider(names_from = Age, values_from = Escapement_to_Hatchery, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Age1Hat = sum(`1`, na.rm = TRUE), Age2Hat = sum(`2`, na.rm = TRUE), Age3Hat = sum(`3`, na.rm = TRUE) 
            , Age4Hat = sum(`4`, na.rm = TRUE))

# write.csv(EscapeToHatchery, file = "Escapement to Hatchery.csv", row.names = FALSE)


#Getting Fry released
CWT_Hatchery_Releases<- CWT_Releases %>%
  group_by(brood_year) %>%
  summarise(Individuals_Released = sum(Total_Released))
# write.csv(CWT_Hatchery_Releases, file="Hatchery Release.csv", row.names=FALSE)

#Subtracting hatchery fish from Total Hatchery fish escapement. Remainder is natural-origin
HatcheryEscapement_All<-read.csv("Total Escapement to Hatchery.csv")
EscapeToHatchery_long<- EscapeToHatchery_long %>%
  group_by(run_year)%>%
  summarize(Hatchery_to_Hatchery = sum(Escapement_to_Hatchery))
HatcheryEscapement_All<-left_join(HatcheryEscapement_All,EscapeToHatchery_long, by="run_year")
HatcheryEscapement_All$Hatchery_to_Hatchery[c(1,2,6,9)]<-c(149,163,0,0) #2010 and 2013 had no CWT fish taken to hatchery. Most up-to-date hatchery from Mike
HatcheryEscapement_All<-HatcheryEscapement_All %>% map_df(rev)
HatcheryEscapement_All$Natural_to_Hatchery<-HatcheryEscapement_All$Hatchery.Escapement-HatcheryEscapement_All$Hatchery_to_Hatchery
write.csv(HatcheryEscapement_All, "Natural Escapement to Hatchery.csv", row.names = FALSE)

#############################################
#######  Escapement to Spawning Grounds #####
#############################################
Escapement<-read.csv("CWT Recoveries SG.csv") 

Escapement<- Escapement %>%
  mutate(run_year = brood_year + Age) %>%
  mutate(Individuals = numtags*Value_Expanded) %>%
  group_by(run_year, brood_year, Age) %>%
  summarise(Individuals= sum(Individuals))

Escapement<-Escapement %>% #making each Age into its own column
  pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE) 
Escapement_BY<-Escapement %>%
  group_by(brood_year) %>%
  summarize(Age1Sp = sum(`1`, na.rm = TRUE), Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
            , Age4Sp = sum(`4`, na.rm = TRUE), Age5Sp = sum(`5`, na.rm = TRUE))

#BOOTSTRAP for uncertainty
SG_Escapement<-read.csv("CWT Recoveries SG.csv") 
SG_Escapement_Sum<-matrix(NA, nrow=18, ncol = 1000)
Cohort<-list() #Cohort will be a table of Spawners from each age from 20 years, sampled 1000 times
for(j in 1:1000){
  SG_Escapement$Resampled<-NA
  for(i in 1:length(SG_Escapement$cwtcode)){
    SG_Escapement$Resampled[i]<-sum(c(1,rnbinom(1, 1, 1/SG_Escapement$numtags[i])))*SG_Escapement$Value_Expanded[i]
  }
  SG_Escapement_Sum[,j]<-SG_Escapement %>%
    mutate(run_year = brood_year + Age) %>%
    group_by(run_year)%>%
    summarise(Run = sum(Resampled))%>%
    filter(run_year != 2001) %>%
    pull(Run)
  Cohort[[j]]<-SG_Escapement %>%
    mutate(run_year = brood_year + Age) %>%
    group_by(run_year, brood_year, Age) %>%
    summarise(Individuals = sum(Resampled))%>% #making each Age into its own column
    pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE)%>%
    group_by(brood_year) %>%
    summarize(Age1Sp = sum(`1`, na.rm = TRUE), Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
              , Age4Sp = sum(`4`, na.rm = TRUE), Age5Sp = sum(`5`, na.rm = TRUE))
}

# saveRDS(Cohort, file = "CWTBootstraps.Rds")


