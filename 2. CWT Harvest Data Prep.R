###############################################
######     CWT Fishery Data Prep       ########
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("~/example-cohort-reconstruction")

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


CWT_Recoveries<-read.csv("CWTRecoveries.csv")
names(CWT_Releases)[7]<-"tag_code"
CWT_Recoveries<- CWT_Recoveries %>%
  select(reporting_agency,run_year,recovery_date, fishery, tag_code, estimated_number, recovery_location_name,sampling_site , brood_year)

Phi_by_tag<-CWT_Releases %>% #phi for each batch
  select(tag_code, Phi)

CWT_Recoveries<-left_join(CWT_Recoveries, Phi_by_tag, by="tag_code")

CWT_Recoveries <- CWT_Recoveries %>%
  mutate(Value_Expanded = 1/Phi)
#############################################
#############  In-River Harvest #############
#############################################
#if in-river harvest happened in late fall (Nov&Dec), that belongs to the run
#year of the next year
CWT_Recoveries_River<-CWT_Recoveries %>%
  filter(fishery == 46) %>%
  mutate(run_year= ifelse(month(ymd(recovery_date))>= 11, run_year+1,run_year)) %>%
  mutate(Harvested = estimated_number/Phi) %>%
  mutate(Age = run_year-brood_year) %>%
  group_by(run_year, brood_year, Age) %>%
  summarise(Harvested = sum(Harvested)) %>%
  pivot_wider(values_from = "Harvested", names_from = "Age") %>%
  select(run_year,brood_year, `3`,`4`) %>%
  filter(brood_year != 2017)

CWT_Recoveries_River[is.na(CWT_Recoveries_River)]<-0
colnames(CWT_Recoveries_River)[c(3,4)]<-c("InRiver3", "InRiver4")

# write.csv(CWT_Recoveries_River, "River Harvest.csv", row.names = FALSE)
#############################################
############## Ocean Harvest ################
#############################################
SiteCodes<-read.csv("sitearea.modified.csv")
SiteCodes<-SiteCodes %>%
  select(sampling_site,area.1)
colnames(SiteCodes)[2]<-"Location"
Size_Limits<-read.csv("Size_limits.csv")  
Release_mort<-read.csv("release.mort.rate.csv")
Release_mort<-Release_mort[,1:5] #removing comments. Most recent year is missing estimate. Commercial standard .26 during those years. Recreational standard .14 during those years. 
SizeAge<-read.csv("length.at.age.csv")
######################
#Point estimates
CWT_Recoveries_Ocean<-CWT_Recoveries %>% #adding Month of record
  mutate(Month =month(ymd(CWT_Recoveries$recovery_date))) %>% 
  mutate(Age = run_year-brood_year) %>%
  filter(fishery == 10|fishery == 40) %>%
  mutate(Harvested = estimated_number/Phi) %>% #How many fish each CWT fish represents = 1/(estimated number (estimated sampling effort) * phi)
  left_join(SiteCodes) %>% #Appending region for each recovery location name. 
  left_join(Size_Limits) %>% #Appending size limit based on year, fishery type, region, and month
  left_join(Release_mort)#Appending release mortality based on year, fishery type, region, and month

#Commercial Fisheries
CWT_Recoveries.Com<-CWT_Recoveries %>%
  filter(fishery==10)
#Recreation Fisheries
CWT_Recoveries.Rec<-CWT_Recoveries %>%
  filter(fishery==40)

#Function: Present Harvest requires Location (e.g. FB, SF, MO), Month, Brood Year, Run Year, and 
#produces the percentage of fish in that class that can be taken by the fishery. 

Percent_Harvest<-function(Month, Age, Size_Limit){
  1-pnorm(Size_Limit, mean = SizeAge$mean[which(SizeAge$age == Age+1 & SizeAge$month == Month)], sd = SizeAge$sd[which(SizeAge$age == Age+1 & SizeAge$month == Month)])
}

#Calculating percent harvestable, catch, release and drop mortality and total impact
CWT_Recoveries_Ocean$Percent_Harvestable<-as.numeric(as.character(mapply(Percent_Harvest, CWT_Recoveries_Ocean$Month, CWT_Recoveries_Ocean$Age, CWT_Recoveries_Ocean$limit)))
CWT_Recoveries_Ocean<-CWT_Recoveries_Ocean %>%
  mutate(Catch=Harvested/Percent_Harvestable) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*Release.mort.rate) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) # I = H+S+D

#Stats by age*month for commercial fishery
Commercial_Harvest<-CWT_Recoveries_Ocean %>%
  filter(fishery == 10) %>%
  group_by(brood_year, run_year, Month) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch), Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact))
#Stats by age*month for recreational fishery
Recreational_Harvest<-CWT_Recoveries_Ocean %>%
  filter(fishery == 40) %>%
  group_by(brood_year, run_year, Month) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact))

Impact<-CWT_Recoveries_Ocean %>%
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact)) %>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
#####################
###Bootstrapping. 
#Recreational
CWT_Recoveries_Ocean$Harvested_Sample<-NA
CWT_Recoveries_Ocean$Catch_Sample<-NA
CWT_Recoveries_Ocean$Drop_Sample<-NA
CWT_Recoveries_Ocean$Release_Sample<-NA
CWT_Recoveries_Ocean$Impact_Sample<-NA

CWT_Recoveries.list<-list() #each item in the list will contain a resample of harvest, catch, release mort, impact, drop mort
Harvest.list<-list() #where we summarized Impact data by year, month, age
years<-read.csv("years with values.csv")

for(j in 1:1000){
  CWT_Recoveries.list[[j]]<-CWT_Recoveries_Ocean #Having point estimate info within template of bootstrapped sample
  for(i in 1:length(CWT_Recoveries_Ocean$estimated_number)){
#How many fish each tag represents harvested = (1 + unrecovered tags)/Phi 
    CWT_Recoveries.list[[j]]$Harvested_Sample[i]<-as.numeric(sum(c(1,rnbinom(1, 1,1/(CWT_Recoveries_Ocean$estimated_number[i]))),
                                                                     na.rm =TRUE)*CWT_Recoveries_Ocean$Value_Expanded[i])
    CWT_Recoveries.list[[j]]$Catch_Sample[i]<-CWT_Recoveries.list[[j]]$Harvested_Sample[i]/CWT_Recoveries.list[[j]]$Percent_Harvestable[i]
    CWT_Recoveries.list[[j]]$Drop_Sample[i]<-CWT_Recoveries.list[[j]]$Catch_Sample[i]*.05
    CWT_Recoveries.list[[j]]$Release_Sample[i]<-(CWT_Recoveries.list[[j]]$Catch_Sample[i]-CWT_Recoveries.list[[j]]$Harvested_Sample[i])*CWT_Recoveries.list[[j]]$Release.mort.rate[i]
    CWT_Recoveries.list[[j]]$Impact_Sample[i]<-CWT_Recoveries.list[[j]]$Drop_Sample[i]+CWT_Recoveries.list[[j]]$Harvested_Sample[i]+CWT_Recoveries.list[[j]]$Release_Sample[i]
       }
  Harvest.list[[j]]<-full_join(years, CWT_Recoveries.list[[j]], multiple= "all")
  Harvest.list[[j]]<-Harvest.list[[j]]%>%
    group_by(brood_year, run_year, Month, Age) %>%
    summarise(Tags_Collected = n(), Catch_Sample =sum(Catch_Sample), Harvested_Sample=sum(Harvested_Sample), Release_Sample=sum(Release_Sample), Drop_Sample=sum(Drop_Sample), Impact_Sample=sum(Impact_Sample)) %>%
    replace(is.na(.),0) %>%
    mutate(Tags_Collected = ifelse(Catch_Sample ==0, 0, Catch_Sample))
}
test<-Harvest.list[[1]]
# saveRDS(Harvest.list, file = "Catch Representation.Rds")
Impact_Bootstrap<-list() 
for (i in 1:1000){
  Impact_Bootstrap[[i]]<-Harvest.list[[i]] %>%
    pivot_wider(names_from = c(Age,Month),values_from= Impact_Sample, names_sort = TRUE )%>%
    group_by(brood_year) %>%
    summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
              , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
              Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
              May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
              Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
              Nov4 = sum(`3_11`, na.rm = TRUE))
}
saveRDS(Impact_Bootstrap, file = "Impact Bootstrap.Rds")

