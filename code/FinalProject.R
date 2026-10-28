#STAT260 FINAL GROUP PROJECT

#READING DATA SETS: selecting and renaming columns

library(foreign)
library(tidyverse)
library(haven)
library(fstcore)

#brfss2024<-read.xport("LLCP2024.XPT ") #only for first time
#brfss2024<-write_fst(brfss2024, "/Users/ellapace/Desktop/STAT260 Final Project/brfss2024")
brfss2024<-read_fst("brfss2024") #use this to read in faster after first time
dim(brfss2024)
brfss2024<-as_tibble(brfss2024)
d_2024 <- brfss2024 %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2024$date <- as.Date(brfss2024$IDATE, "%m%d%Y")

#brfss2023<-read.xport("LLCP2023.XPT ") #only for first time
#brfss2023<-write_fst(brfss2023, "/Users/ellapace/Desktop/STAT260 Final Project/brfss2023")
brfss2023<-read_fst("brfss2023") #use this to read in faster after first time
dim(brfss2023)
brfss2023<-as_tibble(brfss2023)
d_2023 <- brfss2023 %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2023$date <- as.Date(brfss2023$IDATE, "%m%d%Y")

#brfss2022<-read.xport("LLCP2022.XPT ") #only for first time
#brfss2022<-write_fst(brfss2022, "/Users/ellapace/Desktop/STAT260 Final Project/brfss2022")
brfss2022<-read_fst("brfss2022") #use this to read in faster after first time
dim(brfss2022)
brfss2022<-as_tibble(brfss2022)
d_2022 <- brfss2022 %>% select(state=X_STATE, sex=X_SEX, race=X_RACE1, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2022$date <- as.Date(brfss2022$IDATE, "%m%d%Y")

#brfss2021<-read.xport("LLCP2021.XPT ") #only for first time
#brfss2021<-write_fst(brfss2021, "/Users/ellapace/Desktop/STAT260 Final Project/brfss2021")
brfss2021<-read_fst("brfss2021") #use this to read in faster after first time
dim(brfss2021)
brfss2021<-as_tibble(brfss2021)
d_2021 <- brfss2021 %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2021$date <- as.Date(brfss2021$IDATE, "%m%d%Y")

#brfss2020<-read.xport("LLCP2020.XPT ") #only for first time
#brfss2020<-write_fst(brfss2020, "/Users/ellapace/Desktop/STAT260 Final Project/brfss2020")
brfss2020<-read_fst("brfss2020") #use this to read in faster after first time
dim(brfss2020)
brfss2020<-as_tibble(brfss2020)
d_2020 <- brfss2020 %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2020$date <- as.Date(brfss2020$IDATE, "%m%d%Y")

#brfss2019<-read.xport("LLCP2019.XPT ") #only for first time
#brfss2019<-write_fst(brfss2019, "/Users/ellapace/Desktop/STAT260 Final Project/brfss2019")
brfss2019<-read_fst("brfss2019") #use this to read in faster after first time
dim(brfss2019)
brfss2019<-as_tibble(brfss2019)
d_2019 <- brfss2019 %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2019$date <- as.Date(brfss2019$IDATE, "%m%d%Y")

#brfss2018<-read.xport("LLCP2018.XPT ") #only for first time
#brfss2018<-write_fst(brfss2018, "/Users/ellapace/Desktop/STAT260 Final Project/brfss2018")
brfss2018<-read_fst("brfss2018") #use this to read in faster after first time
dim(brfss2018)
brfss2018<-as_tibble(brfss2018)
d_2018 <- brfss2018 %>% select(state=X_STATE, sex=SEX1, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT6)
d_2018$date <- as.Date(brfss2018$IDATE, "%m%d%Y")

d_combined<- bind_rows(d_2018, d_2019, d_2020, d_2021, d_2022, d_2023, d_2024)
dim(d_combined)


#ignore rest for now....


#CLEANING 2018 VACCINE

brfss2018$clean_vac<- rep(NA, nrow(brfss2018))
brfss2018$clean_vac[brfss2018$FLUSHOT6==9]<- NA
brfss2018$clean_vac[!is.na(brfss2018$FLUSHOT6) & brfss2018$FLUSHOT6==1]<- "yes"
brfss2018$clean_vac[!is.na(brfss2018$FLUSHOT6) & brfss2018$FLUSHOT6==2]<- "no"
brfss2018$clean_vac[!is.na(brfss2018$FLUSHOT6) & brfss2018$FLUSHOT6==7]<- "DK"

#saving 'refused' 
brfss2018$clean_vac_refused<- ifelse(brfss2018$FLUSHOT6==9, 1, 0) 

#graphing
vac_freq_2018<- table(brfss2018$clean_vac)
par(las=1)
barplot(vac_freq_2018,
	main="CDC BRFSS 2018: Did you receive a flu vaccine in the past year?")
	
#CLEANING 2019 VACCINE

brfss2019$clean_vac<- rep(NA, nrow(brfss2019))
brfss2019$clean_vac[brfss2019$FLUSHOT7==9]<- NA
brfss2019$clean_vac[!is.na(brfss2019$FLUSHOT7) & brfss2019$FLUSHOT7==1]<- "yes"
brfss2019$clean_vac[!is.na(brfss2019$FLUSHOT7) & brfss2019$FLUSHOT7==2]<- "no"
brfss2019$clean_vac[!is.na(brfss2019$FLUSHOT7) & brfss2019$FLUSHOT7==7]<- "DK"

#saving 'refused' 
brfss2019$clean_vac_refused<- ifelse(brfss2019$FLUSHOT7==9, 1, 0) 

#graphing 
vac_freq_2019<- table(brfss2019$clean_vac)
par(las=1)
barplot(vac_freq_2019,
	main="CDC BRFSS 2019: Did you receive a flu vaccine in the past year?")

#CLEANING 2020 VACCINE

brfss2020$clean_vac<- rep(NA, nrow(brfss2020))
brfss2020$clean_vac[brfss2020$FLUSHOT7==9]<- NA
brfss2020$clean_vac[!is.na(brfss2020$FLUSHOT7) & brfss2020$FLUSHOT7==1]<- "yes"
brfss2020$clean_vac[!is.na(brfss2020$FLUSHOT7) & brfss2020$FLUSHOT7==2]<- "no"
brfss2020$clean_vac[!is.na(brfss2020$FLUSHOT7) & brfss2020$FLUSHOT7==7]<- "DK"

#saving 'refused' 
brfss2020$clean_vac_refused<- ifelse(brfss2020$FLUSHOT7==9, 1, 0) 

#graphing 
vac_freq_2020<- table(brfss2020$clean_vac)
par(las=1)
barplot(vac_freq_2020,
	main="CDC BRFSS 2020: Did you receive a flu vaccine in the past year?")

#CLEANING 2021 VACCINE

brfss2021$clean_vac<- rep(NA, nrow(brfss2021))
brfss2021$clean_vac[brfss2021$FLUSHOT7==9]<- NA
brfss2021$clean_vac[!is.na(brfss2021$FLUSHOT7) & brfss2021$FLUSHOT7==1]<- "yes"
brfss2021$clean_vac[!is.na(brfss2021$FLUSHOT7) & brfss2021$FLUSHOT7==2]<- "no"
brfss2021$clean_vac[!is.na(brfss2021$FLUSHOT7) & brfss2021$FLUSHOT7==7]<- "DK"

#saving 'refused' 
brfss2021$clean_vac_refused<- ifelse(brfss2021$FLUSHOT7==9, 1, 0) 

#graphing 
vac_freq_2021<- table(brfss2021$clean_vac)
par(las=1)
barplot(vac_freq_2021,
	main="CDC BRFSS 2021: Did you receive a flu vaccine in the past year?")

#CLEANING 2022 VACCINE

brfss2022$clean_vac<- rep(NA, nrow(brfss2022))
brfss2022$clean_vac[brfss2022$FLUSHOT7==9]<- NA
brfss2022$clean_vac[!is.na(brfss2022$FLUSHOT7) & brfss2022$FLUSHOT7==1]<- "yes"
brfss2022$clean_vac[!is.na(brfss2022$FLUSHOT7) & brfss2022$FLUSHOT7==2]<- "no"
brfss2022$clean_vac[!is.na(brfss2022$FLUSHOT7) & brfss2022$FLUSHOT7==7]<- "DK"

#saving 'refused' 
brfss2022$clean_vac_refused<- ifelse(brfss2022$FLUSHOT7==9, 1, 0) 

#graphing 
vac_freq_2022<- table(brfss2022$clean_vac)
par(las=1)
barplot(vac_freq_2022,
	main="CDC BRFSS 2022: Did you receive a flu vaccine in the past year?")

#CLEANING 2023 VACCINE

brfss2023$clean_vac<- rep(NA, nrow(brfss2023))
brfss2023$clean_vac[brfss2023$FLUSHOT7==9]<- NA
brfss2023$clean_vac[!is.na(brfss2023$FLUSHOT7) & brfss2023$FLUSHOT7==1]<- "yes"
brfss2023$clean_vac[!is.na(brfss2023$FLUSHOT7) & brfss2023$FLUSHOT7==2]<- "no"
brfss2023$clean_vac[!is.na(brfss2023$FLUSHOT7) & brfss2023$FLUSHOT7==7]<- "DK"

#saving 'refused' 
brfss2023$clean_vac_refused<- ifelse(brfss2023$FLUSHOT7==9, 1, 0) 

#graphing 
vac_freq_2023<- table(brfss2023$clean_vac)
par(las=1)
barplot(vac_freq_2023,
	main="CDC BRFSS 2023: Did you receive a flu vaccine in the past year?")

#CLEANING 2024 VACCINE

brfss2024$clean_vac<- rep(NA, nrow(brfss2024))
brfss2024$clean_vac[brfss2024$FLUSHOT7==9]<- NA
brfss2024$clean_vac[!is.na(brfss2024$FLUSHOT7) & brfss2024$FLUSHOT7==1]<- "yes"
brfss2024$clean_vac[!is.na(brfss2024$FLUSHOT7) & brfss2024$FLUSHOT7==2]<- "no"
brfss2024$clean_vac[!is.na(brfss2024$FLUSHOT7) & brfss2024$FLUSHOT7==7]<- "DK"

#saving 'refused' 
brfss2024$clean_vac_refused<- ifelse(brfss2024$FLUSHOT7==9, 1, 0) 

#graphing 
vac_freq_2024<- table(brfss2024$clean_vac)
par(las=1)
barplot(vac_freq_2024,
	main="CDC BRFSS 2024: Did you receive a flu vaccine in the past year?")





