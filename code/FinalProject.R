#STAT260 FINAL GROUP PROJECT

#READING DATA SETS: randomly selecting 1000/year and renaming columns

library(foreign)
library(tidyverse)
library(haven)
library(fstcore)
library(fst)

set.seed(8)
#brfss2024<-read.xport("data/LLCP2024.XPT") #only for first time
#brfss2024<-write_fst(brfss2024, "data/brfss2024")
brfss2024<-read_fst("data/brfss2024") #use this to read in faster after first time
brfss2024_sample <- brfss2024[sample(nrow(brfss2024), 1000), ]
dim(brfss2024_sample)
brfss2024_sample<-as_tibble(brfss2024_sample)
d_2024 <- brfss2024_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2024$date <- as.Date(brfss2024_sample$IDATE, "%m%d%Y")

#brfss2023<-read.xport("data/LLCP2023.XPT") #only for first time
#brfss2023<-write_fst(brfss2023, "data/brfss2023")
brfss2023<-read_fst("data/brfss2023") #use this to read in faster after first time
brfss2023_sample <- brfss2023[sample(nrow(brfss2023), 1000), ]
dim(brfss2023_sample)
brfss2023_sample<-as_tibble(brfss2023_sample)
d_2023 <- brfss2023_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2023$date <- as.Date(brfss2023_sample$IDATE, "%m%d%Y")

#brfss2022<-read.xport("data/LLCP2022.XPT") #only for first time
#brfss2022<-write_fst(brfss2022, "data/brfss2022")
brfss2022<-read_fst("data/brfss2022") #use this to read in faster after first time
brfss2022_sample <- brfss2022[sample(nrow(brfss2022), 1000), ]
dim(brfss2022)
brfss2022_sample<-as_tibble(brfss2022_sample)
d_2022 <- brfss2022_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE1, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2022$date <- as.Date(brfss2022_sample$IDATE, "%m%d%Y")

#brfss2021<-read.xport("data/LLCP2021.XPT") #only for first time
#brfss2021<-write_fst(brfss2021, "data/brfss2021")
brfss2021<-read_fst("data/brfss2021") #use this to read in faster after first time
brfss2021_sample <- brfss2021[sample(nrow(brfss2021), 1000), ]
dim(brfss2021_sample)
brfss2021_sample<-as_tibble(brfss2021_sample)
d_2021 <- brfss2021_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2021$date <- as.Date(brfss2021_sample$IDATE, "%m%d%Y")

#brfss2020<-read.xport("data/LLCP2020.XPT") #only for first time
#brfss2020<-write_fst(brfss2020, "data/brfss2020")
brfss2020<-read_fst("data/brfss2020") #use this to read in faster after first time
brfss2020_sample <- brfss2020[sample(nrow(brfss2020), 1000), ]
dim(brfss2020_sample)
brfss2020_sample<-as_tibble(brfss2020_sample)
d_2020 <- brfss2020_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2020$date <- as.Date(brfss2020_sample$IDATE, "%m%d%Y")

#brfss2019<-read.xport("data/LLCP2019.XPT") #only for first time
#brfss2019<-write_fst(brfss2019, "data/brfss2019")
brfss2019<-read_fst("data/brfss2019") #use this to read in faster after first time
brfss2019_sample <- brfss2019[sample(nrow(brfss2019), 1000), ]
dim(brfss2019_sample)
brfss2019_sample<-as_tibble(brfss2019_sample)
d_2019 <- brfss2019_sample %>% select(state=X_STATE, sex=X_SEX, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT7)
d_2019$date <- as.Date(brfss2019_sample$IDATE, "%m%d%Y")

#brfss2018<-read.xport("data/LLCP2018.XPT") #only for first time
#brfss2018<-write_fst(brfss2018, "data/brfss2018")
brfss2018<-read_fst("data/brfss2018") #use this to read in faster after first time
brfss2018_sample <- brfss2018[sample(nrow(brfss2018), 1000), ]
dim(brfss2018_sample)
brfss2018_sample<-as_tibble(brfss2018_sample)
d_2018 <- brfss2018_sample %>% select(state=X_STATE, sex=SEX1, race=X_RACE, age=X_AGEG5YR, vaccine=FLUSHOT6)
d_2018$date <- as.Date(brfss2018_sample$IDATE, "%m%d%Y")

d_combined<- bind_rows(d_2018, d_2019, d_2020, d_2021, d_2022, d_2023, d_2024)
dim(d_combined)

# exporting the yearly datasets
#write.csv(brfss2018_sample, "data/brfss2018_sample.csv")
#write.csv(brfss2019_sample, "data/brfss2019_sample.csv")
#write.csv(brfss2020_sample, "data/brfss2020_sample.csv")
#write.csv(brfss2021_sample, "data/brfss2021_sample.csv")
#write.csv(brfss2022_sample, "data/brfss2022_sample.csv")
#write.csv(brfss2023_sample, "data/brfss2023_sample.csv")
#write.csv(brfss2024_sample, "data/brfss2024_sample.csv")


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


#CLEANING STATE
any(is.na(d_2018$state)) #no NA values
any(is.na(d_2019$state))
any(is.na(d_2020$state))
any(is.na(d_2021$state))
any(is.na(d_2022$state))
any(is.na(d_2023$state))
any(is.na(d_2024$state))

state_dict <- c(
  "1" = "Alabama",
  "2" = "Alaska",
  "4" = "Arizona",
  "5" = "Arkansas",
  "6" = "California",
  "8" = "Colorado",
  "9" = "Connecticut",
  "10" = "Delaware",
  "11" = "Washington D.C.",
  "12" = "Florida",
  "13" = "Georgia",
  "15" = "Hawaii",
  "16" = "Idaho",
  "17" = "Illinois",
  "18" = "Indiana",
  "19" = "Iowa",
  "20" = "Kansas",
  "21" = "Kentucky",
  "22" = "Louisiana",
  "23" = "Maine",
  "24" = "Maryland",
  "25" = "Massachusetts",
  "26" = "Michigan",
  "27" = "Minnesota",
  "28" = "Mississippi",
  "29" = "Missouri",
  "30" = "Montana",
  "31" = "Nebraska",
  "32" = "Nevada",
  "33" = "New Hampshire",
  "34" = "New Jersey",
  "35" = "New Mexico",
  "36" = "New York",
  "37" = "North Carolina",
  "38" = "North Dakota",
  "39" = "Ohio",
  "40" = "Oklahoma",
  "41" = "Oregon",
  "42" = "Pennsylvania",
  "44" = "Rhode Island",
  "45" = "South Carolina",
  "46" = "South Dakota",
  "47" = "Tennessee",
  "48" = "Texas",
  "49" = "Utah",
  "50" = "Vermont",
  "51" = "Virginia",
  "53" = "Washington",
  "54" = "West Virginia",
  "55" = "Wisconsin",
  "56" = "Wyoming",
  "66" = "Guam",
  "72" = "Puerto Rico",
  "78" = "US Virgin Islands"
)

d_2018<- d_2018 %>% 
  mutate(clean_states = state_dict[state])

d_2019<- d_2019 %>% 
  mutate(clean_states = state_dict[state])

d_2020<- d_2020 %>% 
  mutate(clean_states = state_dict[state])

d_2021<- d_2021 %>% 
  mutate(clean_states = state_dict[state])

d_2022<- d_2022 %>% 
  mutate(clean_states = state_dict[state])
  
d_2023<- d_2023 %>% 
  mutate(clean_states = state_dict[state])

d_2024<- d_2024 %>% 
  mutate(clean_states = state_dict[state])




