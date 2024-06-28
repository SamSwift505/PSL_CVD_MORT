#install.packages("estimatr")

library(tidyverse)
library(estimatr)
library(lfe)
library(fixest)

#Read in CDC Data
CVD_MORT_FEMALE <- read.delim(file = "C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/MORT_DATA/CVD_MORT_FEMALE.txt", header = TRUE, sep = "\t", dec = ".")
CVD_MORT_MALE <- read.delim(file = "C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/MORT_DATA/CVD_MORT_MALE.txt", header = TRUE, sep = "\t", dec = ".")
CVD_MORT_AA <- read.delim(file = "C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/MORT_DATA/CVD_MORT_AA.txt", header = TRUE, sep = "\t", dec = ".")



####FEMALE 

#Some Re-coding -FEMALE
names(CVD_MORT_FEMALE)[names(CVD_MORT_FEMALE) == "County.Code"] <- "FIPS"
CVD_MORT_FEMALE$CRUDE <- as.numeric(CVD_MORT_FEMALE$Crude.Rate)
CVD_MORT_FEMALE$LOG_CRUDE <- log(CVD_MORT_FEMALE$CRUDE)
CVD_MORT_FEMALE$IHS_CRUDE <- asinh(CVD_MORT_FEMALE$CRUDE)
CVD_MORT_FEMALE$LOG_CRUDE <- log(CVD_MORT_FEMALE$CRUDE)
CVD_MORT_FEMALE$POP <- as.numeric(CVD_MORT_FEMALE$Population)
CVD_MORT_FEMALE$YEAR_FACTOR <- as.factor(CVD_MORT_FEMALE$Year)
CVD_MORT_FEMALE$YEAR_CVD <- CVD_MORT_FEMALE$Year

#Create the data####################################################################################



#These years came from the National Conference of State Legislatures 
#https://www.ncsl.org/labor-and-employment/paid-sick-leave
CVD_MORT_FEMALE<-CVD_MORT_FEMALE %>% 
          mutate(PSL_YEAR=case_when(State == 'Connecticut' ~ 2012,
                                    State == 'District of Columbia' ~ 2014,
                                    State == 'California' ~ 2015,
                                    State == 'Massachusetts' ~ 2015,
                                    State == 'Oregon' ~ 2016,
                                    State == 'Arizona' ~ 2017,
                                    State == 'Vermont' ~ 2017,
                                    State == 'Maryland' ~ 2018,
                                    State == 'Rhode Island' ~ 2018,
                                    State == 'New Jersey' ~ 2018,
                                    State == 'Washington' ~ 2018))

CVD_MORT_FEMALE<-CVD_MORT_FEMALE %>% 
          mutate(TREATED=case_when(State == 'Connecticut' ~ 1,
                                   State == 'District of Columbia' ~ 1,
                                   State == 'California' ~ 1,
                                   State == 'Massachusetts' ~ 1,
                                   State == 'Oregon' ~ 1,
                                   State == 'Arizona' ~ 1,
                                   State == 'Vermont' ~ 1,
                                   State == 'Maryland' ~ 1,
                                   State == 'New Jersey' ~ 1,
                                   State == 'Rhode Island' ~ 1,
                                   State == 'Washington' ~ 1,
                                   TRUE ~ 0))




CVD_MORT_FEMALE <-CVD_MORT_FEMALE %>%
  mutate(CENSUSREGION=case_when (State == 'Connecticut' ~ "NE",
                                 State == 'Maine' ~ "NE",
                                 State == 'Massachusetts' ~ "NE",
                                 State == 'New Hampshire' ~ "NE",
                                 State == 'Rhode Island' ~ "NE",
                                 State == 'Vermont' ~ "NE",
                                 State == 'New Jersey' ~ "NE",
                                 State == 'New York' ~ "NE",
                                 State == 'Pennsylvania' ~ "NE",
                                 
                                 State == 'Indiana' ~ "MW",
                                 State == 'Illinois' ~ "MW",
                                 State == 'Michigan' ~ "MW",
                                 State == 'Ohio' ~ "MW",
                                 State == 'Wisconsin' ~ "MW",
                                 State == 'Iowa' ~ "MW",
                                 State == 'Kansas' ~ "MW",
                                 State == 'Minnesota' ~ "MW",
                                 State == 'Missouri' ~ "MW",
                                 State == 'Nebraska' ~ "MW",
                                 State == 'North Dakota' ~ "MW",
                                 State == 'South Dakota' ~ "MW",
                                 
                                 State == 'Delaware' ~ "SO",
                                 State == 'District of Columbia' ~ "SO",
                                 State == 'Florida' ~ "SO",
                                 State == 'Georgia' ~ "SO",
                                 State == 'Maryland' ~ "SO",
                                 State == 'North Carolina' ~ "SO",
                                 State == 'South Carolina' ~ "SO",
                                 State == 'Virginia' ~ "SO",
                                 State == 'West Virginia' ~ "SO",
                                 State == 'Alabama' ~ "SO",
                                 State == 'Kentucky' ~ "SO",
                                 State == 'Mississippi' ~ "SO",
                                 State == 'Tennessee' ~ "SO",
                                 State == 'Arkansas' ~ "SO",
                                 State == 'Louisiana' ~ "SO",
                                 State == 'Oklahoma' ~ "SO",
                                 State == 'Texas' ~ "SO",
                                 
                                 State == 'Arizona' ~ "WT",
                                 State == 'Colorado' ~ "WT",
                                 State == 'Idaho' ~ "WT",
                                 State == 'New Mexico' ~ "WT",
                                 State == 'Montana' ~ "WT",
                                 State == 'Utah' ~ "WT",
                                 State == 'Nevada' ~ "WT",
                                 State == 'Wyoming' ~ "WT",
                                 State == 'Alaska' ~ "WT",
                                 State == 'California' ~ "WT",
                                 State == 'Hawaii' ~ "WT",
                                 State == 'Oregon' ~ "WT",
                                 State == 'Washington' ~ "WT",
                                 
                                 
  ))



CVD_MORT_FEMALE$TREATED <- as.factor(CVD_MORT_FEMALE$TREATED)

CVD_MORT_FEMALE<-CVD_MORT_FEMALE %>% 
          mutate(POLICY_YEAR=case_when(!is.na(CVD_MORT_FEMALE$PSL_YEAR) ~ Year-PSL_YEAR))

CVD_MORT_FEMALE<-CVD_MORT_FEMALE %>% 
  mutate(SAYRTRT=case_when(!is.na(CVD_MORT_FEMALE$PSL_YEAR) ~ PSL_YEAR))

CVD_MORT_FEMALE$SAYRTRT[is.na(CVD_MORT_FEMALE$PSL_YEAR)]<-100000

#LAGS AND LEADS FOR EVENT STUDY 
CVD_MORT_FEMALE<-CVD_MORT_FEMALE %>% 
  mutate(
    lead1 = case_when(POLICY_YEAR == -1 ~ 1, TRUE ~ 0),
    lead2 = case_when(POLICY_YEAR == -2 ~ 1, TRUE ~ 0),
    lead3 = case_when(POLICY_YEAR == -3 ~ 1, TRUE ~ 0),
    lead4 = case_when(POLICY_YEAR == -4 ~ 1, TRUE ~ 0),
    lead5 = case_when(POLICY_YEAR == -5 ~ 1, TRUE ~ 0),
    lead6 = case_when(POLICY_YEAR == -6 ~ 1, TRUE ~ 0),
    lead7 = case_when(POLICY_YEAR == -7 ~ 1, TRUE ~ 0),
    lead8 = case_when(POLICY_YEAR == -8 ~ 1, TRUE ~ 0),
    lead9 = case_when(POLICY_YEAR == -9 ~ 1, TRUE ~ 0),
    lead10 = case_when(POLICY_YEAR == -10 ~ 1, TRUE ~ 0),
    
    lag0 = case_when(POLICY_YEAR == 0 ~ 1, TRUE ~ 0),
    lag1 = case_when(POLICY_YEAR == 1 ~ 1, TRUE ~ 0),
    lag2 = case_when(POLICY_YEAR == 2 ~ 1, TRUE ~ 0),
    lag3 = case_when(POLICY_YEAR == 3 ~ 1, TRUE ~ 0),
    lag4 = case_when(POLICY_YEAR == 4 ~ 1, TRUE ~ 0),
    lag5 = case_when(POLICY_YEAR == 5 ~ 1, TRUE ~ 0),
    lag6 = case_when(POLICY_YEAR == 6 ~ 1, TRUE ~ 0),
    lag7 = case_when(POLICY_YEAR == 7 ~ 1, TRUE ~ 0),
    lag8 = case_when(POLICY_YEAR == 8 ~ 1, TRUE ~ 0),
    lag9 = case_when(POLICY_YEAR == 9 ~ 1, TRUE ~ 0))



#Post Variable for TWFE
CVD_MORT_FEMALE<-CVD_MORT_FEMALE %>% 
  mutate(POST=case_when(POLICY_YEAR>0 ~ 1,
                           TRUE ~ 0))

table(CVD_MORT_FEMALE$POST, useNA="always")
summary(CVD_MORT_FEMALE$CRUDE, useNA="always", fun=mean)


#####BRING IN COVARIATES 
COVARS<-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/COVARS.CSV")


ALL_FEMALE<-left_join(CVD_MORT_FEMALE, COVARS, by = c("FIPS" = "FULL_FIPS2","YEAR_CVD"="Year"))

ALL_FEMALE_NE <-subset(ALL_FEMALE, CENSUSREGION=="NE") 
ALL_FEMALE_WT <-subset(ALL_FEMALE, CENSUSREGION=="WT") 

##Event Study 
event_FEMALE_NE<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
               lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7 + MED_INC + POV_PERCENT_ALL + 
               UNEMP_RATE + UNINSURED_RATE
             |County + Year|0|State, weights=ALL_FEMALE_NE$Population, data = ALL_FEMALE_NE)
summary(event_FEMALE_NE)
##Event Study 
event_FEMALE_WT<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
                         lag1+ lag2 +lag3 +lag4 + MED_INC + POV_PERCENT_ALL + 
                         UNEMP_RATE + UNINSURED_RATE
                       |County + Year|0|State, weights=ALL_FEMALE_WT$Population, data = ALL_FEMALE_WT)
summary(event_FEMALE_WT)


####MALE 

#Some Re-coding -MALE
names(CVD_MORT_MALE)[names(CVD_MORT_MALE) == "County.Code"] <- "FIPS"
CVD_MORT_MALE$CRUDE <- as.numeric(CVD_MORT_MALE$Crude.Rate)
CVD_MORT_MALE$LOG_CRUDE <- log(CVD_MORT_MALE$CRUDE)
CVD_MORT_MALE$IHS_CRUDE <- asinh(CVD_MORT_MALE$CRUDE)
CVD_MORT_MALE$LOG_CRUDE <- log(CVD_MORT_MALE$CRUDE)
CVD_MORT_MALE$POP <- as.numeric(CVD_MORT_MALE$Population)
CVD_MORT_MALE$YEAR_FACTOR <- as.factor(CVD_MORT_MALE$Year)
CVD_MORT_MALE$YEAR_CVD <- CVD_MORT_MALE$Year

#Create the data####################################################################################



#These years came from the National Conference of State Legislatures 
#https://www.ncsl.org/labor-and-employment/paid-sick-leave
CVD_MORT_MALE<-CVD_MORT_MALE %>% 
  mutate(PSL_YEAR=case_when(State == 'Connecticut' ~ 2012,
                            State == 'District of Columbia' ~ 2014,
                            State == 'California' ~ 2015,
                            State == 'Massachusetts' ~ 2015,
                            State == 'Oregon' ~ 2016,
                            State == 'Arizona' ~ 2017,
                            State == 'Vermont' ~ 2017,
                            State == 'Maryland' ~ 2018,
                            State == 'Rhode Island' ~ 2018,
                            State == 'New Jersey' ~ 2018,
                            State == 'Washington' ~ 2018))

CVD_MORT_MALE<-CVD_MORT_MALE %>% 
  mutate(TREATED=case_when(State == 'Connecticut' ~ 1,
                           State == 'District of Columbia' ~ 1,
                           State == 'California' ~ 1,
                           State == 'Massachusetts' ~ 1,
                           State == 'Oregon' ~ 1,
                           State == 'Arizona' ~ 1,
                           State == 'Vermont' ~ 1,
                           State == 'Maryland' ~ 1,
                           State == 'New Jersey' ~ 1,
                           State == 'Rhode Island' ~ 1,
                           State == 'Washington' ~ 1,
                           TRUE ~ 0))



CVD_MORT_MALE <-CVD_MORT_MALE %>%
  mutate(CENSUSREGION=case_when (State == 'Connecticut' ~ "NE",
                                 State == 'Maine' ~ "NE",
                                 State == 'Massachusetts' ~ "NE",
                                 State == 'New Hampshire' ~ "NE",
                                 State == 'Rhode Island' ~ "NE",
                                 State == 'Vermont' ~ "NE",
                                 State == 'New Jersey' ~ "NE",
                                 State == 'New York' ~ "NE",
                                 State == 'Pennsylvania' ~ "NE",
                                 
                                 State == 'Indiana' ~ "MW",
                                 State == 'Illinois' ~ "MW",
                                 State == 'Michigan' ~ "MW",
                                 State == 'Ohio' ~ "MW",
                                 State == 'Wisconsin' ~ "MW",
                                 State == 'Iowa' ~ "MW",
                                 State == 'Kansas' ~ "MW",
                                 State == 'Minnesota' ~ "MW",
                                 State == 'Missouri' ~ "MW",
                                 State == 'Nebraska' ~ "MW",
                                 State == 'North Dakota' ~ "MW",
                                 State == 'South Dakota' ~ "MW",
                                 
                                 State == 'Delaware' ~ "SO",
                                 State == 'District of Columbia' ~ "SO",
                                 State == 'Florida' ~ "SO",
                                 State == 'Georgia' ~ "SO",
                                 State == 'Maryland' ~ "SO",
                                 State == 'North Carolina' ~ "SO",
                                 State == 'South Carolina' ~ "SO",
                                 State == 'Virginia' ~ "SO",
                                 State == 'West Virginia' ~ "SO",
                                 State == 'Alabama' ~ "SO",
                                 State == 'Kentucky' ~ "SO",
                                 State == 'Mississippi' ~ "SO",
                                 State == 'Tennessee' ~ "SO",
                                 State == 'Arkansas' ~ "SO",
                                 State == 'Louisiana' ~ "SO",
                                 State == 'Oklahoma' ~ "SO",
                                 State == 'Texas' ~ "SO",
                                 
                                 State == 'Arizona' ~ "WT",
                                 State == 'Colorado' ~ "WT",
                                 State == 'Idaho' ~ "WT",
                                 State == 'New Mexico' ~ "WT",
                                 State == 'Montana' ~ "WT",
                                 State == 'Utah' ~ "WT",
                                 State == 'Nevada' ~ "WT",
                                 State == 'Wyoming' ~ "WT",
                                 State == 'Alaska' ~ "WT",
                                 State == 'California' ~ "WT",
                                 State == 'Hawaii' ~ "WT",
                                 State == 'Oregon' ~ "WT",
                                 State == 'Washington' ~ "WT",
                                 
                                 
  ))

CVD_MORT_MALE$TREATED <- as.factor(CVD_MORT_MALE$TREATED)

CVD_MORT_MALE<-CVD_MORT_MALE %>% 
  mutate(POLICY_YEAR=case_when(!is.na(CVD_MORT_MALE$PSL_YEAR) ~ Year-PSL_YEAR))

CVD_MORT_MALE<-CVD_MORT_MALE %>% 
  mutate(SAYRTRT=case_when(!is.na(CVD_MORT_MALE$PSL_YEAR) ~ PSL_YEAR))

CVD_MORT_MALE$SAYRTRT[is.na(CVD_MORT_MALE$PSL_YEAR)]<-100000


table(CVD_MORT_MALE$POLICY_YEAR)
table(CVD_MORT_MALE$CENSUSREGION)

#LAGS AND LEADS FOR EVENT STUDY 
CVD_MORT_MALE<-CVD_MORT_MALE %>% 
  mutate(
    lead1 = case_when(POLICY_YEAR == -1 ~ 1, TRUE ~ 0),
    lead2 = case_when(POLICY_YEAR == -2 ~ 1, TRUE ~ 0),
    lead3 = case_when(POLICY_YEAR == -3 ~ 1, TRUE ~ 0),
    lead4 = case_when(POLICY_YEAR == -4 ~ 1, TRUE ~ 0),
    lead5 = case_when(POLICY_YEAR == -5 ~ 1, TRUE ~ 0),
    lead6 = case_when(POLICY_YEAR == -6 ~ 1, TRUE ~ 0),
    lead7 = case_when(POLICY_YEAR == -7 ~ 1, TRUE ~ 0),
    lead8 = case_when(POLICY_YEAR == -8 ~ 1, TRUE ~ 0),
    lead9 = case_when(POLICY_YEAR == -9 ~ 1, TRUE ~ 0),
    lead10 = case_when(POLICY_YEAR == -10 ~ 1, TRUE ~ 0),
    
    lag0 = case_when(POLICY_YEAR == 0 ~ 1, TRUE ~ 0),
    lag1 = case_when(POLICY_YEAR == 1 ~ 1, TRUE ~ 0),
    lag2 = case_when(POLICY_YEAR == 2 ~ 1, TRUE ~ 0),
    lag3 = case_when(POLICY_YEAR == 3 ~ 1, TRUE ~ 0),
    lag4 = case_when(POLICY_YEAR == 4 ~ 1, TRUE ~ 0),
    lag5 = case_when(POLICY_YEAR == 5 ~ 1, TRUE ~ 0),
    lag6 = case_when(POLICY_YEAR == 6 ~ 1, TRUE ~ 0),
    lag7 = case_when(POLICY_YEAR == 7 ~ 1, TRUE ~ 0),
    lag8 = case_when(POLICY_YEAR == 8 ~ 1, TRUE ~ 0),
    lag9 = case_when(POLICY_YEAR == 9 ~ 1, TRUE ~ 0))



#Post Variable for TWFE
CVD_MORT_MALE<-CVD_MORT_MALE %>% 
  mutate(POST=case_when(POLICY_YEAR>0 ~ 1,
                        TRUE ~ 0))

table(CVD_MORT_MALE$POST, useNA="always")
summary(CVD_MORT_MALE$CRUDE, useNA="always", fun=mean)


#####BRING IN COVARIATES 
COVARS<-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/COVARS.CSV")

ALL_MALE<-left_join(CVD_MORT_MALE, COVARS, by = c("FIPS" = "FULL_FIPS2","YEAR_CVD"="Year"))
ALL_MALE_NE <-subset(ALL_MALE, CENSUSREGION=="NE") 
ALL_MALE_WT <-subset(ALL_MALE, CENSUSREGION=="WT") 


##Event Study 

##NORTHEAST
event_MALE_NE_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                              + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                              |County + Year,
                              cluster=~State, 
                              weights=ALL_MALE_NE$Population, data = ALL_MALE_NE)
summary(event_MALE_NE_SAB)

event_FEMALE_NE_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                          + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                          |County + Year,
                          cluster=~State, 
                          weights=ALL_FEMALE_NE$Population, data = ALL_FEMALE_NE)
summary(event_FEMALE_NE_SAB)

##WEST
event_MALE_WT_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                          + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                          |County + Year,
                          cluster=~State, 
                          weights=ALL_MALE_WT$Population, data = ALL_MALE_WT)
summary(event_MALE_WT_SAB)

event_FEMALE_WT_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                            + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                            |County + Year,
                            cluster=~State, 
                            weights=ALL_FEMALE_WT$Population, data = ALL_FEMALE_WT)
summary(event_FEMALE_WT_SAB)

##nationwide
event_MALE_US_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                          + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                          |County + Year,
                          cluster=~State, 
                          weights=ALL_MALE$Population, data = ALL_MALE)
summary(event_MALE_US_SAB)

event_FEMALE_US_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                            + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                            |County + Year,
                            cluster=~State, 
                            weights=ALL_FEMALE$Population, data = ALL_FEMALE)
summary(event_FEMALE_US_SAB)




####AA 

#Some Re-coding AA
names(CVD_MORT_AA)[names(CVD_MORT_AA) == "County.Code"] <- "FIPS"
CVD_MORT_AA$CRUDE <- as.numeric(CVD_MORT_AA$Crude.Rate)
CVD_MORT_AA$LOG_CRUDE <- log(CVD_MORT_AA$CRUDE)
CVD_MORT_AA$IHS_CRUDE <- asinh(CVD_MORT_AA$CRUDE)
CVD_MORT_AA$LOG_CRUDE <- log(CVD_MORT_AA$CRUDE)
CVD_MORT_AA$POP <- as.numeric(CVD_MORT_AA$Population)
CVD_MORT_AA$YEAR_FACTOR <- as.factor(CVD_MORT_AA$Year)
CVD_MORT_AA$YEAR_CVD <- CVD_MORT_AA$Year

#Create the data####################################################################################



#These years came from the National Conference of State Legislatures 
#https://www.ncsl.org/labor-and-employment/paid-sick-leave
CVD_MORT_AA<-CVD_MORT_AA %>% 
  mutate(PSL_YEAR=case_when(State == 'Connecticut' ~ 2012,
                            State == 'District of Columbia' ~ 2014,
                            State == 'California' ~ 2015,
                            State == 'Massachusetts' ~ 2015,
                            State == 'Oregon' ~ 2016,
                            State == 'Arizona' ~ 2017,
                            State == 'Vermont' ~ 2017,
                            State == 'Maryland' ~ 2018,
                            State == 'Rhode Island' ~ 2018,
                            State == 'New Jersey' ~ 2018,
                            State == 'Washington' ~ 2018))

CVD_MORT_AA<-CVD_MORT_AA %>% 
  mutate(TREATED=case_when(State == 'Connecticut' ~ 1,
                           State == 'District of Columbia' ~ 1,
                           State == 'California' ~ 1,
                           State == 'Massachusetts' ~ 1,
                           State == 'Oregon' ~ 1,
                           State == 'Arizona' ~ 1,
                           State == 'Vermont' ~ 1,
                           State == 'Maryland' ~ 1,
                           State == 'New Jersey' ~ 1,
                           State == 'Rhode Island' ~ 1,
                           State == 'Washington' ~ 1,
                           TRUE ~ 0))


CVD_MORT_AA <-CVD_MORT_AA %>%
  mutate(CENSUSREGION=case_when (State == 'Connecticut' ~ "NE",
                                 State == 'Maine' ~ "NE",
                                 State == 'Massachusetts' ~ "NE",
                                 State == 'New Hampshire' ~ "NE",
                                 State == 'Rhode Island' ~ "NE",
                                 State == 'Vermont' ~ "NE",
                                 State == 'New Jersey' ~ "NE",
                                 State == 'New York' ~ "NE",
                                 State == 'Pennsylvania' ~ "NE",
                                 
                                 State == 'Indiana' ~ "MW",
                                 State == 'Illinois' ~ "MW",
                                 State == 'Michigan' ~ "MW",
                                 State == 'Ohio' ~ "MW",
                                 State == 'Wisconsin' ~ "MW",
                                 State == 'Iowa' ~ "MW",
                                 State == 'Kansas' ~ "MW",
                                 State == 'Minnesota' ~ "MW",
                                 State == 'Missouri' ~ "MW",
                                 State == 'Nebraska' ~ "MW",
                                 State == 'North Dakota' ~ "MW",
                                 State == 'South Dakota' ~ "MW",
                                 
                                 State == 'Delaware' ~ "SO",
                                 State == 'District of Columbia' ~ "SO",
                                 State == 'Florida' ~ "SO",
                                 State == 'Georgia' ~ "SO",
                                 State == 'Maryland' ~ "SO",
                                 State == 'North Carolina' ~ "SO",
                                 State == 'South Carolina' ~ "SO",
                                 State == 'Virginia' ~ "SO",
                                 State == 'West Virginia' ~ "SO",
                                 State == 'Alabama' ~ "SO",
                                 State == 'Kentucky' ~ "SO",
                                 State == 'Mississippi' ~ "SO",
                                 State == 'Tennessee' ~ "SO",
                                 State == 'Arkansas' ~ "SO",
                                 State == 'Louisiana' ~ "SO",
                                 State == 'Oklahoma' ~ "SO",
                                 State == 'Texas' ~ "SO",
                                 
                                 State == 'Arizona' ~ "WT",
                                 State == 'Colorado' ~ "WT",
                                 State == 'Idaho' ~ "WT",
                                 State == 'New Mexico' ~ "WT",
                                 State == 'Montana' ~ "WT",
                                 State == 'Utah' ~ "WT",
                                 State == 'Nevada' ~ "WT",
                                 State == 'Wyoming' ~ "WT",
                                 State == 'Alaska' ~ "WT",
                                 State == 'California' ~ "WT",
                                 State == 'Hawaii' ~ "WT",
                                 State == 'Oregon' ~ "WT",
                                 State == 'Washington' ~ "WT",
                                 
                                 
  ))


CVD_MORT_AA$TREATED <- as.factor(CVD_MORT_AA$TREATED)

CVD_MORT_AA<-CVD_MORT_AA %>% 
  mutate(POLICY_YEAR=case_when(!is.na(CVD_MORT_AA$PSL_YEAR) ~ Year-PSL_YEAR))

CVD_MORT_AA<-CVD_MORT_AA %>% 
  mutate(SAYRTRT=case_when(!is.na(CVD_MORT_AA$PSL_YEAR) ~ PSL_YEAR))

CVD_MORT_AA$SAYRTRT[is.na(CVD_MORT_AA$PSL_YEAR)]<-100000


table(CVD_MORT_AA$POLICY_YEAR)
table(CVD_MORT_AA$CENSUSREGION)

#LAGS AND LEADS FOR EVENT STUDY 
CVD_MORT_AA<-CVD_MORT_AA %>% 
  mutate(
    lead1 = case_when(POLICY_YEAR == -1 ~ 1, TRUE ~ 0),
    lead2 = case_when(POLICY_YEAR == -2 ~ 1, TRUE ~ 0),
    lead3 = case_when(POLICY_YEAR == -3 ~ 1, TRUE ~ 0),
    lead4 = case_when(POLICY_YEAR == -4 ~ 1, TRUE ~ 0),
    lead5 = case_when(POLICY_YEAR == -5 ~ 1, TRUE ~ 0),
    lead6 = case_when(POLICY_YEAR == -6 ~ 1, TRUE ~ 0),
    lead7 = case_when(POLICY_YEAR == -7 ~ 1, TRUE ~ 0),
    lead8 = case_when(POLICY_YEAR == -8 ~ 1, TRUE ~ 0),
    lead9 = case_when(POLICY_YEAR == -9 ~ 1, TRUE ~ 0),
    lead10 = case_when(POLICY_YEAR == -10 ~ 1, TRUE ~ 0),
    
    lag0 = case_when(POLICY_YEAR == 0 ~ 1, TRUE ~ 0),
    lag1 = case_when(POLICY_YEAR == 1 ~ 1, TRUE ~ 0),
    lag2 = case_when(POLICY_YEAR == 2 ~ 1, TRUE ~ 0),
    lag3 = case_when(POLICY_YEAR == 3 ~ 1, TRUE ~ 0),
    lag4 = case_when(POLICY_YEAR == 4 ~ 1, TRUE ~ 0),
    lag5 = case_when(POLICY_YEAR == 5 ~ 1, TRUE ~ 0),
    lag6 = case_when(POLICY_YEAR == 6 ~ 1, TRUE ~ 0),
    lag7 = case_when(POLICY_YEAR == 7 ~ 1, TRUE ~ 0),
    lag8 = case_when(POLICY_YEAR == 8 ~ 1, TRUE ~ 0),
    lag9 = case_when(POLICY_YEAR == 9 ~ 1, TRUE ~ 0))



#Post Variable for TWFE
CVD_MORT_AA<-CVD_MORT_AA %>% 
  mutate(POST=case_when(POLICY_YEAR>0 ~ 1,
                        TRUE ~ 0))

table(CVD_MORT_AA$POST, useNA="always")
summary(CVD_MORT_AA$CRUDE, useNA="always", fun=mean)


#####BRING IN COVARIATES 
COVARS<-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/COVARS.CSV")

ALL_AA<-left_join(CVD_MORT_AA, COVARS, by = c("FIPS" = "FULL_FIPS2","YEAR_CVD"="Year"))

#############TABLE 1#############

ALL_years_AA <- ALL_AA %>%
  group_by(FIPS)%>%
  mutate(
    year_count = seq_along(FIPS)
    ,year_count_max = max(year_count, na.rm = T)
    , .after = Year.Code
  )%>%
  ungroup()

ALL_years_AA <- ALL_years_AA %>%
  filter(
    year_count_max == 12
  )


ALL_years_AA <- ALL_years_AA %>%
  group_by(FIPS)%>%
  mutate(
    mean_pop = mean(Population)
  )%>%
  ungroup()

AA_NE <-subset(ALL_years_AA, CENSUSREGION=="NE") 
AA_WT <-subset(ALL_years_AA, CENSUSREGION=="WT")

##Event Study 

##NORTHEAST
event_AA_NE_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                          + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE
                          |County + Year,
                          cluster=~State, 
                          weights=AA_NE$Population, data = AA_NE)
summary(event_AA_NE_SAB)
##west
event_AA_WT_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                            + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE
                            |County + Year,
                            cluster=~State, 
                            weights=AA_WT$Population, data = AA_WT)
summary(event_AA_WT_SAB)

##US
event_AA_US_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                        + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE
                        |County + Year,
                        cluster=~State, 
                        weights=ALL_years_AA$Population, data = ALL_years_AA)
summary(event_AA_US_SAB)


