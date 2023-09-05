#install.packages("estimatr")

library(tidyverse)
library(estimatr)
library(lfe)

#Read in CDC Data
CVD_MORT_FEMALE <- read.delim(file = "YOUR PATH HERE.txt", header = TRUE, sep = "\t", dec = ".")
CVD_MORT_MALE <- read.delim(file = "YOUR PATH HERE.txt", header = TRUE, sep = "\t", dec = ".")
CVD_MORT_AA <- read.delim(file = "YOUR PATH HERE.txt", header = TRUE, sep = "\t", dec = ".")


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

CVD_MORT_AA$TREATED <- as.factor(CVD_MORT_AA$TREATED)

CVD_MORT_AA<-CVD_MORT_AA %>% 
  mutate(POLICY_YEAR=case_when(!is.na(CVD_MORT_AA$PSL_YEAR) ~ Year-PSL_YEAR))

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
COVARS<-read_csv("YOUR PATH HERE.CSV")

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



##Event Study 
event_AA<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
                      lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7 + MED_INC + POV_PERCENT_ALL + 
                      UNEMP_RATE + UNINSURED_RATE
                    |County + Year|0|State, weights=ALL_years_AA$mean_pop, data = ALL_years_AA)
summary(event_AA)



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

CVD_MORT_FEMALE$TREATED <- as.factor(CVD_MORT_FEMALE$TREATED)

CVD_MORT_FEMALE<-CVD_MORT_FEMALE %>% 
          mutate(POLICY_YEAR=case_when(!is.na(CVD_MORT_FEMALE$PSL_YEAR) ~ Year-PSL_YEAR))

table(CVD_MORT_FEMALE$POLICY_YEAR)

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
COVARS<-read_csv("YOUR PATH HERE.CSV")

ALL_FEMALE<-left_join(CVD_MORT_FEMALE, COVARS, by = c("FIPS" = "FULL_FIPS2","YEAR_CVD"="Year"))

#############TABLE 1#############

ALL_years_FEMALE <- ALL_FEMALE %>%
  group_by(FIPS)%>%
  mutate(
    year_count = seq_along(FIPS)
    ,year_count_max = max(year_count, na.rm = T)
    , .after = Year.Code
  )%>%
  ungroup()

ALL_years_FEMALE <- ALL_years_FEMALE %>%
  filter(
    year_count_max == 12
  )


ALL_years_FEMALE <- ALL_years_FEMALE %>%
  group_by(FIPS)%>%
  mutate(
    mean_pop = mean(Population)
  )%>%
  ungroup()



##Event Study 
event_FEMALE<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
               lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7 + MED_INC + POV_PERCENT_ALL + 
               UNEMP_RATE + UNINSURED_RATE
             |County + Year|0|State, weights=ALL_years_FEMALE$mean_pop, data = ALL_years_FEMALE)
summary(event_FEMALE)


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

CVD_MORT_MALE$TREATED <- as.factor(CVD_MORT_MALE$TREATED)

CVD_MORT_MALE<-CVD_MORT_MALE %>% 
  mutate(POLICY_YEAR=case_when(!is.na(CVD_MORT_MALE$PSL_YEAR) ~ Year-PSL_YEAR))

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
COVARS<-read_csv("YOUR PATH HERE.CSV")

ALL_MALE<-left_join(CVD_MORT_MALE, COVARS, by = c("FIPS" = "FULL_FIPS2","YEAR_CVD"="Year"))

#############TABLE 1#############

ALL_years_MALE <- ALL_MALE %>%
  group_by(FIPS)%>%
  mutate(
    year_count = seq_along(FIPS)
    ,year_count_max = max(year_count, na.rm = T)
    , .after = Year.Code
  )%>%
  ungroup()

ALL_years_MALE <- ALL_years_MALE %>%
  filter(
    year_count_max == 12
  )


ALL_years_MALE <- ALL_years_MALE %>%
  group_by(FIPS)%>%
  mutate(
    mean_pop = mean(Population)
  )%>%
  ungroup()



##Event Study 
event_MALE<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
                      lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7 +lag8 + MED_INC + POV_PERCENT_ALL + 
                      UNEMP_RATE + UNINSURED_RATE
                    |County + Year|0|State, weights=ALL_years_MALE$mean_pop, data = ALL_years_MALE)
summary(event_MALE)

