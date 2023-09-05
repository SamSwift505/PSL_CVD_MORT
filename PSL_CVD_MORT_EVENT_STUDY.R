#install.packages("estimatr")

library(tidyverse)
library(estimatr)
library(lfe)

#Read in CDC Data
CVD_MORT <- read.delim(file = "ADD YOUR PATH HERE.txt", header = TRUE, sep = "\t", dec = ".")
#Get rid of notes
CVD_MORT<-subset(CVD_MORT,(!is.na(CVD_MORT$State.Code)))

#Some Re-coding
names(CVD_MORT)[names(CVD_MORT) == "County.Code"] <- "FIPS"
#CVD_MORT$AA_RATE <- as.numeric(CVD_MORT$Age.Adjusted.Rate)
CVD_MORT$CRUDE <- as.numeric(CVD_MORT$Crude.Rate)
CVD_MORT$LOG_CRUDE <- log(CVD_MORT$CRUDE)
CVD_MORT$IHS_CRUDE <- asinh(CVD_MORT$CRUDE)
CVD_MORT$LOG_CRUDE <- log(CVD_MORT$CRUDE)
CVD_MORT$POP <- as.numeric(CVD_MORT$Population)
CVD_MORT$YEAR_FACTOR <- as.factor(CVD_MORT$Year)
CVD_MORT$YEAR_CVD <- CVD_MORT$Year

#Create the data####################################################################################



#These years came from the National Conference of State Legislatures 
#https://www.ncsl.org/labor-and-employment/paid-sick-leave
CVD_MORT<-CVD_MORT %>% 
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

CVD_MORT<-CVD_MORT %>% 
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


CVD_MORT <-CVD_MORT %>%
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
                                          State == 'Washington' ~ "WT"
                                          
                                          
                                          ))
          


CVD_MORT$TREATED <- as.factor(CVD_MORT$TREATED)

CVD_MORT<-CVD_MORT %>% 
          mutate(POLICY_YEAR=case_when(!is.na(CVD_MORT$PSL_YEAR) ~ Year-PSL_YEAR))

table(CVD_MORT$POLICY_YEAR)
table(CVD_MORT$CENSUSREGION)

#LAGS AND LEADS FOR EVENT STUDY 
CVD_MORT<-CVD_MORT %>% 
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
CVD_MORT<-CVD_MORT %>% 
  mutate(POST=case_when(POLICY_YEAR>0 ~ 1,
                           TRUE ~ 0))

table(CVD_MORT$POST, useNA="always")
summary(CVD_MORT$CRUDE, useNA="always", fun=mean)

#Check it
table(CVD_MORT$State,CVD_MORT$PSL_YEAR, useNA="always")
table(CVD_MORT$TREATED,CVD_MORT$PSL_YEAR, useNA="always")
table(CVD_MORT$State,CVD_MORT$TREATED, useNA="always")



####### Analysis and Analytic Data
CVD_MORT<-subset(CVD_MORT,(!is.na(CVD_MORT$LOG_CRUDE)))
CVD_MORT<-subset(CVD_MORT,Year !=2020)
#####BRING IN COVARIATES 
COVARS<-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/COVARS.CSV")

ALL<-left_join(CVD_MORT, COVARS, by = c("FIPS" = "FULL_FIPS2","YEAR_CVD"="Year"))

#############Analytic Dataset, N=1054#############

ALL_years <- ALL %>%
  group_by(FIPS)%>%
  mutate(
    year_count = seq_along(FIPS)
    ,year_count_max = max(year_count, na.rm = T)
    , .after = Year.Code
  )%>%
  ungroup()

ALL_years <- ALL_years %>%
  filter(
    year_count_max == 12
  )


ALL_years <- ALL_years %>%
  group_by(FIPS)%>%
  mutate(
    mean_pop = mean(Population)
  )%>%
  ungroup()

write.csv(ALL_years , "C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/MORT_DATA/FINAL_DATASET.csv", row.names=FALSE)

###################### Table 1 #############################

ALL_years_NE <- subset(ALL_years, CENSUSREGION =="NE")
ALL_years_SO <- subset(ALL_years, CENSUSREGION =="SO")
ALL_years_MW <- subset(ALL_years, CENSUSREGION =="MW")
ALL_years_WT <- subset(ALL_years, CENSUSREGION =="WT")


### 2008 VARIABLES



BASELINE<-subset(ALL_years, Year==2008)
aggregate(BASELINE$MED_INC, by=list(BASELINE$TREATED), FUN=mean)
INC <- t.test(BASELINE$MED_INC ~ BASELINE$TREATED, data = ALL, var.equal = TRUE)
INC
aggregate(BASELINE$MED_INC, by=list(BASELINE$TREATED), FUN=sd)
POV <- t.test(BASELINE$POV_PERCENT_ALL ~ BASELINE$TREATED, data = ALL, var.equal = TRUE)
POV
aggregate(BASELINE$POV_PERCENT_ALL, by=list(BASELINE$TREATED), FUN=sd)
UNEMP <- t.test(BASELINE$UNEMP_RATE ~ BASELINE$TREATED, data = ALL, var.equal = TRUE)
UNEMP
aggregate(BASELINE$UNEMP_RATE, by=list(BASELINE$TREATED), FUN=sd)
UNINS <- t.test(BASELINE$UNINSURED_RATE ~ BASELINE$TREATED, data = ALL, var.equal = TRUE)
UNINS
aggregate(BASELINE$UNINSURED_RATE, by=list(BASELINE$TREATED), FUN=sd)

############################# PERCENT CHANGE OVER TIME
CHANGE08<-subset(ALL_years, Year==2008)
CHANGE08$INC08<-CHANGE08$MED_INC
CHANGE08$POV08<-CHANGE08$POV_PERCENT_ALL
CHANGE08$UNEMP08<-CHANGE08$UNEMP_RATE
CHANGE08$UNINS08<-CHANGE08$UNINSURED_RATE
CHANGE08$CRUDE08<-CHANGE08$CRUDE
CHANGE08<-subset(CHANGE08, select = c(FIPS, County,INC08,POV08,UNEMP08,UNINS08,TREATED,CRUDE08))

CHANGE19<-subset(ALL_years, Year==2019)
CHANGE19$INC19<-CHANGE19$MED_INC
CHANGE19$POV19<-CHANGE19$POV_PERCENT_ALL
CHANGE19$UNEMP19<-CHANGE19$UNEMP_RATE
CHANGE19$UNINS19<-CHANGE19$UNINSURED_RATE
CHANGE19$CRUDE19<-CHANGE19$CRUDE
CHANGE19<-subset(CHANGE19, select = c(FIPS, NAME.x,INC19,POV19,UNEMP19,UNINS19,CRUDE19))

CHANGE<-left_join(CHANGE08, CHANGE19, by = c("FIPS" = "FIPS"))

CHANGE$INCDIFF<-CHANGE$INC19-CHANGE$INC08
CHANGE$POVDIFF<-CHANGE$POV19-CHANGE$POV08
CHANGE$UNEMPDIFF<-CHANGE$UNEMP19-CHANGE$UNEMP08
CHANGE$UNINSDIFF<-CHANGE$UNINS19-CHANGE$UNINS08
CHANGE$CRUDEDIFF<-CHANGE$CRUDE19-CHANGE$CRUDE08
CHANGE %>%
reframe(quantile = scales::percent(c(0.25, 0.5, 0.75)),
          CRUDEDIFF = quantile(CRUDEDIFF, c(0.25, 0.5, 0.75)))
CHANGE <- CHANGE %>%
  mutate(ChangeCat = case_when(CRUDEDIFF <= 0 ~ "Decreased or No Change",
                          CRUDEDIFF > 0 & CRUDEDIFF<=10 ~ "Increased by 0-10 deaths/ 100K",
                          CRUDEDIFF>10 ~ "Increased by greater than 10 deaths/ 100K"))
CHANGEMAP<-subset(CHANGE, select = c(NAME.x,ChangeCat))

write.csv(CHANGEMAP, "C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/MORT_DATA/CHANGEMAP.csv", row.names=FALSE)

INCCHANGE <- t.test(CHANGE$INCDIFF ~ CHANGE$TREATED, data = ALL, var.equal = TRUE)
INCCHANGE
aggregate(CHANGE$INCDIFF, by=list(CHANGE$TREATED), FUN=sd, na.rm = TRUE)
POVCHANGE <- t.test(CHANGE$POVDIFF ~ CHANGE$TREATED, data = ALL, var.equal = TRUE)
POVCHANGE
aggregate(CHANGE$POVDIFF, by=list(CHANGE$TREATED), FUN=sd, na.rm = TRUE)
CHANGE$TREATED
UNEMPCHANGE <- t.test(CHANGE$UNEMPDIFF ~ CHANGE$TREATED, data = ALL, var.equal = TRUE)
UNEMPCHANGE
aggregate(CHANGE$UNEMPDIFF, by=list(CHANGE$TREATED), FUN=sd, na.rm = TRUE)
UNINSCHANGE <- t.test(CHANGE$UNINSDIFF ~ CHANGE$TREATED, data = ALL, var.equal = TRUE)
UNINSCHANGE
aggregate(CHANGE$UNINSDIFF, by=list(CHANGE$TREATED), FUN=sd, na.rm = TRUE)

event_unadj<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
                     lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7  
                   |County + Year|0|State, weights=ALL_years$mean_pop, data = ALL_years)
summary(event_unadj)

######################  Event Studies 
event<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
               lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7 + MED_INC + POV_PERCENT_ALL + 
               UNEMP_RATE + UNINSURED_RATE
             |County + Year|0|State, weights=ALL_years$mean_pop, data = ALL_years)
summary(event)

##################### EVENT STUDY BY REGION 

eventNE<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
               lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7 + MED_INC + POV_PERCENT_ALL + 
               UNEMP_RATE + UNINSURED_RATE
             |County + Year|0|State, weights=ALL_years_NE$mean_pop, data = ALL_years_NE)
summary(eventNE)

eventWT<- felm(CRUDE~ lead1 + lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
                 lag1+ lag2 +lag3 +lag4 + MED_INC + POV_PERCENT_ALL + 
                 UNEMP_RATE + UNINSURED_RATE
               |County + Year|0|State, weights=ALL_years_WT$mean_pop, data = ALL_years_WT)
summary(eventWT)
