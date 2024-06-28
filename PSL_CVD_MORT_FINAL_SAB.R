#install.packages("estimatr")

library(tidyverse)
library(estimatr)
library(lfe)
library(fixest)
library(magrittr)
library(dplyr)
library(augsynth)


###Vingettes for analysis came from here 
###Event Studies with staggered diff in diff
##https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

####Aggregating SAB Coefficients
##https://lrberge.github.io/fixest/reference/aggregate.fixest.html

###Local PSL policies https://www.abetterbalance.org/paid-sick-time-laws/

##Staggered synthetic control 
##https://github.com/ebenmichael/augsynth/blob/master/vignettes/multisynth-vignette.md


#Read in CDC Data
CVD_MORT <- read.delim(file = "C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/MORT_DATA/CVD_MORT_15_64.txt", header = TRUE, sep = "\t", dec = ".")
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


######TREATMENT INCLUDING LOCAL JURISDICTIONS########################
CVD_MORT<-CVD_MORT %>% 
  mutate(PSL_YEAR_LOCAL=case_when(State == 'Connecticut' ~ 2012,
                            State == 'District of Columbia' ~ 2014,
                            (State == 'California' & FIPS !=06075)~ 2015,##Take out CA FIPS
                            State == 'Massachusetts' ~ 2015,
                            State == 'Oregon' ~ 2016,
                            State == 'Arizona' ~ 2017,
                            State == 'Vermont' ~ 2017,
                            (State == 'Maryland' & FIPS !=24031)~ 2018,
                            State == 'Rhode Island' ~ 2018,
                            State == 'New Jersey' ~ 2018,
                            State == 'Washington' ~ 2018,
                            
                            FIPS  == 06075~ 2008, #San Francisco 
                            FIPS  == 42101~ 2014,  #Philadelphia, PA 
                            FIPS  == 24031~ 2016,  #Montgomery County, MD 
                            
                            FIPS  == 27053~ 2017, #Minneapolis, MN 
                            FIPS  == 27123~ 2018, #ST Paul MN 
                            
                            FIPS  == 17031~ 2017, #Cook County Illinois 
                            FIPS  == 36005~ 2014, #The Bronx
                            FIPS  == 36047~ 2014, #Brooklyn
                            FIPS  == 36061~ 2014, #Manhattan
                            FIPS  == 36081~ 2014, #Queens
                            FIPS  == 36085~ 2014, #Staten Island 
                            FIPS  == 42101~ 2015  #Philadelphia, PA 
                            ))

CVD_MORT<-CVD_MORT %>% 
  mutate(TREATED_LOCAL=case_when(State == 'Connecticut' ~ 1,
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
                           
                           ## Double check local policies!! 
                           FIPS  == 06075~ 1, #San Francisco 
                           FIPS  == 42101~ 1,  #Philadelphia, PA 
                           FIPS  == 24031~ 1,  #Montgomery County, MD 
                           
                           FIPS  == 27053~ 1, #Minneapolis, MN 
                           FIPS  == 27123~ 1, #ST Paul MN 
                           
                           FIPS  == 17031~ 1, #Cook County Illinois 
                           FIPS  == 36005~ 1, #The Bronx
                           FIPS  == 36047~ 1, #Brooklyn
                           FIPS  == 36061~ 1, #Manhattan
                           FIPS  == 36081~ 1, #Queens
                           FIPS  == 36085~ 1, #Staten Island 
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
                                          State == 'Washington' ~ "WT",
                                          
                                          
                                          ))
          


CVD_MORT$TREATED <- as.factor(CVD_MORT$TREATED)

##Include local policies 
CVD_MORT$TREATED_LOCAL<- as.factor(CVD_MORT$TREATED_LOCAL)

CVD_MORT<-CVD_MORT %>% 
          mutate(POLICY_YEAR=case_when(!is.na(CVD_MORT$PSL_YEAR) ~ Year-PSL_YEAR))

CVD_MORT<-CVD_MORT %>% 
  mutate(POLICY_YEAR_LOCAL=case_when(!is.na(CVD_MORT$PSL_YEAR_LOCAL) ~ Year-PSL_YEAR_LOCAL))

#Policy Year 2 to check FELM package against FEOLS from Fixest package
CVD_MORT<-CVD_MORT %>% 
  mutate(POLICY_YEAR2=case_when(!is.na(CVD_MORT$PSL_YEAR) ~ Year-PSL_YEAR))
  CVD_MORT$POLICY_YEAR2[is.na(CVD_MORT$POLICY_YEAR2)]<-0

table(CVD_MORT$POLICY_YEAR2)
table(CVD_MORT$CENSUSREGION)

#LAGS AND LEADS FOR EVENT STUDY TWFE
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

CVD_MORT<-CVD_MORT %>% 
  mutate(SAYRTRT=case_when(!is.na(CVD_MORT$PSL_YEAR) ~ PSL_YEAR))
CVD_MORT$SAYRTRT[is.na(CVD_MORT$PSL_YEAR)]<-100000


CVD_MORT<-CVD_MORT %>% 
  mutate(SAYRTRT_LOCAL=case_when(!is.na(CVD_MORT$PSL_YEAR_LOCAL) ~ PSL_YEAR_LOCAL))

CVD_MORT$SAYRTRT_LOCAL[is.na(CVD_MORT$PSL_YEAR_LOCAL)]<-100000

#Post Variable for TWFE
CVD_MORT<-CVD_MORT %>% 
  mutate(POST=case_when(POLICY_YEAR>0 ~ 1,
                           TRUE ~ 0))

#Treated Year for Synth(Begins in treated year not year after)
CVD_MORT<-CVD_MORT %>% 
  mutate(SYNTH_TRT=case_when(POLICY_YEAR>-1 ~ 1,
                        TRUE ~ 0))

#Treated Year for Synth(Begins in treated year not year after)
CVD_MORT<-CVD_MORT %>% 
  mutate(SYNTH_TRT_LOCAL=case_when(POLICY_YEAR_LOCAL>-1 ~ 1,
                             TRUE ~ 0))


####### Analysis and Analytic Data
CVD_MORT<-subset(CVD_MORT,(!is.na(CVD_MORT$CRUDE)))
CVD_MORT<-subset(CVD_MORT,Year !=2020)
#####BRING IN COVARIATES 
COVARS<-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/COVARS.CSV")

ALL<-left_join(CVD_MORT, COVARS, by = c("FIPS" = "FULL_FIPS2","YEAR_CVD"="Year"))

length(unique(ALL$FIPS))


#############TABLE 1############################################################

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

length(unique(ALL_years$FIPS))

cor(ALL_years$UNINSURED_RATE,ALL_years$POV_PERCENT_ALL)
cor(ALL_years$UNINSURED_RATE,ALL_years$MEDICAID)
cor(ALL_years$UNINSURED_RATE,ALL_years$MED_INC)
cor(ALL_years$UNINSURED_RATE,ALL_years$UNEMP_RATE)
cor(ALL_years$POV_PERCENT_ALL,ALL_years$MEDICAID)
cor(ALL_years$POV_PERCENT_ALL,ALL_years$MED_INC)
cor(ALL_years$POV_PERCENT_ALL,ALL_years$UNEMP_RATE)
cor(ALL_years$MEDICAID,ALL_years$MED_INC)
cor(ALL_years$MEDICAID,ALL_years$UNEMP_RATE)
### 2008 VARIABLES

BASELINE<-subset(ALL, Year==2008)
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

###Expanded Medicaid by 2019
expanded<-subset(ALL_years, Year==2019)
EXPAND<-table(expanded$MEDICAID,expanded$TREATED)
chisq.test(EXPAND)


### PERCENT CHANGE OVER TIME
CHANGE08<-subset(ALL, Year==2008)
CHANGE08$INC08<-CHANGE08$MED_INC
CHANGE08$POV08<-CHANGE08$POV_PERCENT_ALL
CHANGE08$UNEMP08<-CHANGE08$UNEMP_RATE
CHANGE08$UNINS08<-CHANGE08$UNINSURED_RATE
CHANGE08<-subset(CHANGE08, select = c(County,INC08,POV08,UNEMP08,UNINS08,TREATED))

CHANGE19<-subset(ALL, Year==2019)
CHANGE19$INC19<-CHANGE19$MED_INC
CHANGE19$POV19<-CHANGE19$POV_PERCENT_ALL
CHANGE19$UNEMP19<-CHANGE19$UNEMP_RATE
CHANGE19$UNINS19<-CHANGE19$UNINSURED_RATE
CHANGE19<-subset(CHANGE19, select = c(NAME.x,INC19,POV19,UNEMP19,UNINS19))

CHANGE<-left_join(CHANGE08, CHANGE19, by = c("County" = "NAME.x"))

CHANGE$INCDIFF<-CHANGE$INC19-CHANGE$INC08
CHANGE$POVDIFF<-CHANGE$POV19-CHANGE$POV08
CHANGE$UNEMPDIFF<-CHANGE$UNEMP19-CHANGE$UNEMP08
CHANGE$UNINSDIFF<-CHANGE$UNINS19-CHANGE$UNINS08

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

#### CHECK MISSING COVARIATE DATA 

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









#### FIGURE 3 EVENT STUDIES BY REGION AND ALL US MODELS ############################

####################################################################################

ALL_years_NE <-subset(ALL_years, CENSUSREGION=="NE") 
#South just for DC and MARYLAND
ALL_years_SO <-subset(ALL_years, CENSUSREGION=="SO") 
ALL_years_WT <-subset(ALL_years, CENSUSREGION=="WT") 


######## NORTHEAST ######################################################


##### ADJUSTED #
eventNORTHEAST<- felm(CRUDE~ lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
                        lag0 + lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7 + POV_PERCENT_ALL + 
               UNEMP_RATE + UNINSURED_RATE + MEDICAID
             |County + Year|0|State, weights=ALL_years_NE$Population, data = ALL_years_NE)
summary(eventNORTHEAST)
confint(eventNORTHEAST)

##### ADJUSTED SUNAB #
eventNORTHEAST_64_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                             + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID 
                            |County + Year,
                            cluster=~State, 
                            weights=ALL_years_NE$Population, data = ALL_years_NE)
summary(eventNORTHEAST_64_SAB)
confint(eventNORTHEAST_64_SAB)


######## WEST ######################################################


##### ADJUSTED #
eventWEST<- felm(CRUDE~ lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
               lag0+ lag1+ lag2 +lag3 +lag4 + MED_INC + POV_PERCENT_ALL + 
               UNEMP_RATE + UNINSURED_RATE + MEDICAID
             |County + Year|0|State, weights=ALL_years_WT$Population, data = ALL_years_WT)
summary(eventWEST)

###SAB#####
eventWEST_64_SAB<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                              + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                              |County + Year,
                              cluster=~State, 
                              weights=ALL_years_WT$Population, data = ALL_years_WT)
summary(eventWEST_64_SAB)


##################### ALL US ###########################################

##### ADJUSTED TWFE ALL US#
eventUS<- felm(CRUDE~ lead2 +lead3 +lead4 +lead5 +lead6 +lead7 +lead8 +lead9 +lead10 +
                   lag0+ lag1+ lag2 +lag3 +lag4 +lag5 +lag6 +lag7 + MED_INC + POV_PERCENT_ALL + 
                   UNEMP_RATE + UNINSURED_RATE
                 |County + Year|0|State, weights=ALL_years$Population, data = ALL_years)
summary(eventUS)


event_US_SAB <- feols(CRUDE~ sunab(SAYRTRT, Year) 
                  + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                  |County + Year,
                  cluster=~State, 
                  weights=ALL_years$Population, data = ALL_years)
summary(event_US_SAB)


































################################################################################################################
#### TABLE 2 ATT ESTIMATES
################################################################################################################

#####MODEL 1 UN ADJUSTED SUNAB NORTHEAST #
M1_ATT_NE<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                              |County + Year,
                              cluster=~State, 
                              weights=ALL_years_NE$Population, data = ALL_years_NE)
aggregate(M1_ATT_NE,"att")
-1.402356+(0.2346695*1.96)
-1.402356-(0.2346695*1.96)

#####MODEL 2 ADJUSTED SUNAB NORTHEAST #
M2_ATT_NE<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                              + MED_INC + UNEMP_RATE + UNINSURED_RATE 
                              |County + Year,
                              cluster=~State, 
                              weights=ALL_years_NE$Population, data = ALL_years_NE)
aggregate(M2_ATT_NE,"att")
-2.048626+(0.7233546*1.96)
-2.048626-(0.7233546*1.96)


#####MODEL 3 ADJUSTED SUNAB NORTHEAST #
M3_ATT_NE<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                              + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                              |County + Year,
                              cluster=~State, 
                              weights=ALL_years_NE$Population, data = ALL_years_NE)
aggregate(M3_ATT_NE,"att")
-1.983669+(0.7012966*1.96)
-1.983669-(0.7012966*1.96)



######## WEST ######################################################

#####MODEL 1 UN ADJUSTED SUNAB WEST #
M1_ATT_NE<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                  |County + Year,
                  cluster=~State, 
                  weights=ALL_years_WT$Population, data = ALL_years_WT)
aggregate(M1_ATT_NE,"att")
1.857644+(1.162567*1.96)
1.857644-(1.162567*1.96)

#####MODEL 2 ADJUSTED SUNAB WEST #
M2_ATT_NE<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                  + MED_INC + UNEMP_RATE + UNINSURED_RATE 
                  |County + Year,
                  cluster=~State, 
                  weights=ALL_years_WT$Population, data = ALL_years_WT)
aggregate(M2_ATT_NE,"att")
2.874865+(1.144731*1.96)
2.874865-(1.144731*1.96)


#####MODEL 3 ADJUSTED SUNAB WEST #
M3_ATT_NE<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                         + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                         |County + Year,
                         cluster=~State, 
                         weights=ALL_years_WT$Population, data = ALL_years_WT)
aggregate(M3_ATT_NE,"att")
2.874865+(1.144731*1.96)
2.874865-(1.144731*1.96)





##### ALL US #######
#####MODEL 1 UNADJUSTED SUNAB US #
M1_ATT_US <- feols(CRUDE~ sunab(SAYRTRT, Year) 
                   |County + Year,
                   cluster=~State, 
                   weights=ALL_years$Population, data = ALL_years)
aggregate(M1_ATT_US,"att")
-0.4365649+(0.3243811*1.96)
-0.4365649-(0.3243811*1.96)

#####MODEL 2 ADJUSTED SUNAB US #
M2_ATT_US <- feols(CRUDE~ sunab(SAYRTRT, Year) 
                   + MED_INC + UNEMP_RATE + UNINSURED_RATE 
                   |County + Year,
                   cluster=~State, 
                   weights=ALL_years$Population, data = ALL_years)
aggregate(M2_ATT_US,"att")
0.9771988+(0.4240244*1.96)
0.9771988-(0.4240244*1.96)

#####MODEL 3 ADJUSTED SUNAB US #
M3_ATT_US <- feols(CRUDE~ sunab(SAYRTRT, Year) 
                      + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                      |County + Year,
                      cluster=~State, 
                      weights=ALL_years$Population, data = ALL_years)
aggregate(M3_ATT_US,"att")
0.9262361+(0.4212802*1.96)
0.9262361-(0.4212802*1.96)






####################################SYNTHETIC CONTROL FOR STAGGERED TIMING####################################
##### TABLE 2 
##############################################################################################################
PSL_synth <- multisynth(CRUDE ~ SYNTH_TRT, FIPS, Year, ALL_years)

print(PSL_synth$nu)

PSL_synth_summ<-summary(PSL_synth)

PSL_synth_summ
-2.921+(1.115*1.96)
-2.921-(1.115*1.96)

plot(PSL_synth_summ, levels="Average")

###SYNTHETIC CONTROL FOR STAGGERED TIMING INCLUDING COVARIATES Model 2
PSL_synth_COVARS <- multisynth(CRUDE ~ SYNTH_TRT | MED_INC + UNEMP_RATE + UNINSURED_RATE, FIPS, Year, ALL_years)

table(ALL_years$MEDICAID,ALL_years$Year)

print(PSL_synth_COVARS$nu)

PSL_synth_summ_COVARS<-summary(PSL_synth_COVARS)

PSL_synth_summ_COVARS

plot(PSL_synth_summ_COVARS, levels="Average")
-1.704+(1.472*1.96)
-1.704-(1.472*1.96)




###SYNTHETIC CONTROL FOR STAGGERED TIMING INCLUDING COVARIATES
PSL_synth_COVARS <- multisynth(CRUDE ~ SYNTH_TRT | MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE, FIPS, Year, ALL_years)

table(ALL_years$MEDICAID,ALL_years$Year)

print(PSL_synth_COVARS$nu)

PSL_synth_summ_COVARS<-summary(PSL_synth_COVARS)

PSL_synth_summ_COVARS

plot(PSL_synth_summ_COVARS, levels="Average")
-1.466+(1.320*1.96)
-1.466-(1.320*1.96)

##Synthetic Control Local Policies
ALL_years_SYN<-subset(ALL_years, FIPS !=6075)
PSL_synth_COVARS <- multisynth(CRUDE ~ SYNTH_TRT_LOCAL | MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE, FIPS, Year, ALL_years_SYN)

print(PSL_synth_COVARS$nu)

PSL_synth_summ_COVARS<-summary(PSL_synth_COVARS)

PSL_synth_summ_COVARS

plot(PSL_synth_summ_COVARS, levels="Average")











#####SUPPLIMENTARY figure 3 LOCAL POLICIES##### 

##### ADJUSTED SUNABLOCAL #
eventNORTHEAST_64_SAB_LOCAL<- feols(CRUDE~ sunab(SAYRTRT_LOCAL, Year) 
                                    + MED_INC+ POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                                    |County + Year,
                                    cluster=~State, 
                                    weights=ALL_years_NE$Population, data = ALL_years_NE)
summary(eventNORTHEAST_64_SAB_LOCAL)
aggregate(eventNORTHEAST_64_SAB_LOCAL,"att")

###SAB WEST LOCAL#####
eventWEST_64_SAB_LOCAL<- feols(CRUDE~ sunab(SAYRTRT_LOCAL, Year) 
                               + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                               |County + Year,
                               cluster=~State, 
                               weights=ALL_years_WT$Population, data = ALL_years_WT)
summary(eventWEST_64_SAB_LOCAL)

##### ADJUSTED SUNAB ALL US LOCAL#
eventUS_64_SAB_LOCAL<- feols(CRUDE~ sunab(SAYRTRT_LOCAL, Year) 
                        + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                        |County + Year,
                        cluster=~State, 
                        weights=ALL_years$Population, data = ALL_years)
summary(eventUS_SAB_LOCAL)
aggregate(eventUS_SAB_LOCAL,"cohort")

### EXCLUDE LOCAL MUNICIPALITIES ###########################################
ALL_years_NOLOCAL<-subset(ALL_years, (FIPS !=6075 & FIPS !=42101 & FIPS !=24031 & FIPS !=27053 & FIPS !=27123 & FIPS !=17031
                                      & FIPS !=36005 & FIPS !=36047 & FIPS !=36061 & FIPS !=36081 & FIPS !=36085 & FIPS !=42101))


eventUS_64_SAB_NOLOCAL<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                               + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                               |County + Year,
                               cluster=~State, 
                               weights=ALL_years_NOLOCAL$Population, data = ALL_years_NOLOCAL)
summary(eventUS_64_SAB_NOLOCAL)

ALL_years_NE_NOLOCAL<-subset(ALL_years_NE, (FIPS !=6075 & FIPS !=42101 & FIPS !=24031 & FIPS !=27053 & FIPS !=27123 & FIPS !=17031
                                      & FIPS !=36005 & FIPS !=36047 & FIPS !=36061 & FIPS !=36081 & FIPS !=36085 & FIPS !=42101))
eventNORTHEAST_64_SAB_NOLOCAL<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                             + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                             |County + Year,
                             cluster=~State, 
                             weights=ALL_years_NE_NOLOCAL$Population, data = ALL_years_NE_NOLOCAL)
summary(eventNORTHEAST_64_SAB_NOLOCAL)

ALL_years_WT_NOLOCAL<-subset(ALL_years_WT, (FIPS !=6075 & FIPS !=42101 & FIPS !=24031 & FIPS !=27053 & FIPS !=27123 & FIPS !=17031
                                            & FIPS !=36005 & FIPS !=36047 & FIPS !=36061 & FIPS !=36081 & FIPS !=36085 & FIPS !=42101))
eventWEST_64_SAB_NOLOCAL<- feols(CRUDE~ sunab(SAYRTRT, Year) 
                                    + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                                    |County + Year,
                                    cluster=~State, 
                                    weights=ALL_years_WT_NOLOCAL$Population, data = ALL_years_WT_NOLOCAL)
summary(eventWEST_64_SAB_NOLOCAL)






######################Models without Population Weights##################


##NE
event_NE_SAB_NOWEIGHTS <- feols(CRUDE~ sunab(SAYRTRT, Year) 
                      + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                      |County + Year,
                      cluster=~State, 
                      data = ALL_years_NE)
summary(event_NE_SAB_NOWEIGHTS)
##WT
event_WT_SAB_NOWEIGHTS <- feols(CRUDE~ sunab(SAYRTRT, Year) 
                                + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                                |County + Year,
                                cluster=~State, 
                                data = ALL_years_WT)
summary(event_WT_SAB_NOWEIGHTS)
##US
event_US_SAB_NOWEIGHTS <- feols(CRUDE~ sunab(SAYRTRT, Year) 
                                + MED_INC + POV_PERCENT_ALL + UNEMP_RATE + UNINSURED_RATE + MEDICAID
                                |County + Year,
                                cluster=~State, 
                                data = ALL_years)
summary(event_US_SAB_NOWEIGHTS)






































#####################CALC_MIDPOINT_POP#############################################

POP <- read.csv(file = "C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/Pop/POP1565.csv", header = TRUE)

POP$YEAR2<-as.numeric(POP$year)

POP <- POP %>%
  mutate(FULL_FIPS2=case_when((STATE_FIPS<10 & COUNTY_FIPS<10) ~ as.numeric(paste0(STATE_FIPS,"00",COUNTY_FIPS)),
                              (STATE_FIPS<10 & COUNTY_FIPS>=10 & COUNTY_FIPS<100) ~ as.numeric(paste0(STATE_FIPS,"0",COUNTY_FIPS)),
                              (STATE_FIPS<10 & COUNTY_FIPS>=100) ~ as.numeric(paste0(STATE_FIPS,COUNTY_FIPS)),
                              (STATE_FIPS>=10 & COUNTY_FIPS<10) ~ as.numeric(paste0(STATE_FIPS,"00",COUNTY_FIPS)),
                              (STATE_FIPS>=10 & COUNTY_FIPS>=10 & COUNTY_FIPS<100) ~ as.numeric(paste0(STATE_FIPS,"0",COUNTY_FIPS)),
                              (STATE_FIPS>=10 & COUNTY_FIPS>=100) ~ as.numeric(paste0(STATE_FIPS,COUNTY_FIPS))))


ALL_MORT<-left_join(ALL_years, POP, by = c("FIPS" = "FULL_FIPS2","Year"="YEAR2"))

STDYsum<-subset(ALL_MORT, Year==2014|Year==2013)
sum(STDYsum$Population)

ALLsum<-subset(POP, year==2014|year==2013)
ALLsum<-subset(ALLsum,(!is.na(STATE_FIPS)))
ALLsum<-subset(ALLsum,(!is.na(COUNTY_FIPS)))
sum(ALLsum$POPCOUNT)





