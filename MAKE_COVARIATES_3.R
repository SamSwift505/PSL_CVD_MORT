library(tidyverse)




#POV DATA

# #import without the list 
POV08 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est08all.csv")
POV09 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est09all.csv")
POV10 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est10all.csv")
POV11 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est11all.csv")
POV12 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est12all.csv")
POV13 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est13all.csv")
POV14 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est14all.csv")
POV15 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est15all.csv")
POV16 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est16all.csv")
POV17 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est17all.csv")
POV18 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est18all.csv")
POV19 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAIPE/est19all.csv")


POV08 <- subset(POV08,select=c("State FIPS", "County FIPS", "Name", "Poverty Percent All Ages","Median Household Income"))
POV08$Year<-2008
names(POV08)[names(POV08) == "State FIPS"] <- "State_FIPS"
names(POV08)[names(POV08) == "County FIPS"] <- "County_FIPS"
names(POV08)[names(POV08) == "Poverty Percent All Ages"] <- "POV_PERCENT_ALL"
names(POV08)[names(POV08) == "Median Household Income"] <- "MED_INC"


POV09 <- subset(POV09,select=c("State FIPS", "County FIPS", "Name", "Poverty Percent All Ages","Median Household Income"))
POV09$Year<-2009
names(POV09)[names(POV09) == "State FIPS"] <- "State_FIPS"
names(POV09)[names(POV09) == "County FIPS"] <- "County_FIPS"
names(POV09)[names(POV09) == "Poverty Percent All Ages"] <- "POV_PERCENT_ALL"
names(POV09)[names(POV09) == "Median Household Income"] <- "MED_INC"

POV10 <- subset(POV10,select=c("State FIPS", "County FIPS", "Name", "Poverty Percent All Ages","Median Household Income"))
POV10$Year<-2010
names(POV10)[names(POV10) == "State FIPS"] <- "State_FIPS"
names(POV10)[names(POV10) == "County FIPS"] <- "County_FIPS"
names(POV10)[names(POV10) == "Poverty Percent All Ages"] <- "POV_PERCENT_ALL"
names(POV10)[names(POV10) == "Median Household Income"] <- "MED_INC"

POV11 <- subset(POV11,select=c("State FIPS", "County FIPS", "Name", "Poverty Percent All Ages","Median Household Income"))
POV11$Year<-2011
names(POV11)[names(POV11) == "State FIPS"] <- "State_FIPS"
names(POV11)[names(POV11) == "County FIPS"] <- "County_FIPS"
names(POV11)[names(POV11) == "Poverty Percent All Ages"] <- "POV_PERCENT_ALL"
names(POV11)[names(POV11) == "Median Household Income"] <- "MED_INC"

POV12 <- subset(POV12,select=c("State FIPS Code", "County FIPS Code", "Name", "Poverty Percent, All Ages", "Median Household Income"))
POV12$Year<-2012
names(POV12)[names(POV12) == "State FIPS Code"] <- "State_FIPS"
names(POV12)[names(POV12) == "County FIPS Code"] <- "County_FIPS"
names(POV12)[names(POV12) == "Poverty Percent, All Ages"] <- "POV_PERCENT_ALL"
names(POV12)[names(POV12) == "Median Household Income"] <- "MED_INC"

POV13 <- subset(POV13,select=c("State FIPS Code", "County FIPS Code", "Name", "Poverty Percent, All Ages", "Median Household Income"))
POV13$Year<-2013
names(POV13)[names(POV13) == "State FIPS Code"] <- "State_FIPS"
names(POV13)[names(POV13) == "County FIPS Code"] <- "County_FIPS"
names(POV13)[names(POV13) == "Poverty Percent, All Ages"] <- "POV_PERCENT_ALL"
names(POV13)[names(POV13) == "Median Household Income"] <- "MED_INC"

POV14 <- subset(POV14,select=c("State FIPS Code", "County FIPS Code", "Name", "Poverty Percent, All Ages", "Median Household Income"))
POV14$Year<-2014
names(POV14)[names(POV14) == "State FIPS Code"] <- "State_FIPS"
names(POV14)[names(POV14) == "County FIPS Code"] <- "County_FIPS"
names(POV14)[names(POV14) == "Poverty Percent, All Ages"] <- "POV_PERCENT_ALL"
names(POV14)[names(POV14) == "Median Household Income"] <- "MED_INC"

POV15 <- subset(POV15,select=c("State FIPS Code", "County FIPS Code", "Name", "Poverty Percent, All Ages", "Median Household Income"))
POV15$Year<-2015
names(POV15)[names(POV15) == "State FIPS Code"] <- "State_FIPS"
names(POV15)[names(POV15) == "County FIPS Code"] <- "County_FIPS"
names(POV15)[names(POV15) == "Poverty Percent, All Ages"] <- "POV_PERCENT_ALL"
names(POV15)[names(POV15) == "Median Household Income"] <- "MED_INC"

POV16 <- subset(POV16,select=c("State FIPS Code", "County FIPS Code", "Name", "Poverty Percent, All Ages", "Median Household Income"))
POV16$Year<-2016
names(POV16)[names(POV16) == "State FIPS Code"] <- "State_FIPS"
names(POV16)[names(POV16) == "County FIPS Code"] <- "County_FIPS"
names(POV16)[names(POV16) == "Poverty Percent, All Ages"] <- "POV_PERCENT_ALL"
names(POV16)[names(POV16) == "Median Household Income"] <- "MED_INC"

POV17 <- subset(POV17,select=c("State FIPS Code", "County FIPS Code", "Name", "Poverty Percent, All Ages", "Median Household Income"))
POV17$Year<-2017
names(POV17)[names(POV17) == "State FIPS Code"] <- "State_FIPS"
names(POV17)[names(POV17) == "County FIPS Code"] <- "County_FIPS"
names(POV17)[names(POV17) == "Poverty Percent, All Ages"] <- "POV_PERCENT_ALL"
names(POV17)[names(POV17) == "Median Household Income"] <- "MED_INC"

POV18 <- subset(POV18,select=c("State FIPS Code", "County FIPS Code", "Name", "Poverty Percent, All Ages", "Median Household Income"))
POV18$Year<-2018
names(POV18)[names(POV18) == "State FIPS Code"] <- "State_FIPS"
names(POV18)[names(POV18) == "County FIPS Code"] <- "County_FIPS"
names(POV18)[names(POV18) == "Poverty Percent, All Ages"] <- "POV_PERCENT_ALL"
names(POV18)[names(POV18) == "Median Household Income"] <- "MED_INC"

POV19 <- subset(POV19,select=c("State FIPS Code", "County FIPS Code", "Name", "Poverty Percent, All Ages", "Median Household Income"))
POV19$Year<-2019
names(POV19)[names(POV19) == "State FIPS Code"] <- "State_FIPS"
names(POV19)[names(POV19) == "County FIPS Code"] <- "County_FIPS"
names(POV19)[names(POV19) == "Poverty Percent, All Ages"] <- "POV_PERCENT_ALL"
names(POV19)[names(POV19) == "Median Household Income"] <- "MED_INC"


POV <-rbind(POV08,POV09,POV10,POV11,POV12,POV13,POV14,POV15,POV16,POV17,POV18,POV19)

table(POV$State_FIPS)
table(POV$County_FIPS)

POV$COUNTY_FIPS <-as.numeric(POV$County_FIPS)
POV$STATE_FIPS <-as.numeric(POV$State_FIPS)
POV$POV_YEAR<-POV$Year

POV<-POV %>% 
  mutate(FULL_FIPS=case_when((STATE_FIPS<10 & COUNTY_FIPS<10) ~ as.numeric(paste0(STATE_FIPS,"00",COUNTY_FIPS)),
                             (STATE_FIPS<10 & COUNTY_FIPS>=10 & COUNTY_FIPS<100) ~ as.numeric(paste0(STATE_FIPS,"0",COUNTY_FIPS)),
                             (STATE_FIPS<10 & COUNTY_FIPS>=100) ~ as.numeric(paste0(STATE_FIPS,COUNTY_FIPS)),
                             (STATE_FIPS>=10 & COUNTY_FIPS<10) ~ as.numeric(paste0(STATE_FIPS,"00",COUNTY_FIPS)),
                             (STATE_FIPS>=10 & COUNTY_FIPS>=10 & COUNTY_FIPS<100) ~ as.numeric(paste0(STATE_FIPS,"0",COUNTY_FIPS)),
                             (STATE_FIPS>=10 & COUNTY_FIPS>=100) ~ as.numeric(paste0(STATE_FIPS,COUNTY_FIPS))
  )
  )


#SAHIE DATA 
SAHIE <- read.table("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/SAHIE/SAHIE_RAW.csv", 
                    header = TRUE, sep=",")

SAHIE$UNINSURED_RATE <- SAHIE$Uninsured...

#once the gsub works join here by county. 
POV_SAHIE<-inner_join(POV, SAHIE, by = c("FULL_FIPS" = "ID", "POV_YEAR" = "Year"))

not_POV<-anti_join(POV,SAHIE, by = c("FULL_FIPS" = "ID", "POV_YEAR" = "Year"))
not_SAHIE<-anti_join(SAHIE,POV, by = c("ID"="FULL_FIPS", "Year"="POV_YEAR"))
#the 3194 here are 2020s 
POV_SAHIE$NAME<-POV_SAHIE$Name.x


POV_SAHIE <- subset(POV_SAHIE,select=c(STATE_FIPS, COUNTY_FIPS, NAME, FULL_FIPS, POV_PERCENT_ALL, MED_INC, UNINSURED_RATE, POV_YEAR))

#BLS 
BLS08 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty08.csv")
BLS09 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty09.csv")
BLS10 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty10.csv")
BLS11 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty11.csv")
BLS12 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty12.csv")
BLS13 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty13.csv")
BLS14 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty14.csv")
BLS15 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty15.csv")
BLS16 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty16.csv")
BLS17 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty17.csv")
BLS18 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty18.csv")
BLS19 <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/laucnty19.csv")
#BLSDC <-read_csv("C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/Original/BLS/LAU_DC.csv")

BLS_0 <-rbind(BLS08,BLS09,BLS10,BLS11,BLS12,BLS13,BLS14,BLS15,BLS16,BLS17,BLS18,BLS19)

BLS_0$COUNTY_FIPS <-as.numeric(BLS_0$COUNTY_FIPS)
BLS_0$STATE_FIPS <-as.numeric(BLS_0$STATE_FIPS)

BLS_0<-BLS_0 %>% 
  mutate(FULL_FIPS2=case_when((STATE_FIPS<10 & COUNTY_FIPS<10) ~ as.numeric(paste0(STATE_FIPS,"00",COUNTY_FIPS)),
                             (STATE_FIPS<10 & COUNTY_FIPS>=10 & COUNTY_FIPS<100) ~ as.numeric(paste0(STATE_FIPS,"0",COUNTY_FIPS)),
                             (STATE_FIPS<10 & COUNTY_FIPS>=100) ~ as.numeric(paste0(STATE_FIPS,COUNTY_FIPS)),
                             (STATE_FIPS>=10 & COUNTY_FIPS<10) ~ as.numeric(paste0(STATE_FIPS,"00",COUNTY_FIPS)),
                             (STATE_FIPS>=10 & COUNTY_FIPS>=10 & COUNTY_FIPS<100) ~ as.numeric(paste0(STATE_FIPS,"0",COUNTY_FIPS)),
                             (STATE_FIPS>=10 & COUNTY_FIPS>=100) ~ as.numeric(paste0(STATE_FIPS,COUNTY_FIPS))
  )
  )

BLS<-subset(BLS_0,select=-c(...5))
#BLS n=38633
COVARS<-inner_join(BLS, POV_SAHIE, by = c("FULL_FIPS2" = "FULL_FIPS","Year"="POV_YEAR"))
#Covars n=37670
COVARS$STATE_FIPS<-COVARS$STATE_FIPS.x

not_PS<-anti_join(BLS, POV_SAHIE, by = c("FULL_FIPS2" = "FULL_FIPS","Year"="POV_YEAR"))
NOT_PS_NOTPR<-subset(not_PS, STATE_FIPS != 72)
#Ogala Lakota is missing figure out why 

not_BLS<-anti_join(POV_SAHIE, BLS, by = c("FULL_FIPS" = "FULL_FIPS2","POV_YEAR"="Year"))
#NEED District of Colombia 

COVARS<-subset(COVARS,select=c(FULL_FIPS2,NAME.x,Year,UNEMP_RATE,POV_PERCENT_ALL,MED_INC,UNINSURED_RATE,STATE_FIPS))

### Medicaid Expansion

## Data come from here https://www.kff.org/affordable-care-act/issue-brief/status-of-state-medicaid-expansion-decisions-interactive-map/
COVARS<-COVARS %>%
  mutate(MEDICAID=case_when(
                           (STATE_FIPS ==	1	)	~	0,	
                           (STATE_FIPS ==	2	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	4	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	5	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	6	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	8	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	9	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	10	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	11	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	12	)	~	0,	
                           (STATE_FIPS ==	13	)	~	0,	
                           (STATE_FIPS ==	15	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	16	)	~	0,	
                           (STATE_FIPS ==	17	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	18	&	Year >=	2015	) ~ 1,
                           (STATE_FIPS ==	19	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	20	)	~	0,	
                           (STATE_FIPS ==	21	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	22	&	Year >=	2016	) ~ 1,
                           (STATE_FIPS ==	23	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	24	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	25	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	26	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	27	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	28	)	~	0,	
                           (STATE_FIPS ==	29	)	~	0,	
                           (STATE_FIPS ==	30	&	Year >=	2016	) ~ 1,
                           (STATE_FIPS ==	31	)	~	0,	
                           (STATE_FIPS ==	32	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	33	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	34	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	35	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	36	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	37	)	~	0,	
                           (STATE_FIPS ==	38	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	39	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	40	)	~	0,	
                           (STATE_FIPS ==	41	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	42	&	Year >=	2015	) ~ 1,
                           (STATE_FIPS ==	44	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	45	)	~	0,	
                           (STATE_FIPS ==	46	)	~	0,	
                           (STATE_FIPS ==	47	)	~	0,	
                           (STATE_FIPS ==	48	)	~	0,	
                           (STATE_FIPS ==	49	&	Year >=	2020	) ~ 1,
                           (STATE_FIPS ==	50	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	51	&	Year >=	2019	) ~ 1,
                           (STATE_FIPS ==	53	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	54	&	Year >=	2014	) ~ 1,
                           (STATE_FIPS ==	55	)	~	0,	
                           (STATE_FIPS ==	56	)	~	0,	
                            TRUE~0))



write.csv(COVARS,"C:/Users/samswift/Dropbox/PAID_SICK_LEAVE/DEATH/ACS/COVARS.CSV")