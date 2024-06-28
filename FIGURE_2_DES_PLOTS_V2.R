#install.packages("grid.extra")
#install.packages("grid")

install.packages("patchwork")
library(patchwork)

library(ggpattern)
library(ggpubr)
library(gridExtra)
library(grid)

ALL_years<-subset(ALL_years,(Year!=2020))
ALL_years_WT <-subset(ALL_years, CENSUSREGION=="WT") 
ALL_years_NE <-subset(ALL_years, CENSUSREGION=="NE") 
#South just for DC and MARYLAND
ALL_years_SO <-subset(ALL_years, CENSUSREGION=="SO") 


####################PLOTS#####################################


####################PLOTS#####################################
#CT 
CT<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$PSL_YEAR==2012)
CT$TREATED <- as.factor(CT$TREATED)
CT2 <- aggregate(CT$CRUDE,by=list(CT$Year,CT$TREATED),FUN=mean)

names(CT2)[names(CT2) == "Group.1"] <- "Year"
names(CT2)[names(CT2) == "Group.2"] <- "TREATED"
names(CT2)[names(CT2) == "x"] <- "CRUDE"

CT_PLOT<- ggplot(data=CT2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2012,color="red")+
#  annotate("text", x=2010.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis lables 
CT_PLOT<- CT_PLOT + labs(x = "", y = "Rate*")
CT_PLOT


#add a title
CT_PLOT <- CT_PLOT + ggtitle("Connecticut (2012)")

#make background white
CT_PLOT <- CT_PLOT + theme_classic() #axis lines but no gridlines
CT_PLOT

#get rid of legend 
CT_PLOT <- CT_PLOT + theme(legend.position="none")
CT_PLOT <- CT_PLOT + theme(axis.text = element_text(size = 10))  
CT_PLOT <- CT_PLOT + theme(axis.title = element_text(size = 10))
CT_PLOT <- CT_PLOT + theme(plot.title = element_text(size = 15))  
CT_PLOT <- CT_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
CT_PLOT <- CT_PLOT + ylim(20,105)
CT_PLOT


#################################################################################################

#DC 
DC<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$PSL_YEAR==2014)
DC$TREATED <- as.factor(DC$TREATED)
DC2 <- aggregate(DC$CRUDE,by=list(DC$Year,DC$TREATED),FUN=mean)

names(DC2)[names(DC2) == "Group.1"] <- "Year"
names(DC2)[names(DC2) == "Group.2"] <- "TREATED"
names(DC2)[names(DC2) == "x"] <- "CRUDE"

DC_PLOT<- ggplot(data=DC2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2014,color="red")+
#  annotate("text", x=2013.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis lables 
DC_PLOT<- DC_PLOT + labs(x = "", y = "Rate*")
DC_PLOT


#add a title
DC_PLOT <- DC_PLOT + ggtitle("District of Columbia (2014)")

#make background white
DC_PLOT <- DC_PLOT + theme_classic() #axis lines but no gridlines
DC_PLOT

#get rid of legend 
DC_PLOT <- DC_PLOT + theme(legend.position="none")
DC_PLOT <- DC_PLOT + theme(axis.text = element_text(size = 10))  
DC_PLOT <- DC_PLOT + theme(axis.title = element_text(size = 10))
DC_PLOT <- DC_PLOT + theme(plot.title = element_text(size = 15))  
DC_PLOT <- DC_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
DC_PLOT <- DC_PLOT + ylim(20,105)
DC_PLOT



####### 2015 CA 
#################################################################################################


CA<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==6)
CA$TREATED <- as.factor(CA$TREATED)
CA2 <- aggregate(CA$CRUDE,by=list(CA$Year,CA$TREATED),FUN=mean)

names(CA2)[names(CA2) == "Group.1"] <- "Year"
names(CA2)[names(CA2) == "Group.2"] <- "TREATED"
names(CA2)[names(CA2) == "x"] <- "CRUDE"

CA_PLOT<- ggplot(data=CA2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2015,color="red")+
#  annotate("text", x=2014.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis labels 
CA_PLOT<- CA_PLOT + labs(x = "", y = "Rate*")
CA_PLOT


#add a title
CA_PLOT <- CA_PLOT + ggtitle("California (2015)")

#make background white
CA_PLOT <- CA_PLOT + theme_classic() #axis lines but no gridlines
CA_PLOT

#get rid of legend 
CA_PLOT <- CA_PLOT + theme(legend.position="none")
CA_PLOT <- CA_PLOT + theme(axis.text = element_text(size = 10))  
CA_PLOT <- CA_PLOT + theme(axis.title = element_text(size = 10))
CA_PLOT <- CA_PLOT + theme(plot.title = element_text(size = 15))  
CA_PLOT <- CA_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
CA_PLOT <- CA_PLOT + ylim(20,105)
CA_PLOT

#################################################################################################
#### MA 


MA<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==25)
MA$TREATED <- as.factor(MA$TREATED)
MA2 <- aggregate(MA$CRUDE,by=list(MA$Year,MA$TREATED),FUN=mean)

names(MA2)[names(MA2) == "Group.1"] <- "Year"
names(MA2)[names(MA2) == "Group.2"] <- "TREATED"
names(MA2)[names(MA2) == "x"] <- "CRUDE"

MA_PLOT<- ggplot(data=MA2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2015,color="red")+
#  annotate("text", x=2014.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis lables 
MA_PLOT<- MA_PLOT + labs(x = "", y = "Rate*")
MA_PLOT


#add a title
MA_PLOT <- MA_PLOT + ggtitle("Massachutsetts (2015)")

#make background white
MA_PLOT <- MA_PLOT + theme_classic() #axis lines but no gridlines
MA_PLOT

#get rid of legend 
MA_PLOT <- MA_PLOT + theme(legend.position="none")
MA_PLOT <- MA_PLOT + theme(axis.text = element_text(size = 10))  
MA_PLOT <- MA_PLOT + theme(axis.title = element_text(size = 10))
MA_PLOT <- MA_PLOT + theme(plot.title = element_text(size = 15))  
MA_PLOT <- MA_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
MA_PLOT <- MA_PLOT + ylim(20,105)
MA_PLOT

#################################################################################################
#######2016 OR

#### OR

#OR 
OR<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==41)
OR$TREATED <- as.factor(OR$TREATED)
OR2 <- aggregate(OR$CRUDE,by=list(OR$Year,OR$TREATED),FUN=mean)

names(OR2)[names(OR2) == "Group.1"] <- "Year"
names(OR2)[names(OR2) == "Group.2"] <- "TREATED"
names(OR2)[names(OR2) == "x"] <- "CRUDE"

OR_PLOT<- ggplot(data=OR2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2016,color="red")+
#  annotate("text", x=2015.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis lables 
OR_PLOT<- OR_PLOT + labs(x = "", y = "Rate*")
OR_PLOT


#add a title
OR_PLOT <- OR_PLOT + ggtitle("Oregon (2016)")

#make background white
OR_PLOT <- OR_PLOT + theme_classic() #axis lines but no gridlines
OR_PLOT

#get rid of legend 
OR_PLOT <- OR_PLOT + theme(legend.position="none")
OR_PLOT <- OR_PLOT + theme(axis.text = element_text(size = 10))  
OR_PLOT <- OR_PLOT + theme(axis.title = element_text(size = 10))
OR_PLOT <- OR_PLOT + theme(plot.title = element_text(size = 15))  
OR_PLOT <- OR_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
OR_PLOT <- OR_PLOT + ylim(20,105)
OR_PLOT


#################################################################################################
#######2017 AZ

#### AZ 
#AZ 
AZ<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==04)
AZ$TREATED <- as.factor(AZ$TREATED)
AZ2 <- aggregate(AZ$CRUDE,by=list(AZ$Year,AZ$TREATED),FUN=mean)

names(AZ2)[names(AZ2) == "Group.1"] <- "Year"
names(AZ2)[names(AZ2) == "Group.2"] <- "TREATED"
names(AZ2)[names(AZ2) == "x"] <- "CRUDE"

AZ_PLOT<- ggplot(data=AZ2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2017,color="red")+
#  annotate("text", x=2016.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis lables 
AZ_PLOT<- AZ_PLOT + labs(x = "", y = "Rate*")
AZ_PLOT


#add a title
AZ_PLOT <- AZ_PLOT + ggtitle("Arizona (2017)")

#make background white
AZ_PLOT <- AZ_PLOT + theme_classic() #axis lines but no gridlines
AZ_PLOT

#get rid of legend 
AZ_PLOT <- AZ_PLOT + theme(legend.position="none")
AZ_PLOT <- AZ_PLOT + theme(axis.text = element_text(size = 10))  
AZ_PLOT <- AZ_PLOT + theme(axis.title = element_text(size = 10))
AZ_PLOT <- AZ_PLOT + theme(plot.title = element_text(size = 15))  
AZ_PLOT <- AZ_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
AZ_PLOT <- AZ_PLOT + ylim(20,105)
AZ_PLOT






#######2017 VT  ######################################

#VT 
VT<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==50)
VT$TREATED <- as.factor(VT$TREATED)
VT2 <- aggregate(VT$CRUDE,by=list(VT$Year,VT$TREATED),FUN=mean)

names(VT2)[names(VT2) == "Group.1"] <- "Year"
names(VT2)[names(VT2) == "Group.2"] <- "TREATED"
names(VT2)[names(VT2) == "x"] <- "CRUDE"

VT_PLOT<- ggplot(data=VT2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2017,color="red")+
#  annotate("text", x=2016.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis lables 
VT_PLOT<- VT_PLOT + labs(x = "", y = "Rate*")
VT_PLOT


#add a title
VT_PLOT <- VT_PLOT + ggtitle("Vermont (2017)")

#make background white
VT_PLOT <- VT_PLOT + theme_classic() #axis lines but no gridlines
VT_PLOT

#get rid of legend 
VT_PLOT <- VT_PLOT + theme(legend.position="none")
VT_PLOT <- VT_PLOT + theme(axis.text = element_text(size = 10))  
VT_PLOT <- VT_PLOT + theme(axis.title = element_text(size = 10))
VT_PLOT <- VT_PLOT + theme(plot.title = element_text(size = 15))  
VT_PLOT <- VT_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
VT_PLOT <- VT_PLOT + ylim(20,105)
VT_PLOT




#######2018 MD  ######################################

#MD 
MD<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==24)
MD$TREATED <- as.factor(MD$TREATED)
MD2 <- aggregate(MD$CRUDE,by=list(MD$Year,MD$TREATED),FUN=mean)

names(MD2)[names(MD2) == "Group.1"] <- "Year"
names(MD2)[names(MD2) == "Group.2"] <- "TREATED"
names(MD2)[names(MD2) == "x"] <- "CRUDE"

MD_PLOT<- ggplot(data=MD2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2018,color="red")+
#  annotate("text", x=2017.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis labels 
MD_PLOT<- MD_PLOT + labs(x = "", y = "Rate*")
MD_PLOT


#add a title
MD_PLOT <- MD_PLOT + ggtitle("Maryland (2018)")

#make background white
MD_PLOT <- MD_PLOT + theme_classic() #axis lines but no gridlines
MD_PLOT

#get rid of legend 
MD_PLOT <- MD_PLOT + theme(legend.position="none")
MD_PLOT <- MD_PLOT + theme(axis.text = element_text(size = 10))  
MD_PLOT <- MD_PLOT + theme(axis.title = element_text(size = 10))
MD_PLOT <- MD_PLOT + theme(plot.title = element_text(size = 15))  
MD_PLOT <- MD_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
MD_PLOT <- MD_PLOT + ylim(60,105)
MD_PLOT




#######2018 RI ######################################

#RI 
RI<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==44)
RI$TREATED <- as.factor(RI$TREATED)
RI2 <- aggregate(RI$CRUDE,by=list(RI$Year,RI$TREATED),FUN=mean)

names(RI2)[names(RI2) == "Group.1"] <- "Year"
names(RI2)[names(RI2) == "Group.2"] <- "TREATED"
names(RI2)[names(RI2) == "x"] <- "CRUDE"

RI_PLOT<- ggplot(data=RI2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2018,color="red")+
#  annotate("text", x=2017.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis labels 
RI_PLOT<- RI_PLOT + labs(x = "", y = "Rate*")
RI_PLOT


#add a title
RI_PLOT <- RI_PLOT + ggtitle("Rhode Island (2018)")

#make background white
RI_PLOT <- RI_PLOT + theme_classic() #axis lines but no gridlines
RI_PLOT

#get rid of legend 
RI_PLOT <- RI_PLOT + theme(legend.position="none")
RI_PLOT <- RI_PLOT + theme(axis.text = element_text(size = 10))  
RI_PLOT <- RI_PLOT + theme(axis.title = element_text(size = 10))
RI_PLOT <- RI_PLOT + theme(plot.title = element_text(size = 15))  
RI_PLOT <- RI_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
RI_PLOT <- RI_PLOT + ylim(20,105)
RI_PLOT




#######2018 NJ  ######################################

#NJ Compared to Northeast
NJ<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==34)
NJ$TREATED <- as.factor(NJ$TREATED)
NJ2 <- aggregate(NJ$CRUDE,by=list(NJ$Year,NJ$TREATED),FUN=mean)

names(NJ2)[names(NJ2) == "Group.1"] <- "Year"
names(NJ2)[names(NJ2) == "Group.2"] <- "TREATED"
names(NJ2)[names(NJ2) == "x"] <- "CRUDE"

NJ_PLOT<- ggplot(data=NJ2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2018,color="red")+
  #annotate("text", x=2017.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis labels 
NJ_PLOT<- NJ_PLOT + labs(x = "", y = "Rate*")
NJ_PLOT


#add a title
NJ_PLOT <- NJ_PLOT + ggtitle("New Jersey (2018)")

#make background white
NJ_PLOT <- NJ_PLOT + theme_classic() #axis lines but no gNJdlines
NJ_PLOT

#get NJd of legend 
NJ_PLOT <- NJ_PLOT + theme(legend.position="none")
NJ_PLOT <- NJ_PLOT + theme(axis.text = element_text(size = 10))  
NJ_PLOT <- NJ_PLOT + theme(axis.title = element_text(size = 10))
NJ_PLOT <- NJ_PLOT + theme(plot.title = element_text(size = 15))  
NJ_PLOT <- NJ_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
NJ_PLOT <- NJ_PLOT + ylim(20,105)
NJ_PLOT





#######2018 WA  ######################################

#WA 
WA<-subset(ALL_years,(is.na(ALL_years$PSL_YEAR))|ALL_years$State.Code==53)
WA$TREATED <- as.factor(WA$TREATED)
WA2 <- aggregate(WA$CRUDE,by=list(WA$Year,WA$TREATED),FUN=mean)

names(WA2)[names(WA2) == "Group.1"] <- "Year"
names(WA2)[names(WA2) == "Group.2"] <- "TREATED"
names(WA2)[names(WA2) == "x"] <- "CRUDE"

WA_PLOT<- ggplot(data=WA2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('black','blue'))+
  geom_vline(xintercept=2018,color="red")+
 # annotate("text", x=2017.75, y=75, label="Policy Year",size=7,angle=90)+
  geom_point()


#add some axis lables 
WA_PLOT<- WA_PLOT + labs(x = "", y = "Rate*")
WA_PLOT


#add a title
WA_PLOT <- WA_PLOT + ggtitle("Washington (2018)")

#make background white
WA_PLOT <- WA_PLOT + theme_classic() #axis lines but no gridlines
WA_PLOT

#get rid of legend 
WA_PLOT <- WA_PLOT + theme(legend.position="none")
WA_PLOT <- WA_PLOT + theme(axis.text = element_text(size = 10))  
WA_PLOT <- WA_PLOT + theme(axis.title = element_text(size = 5))
WA_PLOT <- WA_PLOT + theme(plot.title = element_text(size = 15))  
WA_PLOT <- WA_PLOT + scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))
WA_PLOT <- WA_PLOT + ylim(20,105)
WA_PLOT


#Blank plot
BLANK<-ggplot(data=WA2, aes(x=Year, y=CRUDE, group=TREATED)) +
  geom_line(aes(color=TREATED))+ 
  scale_color_manual(values=c('white','white'))+theme_void()+ theme(legend.position="none")




BLANK

figure<- CT_PLOT/MA_PLOT/VT_PLOT/RI_PLOT/NJ_PLOT|CA_PLOT/OR_PLOT/AZ_PLOT/WA_PLOT/BLANK|DC_PLOT/MD_PLOT/BLANK/BLANK/BLANK
figure




