
#install.packages("grid.extra")
#install.packages("grid")

#install.packages("patchwork")
#install.packages("ggsci")
#install.packages("ggpubr")
library(ggpubr)
library(ggsci)
library(patchwork)


# order of the coefficients for the plot
plot_order <- c("Year::-10","Year::-9", "Year::-8", "Year::-7", 
                "Year::-6", "Year::-5", "Year::-4", "Year::-3", 
                "Year::-2", "Year::-1", "Year::0","Year::1", 
                "Year::2", "Year::3", "Year::4", "Year::5",
                "Year::6","Year::7","Year::8")

plot_order_ORIG <- c("lead10","lead9", "lead8", "lead7", 
                "lead6", "lead5", "lead4", "lead3", 
                "lead2", "lead1", "lag0", "lag1", 
                "lag2", "lag3", "lag4", "lag5","lag6","lag7","lag8")

# grab the clustered standard errors
# and average coefficient estimates
# from the regression, label them accordingly
# add a zero'th lag for plotting purposes


####### Regional Controls -- MAIN ANALYSIS 




###Northeast####
#check to make sure you only actually need 17 items from the coeftable for different models
sd_NE_SAB <- eventNORTHEAST_64_SAB$coeftable[1:17,2]


NE_SAB_plot <- tibble(
  sd = sd_NE_SAB[plot_order]
  ,mean = coef(eventNORTHEAST_64_SAB)[plot_order]
  ,label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)

NE_plot_TWFE <- tibble(
  sd = eventNORTHEAST$cse[plot_order_ORIG]
  ,mean = coef(eventNORTHEAST)[plot_order_ORIG]
  ,label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)



NE_SAB_plot$Method<-"Sun and Abraham (2020)"
NE_plot_TWFE$Method<-"Two Way Fixed Effects"


Regions<-rbind(NE_plot_TWFE,NE_SAB_plot)

dodge <- position_dodge(width=0.5)  


regions_p <- ggplot(Regions,aes(fill= Method,color = Method, shape = Method, x = label, y = mean,  
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd, label=mean)) 
regions_p <-regions_p + geom_pointrange(position=dodge) 
regions_p <-regions_p + scale_color_manual(values = c("#000000","#CCCCCC"))
regions_p <-regions_p + scale_shape_manual(values = c(16, 17)) 
regions_p <-regions_p + theme_classic() 
regions_p <-regions_p + labs(title = "Figure 3a. Northeastern Region") 
regions_p <-regions_p + xlab("Years Before and After State Level Paid Sick Leave") 
regions_p <-regions_p + ylab("Difference in CVD Mortality Rate") 
regions_p <-regions_p + geom_hline(yintercept = -0,
             linetype = "dashed") 
regions_p <-regions_p + geom_vline(xintercept = 0,
             linetype = "dashed") 
regions_p <-regions_p + theme(axis.text = element_text(size = 13))  
regions_p <-regions_p + theme(axis.title = element_text(size = 13)) 
regions_p <-regions_p + theme(legend.position = "bottom")
regions_p <-regions_p + theme(legend.key.size = unit(1.5, units = "cm")) 
regions_p <-regions_p + theme(legend.title = element_text(size = 15))
regions_p <-regions_p + theme(legend.text = element_text(size = 15))

regions_p


###Westt####
#check to make sure you only actually need 17 items from the coeftable for different models
sd_WT_SAB <- eventWEST_64_SAB$coeftable[1:17,2]


WT_SAB_plot <- tibble(
  sd = c(sd_WT_SAB[plot_order]),
  mean = c(coef(eventWEST_64_SAB)[plot_order]),
  label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0,  1,2,3,4,5,6,7,8)
)

WT_plot_TWFE <- tibble(
  sd = c(eventWEST$cse[plot_order_ORIG]),
  mean = c(coef(eventWEST)[plot_order_ORIG]),
  label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1, 0, 1,2,3,4,5,6,7,8)
)



WT_SAB_plot$Method<-"Sun and Abraham (2020)"
WT_plot_TWFE$Method<-"Two Way Fixed Effects"


Regions2<-rbind(WT_plot_TWFE,WT_SAB_plot)

dodge <- position_dodge(width=0.5)  


regions_p2 <- ggplot(Regions2,aes(fill= Method,color = Method, shape = Method, x = label, y = mean,  
                                ymin = mean-1.96*sd, 
                                ymax = mean+1.96*sd, label=mean)) 
regions_p2 <-regions_p2 + geom_pointrange(position=dodge) 
regions_p2 <-regions_p2 + scale_color_manual(values = c("#000000","#CCCCCC"))
regions_p2 <-regions_p2 + scale_shape_manual(values = c(16, 17)) 
regions_p2 <-regions_p2 + theme_classic() 
regions_p2 <-regions_p2 + labs(title = "Figure 3b. Western Region") 
regions_p2 <-regions_p2 + xlab("Years Before and After State Level Paid Sick Leave") 
regions_p2 <-regions_p2 + ylab("Difference in CVD Mortality Rate")
regions_p2 <-regions_p2 + geom_hline(yintercept = -0,
                                   linetype = "dashed") 
regions_p2 <-regions_p2 + geom_vline(xintercept = 0,
                                   linetype = "dashed") 
regions_p2 <-regions_p2 + theme(axis.text = element_text(size = 13))  
regions_p2 <-regions_p2 + theme(axis.title = element_text(size = 13)) 
regions_p2 <-regions_p2 + theme(legend.position = "bottom")
regions_p2 <-regions_p2 + theme(legend.key.size = unit(1.5, units = "cm")) 
regions_p2 <-regions_p2 + theme(legend.title = element_text(size = 15))
regions_p2 <-regions_p2 + theme(legend.text = element_text(size = 15))

regions_p2

FIGURE3<- regions_p/regions_p2
FIGURE3




#####All US No REgional Controls 

###Northeast####
#check to make sure you only actually need 17 items from the coeftable for different models
sd_SAB_US <- event_SAB_US$coeftable[1:17,2]


US_SAB_plot <- tibble(
  sd = sd_SAB_US[plot_order]
  ,mean = coef(eventNORTHEAST_64_SAB)[plot_order]
  ,label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)

NE_plot_TWFE <- tibble(
  sd = eventNORTHEAST$cse[plot_order_ORIG]
  ,mean = coef(eventNORTHEAST)[plot_order_ORIG]
  ,label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)



NE_SAB_plot$Method<-"Sun and Abraham (2020)"
NE_plot_TWFE$Method<-"Two Way Fixed Effects"


Regions<-rbind(NE_plot_TWFE,NE_SAB_plot)

dodge <- position_dodge(width=0.5)  


regions_p <- ggplot(Regions,aes(fill= Method,color = Method, shape = Method, x = label, y = mean,  
                                ymin = mean-1.96*sd, 
                                ymax = mean+1.96*sd, label=mean)) 
regions_p <-regions_p + geom_pointrange(position=dodge) 
regions_p <-regions_p + scale_color_manual(values = c("#000000","#CCCCCC"))
regions_p <-regions_p + scale_shape_manual(values = c(16, 17)) 
regions_p <-regions_p + theme_classic() 
regions_p <-regions_p + labs(title = "Figure 3a. Northeastern Region") 
regions_p <-regions_p + xlab("Years Before and After State Level Paid Sick Leave") 
regions_p <-regions_p + ylab("Difference in CVD Mortality Rate") 
regions_p <-regions_p + geom_hline(yintercept = -0,
                                   linetype = "dashed") 
regions_p <-regions_p + geom_vline(xintercept = 0,
                                   linetype = "dashed") 
regions_p <-regions_p + theme(axis.text = element_text(size = 13))  
regions_p <-regions_p + theme(axis.title = element_text(size = 13)) 
regions_p <-regions_p + theme(legend.position = "bottom")
regions_p <-regions_p + theme(legend.key.size = unit(1.5, units = "cm")) 
regions_p <-regions_p + theme(legend.title = element_text(size = 15))
regions_p <-regions_p + theme(legend.text = element_text(size = 15))

regions_p






####### Male and Female by region
sd_FEMALE_NE_SAB <- event_FEMALE_NE_SAB$coeftable[1:17,2]

FEMALE_NE_plot <- tibble(
  sd = sd_FEMALE_NE_SAB[plot_order],
  mean = c(coef(event_FEMALE_NE_SAB)[plot_order]),
  label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)


sd_FEMALE_WT_SAB <- event_FEMALE_WT_SAB$coeftable[1:17,2]

FEMALE_WT_plot <- tibble(
  sd = sd_FEMALE_WT_SAB[plot_order],
  mean = c(coef(event_FEMALE_WT_SAB)[plot_order]),
  label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)


## Male
sd_MALE_NE_SAB <- event_MALE_NE_SAB$coeftable[1:17,2]

MALE_NE_plot <- tibble(
  sd = sd_MALE_NE_SAB[plot_order],
  mean = c(coef(event_MALE_NE_SAB)[plot_order]),
  label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)


sd_MALE_WT_SAB <- event_MALE_WT_SAB$coeftable[1:17,2]

MALE_WT_plot <- tibble(
  sd = sd_MALE_WT_SAB[plot_order],
  mean = c(coef(event_MALE_WT_SAB)[plot_order]),
  label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)


FEMALE_NE_plot$SEX<-"Female"
MALE_NE_plot$SEX<-"Male"
FEMALE_WT_plot$SEX<-"Female"
MALE_WT_plot$SEX<-"Male"


Regions3<-rbind(FEMALE_NE_plot,MALE_NE_plot)

dodge <- position_dodge(width=0.5)  


GE_NE_PLOT <- ggplot(Regions3,aes(fill= SEX,color = SEX, shape = SEX, x = label, y = mean,  
                                ymin = mean-1.96*sd, 
                                ymax = mean+1.96*sd, label=mean)) 
GE_NE_PLOT <-GE_NE_PLOT + geom_pointrange(position=dodge) 
GE_NE_PLOT <-GE_NE_PLOT + scale_color_manual(values = c("#000000","#CCCCCC"))
GE_NE_PLOT <-GE_NE_PLOT + scale_shape_manual(values = c(16, 17)) 
GE_NE_PLOT <-GE_NE_PLOT + theme_classic() 
GE_NE_PLOT <-GE_NE_PLOT + labs(title = "Supplementary Figure 1a. Northeastern Region by Sex") 
GE_NE_PLOT <-GE_NE_PLOT + xlab("Years Before and After State Level Paid Sick Leave") 
GE_NE_PLOT <-GE_NE_PLOT + ylab("Difference in CVD Mortality Rate") 
GE_NE_PLOT <-GE_NE_PLOT + geom_hline(yintercept = -0,
                                   linetype = "dashed") 
GE_NE_PLOT <-GE_NE_PLOT + geom_vline(xintercept = 0,
                                   linetype = "dashed") 
GE_NE_PLOT <-GE_NE_PLOT + theme(axis.text = element_text(size = 13))  
GE_NE_PLOT <-GE_NE_PLOT + theme(axis.title = element_text(size = 15)) 
GE_NE_PLOT <-GE_NE_PLOT + theme(legend.position = "bottom")
GE_NE_PLOT <-GE_NE_PLOT + theme(legend.key.size = unit(1.5, units = "cm")) 
GE_NE_PLOT <-GE_NE_PLOT + theme(legend.title = element_text(size = 15))
GE_NE_PLOT <-GE_NE_PLOT + theme(legend.text = element_text(size = 15))

GE_NE_PLOT

##West by sex 

Regions4<-rbind(FEMALE_WT_plot,MALE_WT_plot)

dodge <- position_dodge(width=0.5)  
GE_WT_PLOT <- ggplot(Regions4,aes(fill= SEX,color = SEX, shape = SEX, x = label, y = mean,  
                                 ymin = mean-1.96*sd, 
                                 ymax = mean+1.96*sd, label=mean)) 
GE_WT_PLOT <-GE_WT_PLOT + geom_pointrange(position=dodge) 
GE_WT_PLOT <-GE_WT_PLOT + scale_color_manual(values = c("#000000","#CCCCCC"))
GE_WT_PLOT <-GE_WT_PLOT + scale_shape_manual(values = c(16, 17)) 
GE_WT_PLOT <-GE_WT_PLOT + theme_classic() 
GE_WT_PLOT <-GE_WT_PLOT + labs(title = "Supplementary Figure 1b. Western Region by Sex") 
GE_WT_PLOT <-GE_WT_PLOT + xlab("Years Before and After State Level Paid Sick Leave") 
GE_WT_PLOT <-GE_WT_PLOT + ylab("Difference in CVD Mortality Rate") 
GE_WT_PLOT <-GE_WT_PLOT + geom_hline(yintercept = -1,
                                   linetype = "dashed") 
GE_WT_PLOT <-GE_WT_PLOT + geom_vline(xintercept = 0,
                                   linetype = "dashed") 
GE_WT_PLOT <-GE_WT_PLOT + theme(axis.text = element_text(size = 13))  
GE_WT_PLOT <-GE_WT_PLOT + theme(axis.title = element_text(size = 15)) 
GE_WT_PLOT <-GE_WT_PLOT + theme(legend.position = "bottom")
GE_WT_PLOT <-GE_WT_PLOT + theme(legend.key.size = unit(1.5, units = "cm")) 
GE_WT_PLOT <-GE_WT_PLOT + theme(legend.title = element_text(size = 15))
GE_WT_PLOT <-GE_WT_PLOT + theme(legend.text = element_text(size = 15))

GE_WT_PLOT


GENDER_FIGURE<- GE_NE_PLOT/GE_WT_PLOT
GENDER_FIGURE



####### Regional Controls -- MAIN ANALYSIS With Age Adjustment 15 64  
sd_AA_NE_SAB <- event_AA_NE_SAB$coeftable[1:17,2]

AA_NE_plot <- tibble(
  sd = sd_AA_NE_SAB[plot_order],
  mean = c(coef(event_AA_NE_SAB)[plot_order]),
  label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)


sd_AA_WT_SAB <- event_AA_WT_SAB$coeftable[1:17,2]

AA_WT_plot <- tibble(
  sd = sd_AA_WT_SAB[plot_order],
  mean = c(coef(event_AA_WT_SAB)[plot_order]),
  label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)


AA_NE_plot$Region<-"Northeast"
AA_WT_plot$Region<-"West"


Regions<-rbind(AA_WT_plot,AA_NE_plot)
dodge <- position_dodge(width=0.5)  
Regions %>%
  ggplot(aes(fill=Region,color = Region, x = label, y = mean, 
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd, label=mean)) +
  geom_pointrange(position=dodge) +
  theme_classic() +
  xlab("Years Before and After State Level Paid Sick Leave") +
  ylab("Difference in CVD Mortality Rate Between 
       Treated and Untreated States") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed") + 
  theme(axis.text = element_text(size = 13))  + 
  theme(axis.title = element_text(size = 15))+
  scale_color_jama()   




