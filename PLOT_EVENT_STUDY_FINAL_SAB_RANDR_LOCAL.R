
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
sd_NE_SAB_NOLOCAL <- eventNORTHEAST_64_SAB_NOLOCAL$coeftable[1:17,2]
NE_SAB_plot_NOLOCAL <- tibble(
  sd = sd_NE_SAB_NOLOCAL[plot_order]
  ,mean = coef(eventNORTHEAST_64_SAB_NOLOCAL)[plot_order]
  ,label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)

sd_WT_SAB_NOLOCAL <- eventWEST_64_SAB_NOLOCAL$coeftable[1:17,2]
WT_SAB_plot_NOLOCAL <- tibble(
  sd = sd_WT_SAB_NOLOCAL[plot_order]
  ,mean = coef(eventWEST_64_SAB_NOLOCAL)[plot_order]
  ,label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)

sd_US_SAB_NOLOCAL <- eventUS_64_SAB_NOLOCAL$coeftable[1:17,2]
US_SAB_plot_NOLOCAL <- tibble(
  sd = sd_US_SAB_NOLOCAL[plot_order]
  ,mean = coef(eventUS_64_SAB_NOLOCAL)[plot_order]
  ,label = c(-10,-9,-8,-7,-6, -5, -4, -3, -2, -1,0, 1,2,3,4,5,6,7,8)
)

NE_SAB_plot_NOLOCAL$Method<-"Northeastern Region"
WT_SAB_plot_NOLOCAL$Method<-"Western Region"
US_SAB_plot_NOLOCAL$Method<-"All United States"

Regions<-rbind(NE_SAB_plot_NOLOCAL,WT_SAB_plot_NOLOCAL,US_SAB_plot_NOLOCAL)

dodge <- position_dodge(width=0.5)  


regions_p <- ggplot(Regions,aes(fill= Method,color = Method, shape = Method, x = label, y = mean,  
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd, label=mean)) 
regions_p <-regions_p + geom_pointrange(position=dodge) 
regions_p <-regions_p + scale_color_manual(values = c("#000000","#0000FF","#CCCCCC"))
regions_p <-regions_p + scale_shape_manual(values = c(16, 17,18)) 
regions_p <-regions_p + theme_classic() 
regions_p <-regions_p + labs(title = "Sun and Abraham (2020) event study coefficients accounting for local policies") 
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


