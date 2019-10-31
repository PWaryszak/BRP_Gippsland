#Plot NEW BRP DATA (See 01_BRP_CN_DATA.R)========
library("tidyverse")#install.packages() first if not in your local library
library(lme4)
library(lmerTest)
library(sjPlot)
library(nlme)
library(sjmisc)

#LOAD and DATA:
setwd("~/00DeakinUni/R/BCL_R/BCL/BRP/GL_REHAB")
NewDATA <- read.csv("~/00DeakinUni/R/BCL_R/BCL/BRP/GL_REHAB/BRP_CN_GL_REHAB_NewDATA.csv")#Sampel CV-163 was deemd "NA as C.Percent == 0
NewDATA <- NewDATA [ !is.na(NewDATA$site),] #remove NA-s if any
NewDATA <- NewDATA [ !is.na(NewDATA$C.percent),] #remove NA-s if any
NewDATA$C.percent <- ifelse(NewDATA$C.percent == 0, 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$DepthTo_cm - NewDATA$DepthFrom_cm) #round % to full numbers to run Poisson
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter_cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3 <- NewDATA$DryWeight_g / NewDATA$SampleVolume.cm3

NewDATA$Core_in.mm <- NewDATA$PipeLength_mm  - NewDATA$Compaction_in # Compaction in mm!
NewDATA$Pipe_in.mm <- NewDATA$PipeLength_mm  - NewDATA$Compaction_out  #Compaction in mm!
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.mm/NewDATA$Pipe_in.mm

NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$dry_bulk_density.gcm3 * NewDATA$Compaction_Correction_Valu

NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C.percent/100
NewDATA$CarbonStock.Mgha <- (((NewDATA$CarbonDensity.gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength.cm )

range(NewDATA$Compaction_Correction_Value)# Check if all values are  below 1 = If value = 1 this core had no records of compaction
range(NewDATA$CarbonStock.Mgha, na.rm = T )# 4.892392 155.705862
range(NewDATA$CarbonDensity.gcm3, na.rm = T )#0.009514284 0.290442278


#CORRECTED C-stock to account for compaction:#################

#Analyze  Coastal Veg DATA from Avalon ======
#Gamma Model:
CN_gamma <- glmer(CarbonStock.Mgha_CORRECTED ~ habitat  +DepthRange.cm +
                    (1|core) + (1|site) ,
                  family = Gamma(link = "inverse"), data=NewDATA)
summary(CN_gamma)
plot(resid(CN_gamma))

#Linear Model:
CN_lm <- lm(CarbonStock.Mgha ~ location  + DepthRange_cm , data = NewDATA)
hist(NewDATA$CarbonStock.Mgha)
plot(resid(CN_lm))

#Log-Linear Model:
CN_log_lm <- lm(log(CarbonStock.Mgha) ~ location  + DepthRange_cm , data = NewDATA)
summary(CN_log_lm)
plot(resid(CN_log_lm))

#Gaussian glmm distribution:=
CN_log_lmer1 <- lmer(log(CarbonStock.Mgha_CORRECTED) ~ location  + DepthRange_cm   +
                   (1|site)  , data=NewDATA)

summary(CN_log_lmer1)
plot(resid(CN_log_lmer1))

#TABLE:
tab_model(CN_log_lmer1)
tab_model(CN_lm)




#Gamma Model:
CN_gamma <- glmer(CarbonStock.Mgha ~ location  + DepthRange_cm +
                     (1|plot) ,
                   family = Gamma(link = "inverse"), data=NewDATA2)

#Gaussian glmm non-transfomred:
CN_lmer2 <- lmer(CarbonStock.Mgha ~ habitat  +  TwoDepths +
                   (1|SiteNumber) +(1|core) , data=NewDATA2)

summary(CN_lmer2)
plot(resid(CN_lmer2))

#Compare all models:
AIC(CN_gamma2,CN_lm2, CN_log_lm2, CN_lmer_log2,CN_lmer2 )


#Draw a Stats Table:
tab_model(CN_lmer_log2)
tab_model(CN_gamma)
tab_model(CN_lm2)


#Plot by by habitat
aa <- ggplot(NewDATA, aes(x = habitat, y = CarbonStock.Mgha)) +
  geom_boxplot() +
  facet_grid(.~ DepthRange.cm)+ geom_jitter( alpha = 0.4)+
  ylab("Organic Carbon Stock (Mg/ha)") + xlab("") +
  theme_bw() +
  coord_flip()+
  ggtitle("Coastal Vegetation (BRP Avalon)")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

NewDATA3 <- NewDATA %>%
  group_by(habitat,core) %>%
  summarise(TotalCarbonStock = sum(CarbonStock.Mgha, na.rm = T))
  
a <- ggplot(NewDATA3, aes(x = "Within Each Core", y = TotalCarbonStock)) +
  geom_boxplot() +
  facet_grid(.~ habitat)+ geom_jitter( alpha = 0.4)+
  ylab("Total Carbon Stock (Mg/ha)") + xlab("") +
  theme_bw() +
  ggtitle("Mean total C-stock per core \nCoastal Vegetation (BRP Avalon)")+
  theme(axis.text.x = element_text(size = 10,face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 16, angle = 90, hjust = 0.5),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 18, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
a
library(grid)
library(gridExtra)
grid.arrange(aa, a)

