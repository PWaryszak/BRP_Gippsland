#LOAD NEW BRP DATA (See 01_BRP_CN_DATA.R)========
library(tidyverse)#install.packages() first if not in your local library
library(lme4)
library(lmerTest)
library(sjPlot)
library(nlme)
library(sjmisc)

#LOAD & CLEAN DATA:========
g1 <- read.csv("BRP_CN_GL_NATURAL_NewDATA.csv")
g2 <- read.csv("BRP_CN_GL_REHAB_NewDATA.csv")
g3 <- read.csv("BRP_CN_GL_CONTROL_NewDATA.csv")

g1.cols <- select(g1, SampleID,CNCode, DepthFrom_cm, DepthTo_cm,CompactionIn_mm,DepthRange_cm,
                  CompactionOut_mm, C.percent,N.percent, PipeLength_mm,PipeDiameter_cm,DryWeight_g ) %>%
  mutate( Site_Type = "natural")

g2.cols <- select(g2, SampleID,CNCode, DepthFrom_cm, DepthTo_cm,CompactionIn_mm,DepthRange_cm,
                  CompactionOut_mm, C.percent,N.percent, PipeLength_mm,PipeDiameter_cm,DryWeight_g ) %>%
  mutate( Site_Type = "rehab")

g3.cols <- select(g3, SampleID,CNCode, DepthFrom_cm, DepthTo_cm,CompactionIn_mm,DepthRange_cm,
                  CompactionOut_mm, C.percent,N.percent, PipeLength_mm,PipeDiameter_cm,DryWeight_g ) %>%
  mutate( Site_Type = "control")

NewDATA <- rbind(g1.cols,g2.cols, g3.cols)
#NewDATA <- NewDATA [ !is.na(NewDATA$C.percent), ] #remove NA-s if any

NewDATA$C.percent <- ifelse(NewDATA$C.percent == 0, 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$DepthTo_cm - NewDATA$DepthFrom_cm) #round % to full numbers to run Poisson
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter_cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3 <- NewDATA$DryWeight_g / NewDATA$SampleVolume.cm3

NewDATA$Core_in.mm <- NewDATA$PipeLength_mm  - NewDATA$CompactionIn_mm # Compaction in mm!
NewDATA$Pipe_in.mm <- NewDATA$PipeLength_mm  - NewDATA$CompactionOut_mm  #Compaction in mm!
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.mm/NewDATA$Pipe_in.mm

NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$dry_bulk_density.gcm3 * NewDATA$Compaction_Correction_Value

NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C.percent/100
NewDATA$CarbonStock.Mgha <- (((NewDATA$CarbonDensity.gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength.cm )

NewDATA$NitrogenDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$N.percent/100
NewDATA$NitrogenStock.Mgha <- (((NewDATA$NitrogenDensity.gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength.cm )

range(NewDATA$Compaction_Correction_Value)# Check if all values are  below 1 = If value = 1 this core had no records of compaction
range(NewDATA$CarbonStock.Mgha, na.rm = T )# 
range(NewDATA$CarbonDensity.gcm3, na.rm = T )#

#SAVE:
GL <- NewDATA %>%
  separate(SampleID, into = c ("GL","site_type", "plot4m2", "date", "slice" ), sep = "_", remove = F) %>%
  unite("core", c("site_type","plot4m2"), remove = F) %>% #Crete core ID
  unite("Plot_ID", c("GL","site_type","plot4m2"), remove = F) #Create PlotID to merge these data with location data

#Join GL data wit location data:
GL_site_location <- read.csv("BRP_GippslandSiteLocations.csv")
names(GL_site_location)

GL_Master <- left_join(GL, GL_site_location, by = "Plot_ID")
write.csv(GL_Master, file = "GL_Master.csv") #Work with this file from now on. 
#I did column to text to produce extra variable off SampleID = locaton,site,plot,core and transect

#Analyze  Coastal Veg DATA from Avalon ======
GL <- read.csv("GL.csv")
#Linear Model:
CN_lm <- lm(TotalCarbonStock ~ site, data = GL2)
#TABLE:
tab_model(CN_lm)


#Plot of Mg/ha OC by  DepthRange_cm============
GL <- read.csv("GL.csv")
#Decide how to compute Mg/ha - best approach is to add-up Mg/ha values of entire core.
core_carbon <- GL %>% 
  group_by(core) %>%
  summarise(CarbonStock.Mgha.core = sum(CarbonStock.Mgha, na.rm = T)) %>%
  separate(core, into = c ("Site_Type","plot" ), sep = "_", remove = F )

core_carbon$location <- substr(core_carbon$plot, 3,3 ) #Get location out of 4-characters
core_carbon$location <- factor(core_carbon$location, levels = c("H","M","L"))
a <- ggplot(core_carbon, aes(x = Site_Type, y = CarbonStock.Mgha.core, color = Site_Type)) +
  geom_boxplot() +
  geom_jitter( alpha = 0.4)+
  facet_grid(.~location)+
  ylab(bquote('Organic Carbon Stock ' (Mg*~ha^-1))) + xlab("") +
  theme_bw() +
  #coord_flip()+
  ggtitle("BRP Sites in Gippsland ")+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "grey90"))
a


GL2 <- GL %>%select(site,core,CarbonStock.Mgha) %>%
  group_by(site,core) %>%
  summarise(TotalCarbonStock = sum(CarbonStock.Mgha, na.rm = T)) 

b <- ggplot(GL2, aes(x = "Within Each Core", y = TotalCarbonStock, color = site)) +
  geom_boxplot() +
  facet_grid(.~ site)+ geom_jitter( alpha = 0.4)+
  ylab("Total Carbon Stock (Mg/ha)") + xlab("") +
  theme_bw() +
  ggtitle("Mean total C-stock per core")+
  theme(axis.text.x = element_text(size = 10,face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 16, angle = 90, hjust = 0.5),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 18, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
b
library(grid)
library(gridExtra)
grid.arrange(a, b)

#Plot by location========
GL <- read.csv("GL.csv")
GL$location <- factor(GL$location, levels = c("L", "M","H"))
c <- ggplot(GL, aes(x = location, y = CarbonStock.Mgha, color = Site_Type)) +
  geom_boxplot() +
  facet_grid(Site_Type~ DepthRange_cm)+ geom_jitter( alpha = 0.4)+
  ylab(bquote('Organic Carbon Stock ' (Mg*~ha^-1))) + xlab("") +
  theme_bw() +
  coord_flip()+
  ggtitle("BRP Sites in Gippsland ")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "grey90"))
c
bquote('Organic Carbon Stock ' (Mg*~ha^-1))