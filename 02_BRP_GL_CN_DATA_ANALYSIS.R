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

g1.cols <- select(g1, SampleID, DepthFrom_cm, DepthTo_cm,CompactionIn_mm,DepthRange_cm,
                  CompactionOut_mm, C.percent,PipeLength_mm,PipeDiameter_cm,DryWeight_g ) %>%
  mutate( Site_Type = "natural")

g2.cols <- select(g2, SampleID, DepthFrom_cm, DepthTo_cm,CompactionIn_mm,DepthRange_cm,
                  CompactionOut_mm, C.percent,PipeLength_mm,PipeDiameter_cm,DryWeight_g ) %>%
  mutate( Site_Type = "rehab")

g3.cols <- select(g3, SampleID, DepthFrom_cm, DepthTo_cm,CompactionIn_mm,DepthRange_cm,
                  CompactionOut_mm, C.percent,PipeLength_mm,PipeDiameter_cm,DryWeight_g ) %>%
  mutate( Site_Type = "control")

NewDATA <- rbind(g1.cols,g2.cols, g3.cols)
NewDATA <- NewDATA [ !is.na(NewDATA$C.percent),] #remove NA-s if any

NewDATA$C.percent <- ifelse(NewDATA$C.percent == 0, 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$DepthTo_cm - NewDATA$DepthFrom_cm) #round % to full numbers to run Poisson
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter_cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3 <- NewDATA$DryWeight_g / NewDATA$SampleVolume.cm3

NewDATA$Core_in.mm <- NewDATA$PipeLength_mm  - NewDATA$CompactionIn_mm # Compaction in mm!
NewDATA$Pipe_in.mm <- NewDATA$PipeLength_mm  - NewDATA$CompactionOut_mm  #Compaction in mm!
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.mm/NewDATA$Pipe_in.mm

NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$dry_bulk_density.gcm3 * NewDATA$Compaction_Correction_Valu

NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C.percent/100
NewDATA$CarbonStock.Mgha <- (((NewDATA$CarbonDensity.gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength.cm )

range(NewDATA$Compaction_Correction_Value)# Check if all values are  below 1 = If value = 1 this core had no records of compaction
range(NewDATA$CarbonStock.Mgha, na.rm = T )# 
range(NewDATA$CarbonDensity.gcm3, na.rm = T )#

#SAVE:
GL <- NewDATA %>%
  mutate(SampleID2 = SampleID) %>%
  separate(SampleID2, into = c ("GL","site", "plot", "date", "slice" ), sep = "_") %>%
  unite("core", c("site","plot"), remove = F)

#write.csv(GL, file = "GL.csv") #Work with this file from now on. 
#I did column to text to produce extra variable off SampleID = locaton,site,plot,core and transect

#Analyze  Coastal Veg DATA from Avalon ======
GL <- read.csv("GL.csv")
#Linear Model:
CN_lm <- lm(TotalCarbonStock ~ site , data = GL2)
#TABLE:
tab_model(CN_lm)


#Plot of Mg/ha OC by  DepthRange_cm============
GL <- read.csv("GL.csv")
a <- ggplot(GL, aes(x = Site_Type, y = CarbonStock.Mgha, color = Site_Type)) +
  geom_boxplot() +
  facet_grid(.~ DepthRange_cm)+ geom_jitter( alpha = 0.4)+
  ylab("Organic Carbon Stock (Mg/ha)") + xlab("") +
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
