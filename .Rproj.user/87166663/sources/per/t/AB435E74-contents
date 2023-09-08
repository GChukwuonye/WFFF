#load package====
library(readxl)
library(tidyverse)
library(readr)
library(stringr)
library(reshape2)
library(table1)
library(ggplot2)
library(EnvStats)
library(tidyverse)
library(Hmisc)
library(correlation)
library(corrplot)
library(ggcorrplot)

#set working directory==== 
setwd("/Users/godsgiftnkechichukwuonye/Library/CloudStorage/Box-Box/R21 Wildfire and Flash Floods - shared all/Results/Turner Lab Results/Combined datasets with MRA lab ID")


#Loading data from excel sheet! Remember to install readxl====
dioxin <- read_xlsx("COmbined_dioxin_data.xlsx", sheet= "cleaned", col_names = TRUE) 
head(dioxin) #this function provides information on the first six lines in the dataset including column names
tail(dioxin) #this function provides information on the last six lines in the dataset including column names
dioxin <- as.data.frame(dioxin)
dioxin$`Individual TEQ`<- as.numeric(dioxin$`Individual TEQ`)
dioxin$Result<- as.numeric(dioxin$Result)
dioxin$EMPC<- as.numeric(dioxin$EMPC)
dioxin$concentration<- with(dioxin, (dioxin$Result+dioxin$EMPC))
dioxin$concentration <- as.numeric(dioxin$concentration)
dioxin$detect = ifelse(dioxin$concentration>0.0000,"Detect","Non-Detect")
dioxin$detect<- replace_na(dioxin$detect, "Non-Detect")
class(dioxin$detect)
class(dioxin$Compound)
dioxin$detect<- as.factor(dioxin$detect)
dioxin$corrected<- replace_na(dioxin$concentration, 0)
dioxin$concentration<- replace_na(dioxin$concentration, 0)
table1(~detect|Compound, 
       data=dioxin,
       overall=F)

dioxin_nonrez<- dioxin[dioxin$Type !="Control", ]  

table1(~detect|Compound, 
       data=dioxin_nonrez,
       overall=F)

#correcting the nondetects====
dioxin_nonrez<- dioxin_nonrez[dioxin_nonrez$Compound !="1,2,3,7,8-PeCDF", ] 

dioxin_nonrez$corrected<- as.numeric(dioxin_nonrez$corrected)

dioxin_nonrez$corrected <- ifelse(dioxin_nonrez$corrected <=(0.00000),
                           formatC(signif(((as.numeric(dioxin_nonrez$EDL)/2)),digits=2), digits=2,format="fg", flag="#"),
                           formatC(signif((dioxin_nonrez$corrected),digits=4), digits=4,format="fg", flag="#"))
dioxin_nonrez$corrected<- as.numeric(dioxin_nonrez$corrected)
dioxin_nonrez$corrected_TEQ<- with(dioxin_nonrez, (dioxin_nonrez$TEF*dioxin_nonrez$corrected))
write_csv(dioxin_nonrez, "dioxin_nonrez.csv")

table1(~corrected|Compound, 
       data=dioxin_nonrez,
       render.continuous=c(.="Mean (sd)", .="Median [Min, Max]",
                          .="GMEAN (GSD)"))

dioxin_short <-dioxin_nonrez[c(3, 5, 16)]
dioxin_wide<- datawizard::data_to_wide(
  dioxin_short,
  id_cols = NULL,
  values_from = "corrected",
  names_from = "Compound",
  values_drop_na = TRUE)

write_csv(dioxin_wide, "dioxin_nonresidential2.csv")

dioxin_teq <-dioxin_nonrez[c(3, 5, 17)]
dioxin_teq_wide<- datawizard::data_to_wide(
  dioxin_teq,
  id_cols = NULL,
  values_from = "corrected_TEQ",
  names_from = "Compound",
  values_drop_na = TRUE)
dioxin_teq_wide$`Total TEQ`<- with(dioxin_teq_wide, (dioxin_teq_wide$`2,3,7,8-TCDF`+
                                                       dioxin_teq_wide$`2,3,7,8-TCDD`+
                                                       dioxin_teq_wide$`2,3,4,7,8-PeCDF` +
                                                       dioxin_teq_wide$`1,2,3,7,8-PeCDD`+
                                                       dioxin_teq_wide$`1,2,3,4,7,8-HxCDF`+
                                                       dioxin_teq_wide$`1,2,3,6,7,8-HxCDF`+
                                                       dioxin_teq_wide$`2,3,4,6,7,8-HxCDF`+
                                                       dioxin_teq_wide$`1,2,3,7,8,9-HxCDF`+
                                                       dioxin_teq_wide$`1,2,3,4,7,8-HxCDD`+
                                                       dioxin_teq_wide$`1,2,3,6,7,8-HxCDD`+
                                                       dioxin_teq_wide$`1,2,3,4,6,7,8-HpCDF`+
                                                       dioxin_teq_wide$`1,2,3,4,7,8,9-HpCDF`+
                                                       dioxin_teq_wide$`1,2,3,4,6,7,8-HpCDF`+
                                                       dioxin_teq_wide$`1,2,3,4,6,7,8-HpCDD`+
                                                       dioxin_teq_wide$OCDF+
                                                       dioxin_teq_wide$OCDD))

teq_wide<- dioxin_teq_wide %>% pivot_longer(
  cols = `2,3,7,8-TCDF`:`Total TEQ`,
  names_to = c("Compound"),
  values_to = "TEQ")

table1(~TEQ|Compound, 
       data=teq_wide,
       render.continuous=c(.="Mean (sd)", .="Median [Min, Max]",
                           .="GMEAN (GSD)"))

write_csv(dioxin_teq_wide, "dioxin_teq_wider.csv")


#PCDD, PCDF, Total PCDD and PCDF====
dioxin_wider <- read_xlsx("dioxin_nonresidential.xlsx", sheet= "dioxin_nonresidential",  col_names = TRUE) 

summary(dioxin_wider$`Total PCDD`)
sd(dioxin_wider$`Total PCDD`)
geoMean(dioxin_wider$`Total PCDD`)
geoSD(dioxin_wider$`Total PCDD`)

summary(dioxin_wider$`Total PCDF`)
sd(dioxin_wider$`Total PCDF`)
geoMean(dioxin_wider$`Total PCDF`)
geoSD(dioxin_wider$`Total PCDF`)

summary(dioxin_wider$`PCDD+PCDF`)
sd(dioxin_wider$`PCDD+PCDF`)
geoMean(dioxin_wider$`PCDD+PCDF`)
geoSD(dioxin_wider$`PCDD+PCDF`)


dioxin_corrected_short <-dioxin[c(3, 5, 16)]
dioxin_corrected_wide<- datawizard::data_to_wide(
  dioxin_corrected_short,
  id_cols = NULL,
  values_from = "corrected",
  names_from = "Compound",
  values_drop_na = TRUE)

write_csv(dioxin_corrected_wide, "dioxin_corrected_wide.csv")

#control=====
#Loading data from excel sheet! Remember to install readxl====

dioxin_control<- dioxin[dioxin$Type !="Sample", ]  
head(dioxin_control) #this function provides information on the first six lines in the dataset including column names
tail(dioxin_control) #this function provides information on the last six lines in the dataset including column names
dioxin_control <- as.data.frame(dioxin_control)
table1(~detect|Compound, 
       data=dioxin_control,
       overall=F)

#correcting the nondetects=====
dioxin_control<- dioxin_control[dioxin_control$Compound !="1,2,3,7,8-PeCDF",] 
dioxin_control<- dioxin_control[dioxin_control$Compound !="1,2,3,4,7,8,9-HpCDF",] 
dioxin_control<- dioxin_control[dioxin_control$Compound !="2,3,7,8-TCDD",] 
dioxin_control<- dioxin_control[dioxin_control$Compound !="2,3,7,8-TCDF",] 


dioxin_control$corrected <- ifelse(dioxin_control$corrected <=(0.00000),
                                  formatC(signif(((as.numeric(dioxin_control$EDL)/2)),digits=2), digits=2,format="fg", flag="#"),
                                  formatC(signif((dioxin_control$corrected),digits=4), digits=4,format="fg", flag="#"))
dioxin_control$corrected<- as.numeric(dioxin_control$corrected)
dioxin_control$corrected_TEQ<- with(dioxin_control, (dioxin_control$TEF*dioxin_control$corrected))


table1(~corrected|Compound, 
       data=dioxin_control,
       render.continuous=c(.="Mean (sd)", .="Median [Min, Max]",
                           .="GMEAN (GSD)"))

control_short <-dioxin_control[c(3, 5, 16)]
control_wide<- datawizard::data_to_wide(
control_short,
  id_cols = NULL,
  values_from = "corrected",
  names_from = "Compound",
  values_drop_na = TRUE)

write_csv(control_wide, "dioxin_control.csv")

control_teq <-dioxin_control[c(3, 5, 17)]
control_teq_wide<- datawizard::data_to_wide(
control_teq,
  id_cols = NULL,
  values_from = "corrected_TEQ",
  names_from = "Compound",
  values_drop_na = TRUE)
control_teq_wide$`Total TEQ`<- with(control_teq_wide,    (control_teq_wide$`2,3,4,7,8-PeCDF` +
                                                         control_teq_wide$`1,2,3,7,8-PeCDD`+
                                                         control_teq_wide$`1,2,3,4,7,8-HxCDF`+
                                                         control_teq_wide$`1,2,3,6,7,8-HxCDF`+
                                                         control_teq_wide$`2,3,4,6,7,8-HxCDF`+
                                                         control_teq_wide$`1,2,3,7,8,9-HxCDF`+
                                                         control_teq_wide$`1,2,3,4,7,8-HxCDD`+
                                                         control_teq_wide$`1,2,3,6,7,8-HxCDD`+
                                                         control_teq_wide$`1,2,3,4,6,7,8-HpCDF`+
                                                          control_teq_wide$`1,2,3,4,6,7,8-HpCDF`+
                                                         control_teq_wide$`1,2,3,4,6,7,8-HpCDD`+
                                                         control_teq_wide$OCDF+
                                                         control_teq_wide$OCDD))

control_teq_wider<-control_teq_wide %>% pivot_longer(
  cols = `2,3,4,7,8-PeCDF` :`Total TEQ`,
  names_to = c("Compound"),
  values_to = "TEQ")

table1(~TEQ|Compound, 
       data=control_teq_wider,
       render.continuous=c(.="Mean (sd)", .="Median [Min, Max]",
                           .="GMEAN (GSD)"))

write_csv(control_teq_wide, "control_teq_wider.csv")


#PCDD, PCDF, Total PCDD and PCDF=====
control_wide <- read_xlsx("dioxin_control.xlsx", sheet= "dioxin_control",  col_names = TRUE) 

summary(control_wide$`Total PCDD`)
sd(control_wide$`Total PCDD`)
geoMean(control_wide$`Total PCDD`)
geoSD(control_wide$`Total PCDD`)

summary(control_wide$`Total PCDF`)
sd(control_wide$`Total PCDF`)
geoMean(control_wide$`Total PCDF`)
geoSD(control_wide$`Total PCDF`)

summary(control_wide$`PCDD/PCDF`)
sd(control_wide$`PCDD/PCDF`)
geoMean(control_wide$`PCDD/PCDF`)
geoSD(control_wide$`PCDD/PCDF`)


#merging control and samples=====
both<- dioxin
both$corrected <- ifelse(both$corrected <=(0.00000),
                                  formatC(signif(((as.numeric(both$EDL)/2)),digits=2), digits=2,format="fg", flag="#"),
                                  formatC(signif((both$corrected),digits=4), digits=4,format="fg", flag="#"))
both$corrected<- as.numeric(both$corrected)

both$corrected_TEQ<- with(both, (both$TEF*both$corrected))

both_short<-both[c(3, 4, 5, 13, 17)]
both_wide<- datawizard::data_to_wide(
  both_short,
  id_cols = NULL,
  values_from = "corrected_TEQ",
  names_from = "Compound",
  values_drop_na = TRUE)


both_wide$`Total TEQ`<- with(both_wide,    (both_wide$`2,3,7,8-TCDF`+
                                           both_wide$`2,3,7,8-TCDD`+
                                           both_wide$`2,3,4,7,8-PeCDF` +
                                           both_wide$`1,2,3,7,8-PeCDD`+
                                           both_wide$`1,2,3,4,7,8-HxCDF`+
                                           both_wide$`1,2,3,6,7,8-HxCDF`+
                                           both_wide$`2,3,4,6,7,8-HxCDF`+
                                           both_wide$`1,2,3,7,8,9-HxCDF`+
                                           both_wide$`1,2,3,4,7,8-HxCDD`+
                                           both_wide$`1,2,3,6,7,8-HxCDD`+
                                           both_wide$`1,2,3,4,6,7,8-HpCDF`+
                                           both_wide$`1,2,3,4,7,8,9-HpCDF`+
                                           both_wide$`1,2,3,4,6,7,8-HpCDF`+
                                           both_wide$`1,2,3,4,6,7,8-HpCDD`+
                                           both_wide$OCDF+
                                           both_wide$OCDD))
both_longer<-both_wide %>% pivot_longer(
  cols = `2,3,7,8-TCDF` :`Total TEQ`,
  names_to = c("Compound"),
  values_to = "TEQ")

both_teq<- both_longer[both_longer$Compound == "Total TEQ", ]  


#WAMI=====
wami<- both_longer[both_longer$Community == "Wash, Miami", ]  
wami_teq<- wami[wami$Compound == "Total TEQ", ]  
kruskal.test(wami_teq$TEQ ~ wami_teq$Type, data=wami_teq)
wilcox.test(TEQ~Type, data=wami_teq)
ggplot(wami_teq, aes(x=wami_teq$Type, y=wami_teq$TEQ,
                 fill = Type,
                 color= Type)) +
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               color = 1) +  
  geom_boxplot(alpha = 0.5,     
               color = 1,          
               outlier.colour = 2) +
  geom_jitter(color=2, size=1.2) +
  theme(axis.text.x=element_text(size=15))+
  labs(y = "Concentration (TEQ ng/kg)",
       x = "Wash, Miami")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="white") +
  scale_y_continuous()


#IHCA====
ihca<- both_longer[both_longer$Community == "Icehouse Canyon", ]  
ihca_teq<- ihca[ihca$Compound == "Total TEQ", ]  
shapiro.test(ihca_teq$TEQ)
kruskal.test(ihca_teq$TEQ ~ ihca_teq$Type, data=ihca_teq)
wilcox.test(TEQ~Type, data=ihca_teq)
ggplot(ihca_teq, aes(x=ihca_teq$Type, y=ihca_teq$TEQ,
                     fill = Type,
                     color= Type)) +
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               color = 1) +  
  geom_boxplot(alpha = 0.5,     
               color = 1,          
               outlier.colour = 2) +
  geom_jitter(color=2, size=1.2) +
  theme(axis.text.x=element_text(size=15))+
  labs(y = "Concentration (TEQ ng/kg)",
       x = "Icehouse Canyon")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="white") +
  scale_y_continuous()

#GLMC====
glmc<- both_longer[both_longer$Community == "Globe/Midland city", ]  
glmc_teq<- glmc[glmc$Compound == "Total TEQ", ]  
shapiro.test(glmc_teq$TEQ)
kruskal.test(glmc_teq$TEQ ~ glmc_teq$Type, data=glmc_teq)
wilcox.test(TEQ~Type, data=glmc_teq)


ggplot(glmc_teq, aes(x=glmc_teq$Type, y=glmc_teq$TEQ,
                     fill = Type,
                     color= Type)) +
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               color = 1) +  
  geom_boxplot(alpha = 0.5,     
               color = 1,          
               outlier.colour = 2) +
  geom_jitter(color=2, size=1.2) +
  theme(axis.text.x=element_text(size=15))+
  labs(y = "Concentration (TEQ ng/kg)",
       x = "Globe/Midland City")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="white") +
  scale_y_continuous()


#TEQ boxplot=====

ggplot(both_teq, aes(x=Type, y=TEQ,
                    fill = Type,
                     color= Type)) +
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               color = 1) +  
  geom_boxplot(alpha = 0.5,     
               color = 1,          
               outlier.colour = 2) +
  geom_jitter(color=2, size=1.2) +
  theme(axis.text.x=element_text(size=15))+
  labs(y = "Concentration (TEQ ng/kg)",
       x = "Location")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="white") +
  scale_y_continuous()

kruskal.test(TEQ ~ Type, data=both_teq)
wilcox.test(TEQ~Type, data=both_teq)


ggplot(both_teq, aes(x=Community, y=TEQ,
                 fill = Type,
                 color= Type)) +
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               color = 1) +  
  geom_boxplot(alpha = 0.5,     
               color = 1,          
               outlier.colour = 2) +
  geom_jitter(color=2, size=1.2) +
  theme(axis.text.x=element_text(size=15))+
  labs(y = "Concentration (TEQ ng/kg)",
       x = "Location")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="white") +
  scale_y_continuous(limits = c(0, 6))

kruskal.test(TEQ ~ Type, data=both_teq)

#concentration boxplot====
both2<- read_xlsx("dioxin_control.xlsx", sheet= "both",  col_names = TRUE) 
ggplot(both2, aes(x=Type, y=`PCDD/PCDF`,
                 fill = Type,
                 color= Type)) +
  stat_boxplot(geom = "errorbar",
               width = 0.5,
               color = 1) +  
  geom_boxplot(alpha = 0.5,     
               color = 1,          
               outlier.colour = 2) +
  geom_jitter(color=2, size=1.2) +
  theme(axis.text.x=element_text(size=12))+
  labs(y = "Total PCDD/PCDF Concentration (ng/kg)",
       x = "Location")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="white") +
  scale_y_continuous()


kruskal.test(both2$`PCDD/PCDF` ~ both2$Type, data=both)


#combined correlation=====
dioxin_correlation<- dioxin_wide%>%select(2:18)
dioxin_cor<- rcorr(as.matrix(dioxin_correlation), type ="spearman")
print(dioxin_cor)
#correlation color plot====
corr <- cor(dioxin_correlation, method ="spearman")
p.mat <- cor_pmat(corr)
ggcorrplot(corr, method = "square", hc.order = TRUE, outline.col = "white")
ggcorrplot(corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat)
ggcorrplot(corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank")
ggcorrplot(corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank", lab = TRUE)

#combined total correlation=====
dioxin2_correlation<- dioxin_wide%>%select(17:24)
dioxin_cor2<- rcorr(as.matrix(dioxin2_correlation), type ="spearman")
print(dioxin_cor2)

#correlation color plot====
corr2 <- cor(dioxin2_correlation, method ="spearman")
p.mat <- cor_pmat(corr2)
ggcorrplot(corr2, method = "square", hc.order = TRUE, outline.col = "white")
ggcorrplot(corr2, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat)
ggcorrplot(corr2, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank")
ggcorrplot(corr2, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank", lab = TRUE)
ggcorrplot(corr2, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, lab = TRUE)



#sample correlation=====
dioxin3_correlation<- dioxin_wider%>%select(3:9, 15:22, 27)
dioxin_cor3<- rcorr(as.matrix(dioxin3_correlation), type ="spearman")
print(dioxin_cor3)

#correlation color plot=====
corr3 <- cor(dioxin3_correlation, method ="spearman")
p.mat <- cor_pmat(corr3)
ggcorrplot(corr3, method = "square", hc.order = TRUE, outline.col = "white")
ggcorrplot(corr3, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat)
ggcorrplot(corr3, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank")
ggcorrplot(corr3, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank", lab = TRUE)
ggcorrplot(corr3, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, lab = TRUE)

#sample correlation=====
dioxin4_correlation<- dioxin_wide%>%select(9:13, 23:27)
dioxin4_cor<- rcorr(as.matrix(dioxin4_correlation), type ="spearman")
print(dioxin4_cor)
#correlation color plot=====
corr4 <- cor(dioxin4_correlation, method ="spearman")
p.mat <- cor_pmat(corr4)
ggcorrplot(corr4, method = "square", hc.order = TRUE, outline.col = "white")
ggcorrplot(corr4, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat)
ggcorrplot(corr4, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank")
ggcorrplot(corr4, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank", lab = TRUE)
ggcorrplot(corr4, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, lab = TRUE)

#control correlation=====
control_correlation<- control_wide%>%select(2:7, 13:19)
control_corr<- rcorr(as.matrix(control_correlation), type ="spearman")
print(control_cor)
#correlation color plot====

control_corr <- cor(control_correlation, method ="spearman")
p.mat <- cor_pmat(control_corr)
ggcorrplot(control_corr, method = "square", hc.order = TRUE, outline.col = "white")
ggcorrplot(control_corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat)
ggcorrplot(control_corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank")
ggcorrplot(control_corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank", lab = TRUE)
ggcorrplot(control_corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, lab = TRUE)

#total compounds=====
totcontrol_correlation<- control_wide%>%select(7:11, 19:23)
totcontrol_corr<- rcorr(as.matrix(totcontrol_correlation), type ="spearman")
print(control_cor)
#correlation color plot
totcontrol_corr <- cor(totcontrol_correlation, method ="spearman")
p.mat <- cor_pmat(totcontrol_corr)
ggcorrplot(totcontrol_corr, method = "square", hc.order = TRUE, outline.col = "white")
ggcorrplot(totcontrol_corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat)
ggcorrplot(totcontrol_corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank")
ggcorrplot(totcontrol_corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, insig = "blank", lab = TRUE)
ggcorrplot(totcontrol_corr, method = "square", hc.order = TRUE, outline.col = "white", p.mat = p.mat, lab = TRUE)



