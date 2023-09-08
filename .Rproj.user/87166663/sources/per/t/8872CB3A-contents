library(readxl)
library(tidyverse)
library(readr)
library(stringr)
library(reshape2)
library(table1)



#####----
#set working directory 
setwd("/Users/godsgiftnkechichukwuonye/Library/CloudStorage/Box-Box/R21 Wildfire and Flash Floods - shared all/Results/Turner Lab Results/Combined datasets with MRA lab ID")

#Soil PAH=====
pah <- read_xlsx("Combined_PAH_Data.xlsx", sheet= "totalsoil", col_names = TRUE) 
head(pah)
pah$RESULT<- as.numeric(pah$RESULT)
pah$detect = ifelse(pah$RESULT>0,"Detect","Non-Detect")
pah$detect<- replace_na(pah$detect, "Non-Detect")
pah$RESULT<- replace_na(pah$RESULT, 0)

pah$corrected<- as.numeric(pah$corrected)
pah$corrected <- ifelse(pah$corrected <=(0.00000),
                        formatC(signif(((as.numeric(pah$DL)/sqrt(2))),digits=2), digits=2,format="fg", flag="#"),
                        formatC(signif((pah$corrected),digits=4), digits=4,format="fg", flag="#"))


pah$corrected<- as.numeric(pah$corrected)
pah<- pah[!is.na(pah$RESULT),]
write.csv(pah, file="pah_omit.csv")


pah_comm<- pah[pah$TYPE != "control", ]  
pah_comm<- pah_comm[pah_comm$TYPE != "nonresidential", ]  

table1(~RESULT|ANALYTE, 
       data=pah_comm,
       overall=F)
table1(~ detect|ANALYTE, 
       data=pah_comm,
       overall=F)
pah_nonrezctrl<- pah_ctrl<- pah[pah$TYPE != "community", ]  
pah_ctrl<- pah[pah$TYPE != "community", ]  
pah_ctrl<- pah_ctrl[pah_ctrl$TYPE != "nonresidential", ]  
pah_nonrezctrl<- pah_nonrezctrl[!(pah_nonrezctrl$RESULT==0),]

table1(~RESULT|ANALYTE+Depth, 
       data=pah_nonrezctrl,
       overall=F)

table1(~ detect|ANALYTE, 
       data=pah_ctrl,
       overall=F)

#soil detection table=====
table1(~ detect|ANALYTE+TYPE, 
       data=pah,
       overall=F)

table1(~ detect|ANALYTE+Depth, 
       data=pah,
       overall=F)
table1(~ detect|ANALYTE+Depth, 
       data=pah_ctrlcomm,
       overall=F)
table1(~RESULT|ANALYTE+Depth, 
       data=pah,
       overall=F)


pah_short <-pah_nonrezctrl[c(7, 18, 21)]
pah_wide<- datawizard::data_to_wide(
  pah_short,
  id_cols = NULL,
  values_from = "RESULT",
  names_from = "ANALYTE",
  values_drop_na = TRUE)

pah_wide$Fluoranthene<- replace_na(pah_wide$Fluoranthene, 0)
pah_wide$Pyrene<- replace_na(pah_wide$Pyrene, 0)
pah_wide$`Benzo(b)fluoranthene`<- replace_na(pah_wide$`Benzo(b)fluoranthene`, 0)
pah_wide$`Benzo(g,h,i)perylene`<- replace_na(pah_wide$`Benzo(g,h,i)perylene`, 0)
pah_wide$Chrysene<- replace_na(pah_wide$Chrysene, 0)
pah_wide$`Indeno(1,2,3-cd)pyrene`<- replace_na(pah_wide$`Indeno(1,2,3-cd)pyrene`, 0)
pah_wide$Naphthalene<- replace_na(pah_wide$Naphthalene, 0)
pah_wide$Acenaphthene<- replace_na(pah_wide$Acenaphthene, 0)
pah_wide$`Benzo(a)anthracene`<- replace_na(pah_wide$`Benzo(a)anthracene`, 0)
pah_wide$Fluorene<- replace_na(pah_wide$Fluorene, 0)
pah_wide$Phenanthrene<- replace_na(pah_wide$Phenanthrene, 0)
pah_wide$`Benzo(a)pyrene`<- replace_na(pah_wide$`Benzo(a)pyrene`, 0)
pah_wide$`Benzo(k)fluoranthene`<- replace_na(pah_wide$`Benzo(k)fluoranthene`, 0)
pah_wide$`Dibenz(a,h)anthracene`<- replace_na(pah_wide$`Dibenz(a,h)anthracene`, 0)
pah_wide$`2-Methylnaphthalene`<- replace_na(pah_wide$`2-Methylnaphthalene`, 0)
pah_wide$Acenaphthylene<- replace_na(pah_wide$Acenaphthylene, 0)
write_csv(pah_wide, "pah_nonres_wide.csv")





#summary statistics=====
pah_ctrlcomm<- pah[pah$TYPE != "community", ] 
pah_ctrlcomm<- pah[pah$TYPE != "control", ] 
table1(~corrected|ANALYTE+TYPE, 
       data=pah,
       overall=F)

table1(~corrected|ANALYTE+Depth, 
       data=pah_ctrlcomm,
       overall=F)



pah$ANALYTE<- as.factor(pah$ANALYTE)

#anthracene plot=====
pah_anthracene<-pah[pah$ANALYTE == 'Anthracene',] 
shapiro.test(log(pah_anthracene$RESULT)) #log transforming the data is the way to go
pah_anthracene_rescon<-pah_anthracene[pah_anthracene$TYPE != "community",] 

ggplot(pah_anthracene, aes(x=TYPE, y=corrected,
                           fill= TYPE)) +
  geom_errorbar(aes(ymin = min(corrected), ymax = max(corrected)), width = 0.2)+
  geom_col()+
  labs(title = "Anthracene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous(limits = c(0, 0.2))





pah_anthracene<-pah[pah$ANALYTE == 'Anthracene',] 

gmeans <- aggregate(pah_anthracene$corrected,
                    by = list(pah_anthracene$TYPE, pah_anthracene$Depth),
                    FUN = geoMean)

gsds <- aggregate(dat$value,
                  by = list(dat$community, dat$samplings),
                  FUN = geoSD)

gdat <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))





ggplot(pah_anthracene_rescon, aes(x=Depth, y=corrected,
                           color= Depth)) +
  geom_boxplot()+
  labs(title = "Anthracene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous(limits = c(0, 0.005))


#1-Methylnaphthalene plot=====
pah1Methylnaphthalene<-pah[pah$ANALYTE == '1-Methylnaphthalene',] 
pah1Methylnaphthalene_rescon<-pah1Methylnaphthalene[pah1Methylnaphthalene$TYPE != "community",] 

ggplot(pah_anthracene, aes(x=TYPE, y=corrected,
                           color= TYPE)) +
  geom_boxplot()+
  labs(title = "1-Methylnaphthalene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pah1Methylnaphthalene_rescon, aes(x=Depth, y=corrected,
                                  color= Depth)) +
  geom_boxplot()+
  labs(title = "1-Methylnaphthalene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous(limits = c(0, 0.01))


#2-Chloronaphthalene plot=====
pahChloronaphthalene<-pah[pah$ANALYTE == '2-Chloronaphthalene',] 
pahChloronaphthalene_rescon<-pahChloronaphthalene[pahChloronaphthalene$TYPE != "community",] 

ggplot(pahChloronaphthalene, aes(x=TYPE, y=corrected,
                           color= TYPE)) +
  geom_boxplot()+
  labs(title = "2-Chloronaphthalene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahChloronaphthalene_rescon, aes(x=Depth, y=corrected,
                                         color= Depth)) +
  geom_boxplot()+
  labs(title = "2-Chloronaphthalene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()



#2-Chloronaphthalene plot=====
pahChloronaphthalene<-pah[pah$ANALYTE == '2-Chloronaphthalene',] 
pahChloronaphthalene_rescon<-pahChloronaphthalene[pahChloronaphthalene$TYPE != "community",] 

ggplot(pahChloronaphthalene, aes(x=TYPE, y=corrected,
                                 color= TYPE)) +
  geom_boxplot()+
  labs(title = "2-Chloronaphthalene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahChloronaphthalene_rescon, aes(x=Depth, y=corrected,
                                        color= Depth)) +
  geom_boxplot()+
  labs(title = "2-Chloronaphthalene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()


#2-Methylnaphthalene plot=====
pah2Methylnaphthalene<-pah[pah$ANALYTE == '2-Methylnaphthalene',] 
pah2Methylnaphthalene_rescon<-pah2Methylnaphthalene[pah2Methylnaphthalene$TYPE != "community",] 

ggplot(pah2Methylnaphthalene, aes(x=TYPE, y=corrected,
                                 color= TYPE)) +
  geom_boxplot()+
  labs(title = "2-Methylnaphthalene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pah2Methylnaphthalene_rescon, aes(x=Depth, y=corrected,
                                        color= Depth)) +
  geom_boxplot()+
  labs(title = "2-Methylnaphthalene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()


#Acenaphthene plot=====
pahAcenaphthene<-pah[pah$ANALYTE == 'Acenaphthene',] 
pahAcenaphthene_rescon<-pahAcenaphthene[pahAcenaphthene$TYPE != "community",] 

ggplot(pahAcenaphthene, aes(x=TYPE, y=corrected,
                                  color= TYPE)) +
  geom_boxplot()+
  labs(title = "Acenaphthene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahAcenaphthene_rescon, aes(x=Depth, y=corrected,
                                         color= Depth)) +
  geom_boxplot()+
  labs(title = "Acenaphthene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()



#Acenaphthylene plot=====
pahAcenaphthylene<-pah[pah$ANALYTE == 'Acenaphthylene',] 
pahAcenaphthylene_rescon<-pahAcenaphthylene[pahAcenaphthylene$TYPE != "community",] 

ggplot(pahAcenaphthylene, aes(x=TYPE, y=corrected,
                            color= TYPE)) +
  geom_boxplot()+
  labs(title = "Acenaphthylene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahAcenaphthylene_rescon, aes(x=Depth, y=corrected,
                                   color= Depth)) +
  geom_boxplot()+
  labs(title = "Acenaphthylene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()


#Benzo(a)anthracene plot=====
pahBenzoanthracene<-pah[pah$ANALYTE == 'Benzo(a)anthracene',] 
pahBenzoanthracene_rescon<-pahBenzoanthracene[pahBenzoanthracene$TYPE != "community",] 

ggplot(pahBenzoanthracene, aes(x=TYPE, y=corrected,
                              color= TYPE)) +
  geom_boxplot()+
  labs(title = "Benzo(a)anthracene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahBenzoanthracene_rescon, aes(x=Depth, y=corrected,
                                     color= Depth)) +
  geom_boxplot()+
  labs(title = "Benzo(a)anthracene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()


#Benzo(a)pyrene plot=====
pahBenzopyrene<-pah[pah$ANALYTE == 'Benzo(a)pyrene',] 
pahBenzopyrene_rescon<-pahBenzopyrene[pahBenzopyrene$TYPE != "community",] 

ggplot(pahBenzopyrene, aes(x=TYPE, y=corrected,
                               color= TYPE)) +
  geom_boxplot()+
  labs(title = "Benzo(a)pyrene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahBenzopyrene_rescon, aes(x=Depth, y=corrected,
                                      color= Depth)) +
  geom_boxplot()+
  labs(title = "Benzo(a)pyrene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

#Benzo(b)fluoranthene plot=====
pahBenzofluoranthene<-pah[pah$ANALYTE == 'Benzo(b)fluoranthene',] 
pahBenzofluoranthene_rescon<-pahBenzofluoranthene[pahBenzofluoranthene$TYPE != "community",] 

ggplot(pahBenzofluoranthene, aes(x=TYPE, y=corrected,
                           color= TYPE)) +
  geom_boxplot()+
  labs(title = "Benzo(b)fluoranthene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahBenzofluoranthene_rescon, aes(x=Depth, y=corrected,
                                  color= Depth)) +
  geom_boxplot()+
  labs(title = "Benzo(b)fluoranthene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

#Benzo(g,h,i)perylene plot=====
pahBenzoperylene<-pah[pah$ANALYTE == 'Benzo(g,h,i)perylene',] 
pahBenzoperylene_rescon<-pahBenzoperylene[pahBenzoperylene$TYPE != "community",] 

ggplot(pahBenzoperylene, aes(x=TYPE, y=corrected,
                                 color= TYPE)) +
  geom_boxplot()+
  labs(title = "Benzo(g,h,i)perylene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahBenzoperylene_rescon, aes(x=Depth, y=corrected,
                                        color= Depth)) +
  geom_boxplot()+
  labs(title = "Benzo(g,h,i)perylene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

#Benzo(k)fluoranthene plot=====
pahBenzofluoranthene<-pah[pah$ANALYTE == 'Benzo(k)fluoranthene',] 
pahBenzofluoranthene_rescon<-pahBenzoperylene[pahBenzoperylene$TYPE != "community",] 

ggplot(pahBenzofluoranthene, aes(x=TYPE, y=corrected,
                             color= TYPE)) +
  geom_boxplot()+
  labs(title = "Benzo(k)fluoranthene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahBenzofluoranthene_rescon, aes(x=Depth, y=corrected,
                                    color= Depth)) +
  geom_boxplot()+
  labs(title = "Benzo(k)fluoranthene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

#Chrysene plot=====
pahChrysene<-pah[pah$ANALYTE == 'Chrysene',] 
pahChrysene_rescon<-pahChrysene[pahChrysene$TYPE != "community",] 

ggplot(pahChrysene, aes(x=TYPE, y=corrected,
                                 color= TYPE)) +
  geom_boxplot()+
  labs(title = "Chrysene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahChrysene_rescon, aes(x=Depth, y=corrected,
                                        color= Depth)) +
  geom_boxplot()+
  labs(title = "Chrysene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

#Dibenz(a,h)anthracene plot=====
pahDibenzanthracene<-pah[pah$ANALYTE == 'Dibenz(a,h)anthracene',] 
pahDibenzanthracene_rescon<-pahDibenzanthracene[pahDibenzanthracene$TYPE != "community",] 

ggplot(pahDibenzanthracene, aes(x=TYPE, y=corrected,
                        color= TYPE)) +
  geom_boxplot()+
  labs(title = "Dibenz(a,h)anthracene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahDibenzanthracene_rescon, aes(x=Depth, y=corrected,
                               color= Depth)) +
  geom_boxplot()+
  labs(title = "Dibenz(a,h)anthracene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()


#Fluoranthene plot=====
pahFluoranthene<-pah[pah$ANALYTE == 'Fluoranthene',] 
pahFluoranthene_rescon<-pahFluoranthene[pahFluoranthene$TYPE != "community",] 

ggplot(pahFluoranthene, aes(x=TYPE, y=corrected,
                                color= TYPE)) +
  geom_boxplot()+
  labs(title = "Fluoranthene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahFluoranthene_rescon, aes(x=Depth, y=corrected,
                                       color= Depth)) +
  geom_boxplot()+
  labs(title = "Fluoranthene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()


#Fluorene plot=====
pahFluorene<-pah[pah$ANALYTE == 'Fluorene',] 
pahFluorene_rescon<-pahFluoranthene[pahFluoranthene$TYPE != "community",] 

ggplot(pahFluorene, aes(x=TYPE, y=corrected,
                            color= TYPE)) +
  geom_boxplot()+
  labs(title = "Fluorene Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahFluorene_rescon, aes(x=Depth, y=corrected,
                                   color= Depth)) +
  geom_boxplot()+
  labs(title = "Fluorene Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

#Indeno(1,2,3-cd)pyrene plot=====
pahIndenopyrene <-pah[pah$ANALYTE == 'Indeno(1,2,3-cd)pyrene',] 
pahIndenopyrene_rescon<-pahIndenopyrene[pahIndenopyrene$TYPE != "community",] 

ggplot(pahIndenopyrene, aes(x=TYPE, y=corrected,
                        color= TYPE)) +
  geom_boxplot()+
  labs(title = "Indeno(1,2,3-cd)pyrene  Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahIndenopyrene_rescon, aes(x=Depth, y=corrected,
                               color= Depth)) +
  geom_boxplot()+
  labs(title = "Indeno(1,2,3-cd)pyrene  Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()


#Phenanthrene plot=====
pahPhenanthrene <-pah[pah$ANALYTE == 'Phenanthrene',] 
pahPhenanthrene_rescon<-pahPhenanthrene[pahPhenanthrene$TYPE != "community",] 

ggplot(pahPhenanthrene, aes(x=TYPE, y=corrected,
                            color= TYPE)) +
  geom_boxplot()+
  labs(title = "Phenanthrene  Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahPhenanthrene_rescon, aes(x=Depth, y=corrected,
                                   color= Depth)) +
  geom_boxplot()+
  labs(title = "Phenanthrene  Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()


#	Pyrene plot=====
pahPyrene <-pah[pah$ANALYTE == 'Pyrene',] 
pahPyrene_rescon<-pahPyrene[pahPyrene$TYPE != "community",] 

ggplot(pahPyrene, aes(x=TYPE, y=corrected,
                            color= TYPE)) +
  geom_boxplot()+
  labs(title = "Pyrene  Concentration by Sampling Location",
       y = "Concentration (mg/kg)",
       x = "Sample Type")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

ggplot(pahPyrene_rescon, aes(x=Depth, y=corrected,
                                   color= Depth)) +
  geom_boxplot()+
  labs(title = "Pyrene  Concentration by Depth",
       y = "Concentration (mg/kg)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()







pah<-pah[pah$ANALYTE != "1-Methylnaphthalene",] 
pah<-pah[pah$ANALYTE != "2-Chloronaphthalene",] 
library(EnvStats)
gmeans <- aggregate(pah$corrected,
                    by = list(pah$ANALYTE, pah$TYPE, pah$Depth),
                    FUN = geoMean)

gsds <- aggregate(pah$corrected,
                  by = list(pah$ANALYTE, pah$TYPE, pah$Depth),
                  FUN = geoSD)

gdat <- full_join(gmeans, gsds, by = c("Group.1", "Group.2", "Group.3"))
names(gdat) <- c("ANALYTE", "TYPE", "Depth","gmean", "gsd")
gdat$gmean <- as.numeric(gdat$gmean)
gdat$gsd <- as.numeric(gdat$gsd)

ggplot(gdat, aes(fill= ANALYTE, y=gmean, x=ANALYTE))+
  geom_bar(position="dodge", stat="identity", color = "black") +
  geom_errorbar(aes(x = ANALYTE, 
                    ymax = gmean*gsd, 
                    ymin = gmean/gsd))

  

  
  

