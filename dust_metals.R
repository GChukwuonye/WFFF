library(readxl)
library(tidyverse)
library(readr)
library(stringr)
library(reshape2)
library(table1)
library(reshape2)
library(ggplot2)

#####----
#set working directory 
setwd("/Users/godsgiftnkechichukwuonye/Downloads")


#Soil PAH=====
dust <- read_xlsx("dust.xlsx", col_names = TRUE) 
dust2 <- read_xlsx("dust2.xlsx",  col_names = TRUE) 
head(dust)

ggplot(dust, aes(x=location, y=log(lead),
                 color=location)) +
  geom_boxplot()+
  labs(title = "Dust Lead Concentration in Indoor Floor and Outdoor Pouch",
       y = " Lead Concentration (ug/square feet)",
       x = "Depth")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  scale_y_continuous()

shapiro.test(log(dust$lead)) #log-normally distributed

head(dust2)
t.test(log(dust2$Indoor), log(dust2$Outdoor), paired=TRUE)
t.test(log(dust2$Indoor), log(dust2$Outdoor))

#Detection=====
setwd("/Users/godsgiftnkechichukwuonye/Library/CloudStorage/Box-Box/R21 Wildfire and Flash Floods - shared all/Results/ALEC Lab Results/Dust-ghost wipes/Calculations")
dust <- read_xlsx("Dust_Consolidated.xlsx", col_names = TRUE) 
#Be===
dust$`9 Be`<- as.numeric(dust$`9 Be`)
summary(dust$`9 Be`)
sd(dust$`9 Be`)
dust$`9 Be` = ifelse(dust$`9 Be`>0,"Detect","Non-Detect")
dust$`9 Be`<- replace_na(dust$`9 Be`, "Non-Detect")
dust$`9 Be`<- as.factor(dust$`9 Be`)
summary(dust$`9 Be`)
#Na====
dust$`23 Na`<- as.numeric(dust$`23 Na`)
summary(dust$`23 Na`)
sd(dust$`23 Na`)
dust$`23 Na` = ifelse(dust$`23 Na`>0,"Detect","Non-Detect")
dust$`23 Na`<- replace_na(dust$`23 Na`, "Non-Detect")
dust$`23 Na`<- as.factor(dust$`23 Na`)
summary(dust$`23 Na`)
#Mg====
dust$`24 Mg`<- as.numeric(dust$`24 Mg`)
dust$`24 Mg` = ifelse(dust$`24 Mg`,"Detect","Non-Detect")
dust$`24 Mg`<- replace_na(dust$`24 Mg`, "Non-Detect")
dust$`24 Mg`<- as.factor(dust$`24 Mg`)
summary(dust$`24 Mg`)
#Al====
dust$`27 Al`<- as.numeric(dust$`27 Al`)
dust$`27 Al` = ifelse(dust$`27 Al`,"Detect","Non-Detect")
dust$`27 Al`<- replace_na(dust$`27 Al`, "Non-Detect")
dust$`27 Al`<- as.factor(dust$`27 Al`)
summary(dust$`27 Al`)
#K====
dust$`39 K`<- as.numeric(dust$`39 K`)
dust$`39 K`= ifelse(dust$`39 K`,"Detect","Non-Detect")
dust$`39 K`<- replace_na(dust$`39 K`, "Non-Detect")
dust$`39 K`<- as.factor(dust$`39 K`)
summary(dust$`39 K`)
#Ca====
dust$`44 Ca`<- as.numeric(dust$`44 Ca`)
dust$`44 Ca`= ifelse(dust$`44 Ca`,"Detect","Non-Detect")
dust$`44 Ca`<- replace_na(dust$`44 Ca`, "Non-Detect")
dust$`44 Ca`<- as.factor(dust$`44 Ca`)
summary(dust$`44 Ca`)
#V====
dust$`51 V`<- as.numeric(dust$`51 V`)
dust$`51 V`= ifelse(dust$`51 V`,"Detect","Non-Detect")
dust$`51 V`<- replace_na(dust$`51 V`, "Non-Detect")
dust$`51 V`<- as.factor(dust$`51 V`)
summary(dust$`51 V`)
#Cr====
dust$`52 Cr`<- as.numeric(dust$`52 Cr`)
dust$`52 Cr`= ifelse(dust$`52 Cr`,"Detect","Non-Detect")
dust$`52 Cr`<- replace_na(dust$`52 Cr`, "Non-Detect")
dust$`52 Cr`<- as.factor(dust$`52 Cr`)
summary(dust$`52 Cr`)

#Mn====
dust$`55 Mn`<- as.numeric(dust$`55 Mn`)
dust$`55 Mn`= ifelse(dust$`55 Mn`,"Detect","Non-Detect")
dust$`55 Mn`<- replace_na(dust$`55 Mn`, "Non-Detect")
dust$`55 Mn`<- as.factor(dust$`55 Mn`)
summary(dust$`55 Mn`)

#Fe====
dust$`56 Fe`<- as.numeric(dust$`56 Fe`)
dust$`56 Fe`= ifelse(dust$`56 Fe`,"Detect","Non-Detect")
dust$`56 Fe`<- replace_na(dust$`56 Fe`, "Non-Detect")
dust$`56 Fe`<- as.factor(dust$`56 Fe`)
summary(dust$`56 Fe`)
#Co====
dust$`59 Co`<- as.numeric(dust$`59 Co`)
dust$`59 Co`= ifelse(dust$`59 Co`,"Detect","Non-Detect")
dust$`59 Co`<- replace_na(dust$`59 Co`, "Non-Detect")
dust$`59 Co`<- as.factor(dust$`59 Co`)
summary(dust$`59 Co`)
#Ni====
dust$`60 Ni`<- as.numeric(dust$`60 Ni`)
dust$`60 Ni`= ifelse(dust$`60 Ni`,"Detect","Non-Detect")
dust$`60 Ni`<- replace_na(dust$`60 Ni`, "Non-Detect")
dust$`60 Ni`<- as.factor(dust$`60 Ni`)
summary(dust$`60 Ni`)
#Cu====
dust$`63 Cu`<- as.numeric(dust$`63 Cu`)
dust$`63 Cu`= ifelse(dust$`63 Cu`,"Detect","Non-Detect")
dust$`63 Cu`<- replace_na(dust$`63 Cu`, "Non-Detect")
dust$`63 Cu`<- as.factor(dust$`63 Cu`)
summary(dust$`63 Cu`)
#Zn====
dust$`66 Zn`<- as.numeric(dust$`66 Zn`)
dust$`66 Zn`= ifelse(dust$`66 Zn`,"Detect","Non-Detect")
dust$`66 Zn`<- replace_na(dust$`66 Zn`, "Non-Detect")
dust$`66 Zn`<- as.factor(dust$`66 Zn`)
summary(dust$`66 Zn`)
#As====
dust$`75 As`<- as.numeric(dust$`75 As`)
dust$`75 As`= ifelse(dust$`75 As`,"Detect","Non-Detect")
dust$`75 As`<- replace_na(dust$`75 As`, "Non-Detect")
dust$`75 As`<- as.factor(dust$`75 As`)
summary(dust$`75 As`)
#Se====
dust$`78 Se`<- as.numeric(dust$`78 Se`)
dust$`78 Se`= ifelse(dust$`78 Se`,"Detect","Non-Detect")
dust$`78 Se`<- replace_na(dust$`75 As`, "Non-Detect")
dust$`78 Se`<- as.factor(dust$`78 Se`)
summary(dust$`78 Se`)
#Mo====
dust$`95 Mo`<- as.numeric(dust$`95 Mo`)
dust$`95 Mo`= ifelse(dust$`95 Mo`,"Detect","Non-Detect")
dust$`95 Mo`<- replace_na(dust$`95 Mo`, "Non-Detect")
dust$`95 Mo`<- as.factor(dust$`95 Mo`)
summary(dust$`95 Mo`)
#Ag====
dust$`107 Ag`<- as.numeric(dust$`107 Ag`)
dust$`107 Ag`= ifelse(dust$`107 Ag`,"Detect","Non-Detect")
dust$`107 Ag`<- replace_na(dust$`107 Ag`, "Non-Detect")
dust$`107 Ag`<- as.factor(dust$`107 Ag`)
summary(dust$`107 Ag`)
 
#Cd====
dust$`111 Cd`<- as.numeric(dust$`111 Cd`)
dust$`111 Cd`= ifelse(dust$`111 Cd`,"Detect","Non-Detect")
dust$`111 Cd`<- replace_na(dust$`111 Cd`, "Non-Detect")
dust$`111 Cd`<- as.factor(dust$`111 Cd`)
summary(dust$`111 Cd`)

#Sn====
dust$`118 Sn`<- as.numeric(dust$`118 Sn`)
dust$`118 Sn`= ifelse(dust$`118 Sn`,"Detect","Non-Detect")
dust$`118 Sn`<- replace_na(dust$`118 Sn`, "Non-Detect")
dust$`118 Sn`<- as.factor(dust$`118 Sn`)
summary(dust$`118 Sn`)

#Sb====
dust$`121 Sb`<- as.numeric(dust$`121 Sb`)
dust$`121 Sb`= ifelse(dust$`121 Sb`,"Detect","Non-Detect")
dust$`121 Sb`<- replace_na(dust$`121 Sb`, "Non-Detect")
dust$`121 Sb`<- as.factor(dust$`121 Sb`)
summary(dust$`121 Sb`)

#Ba====
dust$`137 Ba`<- as.numeric(dust$`137 Ba`)
dust$`137 Ba`= ifelse(dust$`137 Ba`,"Detect","Non-Detect")
dust$`137 Ba`<- replace_na(dust$`137 Ba`, "Non-Detect")
dust$`137 Ba`<- as.factor(dust$`137 Ba`)
summary(dust$`137 Ba`)

#Pb====
dust$`208 Pb`<- as.numeric(dust$`208 Pb`)
dust$`208 Pb`= ifelse(dust$`208 Pb`,"Detect","Non-Detect")
dust$`208 Pb`<- replace_na(dust$`208 Pb`, "Non-Detect")
dust$`208 Pb`<- as.factor(dust$`208 Pb`)
summary(dust$`208 Pb`)
