library(readxl)
library(tidyverse)
library(readr)
library(stringr)
library(reshape2)
library(table1)



#####----
#set working directory 
setwd("/Users/godsgiftnkechichukwuonye/Library/CloudStorage/Box-Box/R21 Wildfire and Flash Floods - shared all/Results/Turner Lab Results/Combined datasets with MRA lab ID")

#Detection_dust_PAH=====
pfas <- read_xlsx("Combined_PFAS_Data.xlsx", sheet= "copy-residential", col_names = TRUE) 
head(pfas)
pfas$Result<- as.numeric(pfas$Result)


pfas$detect = ifelse(pfas$Result>0,"Detect","Non-Detect")
pfas$detect<- replace_na(pfas$detect, "Non-Detect")
head(pfas)
table1(~ detect|Parameter+Laboratory_Qualifiers...6, 
       data=pfas,
       overall=F)
table1(~Laboratory_Qualifiers...6|Parameter, 
       data=pfas,
       overall=F)
