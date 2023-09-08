####-----
#load package
library(readxl)
library(tidyverse)
library(readr)



#####----
#set working directory 
setwd("/users/godsgiftnkechichukwuonye/Downloads")

#Loading data from excel sheet! Remember to install readxl
dioxin <- read_xlsx("Turner1.xlsx", col_names = TRUE) #corrected mean the corrected tab in the excel sheet
head(dioxin) #this function provides information on the first six lines in the dataset including column names
tail(dioxin) #this function provides information on the last six lines in the dataset including column names


dioxin_wider1<- pivot_wider(dioxin,  names_from = Compound, values_from = Result)

dioxin_wider<-dioxin %>% 
  pivot_wider(
    id_cols= `Sample ID`,
    names_from = Compound,
    values_from = Result)
write.csv(dioxin_wider,"Dioxin_clean.csv", row.names = TRUE)

pah<- read_xlsx("PAH_Clean.xlsx", col_names = TRUE)
pah_wider<-pah %>% 
  pivot_wider(
    id_cols= `MRA_Sample_ID`, 
    names_from = ANALYTE,
    values_from = RESULT)
write.csv(pah_wider,"PAH_clean.csv", row.names = TRUE)

