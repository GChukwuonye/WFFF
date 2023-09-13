library(readxl)
library(tidyverse)
library(readr)
library(stringr)
library(reshape2)
library(table1)

#####----
#set working directory 
setwd("/Users/Gift/Library/CloudStorage/Box-Box/R21 Wildfire and Flash Floods - shared all/Results/Turner Lab Results/Combined datasets with MRA lab ID")

#Detection_dust_PAFAS=====
pfas <- read_xlsx("Combined_PFAS_Data.xlsx", sheet= "copy-residential", col_names = TRUE) 
head(pfas)
pfas$Result<- as.numeric(pfas$Result)
#pfas <- pfas[-812,]

#pfas<- pfas[pfas$Parameter != "11Cl-PF3OUdS", ] 
#pfas<- pfas[pfas$Parameter != "4:2 Fluorotelomer sulfonate (4:2 FTS)", ] 
pfas<- pfas[pfas$Parameter != "6:2 Fluorotelomer sulfonate (6:2 FTS)", ] 
pfas<- pfas[pfas$Parameter != "6:2 FTS", ] 
#pfas<- pfas[pfas$Parameter != "4:2 FTS", ] 
pfas<- pfas[pfas$Parameter != "9Cl-PF3ONS", ] 
pfas<- pfas[pfas$Parameter != "ADONA", ] 
#pfas<- pfas[pfas$Parameter != "HFPO-DA", ] 

pfas$detect = ifelse(pfas$Result>0,"Detect","Non-Detect")
pfas$detect<- replace_na(pfas$detect, "Non-Detect")
head(pfas)
pfas<- pfas[pfas$Unit != "ng/L", ] 
pfas$Result<- replace(pfas$Result, is.na(pfas$Result), 0)  
pfas$Result<- replace_na(pfas$Result, 0)
pfas$Result<- as.numeric(pfas$Result)

table1(~Laboratory_Qualifiers|Parameter, 
       data=pfas,
       overall=F)

#pfas<- na.omit(pfas)
pfas$corrected<- replace_na(pfas$Result)

pfas$corrected <- ifelse(pfas$Result <=(0.00000),
                        formatC(signif(((as.numeric(pfas$Detection_Limit)/2)),digits=2), digits=2,format="fg", flag="#"),
                        formatC(signif((pfas$corrected),digits=4), digits=4,format="fg", flag="#"))




pfas_short <- pfas[c(3, 5, 22)]

#is.na(pfas_short)

pfas_wide<- pivot_wider(
  pfas_short,
  values_from = "corrected",
  names_from = "Parameter")


write_csv(pfas_wide, "pfas_final_copy.csv")


#control=====

#pfas_control=====
control <- read_xlsx("Combined_PFAS_Data.xlsx", sheet= "copy-Control", col_names = TRUE) 
head(control)
control$Result<- as.numeric(control$Result)


control$detect = ifelse(control$Result>0,"Detect","Non-Detect")
control$detect<- replace_na(control$detect, "Non-Detect")
head(control)
control<- control[control$Unit != "ng/L", ] 
table1(~ detect|Parameter, 
       data=control,
       overall=F)


control<- control[control$Parameter != "11Cl-PF3OUdS", ] 
control<- control[control$Parameter != "4:2 Fluorotelomer sulfonate (4:2 FTS)", ] 
control<- control[control$Parameter != "6:2 Fluorotelomer sulfonate (6:2 FTS)", ] 
control<- control[control$Parameter != "8:2 Fluorotelomer sulfonate (8:2 FTS)", ] 
control<- control[control$Parameter != "9Cl-PF3ONS", ] 
control<- control[control$Parameter != "ADONA", ] 
control<- control[control$Parameter != "HFPO-DA", ] 
control<- control[control$Parameter != "N-ethylperfluorooctanesulfonamidoacetic acid (NEtFOSAA)", ] 
control<- control[control$Parameter != "N-methylperfluorooctanesulfonamidoacetic acid (NMeFOSAA)", ] 
control<- control[control$Parameter != "Perfluoro-1-heptanesulfonate (PFHpS)", ] 
control<- control[control$Parameter != "Perfluorodecane Sulfonate (PFDS)", ] 
control<- control[control$Parameter != "Perfluoropentanoic acid (PFPeA)", ] 
control<- control[control$Parameter != "Perfluorotetradecanoic acid (PFTeDA)", ] 
control<- control[control$Parameter != "Perfluorotridecanoic acid (PFTrDA)", ] 
control<- control[control$Parameter != "PFNS", ] 


control$Result<- replace(control$Result, is.na(control$Result), 0)  
control$corrected<- replace_na(control$Result)

control$corrected <- ifelse(control$Result <=(0.00000),
                         formatC(signif(((as.numeric(control$Detection_Limit)/2)),digits=2), digits=2,format="fg", flag="#"),
                         formatC(signif((control$corrected),digits=4), digits=4,format="fg", flag="#"))


control_short <- control[c(3, 5, 23)]

control_wide<- datawizard::data_to_wide(
  control_short,
  id_cols = NULL,
  values_from = "corrected",
  names_from = "Parameter",
  values_drop_na = TRUE)

write_csv(control_wide, "pfas_control_cleaned.csv")





#short chain======
pfas_short <- pfas_main[c(3, 5:6)]

pfas_short$Result<- replace(pfas_short$Result, is.na(pfas_short$Result), 0)  

pfas_short$Result<- as.numeric(pfas_short$Result)

pfas_wide <- pfas_short %>% pivot_wider( names_from= "Parameter",
                                        values_from= "Result",
                                        names_sep = ".",
                                        values_fn = list)
                        
pfas_short_chain <- pfas_wide[c(1, 3, 4, 7,8,12,11,16,18, 25:26)]

pfas_short_chain<- replace(pfas_short_chain, is.na(pfas_short_chain), 0)


write_csv(pfas_short_chain, "pfas_short_chains_real.csv")


pfas_long_chain <- pfas_wide[c(1, 2, 5, 6,9, 10, 17, 14, 13, 15, 17, 19:24, 27:29)]
pfas_long_chain<- replace(pfas_long_chain, is.na(pfas_long_chain), 0) 

pfas_long_chain<- replace(pfas_long_chain, is.null(pfas_long_chain), 0) 

 write_csv(pfas_long_chain, "pfas_long_chains.csv")

#LOD

pfas_main$Detection_Limit<- as.numeric(pfas_main$Detection_Limit)
table1(~Detection_Limit|Parameter, 
       data=pfas_main,
       overall=F)
summary(pfas_long_chain)

summary(pfas_short_chain)



#Detection_dust_PAH=====
pfas <- read_xlsx("Combined_PFAS_Data.xlsx", sheet= "copy-residential", col_names = TRUE) 
head(pfas)
pfas$Result<- as.numeric(pfas$Result)


pfas$detect = ifelse(pfas$Result>0,"Detect","Non-Detect")
pfas$detect<- replace_na(pfas$detect, "Non-Detect")
head(pfas)
table1(~ detect|Parameter, 
       data=pfas,
       overall=F)

table1(~Laboratory_Qualifiers|Parameter, 
       data=pfas,
       overall=F)
pfas_main<- pfas[pfas$Unit != "ng/L", ] 


table1(~Laboratory_Qualifiers|Parameter, 
       data=pfas_main,
       overall=F)

table1(~ detect|Parameter, 
       data=pfas_main,
       overall=F)


pfas_short <- pfas_main[c(3, 5:6)]

pfas_wide<- datawizard::data_to_wide(
  pfas_main,
  id_cols = NULL,
  values_from = "Result",
  names_from = "Parameter",
  values_drop_na = TRUE)

pfas_wide <- pfas_short %>% pivot_wider( names_from= "Parameter",
                                        values_from= "Result",
                                        names_sep = ".",
                                        values_fn = list)
                        
pfas_short_chain <- pfas_wide[c(1, 3, 4, 7,8,12,11,16,18, 25:26)


pfas_short_chain<- replace(pfas_short_chain, is.na(pfas_short_chain), 0)
pfas_main$Result- replace(pfas_main$Result, is.na(pfas_main$Result), 0)  

write_csv(pfas_short_chain, "pfas_short_chain.csv")


pfas_long_chain <- pfas_wide[c(1, 2, 5, 6,9, 10, 17, 14, 13, 15, 17, 19:24, 27:29)]
pfas_long_chain<- replace(pfas_long_chain, is.na(pfas_long_chain), 0)  
write_csv(pfas_long_chain, "pfas_long_chain.csv")

#LOD

pfas_main$Detection_Limit<- as.numeric(pfas_main$Detection_Limit)
table1(~Detection_Limit|Parameter, 
       data=pfas_main,
       overall=F)
summary(pfas_long_chain)

summary(pfas_short_chain)
summary(pfas_wide)
table1(~Result|Parameter, 
       data=pfas_main,
       overall=F)

exp(mean(log(pfas_long_chain$`11Cl-PF3OUdS`)))
exp(mean(log(pfas_long_chain$`4:2 Fluorotelomer sulfonate (4:2 FTS)`)))
exp(mean(log(pfas_long_chain$`6:2 Fluorotelomer sulfonate (6:2 FTS)`)))
exp(mean(log(pfas_long_chain$`8:2 Fluorotelomer sulfonate (8:2 FTS)`)))
exp(mean(log(pfas_long_chain$`9Cl-PF3ONS`)))
exp(mean(log(pfas_long_chain$ADONA)))
exp(mean(log(pfas_long_chain$`N-ethylperfluorooctanesulfonamidoacetic acid (NEtFOSAA)`)))
exp(mean(log(pfas_long_chain$`N-methylperfluorooctanesulfonamidoacetic acid (NMeFOSAA)`)))
exp(mean(log(pfas_long_chain$`Perfluorodecanoic acid (PFDA)`)))
exp(mean(log(pfas_long_chain$`Perfluorodecanesulfonic acid (PFDS)`)))
exp(mean(log(pfas_long_chain$`Perfluorododecanoic acid (PFDoA)`)))
exp(mean(log(pfas_long_chain$`Perfluoroheptanoic acid (PFHpA)`)))
exp(mean(log(pfas_long_chain$`Perfluoro-1-heptanesulfonate (PFHpS)`)))

#pfas_control=====
control <- read_xlsx("Combined_PFAS_Data.xlsx", sheet= "copy-Control", col_names = TRUE) 
head(control)
control$Result<- as.numeric(control$Result)


control$detect = ifelse(control$Result>0,"Detect","Non-Detect")
control$detect<- replace_na(control$detect, "Non-Detect")
head(control)
table1(~ detect|Parameter, 
       data=control,
       overall=F)

table1(~Laboratory_Qualifiers|Parameter, 
       data=control,
       overall=F)
control_main<- control[control$Unit != "ng/L", ] 


table1(~Laboratory_Qualifiers|Parameter, 
       data=control_main,
       overall=F)


control_short <- control_main[c(3, 5, 7)]

control_wide<- datawizard::data_to_wide(
  control_short,
  id_cols = NULL,
  values_from = "Result",
  names_from = "Parameter",
  values_drop_na = TRUE)

control_wide <- control_short %>% pivot_wider( names_from= "Parameter",
                                         values_from= "Result",
                                         names_sep = ".")


control_short_chain <- control_wide[c(1, 3, 4, 7,8,12,11,16,18, 25:26)]

control_short_chain<- replace(control_short_chain, is.na(control_short_chain), 0)

write_csv(control_short_chain, "pfas_control_short_chains.csv")


control_long_chain <- control_wide[c(1, 2, 5, 6,9, 10, 17, 14, 13, 15, 17, 19:24, 27:29)]
control_long_chain<- replace(control_long_chain, is.na(control_long_chain), 0)  
write_csv(control_long_chain, "pfas_long_chains.csv")


#LOD

control_main$Detection_Limit<- as.numeric(control_main$Detection_Limit)
table1(~Detection_Limit|Parameter, 
       data=control_main,
       overall=F)
table1(~Detection_Limit|Parameter, 
       data=pfas_main,
       overall=F)

summary(control_wide)

summary(control_short_chain)



