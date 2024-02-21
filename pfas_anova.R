#libraries====
library(tidyverse)
library(agricolae)
library(car)
library(readxl)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(lme4)
library(lmerTest)
library(performance)
library(effects)
library(ggeffects)
library(ggpubr)
library(emmeans)
library(multcomp)
library(patchwork)
library(readxl)
library(tidyverse)
library(readr)
library(stringr)
library(reshape2)
library(table1)
library("ggpubr")
library(readxl)
library(tidyverse)
library(ggplot2)
library(readxl)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(lme4)
library(lmerTest)
library(ggeffects)
library(performance)
library(effects)
library(ggpubr)
library(emmeans)
library(multcomp)
library(sparklyr)
library(tensorflow)
library(keras)
library(table1)
library(dplyr)
library(Hmisc)
library(correlation)
library(corrplot)
library(ggcorrplot)
require(MASS)
library(leaps)
library(heplots)
library(car)
library(ggplot2)
library(broom)


#set working directory =====
setwd("/Users/Gift/Library/CloudStorage/Box-Box/R21 Wildfire and Flash Floods - shared all/Results/Turner Lab Results/Combined datasets with MRA lab ID")
# Load the data====
longer <- read_xlsx("pfas_long.xlsx", sheet= "latlong", col_names = TRUE) 
long <- pivot_longer(longer, cols = c(6:28), names_to = "Parameter", values_to = "Concentration")
#long <- pivot_longer(long, cols = c(6:9), names_to = "Functional Group", values_to = "Total Concentration")


fire <- read_xlsx("pfas_long.xlsx", sheet= "full_pfas+other", col_names = TRUE) 


#test of significance====
#PFBS====
hist(longer$`Perfluorobutanesulfonic acid (PFBS)`)
kruskal.test(log(longer$`Perfluorobutanesulfonic acid (PFBS)`), longer$Type)
#PFBA====
hist(log(longer$`Perfluorobutanoic acid (PFBA)`))
kruskal.test(log(longer$`Perfluorobutanoic acid (PFBA)`), longer$Type)

#PFDA====
hist(log(longer$`Perfluorodecanoic acid (PFDA)`))
kruskal.test(log(longer$`Perfluorodecanoic acid (PFDA)`), longer$Type)

ggplot(longer, aes(x = Type, y = log(`Perfluorodecanoic acid (PFDA)`), fill = Type)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  labs(x = "Sample Type",
       y = "Log (Perfluorodecanoic acid (PFDA) Concentration (ug/kg))") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold")) +
  scale_fill_manual(values = c("#fc8d62", "#8da0cb")) 

dev.print(png, "pfdaplot.png", res=300, height=8, width=8, units="in")

#PFDoA====
hist(log(longer$`Perfluorododecanoic acid (PFDoA)`))
kruskal.test(longer$`Perfluorododecanoic acid (PFDoA)`, longer$Type)
ggplot(longer, aes(x = Type, y = log(`Perfluorodecanoic acid (PFDA)`), fill = Type)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  labs(x = "Sample Type",
       y = "Log (Perfluorododecanoic acid (PFDoA) Concentration (ug/kg))") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold")) +
  scale_fill_manual(values = c("#fc8d62", "#8da0cb")) 

dev.print(png, "pfdaplot.png", res=300, height=8, width=8, units="in")



#PFHpA====
hist(log(longer$`Perfluoroheptanoic acid (PFHpA)`))
kruskal.test(longer$`Perfluoroheptanoic acid (PFHpA)`, longer$Type)

#PFHxS====
hist(log(longer$`Perfluorohexanesulfonic acid (PFHxS)`))
kruskal.test(longer$`Perfluorohexanesulfonic acid (PFHxS)`, longer$Type)

#PFHxA====
hist(log(longer$`Perfluorohexanoic acid (PFHxA)`))
kruskal.test(longer$`Perfluorohexanoic acid (PFHxA)`, longer$Type)

#PFNA====
hist(log(longer$`Perfluorononanoic acid (PFNA)`))
kruskal.test(longer$`Perfluorononanoic acid (PFNA)`, longer$Type)

#FOSA====
hist(log(longer$`Perfluorooctanesulfonamide (FOSA)`
kruskal.test(longer$`Perfluorooctanesulfonamide (FOSA)`, longer$Type)

#PFOS====
hist(log(longer$`Perfluorooctanesulfonic acid (PFOS)`))
         kruskal.test(longer$`Perfluorooctanesulfonic acid (PFOS)`, longer$Type)
#PFOA====
hist(log(longer$`Perfluorooctanoic acid (PFOA)`))
         kruskal.test(longer$`Perfluorooctanoic acid (PFOA)`, longer$Type)
#PFPeS====
hist(log(longer$`Perfluoropentanesulfonic acid (PFPeS)`))
kruskal.test(longer$`Perfluoropentanesulfonic acid (PFPeS)`, longer$Type)   

#PFPuDA====
hist(log(longer$`Perfluoroundecanoic acid (PFUdA)`))
kruskal.test(longer$`Perfluoroundecanoic acid (PFUdA)`, longer$Type)   
         
         
hist(longer$`Total PFAS`)
hist(log(longer$`Total PFAS`))
hist(log(longer$As))
hist(log(longer$Pb))
hist(log(longer$Mn))
shapiro.test(log(longer$`Total PFAS`)) #the log transformed data is normally distributed

# Mining Metals======
#Sum of PFAS=====
metal <- lm(log(`Total PFAS`) ~ log(Zn)+ log(As)+ log(Cu)+ log(Pb)+log(Ba)+log(Co)+log(Mn)+Type, data = longer)
summary(metal)
anova(metal)
stepwise_model <- stepAIC(metal, direction = "both")
summary(stepwise_model)
performance(stepwise_model)
shapiro.test(resid(stepwise_model))


tidy_metal <- tidy(metal, conf.int = TRUE)
tidy_metal <- tidy_metal[!tidy_metal$term == "(Intercept)",]
ggplot(tidy_metal, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkred", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Relevant Factors in the Total PFAS Model") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkred", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()
dev.print(png, "forestplot_totalpfas.png", res=400, height=14, width=14, units="in")







#mixed_model <- lmer(log(SumPFAS) ~ log(Zn) + log(As) + log(Cu) + log(Pb) + log(Ba) + log(Co) + log(Mn) + Type + (1|Site), data = longer)

# Check for homoscedasticity
plot(stepwise_model, which = 3)
plot(allEffects(stepwise_model))

check_model(stepwise_model)
vif(stepwise_model)
anova(stepwise_model)
print(anova(stepwise_model))
print(summary(stepwise_model))
perf <- performance(stepwise_model)
# Assuming 'model' is your linear regression model
coef(stepwise_model)
perf
write.csv(perf, "sumPFAS_metal_regression.csv")





effects_plot <- allEffects(stepwise_model)
Pb <- effects_plot$`log(Pb)`
Pb<- as.data.frame(Pb)

# Plot the marginal effects using ggplot2
ggplot(Pb, aes(x =log(Pb) , y = fit)) +
  geom_line(color = "blue", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.5) +
  labs( x = "Log(Pb)",
       y = "Sum PFAS") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 14, face="bold"),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"))
dev.print(png, "pb_sumpfas.png", res=300, height=8, width=8, units="in")



# predict.dat.full <- ggeffect(model = stepwise_model,
#                           terms = c("Pb"),
#                           back.transform = F,
#                           type = "re")
# 
# ggplot(data = longer, aes(x = log(Pb), y = log(`Total PFAS`)))+
#   geom_point()+
#   geom_ribbon(data = predict.dat.full, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
#   geom_line(data = predict.dat.full, mapping = aes(x=x, y = predicted))+
#   labs(title = "",
#        y = "ln(Total PFAS) ug/kg\n",
#        x = "\n Ln (Pb) ug/kg")+
#   #facet_grid(Type ~ .) +  
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# dev.print(png, "PLI_TUdisteffectln.png", res=300, height=6, width=8, units="in")













Co <- effects_plot$`log(Co)`
Co<- as.data.frame(Co)

ggplot(Co, aes(x =log(Co) , y = fit)) +
  geom_line(color = "blue", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.5) +
  labs( x = "Log(Co)",
        y = "Sum PFAS") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"))
dev.print(png, "co_sumpfas.png", res=300, height=8, width=8, units="in")





Zn <- effects_plot$`log(Zn)`
Zn<- as.data.frame(Zn)

ggplot(Zn, aes(x =log(Zn) , y = fit)) +
  geom_line(color = "blue", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.5) +
  labs( x = "Log(Zn)",
        y = "Sum PFAS") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"))
dev.print(png, "zn_sumpfas.png", res=300, height=8, width=8, units="in")

Ba <- effects_plot$`log(Ba)`
Ba<- as.data.frame(Ba)

ggplot(Ba, aes(x =log(Ba) , y = fit)) +
  geom_line(color = "blue", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.5) +
  labs( x = "Log(Ba)",
        y = "Sum PFAS") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"))


ggplot(longer, aes(y = log(`Total PFAS`), x = log(Zn), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "Total PFAS Concentration (ln(ug/kg))\n",
    x = "Zn (ln(ug/kg))\n",
    title = "Relationship between Zn and Total PFAS Concentration",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 
dev.print(png, "zn_sumpfas.png", res=300, height=8, width=8, units="in")

ggplot(longer, aes(y = log(`Total PFAS`), x = log(Ba), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "Total PFAS Concentration (ln(ug/kg))\n",
    x = "Ba (ln(ug/kg))\n",
    title = "Relationship between Ba and Total PFAS Concentration",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text( size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 
dev.print(png, "ba_sumpfas.png", res=300, height=8, width=8, units="in")

ggplot(longer, aes(y = log(`Total PFAS`), x = log(Mn), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "Total PFAS Concentration (ln(ug/kg))\n",
    x = "Mn (ln(ug/kg))\n",
    title = "Relationship between Mn and Total PFAS Concentration",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 
dev.print(png, "mn_sumpfas.png", res=300, height=8, width=8, units="in")

 ggplot(longer, aes(y = log(`Total PFAS`),  x = log(Pb), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "Total PFAS Concentration (ln(ug/kg))\n",
    x = "Pb (ln(ug/kg))\n",
    title = "Relationship between Pb and Total PFAS Concentration",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 

dev.print(png, "pb_sumpfas.png", res=300, height=8, width=8, units="in")



#long chain======
metal <- lm(log(`Long Chain`) ~ log(Zn)+ log(As)+ log(Cu)+ log(pH)+ log(EC)+log(Pb)+log(Ba)+log(Co)+log(Mn)+Type, data = longer)
summary(metal)
anova(metal)
stepwise_model3 <- stepAIC(metal, direction = "both")
summary(stepwise_model3)

performance(stepwise_model3)
shapiro.test(resid(stepwise_model))

# Check for homoscedasticity
plot(stepwise_model, which = 3)
plot(allEffects(stepwise_model))

check_model(stepwise_model)
vif(stepwise_model)
anova(stepwise_model)
print(anova(stepwise_model))
print(summary(stepwise_model))
perf <- performance(stepwise_model)
# Assuming 'model' is your linear regression model
coef(stepwise_model)
perf
write.csv(perf, "longchain_metal_regression.csv")






ggplot(longer, aes(y = log(`Long Chain`), x = log(Pb), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[Long Chain PFAS] (ln(ug/kg))\n",
    x = "[Pb] (ln(ug/kg))\n",
    title = "Relationship between [Pb] and Long Chain PFAS",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 

ggplot(longer, aes(y = log(`Long Chain`), x = log(Co), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[Long Chain PFAS] (ln(ug/kg))\n",
    x = "[Co] (ln(mg/kg))\n",
    title = "Relationship between [Co] and Long Chain PFAS",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 



ggplot(longer, aes(y = log(`Long Chain`), x = log(Zn), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[Long Chain PFAS] (ln(ug/kg))\n",
    x = "[Zn] (ln(mg/kg))\n",
    title = "Relationship between [Zn] and Long Chain PFAS",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 
dev.print(png, "zn_longpfas.png", res=300, height=8, width=8, units="in")


ggplot(longer, aes(y = log(`long chain`), x = Type, color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[Long Chain PFAS] (ln(ug/kg))\n",
    x = "Type\n",
    title = "Relationship between [Type] and Long Chain PFAS",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 



#shortchain====
metal <- lm(log(`Short Chain`) ~ log(Zn)+ log(As)+ log(Cu)+ log(Pb)+log(Ba)+log(Co)+log(Mn)+Type, data = longer)
# Check the summary of the regression model
summary(metal)
anova(metal)
stepwise_model2 <- stepAIC(metal, direction = "both")
summary(stepwise_model2)

performance(stepwise_model2)
shapiro.test(resid(stepwise_model))

# Check for homoscedasticity
plot(stepwise_model, which = 3)
plot(allEffects(stepwise_model))

check_model(stepwise_model)
vif(stepwise_model)
anova(stepwise_model)
print(anova(stepwise_model))
print(summary(stepwise_model))
perf <- performance(stepwise_model)
perf
write.csv(perf, "shortchain_metal_regression.csv")


tidy_metal <- tidy(stepwise_model2, conf.int = TRUE)
tidy_metal <- tidy_metal[!tidy_metal$term == "(Intercept)",]
ggplot(tidy_metal, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 8, shape = 21) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkred") + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Relevant Factors in the Long Chain PFAS Model") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkred", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()
dev.print(png, "forestplot_longpfas.png", res=400, height=14, width=14, units="in")


ggplot(longer, aes(y = log(`Short Chain`), x = log(Ba), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[Short Chain PFAS] (ln(ug/kg))\n",
    x = "[Ba] (ln(mg/kg))\n",
    title = "Relationship between [Ba] and Short Chain PFAS",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 




#pfca=====
metal <- lm(log(PFCA) ~ log(Zn)+ log(As)+ log(Cu)+ log(Pb)+log(Ba)+log(Co)+log(Mn)+Type, data = longer)
# Check the summary of the regression model
summary(metal)
anova(metal)
stepwise_model <- stepAIC(metal, direction = "both")
summary(stepwise_model)

performance(stepwise_model)
shapiro.test(resid(stepwise_model))

# Check for homoscedasticity
plot(stepwise_model, which = 3)
plot(allEffects(stepwise_model))

check_model(stepwise_model)
vif(stepwise_model)
anova(stepwise_model)
print(anova(stepwise_model))
print(summary(stepwise_model))
perf <- performance(stepwise_model)
perf
write.csv(perf, "pfca_metal_regression.csv")


tidy_metal <- tidy(stepwise_model, conf.int = TRUE)
tidy_metal <- tidy_metal[!tidy_metal$term == "(Intercept)",]
ggplot(tidy_metal, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 8, shape = 21) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkred") + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Relevant Factors in the PFCA Model") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkred", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()
dev.print(png, "forestplot_pfca.png", res=400, height=14, width=14, units="in")


ggplot(longer, aes(y = log(pfca), x = log(Ba), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[PFCA] (ln(ug/kg))\n",
    x = "[Ba] (ln(mg/kg))\n",
    title = "Relationship between [Barium] and PFCA",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 


ggplot(longer, aes(y = log(pfca), x = log(Pb), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[PFCA] (ln(ug/kg))\n",
    x = "[Pb] (ln(mg/kg))\n",
    title = "Relationship between [Lead] and PFCA",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 

ggplot(longer, aes(y = log(pfca), x = log(Zn), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[PFCA] (ln(ug/kg))\n",
    x = "[Zn] (ln(mg/kg))\n",
    title = "Relationship between [Zinc] and PFCA",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 


#pfsa=====
metal <- lm(log(PFSA) ~ log(Zn)+ log(As)+ log(Cu)+ log(Pb)+log(Ba)+log(Co)+log(Mn)+Type, data = longer)
# Check the summary of the regression model
summary(metal)
anova(metal)
stepwise_model <- stepAIC(metal, direction = "both")
summary(stepwise_model)

performance(stepwise_model)
shapiro.test(resid(stepwise_model))

# Check for homoscedasticity
plot(stepwise_model, which = 3)
plot(allEffects(stepwise_model))

check_model(stepwise_model)
vif(stepwise_model)
anova(stepwise_model)
print(anova(stepwise_model))
print(summary(stepwise_model))
perf <- performance(stepwise_model)
perf
write.csv(perf, "pfca_metal_regression.csv")


ggplot(longer, aes(y = log(pfsa), x = log(Zn), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[PFSA] (ln(ug/kg))\n",
    x = "[Zn] (ln(mg/kg))\n",
    title = "Relationship between [Zinc] and PFSA",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 



#Perfluoroalkane sulfonamido substances=====
metal <- lm(log(`Perfluoroalkane Sulfonamido Substances`) ~ log(Zn)+ log(As)+ log(Cu)+ log(Pb)+log(Ba)+log(Co)+log(Mn)+Type, data = longer)
# Check the summary of the regression model
summary(metal)
anova(metal)
stepwise_model <- stepAIC(metal, direction = "both")
summary(stepwise_model)

performance(stepwise_model)
shapiro.test(resid(stepwise_model))

# Check for homoscedasticity
plot(stepwise_model, which = 3)
plot(allEffects(stepwise_model))

check_model(stepwise_model)
vif(stepwise_model)
anova(stepwise_model)
print(anova(stepwise_model))
print(summary(stepwise_model))
perf <- performance(stepwise_model)
perf
write.csv(perf, "pfca_metal_regression.csv")


ggplot(longer, aes(y = log(`Perfluoroalkane sulfonamido substances`), x = log(Pb), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[Perfluoroalkane sulfonamido substances] (ln(ug/kg))\n",
    x = "[Pb] (ln(mg/kg))\n",
    title = "Relationship between [lead] and Perfluoroalkane sulfonamido substances",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  )





#fts======
fts <- longer[longer$`Fluorotelomer Sulfonic Acids` != 0, ]

metal <- lm(log(`Fluorotelomer Sulfonic Acids`) ~ log(Zn)+ log(As)+ log(Cu)+ log(Pb)+log(Ba)+log(Co)+log(Mn), data = fts)
# Check the summary of the regression model
summary(metal)
anova(metal)
stepwise_model <- stepAIC(metal, direction = "both")
summary(stepwise_model)

performance(stepwise_model)
shapiro.test(resid(stepwise_model))

# Check for homoscedasticity
plot(stepwise_model, which = 3)
plot(allEffects(stepwise_model))

check_model(stepwise_model)
vif(stepwise_model)
anova(stepwise_model)
print(anova(stepwise_model))
print(summary(stepwise_model))
perf <- performance(stepwise_model)
perf
write.csv(perf, "fts_metal_regression.csv")



tidy_metal <- tidy(stepwise_model, conf.int = TRUE)
tidy_metal <- tidy_metal[!tidy_metal$term == "(Intercept)",]
ggplot(tidy_metal, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 8, shape = 21) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkred") + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Relevant Factors in the Long Chain PFAS Model") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkred", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()
dev.print(png, "forestplot_longpfas.png", res=400, height=14, width=14, units="in")
ggplot(fts, aes(y = log(`Fluorotelomer sulfonic acids`), x = log(Cu), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[Fluorotelomer sulfonic acids] (ln(ug/kg))\n",
    x = "[Cu] (ln(mg/kg))\n",
    title = "Relationship between Fluorotelomer sulfonic acids and Copper",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  )+
  geom_smooth(method=lm)

ggplot(fts, aes(y = log(`Fluorotelomer sulfonic acids`), x = log(Ba), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[Fluorotelomer sulfonic acids] (ln(ug/kg))\n",
    x = "[Ba] (ln(mg/kg))\n",
    title = "Relationship between Fluorotelomer sulfonic acids and Barium",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  )+
  geom_smooth(method=lm)






ggplot(fire, aes(y = log(pfsa), x = log(PFSA), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    y = "[PFSA] (ln(ug/kg))\n",
    x = "[Zn] (ln(mg/kg))\n",
    title = "Relationship between [Zinc] and PFSA",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) 














ggplot(fire, aes(y = log(SumPFAS), x = log(TOC), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    x = "[Total Organic Carbon] (ln(ug/kg))\n",
    y = "[SumPFAS] (ln(mg/kg))\n",
    title = "Relationship between TOC and Total PFAS",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank())



ggplot(longer, aes(y = log(SumPFAS), x = log(Sand), color = Type, shape= Type)) +
  geom_point(position = position_jitter(width = 0.2), size = 4, alpha = 0.5) +
  labs(
    x = "[Total Nitrogen] (ln(ug/kg))\n",
    y = "[SumPFAS] (ln(mg/kg))\n",
    title = "Relationship between Total Nitrogen and Total PFAS",
    fill = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Avenir", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(vjust = 0.5, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.line.y = element_blank(),
    axis.line.x = element_blank())+
  geom_smooth(method=lm)





















#correlation plots======

library(corrplot)
# Select variables of interest
variables_of_interest <- fire[c("Total PFAS", "Long Chain", "Short Chain", "PFCA", "PFSA", "pH", "EC", "Sand", "Silt","TN","TOC")]
# Log transformation
variables_of_interest <- log(variables_of_interest)
variables_of_interest <- na.omit(variables_of_interest)
cor_matrix <- cor(variables_of_interest, method = "spearman")
#p_matrix <- rcorr(as.matrix(variables_of_interest))$P

corrplot(cor_matrix, 
         method = "circle", 
         type = "upper", 
         diag = FALSE, 
         tl.cex = 1, 
         tl.col = "darkblue", 
         number.col = "red",
         number.cex = 1, 
         sig.level = 0.05, 
         insig = "blank",
         addCoef.col = "white",
         p.mat = p_matrix, 
         is.corr = FALSE)
dev.print(png, "correlation_plot4.png", res=400, height=14, width=14, units="in")

  






long$Location <- factor(long$Location, levels = unique(long$Location))
long$Concentration <- as.numeric(long$Concentration)



ggplot(long, aes(x = Site, y = Concentration, fill = Parameter, shape=Location)) +
  geom_col(position = "stack", width = 0.5) +
  coord_flip() +
  labs( x = "Site",
        y = "PFAS Concentration (ug/kg)") +
  theme_minimal()+
  theme(legend.position = "bottom",
axis.text = element_text(size = 14, face= "bold"),  # Increase font size and make bold for both axes
axis.title = element_text(size = 14, face= "bold"),
legend.text = element_text(size = 9),  
legend.title = element_text(size = 10, face = "bold")) 
dev.print(png, "individualconcentration.png", res=400, height=14, width=18, units="in")




ggplot(long, aes(x = Site, y = Concentration, fill = Parameter, shape = Location)) +
  geom_col(position = "stack", width = 0.5) +
  coord_flip() +
  labs(x = "Site", y = paste("PFAS Concentration", "(ug/kg)")) +  # Modified here
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 9),  
        legend.title = element_text(size = 10, face = "bold"))


 ggplot(long, aes(x = Site, y = Concentration/`Total PFAS`, fill = Parameter, shape = Location)) +
   geom_col(position = "fill", width = 0.5) +
   labs(x = "Site", y = "PFAS Contribution") +
   coord_flip() +
   theme_minimal() +
   theme(legend.position = "bottom",
 axis.text = element_text(size = 14, face= "bold"),  # Increase font size and make bold for both axes
 axis.title = element_text(size = 14, face= "bold"),
 legend.text = element_text(size = 12),  
 legend.title = element_text(size = 10, face = "bold")) 
 dev.print(png, "individualcongener.png", res=400, height=14, width=18, units="in")
 
 ggplot(long, aes(x = reorder(Site, -as.numeric(Location)), y =`Total Concentration` /`Total PFAS`, fill = `Functional Group`, shape = Location)) +
   geom_col(position = "fill", width = 0.5) +
   labs( x = "Site",
         y = "PFAS Functional Group") +
   coord_flip() +
   theme_minimal() +
   theme(legend.position = "bottom",
         axis.text = element_text(size = 14, face = "bold"),  
         axis.title = element_text(size = 14, face = "bold"),  
         legend.text = element_text(size = 12),  
         legend.title = element_text(size = 12, face = "bold")) 
 dev.print(png, "functionalgroup.png", res=400, height=14, width=14, units="in")
 
 ggplot(long, aes(x = reorder(Site, -as.numeric(Location)), y = `PFAS Concentration`/`Total PFAS`, fill = `Chain Length`, shape = Location)) +
   geom_col(position = "fill", width = 0.5) +
   labs( x = "Site",
         y = "PFAS Chain Length") +
   coord_flip() +
   theme_minimal() +
   theme(legend.position = "bottom",
         axis.text = element_text(size = 14, face = "bold"),  
         axis.title = element_text(size = 14, face = "bold"),  
         legend.text = element_text(size = 12),  
         legend.title = element_text(size = 12, face = "bold")) 
 dev.print(png, "length.png", res=400, height=14, width=14, units="in")
 
 

 #adding to github
 


 
 

 
 
 
