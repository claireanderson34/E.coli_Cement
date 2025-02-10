####INTRODUCTION####
# Loading
library("readxl")
library("ggplot2")
library("dplyr")
library("effectsize")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(car)
library(msm)

#Files
Sdata <- read_excel("SurvivalDataExcel.xlsx", sheet = "Survival")
Sdata <- data.frame(Sdata)
Rdata <- read_excel("RecoveryDataExcel.xlsx", sheet = "Removal")
Rdata <- data.frame(Rdata)

#Standard deviation and confidence interval
i <- 1
for (i in seq(1, nrow(Sdata)-14, by = 1)) {
  mean_C <- mean(Sdata$LN.reduction[i],Sdata$LN.reduction[i+7],Sdata$LN.reduction[i+14])
  var_C <- var(x= c(Sdata$LN.reduction[i],Sdata$LN.reduction[i+7],Sdata$LN.reduction[i+14]))
  Sdata$SE[i]  <- deltamethod(~ log(x1 / 1), mean_C, var_C)
  i <- i + 1
}

i <- 1
for (i in seq(1, nrow(Sdata)-14, by = 1)) {
  Sdata$CILowerR[i] <- t.test(x=c(Sdata$LN.reduction[i],Sdata$LN.reduction[i+7],Sdata$LN.reduction[i+14]), conf.level=0.95)$conf.int[1]
  Sdata$CIUpperR[i] <- t.test(x=c(Sdata$LN.reduction[i],Sdata$LN.reduction[i+7],Sdata$LN.reduction[i+14]), conf.level=0.95)$conf.int[2]
  i <- i + 3
}


####PLOTS####
#Removal
soil <- c("No"="Soil Absent", "Yes"="Soil Present")
ggplot(data=Rdata, aes(x=as.factor(Removal.Activity), group = x))+ 
  geom_errorbar(aes(color=Mix.Design,ymin =X95.l*100, ymax = X95.u*100),
                width = .2,                    # Width of the error bars
                position = position_dodge(.6), alpha=0.7)+
  geom_point(aes(fill=Mix.Design, y=Percent.removal*100, shape=Mix.Design), position=position_dodge(.6), alpha=0.15)+
  geom_point(aes(fill=Mix.Design, shape=Mix.Design, y=Percent.removal.average*100), color="black",position=position_dodge(.6), alpha=1)+
  theme_bw() +
  xlab("Removal Activity") + ylab(expression(paste("Percent Removal of ",italic("E. coli")," (%)")))+
  labs(color = "Mix Design")+
  labs(shape = "Mix Design")+
  labs(fill = "Mix Design")+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.75, hjust=.5),
        strip.text.x = element_text(size = 10),
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 7),
        legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        legend.background = element_rect(color="white",linetype = "solid"))+ 
  facet_wrap(~ Presence.of.soil,labeller = as_labeller(soil))+
  scale_shape_manual(values=c(21, 24, 22),breaks=c("OPC Concrete","OPC Mortar","OPC Fly Ash"), labels=c("OPC Concrete","OPC Mortar","OPC Fly Ash"))+
  scale_color_manual(values=c("black", "dodgerblue1","chartreuse3"),breaks=c("OPC Concrete","OPC Mortar","OPC Fly Ash"), labels=c("OPC Concrete","OPC Mortar","OPC Fly Ash"))+
  scale_fill_manual(values=c("black", "dodgerblue1","chartreuse3"),breaks=c("OPC Concrete","OPC Mortar","OPC Fly Ash"), labels=c("OPC Concrete","OPC Mortar","OPC Fly Ash"))+
  scale_x_discrete(labels = c("Broom"="Sweeping", "Cotton"="Mopping", "Shoe" = "Walking"))
ggsave("PerRemoval.png",dpi=330, path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Claire School/Grad School/Lab/E. coli Concrete/", width = 5, height = 3, device='png')

#Survival
soil <- c("no"="Soil Absent", "yes"="Soil Present", "Low Temp" = "Dry Season\n(15°C, 75% RH)", "High Temp" = "Wet Season\n(34°C, 75% RH)")
#deltamethod
data <- Sdata
results <- lapply(1:nrow(data), function(i) {
  means <- c(data$N_avg[i], data$N0_Avg[i])
  cov_matrix <- matrix(c(data$N_STD[i]^2, 0, 0, data$N0_STD[i]^2), nrow = 2)
  deltamethod(~ log(x1) - log(x2), mean = means, cov = cov_matrix, ses=TRUE)})
standard_deviations <- sapply(results, function(v) v)
final_results <- data.frame(
  N_avg = data$N_avg,
  N0_Avg = data$N0_Avg,
  Standard_Deviation = standard_deviations)

Sdata$STD <- final_results$Standard_Deviation
Sdata$Lower <- Sdata$LN.reduction - (Sdata$STD*2)
Sdata$Upper <- Sdata$LN.reduction + (Sdata$STD*2)
Sdata$Mix.Design <- factor(Sdata$Mix.Design, levels = c("Concrete Mix", "Mortar Mix", "Fly Ash Mix"))
ggplot(data=Sdata, aes(x=(Time..min.)/60, group = Mix.Design))+ 
  geom_errorbar(aes(color=Mix.Design,ymin =Lower, ymax = Upper),
                width = .2, position = position_dodge(width=.3), alpha =0.6)+
  geom_point(aes(fill=Mix.Design, shape=Mix.Design, y=LN.reduction), color="black",position=position_dodge(.3), alpha=.6)+
  theme_bw() +
  xlab("Time (hours)") + ylab(expression("ln(C"[t]*" /C"[0]*")"))+
  labs(color = "Mix Design")+
  labs(shape = "Mix Design")+
  labs(fill = "Mix Design")+
  theme(strip.background =element_rect(fill="gray95"),
        strip.text.x = element_text(size = 9),
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 7),
        legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        legend.background = element_rect(color="white",linetype = "solid"))+ 
  facet_wrap(~ Presence.of.soil + Environmental.condition,labeller = as_labeller(soil))+
  scale_shape_manual(values=c(21, 24, 22),breaks=c("Concrete Mix","Mortar Mix","Fly Ash Mix"), labels=c("OPC Concrete","OPC Mortar","OPC Fly Ash"))+
  scale_color_manual(values=c("black", "dodgerblue1","chartreuse3"),breaks=c("Concrete Mix","Mortar Mix","Fly Ash Mix"), labels=c("OPC Concrete","OPC Mortar","OPC Fly Ash"))+
  scale_fill_manual(values=c("black", "dodgerblue1","chartreuse3"),breaks=c("Concrete Mix","Mortar Mix","Fly Ash Mix"), labels=c("OPC Concrete","OPC Mortar","OPC Fly Ash"))
ggsave("Survival.png",dpi=330, path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Claire School/Grad School/Lab/E. coli Concrete/", width = 9, height = 6, device='png')


####STATISTICS####
#Removal ANOVA
  #ANOVA
  res.aov <- aov(formula= Log.Reduction ~ Mix.Design+Removal.Activity*Presence.of.soil, data=Rdata)
  summary(res.aov)
  #ANOVA Assumptions check
  #Homogeneity of residuals
  Rdata %>%
    group_by(Mix.Design, Removal.Activity, Presence.of.soil) %>%
    shapiro_test(Log.Reduction)
  aov_residuals <- residuals(object = res.aov)
  shapiro.test(x = aov_residuals )
  #Homogeneity of variances
  leveneTest(Log.Reduction ~ Mix.Design*Removal.Activity*Presence.of.soil, data = Rdata)
#Removal PostHoc
tukey.n.way<-TukeyHSD(x=res.aov, conf.level=0.95)
tukey.n.way

#Removal stats
moppingMean <- subset(Rdata, subset = Removal.Activity=="Cotton")
moppingMean <- mean(moppingMean$Percent.removal.average, na.rm=TRUE)
sweepingMean <- subset(Rdata, subset = Removal.Activity=="Broom")
sweepingMean <- mean(sweepingMean$Percent.removal.average, na.rm=TRUE)
walkingMean <- subset(Rdata, subset = Removal.Activity=="Shoe")
walkingMean <- mean(walkingMean$Percent.removal.average, na.rm=TRUE)
(walkingMean-moppingMean)*100
(walkingMean-sweepingMean)*100
(sweepingMean-moppingMean)*100

sweepingsoilMean <- subset(Rdata, subset = Presence.of.soil=="Yes" & Removal.Activity=="Broom")
sweepingsoilMean <- mean(sweepingsoilMean$Percent.removal.average, na.rm=TRUE)
sweepingnosoilMean <- subset(Rdata, subset = Presence.of.soil=="No" & Removal.Activity=="Broom")
sweepingnosoilMean <- mean(sweepingnosoilMean$Percent.removal.average, na.rm=TRUE)
(sweepingsoilMean-sweepingnosoilMean)*100

walkingsoilMean <- subset(Rdata, subset = Presence.of.soil=="Yes" & Removal.Activity=="Shoe")
walkingsoilMean <- mean(walkingsoilMean$Percent.removal.average, na.rm=TRUE)
walkingnosoilMean <- subset(Rdata, subset = Presence.of.soil=="No" & Removal.Activity=="Shoe")
walkingnosoilMean <- mean(walkingnosoilMean$Percent.removal.average, na.rm=TRUE)
(walkingsoilMean-walkingnosoilMean)*100

moppingsoilMean <- subset(Rdata, subset = Presence.of.soil=="Yes" & Removal.Activity=="Cotton")
moppingsoilMean <- mean(moppingsoilMean$Percent.removal.average, na.rm=TRUE)
moppingnosoilMean <- subset(Rdata, subset = Presence.of.soil=="No" & Removal.Activity=="Cotton")
moppingnosoilMean <- mean(moppingnosoilMean$Percent.removal.average, na.rm=TRUE)
(moppingsoilMean-moppingnosoilMean)*100


#Survival k values for ANOVA
fitted_models <- Sdata %>% group_by(Mix.Design,Environmental.condition, Presence.of.soil, Trial..) %>% do(mod1 = (lm(LN.reduction ~ Time..min., data = .))) %>% ungroup()
model_summary <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                glance = map(mod1, broom::glance),
                                augment = map(mod1, broom::augment),
                                rsq = glance %>% map_dbl('r.squared'),
                                slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 

fitted_models1 <- Sdata %>% group_by(Mix.Design,Environmental.condition, Presence.of.soil, Trial..) %>% do(mod1 = glm(LN.reduction ~ Time..min., data = ., family = gaussian(link = "identity"))) %>% ungroup()
model_summary1 <-fitted_models1 %>% mutate(tidy = map(mod1, broom::tidy),
                                         glance = map(mod1, broom::glance),
                                         slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                         int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                         slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                         intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 


#Survival k values for table
fitted_models <- Sdata %>% group_by(Mix.Design,Environmental.condition, Presence.of.soil) %>% do(mod1 = (lm(LN.reduction ~ Time..min., data = .))) %>% ungroup()
model_summary1 <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                         glance = map(mod1, broom::glance),
                                         augment = map(mod1, broom::augment),
                                         rsq = glance %>% map_dbl('r.squared'),
                                         slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                         int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                         slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                         intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summary1$mod1 <- 0
model_summary1$tidy <- 0
model_summary1$glance <- 0
model_summary1$augment <- 0
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
write.excel(model_summary1)

#Survival ANOVA for slope
  #ANOVA
  res.aov <- aov(formula= slope ~ Mix.Design+Environmental.condition+Presence.of.soil, data=model_summary)
  summary(res.aov)
  #ANOVA Assumptions check
    #Homogeneity of residuals
    model_summary %>%
      group_by(Mix.Design, Environmental.condition, Presence.of.soil) %>%
      shapiro_test(slope)
    aov_residuals <- residuals(object = res.aov)
    shapiro.test(x = aov_residuals)
    #Homogeneity of variances
    leveneTest(slope ~ Mix.Design*Environmental.condition*Presence.of.soil, data = model_summary)
#Survival PostHoc
tukey.n.way<-TukeyHSD(x=res.aov, conf.level=0.95)
tukey.n.way

#Survival stats
lowTempMean <- subset(model_summary, subset = Environmental.condition=="Low Temp")
lowTempMean <- mean(lowTempMean$slope)
highTempMean <- subset(model_summary, subset = Environmental.condition=="High Temp")
highTempMean <- mean(highTempMean$slope)
highTempMean-lowTempMean

soilMean <- subset(model_summary, subset = Presence.of.soil=="yes")
soilMean <- mean(soilMean$slope)
nosoilMean <- subset(model_summary, subset = Presence.of.soil=="no")
nosoilMean <- mean(nosoilMean$slope)
soilMean-nosoilMean

