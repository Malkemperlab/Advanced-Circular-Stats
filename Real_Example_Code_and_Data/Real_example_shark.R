# Real example - shark data from Huveneers et al. 2015
# Submission to MEE

#load libraries
library(readxl)
library(circular)
library(CircMLE)
library(data.table)
library(lme4)
library(lmerTest)
library(buildmer)
library(tidyverse)
library(writexl)

#load data 
shark<-read_excel("shark.xlsx")

#load self written function 
source("Functions_Real_Examples.R")

#prepare data
approach<-rad(shark$Approach)
ID<-as.factor(shark$ID)
sun_dir<-rad(shark$Sunglare)
wind_dir<-rad(shark$Wind_direct2)
wind_amp<-as.numeric(shark$Wind_strength)
depth<-as.numeric(shark$Depth)
swell<-as.numeric(shark$Swell)

approach_rel_sun<-(sun_dir-approach)%%(2*pi)

#averaging for each ID 
df_angle<-data.frame(ID,approach_rel_sun) # direction relative to sun direction
df_averaged<-setDT(df_angle)[, mean.circular(c(approach_rel_sun)), by=ID]
df_averaged$V1<-df_averaged$V1%%(2*pi)

# Rayleigh test
rayleigh.test(df_averaged$V1)

# HR test 
set.seed(1)
HR_test(df_averaged$V1) # p = 0.04

#Model comparison 
res_model_comp<-circ_mle(df_averaged$V1,criterion = "AIC", nchains = 5, BadStart = 10^9,
         niter = 5000, method = "BFGS", lambda.min = 0.25, q.diff =pi/4)

res_model_comp[["bestmodel"]] #"M2B" best model "Symmetric modified unimodal" followed by M3B ("Symmetric bimodal", deltaAIC 0.209) and M2C ("Modified unimodal", deltaAIC 0.800)

#### LMM approach 
#prepare data 
variable<-rep(c("Cos","Sin"),times=c(length(approach),length(approach)))  
value<-c((cos(approach)),(sin(approach))) # sun Dir needs to be replaced with real response variable 

sun_dir2<-c(sun_dir,sun_dir)
wind_dir2<-c(wind_dir,wind_dir)
wind_amp2<-c(wind_amp,wind_amp)
depth2<-c(depth,depth)
swell2<-c(swell,swell)
ID2<-c(ID,ID)

# first, full model 
LMMM_shark<-summary(lmer(value ~ variable+(cos(sun_dir2)+sin(sun_dir2))+(cos(wind_dir2)+sin(wind_dir2))*wind_amp2+
                           depth2+swell2+(1|ID2)))

LMMM_shark # this shows the results of the full model 

# now model selection approach using buildmer 
f=value ~ variable+(cos(sun_dir2)+sin(sun_dir2))+(cos(wind_dir2)+sin(wind_dir2))*wind_amp2+depth2+swell2+(1|ID2)
m <- buildmer(f,buildmerControl=buildmerControl(direction='order',crit='AIC'))

sum_m<-summary(m)

# now prepare the table to save it as an Excel file 
df_sum_m<-as.data.frame(sum_m[["coefficients"]])
df_sum_m<-round(df_sum_m,3)
df_sum_m$Predictor<-rownames(df_sum_m)
df_sum_m <- df_sum_m[, c("Predictor", setdiff(names(df_sum_m), "Predictor"))]
colnames(df_sum_m)<-c("Predictor","Estimate", "SE","t","p")
df_sum_m$Predictor <- gsub("\\(Intercept\\)", "Intercept", df_sum_m$Predictor)
df_sum_m$p<-ifelse(df_sum_m$p<0.001,"<0.001", ifelse(is.na(df_sum_m$p), NA, round(df_sum_m$p, 3)))

# Write the summary table to an Excel file
write_xlsx(df_sum_m, "LMM_shark_table.xlsx")


#### MANOVA approach 
#first, the full model 
MANOVA_shark_full<-summary(manova(cbind(cos(approach),sin(approach)) ~ 
                                    (cos(sun_dir)+sin(sun_dir))+
                                    (cos(wind_dir)+sin(wind_dir))*wind_amp+
                                    depth+swell+Error(0+ID)))

#AIC based model selection (self written function)
model_selected<-manova_AIC_selection_randomerror(cbind(cos(approach),sin(approach)),MANOVA_shark_full)

model_selected[[2]] # this shows the best model 

#prepare the table to write the excel file 
Error_ID<-as.data.frame(model_selected[[2]][["Error: ID"]][["stats"]])
Error_Within<-as.data.frame(model_selected[[2]][["Error: Within"]][["stats"]])
Error_ID$Predictor<-rownames(Error_ID); Error_Within$Predictor<-rownames(Error_Within)
Error_ID <- Error_ID[, c("Predictor", setdiff(names(Error_ID), "Predictor"))]
Error_Within <- Error_Within[, c("Predictor", setdiff(names(Error_Within), "Predictor"))]
Error_ID$Predictor <- gsub("\\(Intercept\\)", "Intercept", Error_ID$Predictor)
vect_ID<-t(as.data.frame(c("Error: ID",NA,NA,NA,NA,NA,NA)))
vect_<-t(as.data.frame(c("Error: Within",NA,NA,NA,NA,NA,NA)))
colnames(vect_)<-colnames(Error_ID);colnames(vect_ID)<-colnames(Error_ID);rownames(vect_)<-"";rownames(vect_ID)<-""
MANOVA_shark_table<-rbind(vect_ID,Error_ID,vect_,Error_Within)
MANOVA_shark_table<-select(MANOVA_shark_table,-c("num Df","den Df","Pillai"))
colnames(MANOVA_shark_table)[3:4]<-c("approx. F", "p")
MANOVA_shark_table$`approx. F` <-as.numeric(MANOVA_shark_table$`approx. F`)
MANOVA_shark_table$`approx. F` <- ifelse(is.na(MANOVA_shark_table$`approx. F`), NA, round(MANOVA_shark_table$`approx. F`, 3))
MANOVA_shark_table$p <-as.numeric(MANOVA_shark_table$p)
MANOVA_shark_table$p<-ifelse(MANOVA_shark_table$p<0.001,"<0.001", ifelse(is.na(MANOVA_shark_table$p), NA, round(MANOVA_shark_table$p, 3)))

# Write the summary table to an Excel file
write_xlsx(MANOVA_shark_table, "MANOVA_shark_table.xlsx")



