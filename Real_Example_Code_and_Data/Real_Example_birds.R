# Real example - bird data from Wiltschko 2016 
# Submission to MEE

#load libraries 
library(readxl)
library(data.table)
library(permute) # is needed for G_test function

library(lme4)
library(lmerTest)
library(tidyverse)
library(circular)
library(writexl)

#load self written function 
source("Functions_Real_Examples.R")

#load data - reduced data set from Wiltschko et al. 2016 mag flicker exp. 
Wraw_reduced<-read_excel("Wraw_reduced.xlsx")

#apply the different tests 

#for the traditional tests the IDs need to be avergaed 

Wraw_reduced_averaged<-setDT(Wraw_reduced)[, mean.circular(c(dir)), by=.(Bird, Treatment)]
Wraw_reduced_averaged$V1<-Wraw_reduced_averaged$V1%%(2*pi)

#Need to restructure to apply Watson function  
W_m<-Wraw_reduced_averaged
trt1="P10_90_2011"
trt2="G"
C<-W_m[W_m$Treatment==trt1,V1]
E<-W_m[W_m$Treatment==trt2,V1]

watson_<-watson.two.test(C,E,alpha=0.05) #Reject Null Hypothesis 
watson_$statistic # p<0.05 becasue critical value: 0.187 

#G-test 
set.seed(1)
G_<-G_test(C,E)

#MANOVA

#############
####the basic MANOVA model is as follows 
MANOVA_<-summary(manova(cbind(cos(dir),sin(dir)) ~Treatment*Exp_Sequence+Error(0+Bird), 
                        data=Wraw_reduced),intercept=T)


MANOVA_ # running this shows the model output - however, need to restructure to save to Excel file 

#now we had to restructure the output for writing to Excel file 
Error_ID<-as.data.frame(MANOVA_[["Error: Bird"]][["stats"]]);Error_Within<-as.data.frame(MANOVA_[["Error: Within"]][["stats"]])
Error_ID$Predictor<-rownames(Error_ID);Error_Within$Predictor<-rownames(Error_Within)
Error_ID <- Error_ID[, c("Predictor", setdiff(names(Error_ID), "Predictor"))];Error_Within <- Error_Within[, c("Predictor", setdiff(names(Error_Within), "Predictor"))]
Error_ID$Predictor <- gsub("\\(Intercept\\)", "Intercept", Error_ID$Predictor)
vect_ID<-t(as.data.frame(c("Error: ID",NA,NA,NA,NA,NA,NA)));vect_<-t(as.data.frame(c("Error: Within",NA,NA,NA,NA,NA,NA)))
colnames(vect_)<-colnames(Error_ID);colnames(vect_ID)<-colnames(Error_ID);rownames(vect_)<-"";rownames(vect_ID)<-""
MANOVA_bird_table<-rbind(vect_ID,Error_ID,vect_,Error_Within);MANOVA_bird_table<-select(MANOVA_bird_table,-c("num Df","den Df","Pillai"))
colnames(MANOVA_bird_table)[3:4]<-c("approx. F", "p")
MANOVA_bird_table$`approx. F` <-as.numeric(MANOVA_bird_table$`approx. F`)
MANOVA_bird_table$`approx. F` <- ifelse(is.na(MANOVA_bird_table$`approx. F`), NA, round(MANOVA_bird_table$`approx. F`, 3))
MANOVA_bird_table$p <-as.numeric(MANOVA_bird_table$p)
MANOVA_bird_table$p<-ifelse(MANOVA_bird_table$p<0.001,"<0.001", ifelse(is.na(MANOVA_bird_table$p), NA, round(MANOVA_bird_table$p, 3)))

# Write the summary table to an Excel file
write_xlsx(MANOVA_bird_table, "MANOVA_Wiltschko_table.xlsx")


#############
# the linear mixed effects model 

##first we have to restructure the data 
variable<-rep(c("Cos","Sin"),times=c(length(Wraw_reduced$Bird),length(Wraw_reduced$Bird)))  
dir_<-c(cos(Wraw_reduced$dir),sin(Wraw_reduced$dir))
Trt_2<-c(Wraw_reduced$Treatment,Wraw_reduced$Treatment)
Exp_seq<-c(Wraw_reduced$Exp_Sequence,Wraw_reduced$Exp_Sequence)
Bird_<-c(Wraw_reduced$Bird,Wraw_reduced$Bird)

df__<-data.frame(variable,dir_,Trt_2,Exp_seq,Bird_) #new data frame for lm

####the basic LM model is as follows 
Lmer_res<- lmer(dir_ ~ variable+Trt_2*Exp_seq+ (1|Bird_),data = df__ )
Lmer_res_sum<- summary(Lmer_res)

Lmer_res_sum # runiung this shows lm results


#now prepare results to be stored as an excel file 
df_sum_m<-as.data.frame(Lmer_res_sum[["coefficients"]]); df_sum_m<-round(df_sum_m,3)
df_sum_m<-df_sum_m[,-3];df_sum_m$Predictor<-rownames(df_sum_m)
df_sum_m <- df_sum_m[, c("Predictor", setdiff(names(df_sum_m), "Predictor"))]
colnames(df_sum_m)<-c("Predictor","Estimate", "SE","t","p")
df_sum_m$Predictor <- gsub("\\(Intercept\\)", "Intercept", df_sum_m$Predictor)
df_sum_m$p<-ifelse(df_sum_m$p<0.001,"<0.001", ifelse(is.na(df_sum_m$p), NA, round(df_sum_m$p, 3)))

# Write the summary table to an Excel file
write_xlsx(df_sum_m, "LMM_Wiltschko_table.xlsx")

########
###quick power analysis for bird data 
########

#set up loop 
rans=1000
res_power=list()
nrow_length<-length(levels(as.factor(Wraw_reduced$Bird)))/2

for (e in 1:rans){
  
  df_res <- data.frame(matrix(NA, nrow = nrow_length-2, ncol = 4))
  
  # set column names
  colnames(df_res) <- c("Watson","G-test","Manova","LMM")
  
  for (i in 3:nrow_length){
    
    ###print counter 
    print(e)
    print(i)
    
    ####randomly select individuals from experiments 
    sampled_birds <- Wraw_reduced %>%
      group_by(Treatment) %>%
      distinct(Bird) %>%
      sample_n(i)
    
    random_subset <- Wraw_reduced %>% semi_join(sampled_birds, by = c( "Bird"))
    
    j=i-2 #get index 
    
    random_subset_averaged<-setDT(random_subset)[, mean.circular(c(dir)), by=.(Bird, Treatment)]
    random_subset_averaged$V1<-random_subset_averaged$V1%%(2*pi)
    
    #add watson U2 for the group difference 
    W_m<-random_subset_averaged
    C<-W_m[random_subset_averaged$Treatment==trt2,V1]
    E<-W_m[random_subset_averaged$Treatment==trt1,V1]
    
    watson_<-watson.two.test(C,E)
    df_res[j,1]<-ifelse(watson_$statistic<0.187,0,1) # critical value: 0.187 
    
    #G-test 
    df_res[j,2]<-ifelse(G_test(C,E)>0.05,0,1)
    
    # Use tryCatch to handle errors
    MANOVA_ <- tryCatch({
      summary(manova(cbind(cos(rad(dir)), sin(rad(dir))) ~ 
                       Treatment * Exp_Sequence + Error(0 + Bird), 
                     data = random_subset), intercept = TRUE)
      # Return the result if no error occurs
    }, error = function(e) {
      0  # Return 0 if an error occurs
    })
    
    # Extract the p-value and handle the case where it is logical(0)
    p_value <- ifelse(is.list(MANOVA_), MANOVA_[["Error: Bird"]][["stats"]][2,6],ifelse(MANOVA_==0,0,NA))
    
    # Check if p_value is logical(0) and set it to 0 if true
    if (length(p_value) == 0) {
      df_res[j,3]  <- 0
    } else {
      df_res[j,3]  <- ifelse(p_value < 0.05, 1, 0)
    }
    
    #using the LM function
    variable<-rep(c("Cos","Sin"),times=c(length(random_subset$Bird),length(random_subset$Bird)))  
    dir_<-c(cos(random_subset$dir),sin(random_subset$dir))
    Trt_2<-c(random_subset$Treatment,random_subset$Treatment)
    Exp_seq<-c(random_subset$Exp_Sequence,random_subset$Exp_Sequence)
    Bird_<-c(random_subset$Bird,random_subset$Bird)
    
    df__<-data.frame(variable,dir_,Trt_2,Exp_seq,Bird_)
    
    Lmer_res<- summary(lmer(dir_ ~ variable+Trt_2*Exp_seq+ (1|Bird_),data = df__ ))
    
    df_res[j,4] <- ifelse(unlist(coef(summary(Lmer_res))[, "Pr(>|t|)"])[3] < 0.05, 1, 0)
    
  }#i
  
  # Append the data frame to the list
  res_power[[length(res_power) + 1]] <- df_res
  
}#e

Power_df <- Reduce(`+`, res_power)/rans
saveRDS(Power_df,file=paste0("res_Wilschko_Power_analysis","rans",rans,".RDS")) #save results of power analysis, to be able to load it withour rerunning 


Power_df_n<-data.frame(3:nrow_length,Power_df)
colnames(Power_df_n)[1]<-"nseq"

ylab<-"Power";xlab<-"Sample size per treatment"
xmin<-min(Power_df_n$nseq)
xmax<-max(Power_df_n$nseq)
ymax<-1

Power_df_n<-Power_df_n[-length(3:nrow_length),] #deleting data point with all the data 
Power_df_n<-Power_df_n[-(length(3:nrow_length)-1),] #deleting data point with all the data 

write_csv(Power_df_n, file=paste0("res_Wilschko_Power_analysis","rans",rans,".csv"))

xmax<-max(Power_df_n$nseq)

seglen=3;yesp=1

#now a plot is made using the colors that were used in the rest of the paper 
col_legen_1=colors_plot[c("Rayleigh_col","HR_col","MANOVA_linear_group_groupP_col",
                          "LM_lin_group_P_group_col")]

pdf("Wiltschko_data_Power_analysis.pdf", width=4, height=4)

plot(Power_df_n$nseq,Power_df_n$Watson ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax))
lines(Power_df_n$nseq,Power_df_n$G.test,col=col_legen_1[2],lty=2,type = "b",pch = 1)
lines(Power_df_n$nseq,Power_df_n$Manova,col=col_legen_1[3],lty=3,type = "b",pch = 1)
lines(Power_df_n$nseq,Power_df_n$LMM,col=col_legen_1[4],lty=4,type = "b",pch = 1)

legend("topleft", legend=c("Watson (IDs averaged)","G-test (IDs averaged)","MANOVA",
                           "LMM"), col=col_legen_1,cex = 0.6, lty=c(1:4),pch = 1,bty="n",
                            lwd = 0.8,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )
dev.off()








