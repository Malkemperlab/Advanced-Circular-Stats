######
#hypothetical Emlen Funnel study with video recordings 
# subsamples are eitehr 10, 100 or 1000

#in situation one there is no difference between the groups and in situation 2 they follow the 'magnetic field' which is shifted 90°

#load libraries 
library(compiler)
library(circular)
library(NPCirc)
library(CircMLE)
library(data.table)
library(lme4)
library(lmerTest)
library(foreach)
library(parallel)
options("scipen" = 10)
enableJIT(1)

#set seed - in order to get repeatable results 
set.seed(1)

#load background stuff - including function for Moores paired test
source("Background_stuff_Manova_manuscript.R")

####
#start the run 
##########
rans = 10000

#nseq
nvseq_1= 3:10
n_list_1=list(nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1)

#subsamplings ("orientations per video")
nr10=10
nr100=100
nr1000=1000

nr_list=list(nr10,nr100,nr1000,nr10,nr100,nr1000)

#list with mean direction for each group
dir<-rad(0)
m0<-c(dir,dir)
mg=c(dir,dir+rad(90))
m_list_1=list(m0,m0,m0,mg,mg,mg)

#list for kappa - 0.2 for both
k1<-rep(0.2,2)
k_list_1=list(k1,k1,k1,k1,k1,k1)

#distt
type_vm=rep("vm",2)
type_list_1 = list(type_vm,type_vm,type_vm,type_vm,type_vm,type_vm)  

#plist (list with c(5) )
param_vm<-c("param_vm1","param_vm2")
param_list_1 = list(param_vm,param_vm,param_vm,param_vm,param_vm,param_vm)  

#again combining the chunks 
Name=c("Hypothetical_EmlenFunnel_both0deg_10subsamples",
       "Hypothetical_EmlenFunnel_both0deg_100subsamples",
       "Hypothetical_EmlenFunnel_both0deg_1000subsamples",
       
        "Hypothetical_EmlenFunnel_example_090deg_10subsample",
       "Hypothetical_EmlenFunnel_example_090deg_100subsample",
       "Hypothetical_EmlenFunnel_example_090deg_1000subsample")

#Add the number of IDs used 
ID_number="IDs_are_equal_n_paired" 

for (i in 1:length(Name)) {
  
  #grabbing the correct vectors for the simulation from the list
  nseq=n_list_1[[i]]
  nr=nr_list[[i]]
  k1=k_list_1[[i]]
  m1=m_list_1[[i]]
  distt =type_list_1[[i]]
  plist =param_list_1[[i]]
  
   ####### Prepare data frame for Power results 
 # ncore<-detectCores()
  cl <- parallel::makeCluster(1, setup_strategy = "sequential")#first number changes the cores used
  doParallel::registerDoParallel(cl)
  
Power_df<-foreach(e=1:length(nseq), .combine='rbind',.packages=c("circular","NPCirc","CircMLE","lme4","lmerTest","data.table")) %dopar%{
  
  n = nseq[e]

  #might not need all of these distributions, but leave it here for flexibility   
param_vm1=function(x) {list(p=1,  mu=m1[1], con=k1[1])}
param_vm2=function(x) {list(p=1,  mu=m1[2], con=k1[2])}


#set the random effect for each individual (n=n)
random_error<-sapply(1:rans, function(x) Rfast::rvonmises(n=n, m=0, k=3))

#change random error to match subsamples
error_list<-list()
for (r in 1:n){ 
  r1<-as.data.frame(t(random_error[r,]))
  error_list[[r]]<-r1[rep(seq_len(nrow(r1)), each = nr), ] 
  }#r

### add random error underneath! 
###### set the distribution for each year and then combine  

Trt_0<-list()
Trt_90<-list()

for (y in 1:nrow(random_error)) {
  Trt_01 <- sapply(1:rans, function(x) rcircmix(nr,dist = distt[1],
                                               param=get(plist[1])())) # note: columns here are rounds of randomization and rows subsamples 
  Trt_0[[y]] <- Trt_01+error_list[[y]]
                                               
  Trt_901 <- sapply(1:rans, function(x) rcircmix(nr,dist = distt[1],
                                              param=get(plist[2])()))
  Trt_90[[y]] <- Trt_901+error_list[[y]] #same animals are tested here as well therefore same error_list used 
}#y

#transform to data frame and add identifier for IDs

Trt0_data <-  do.call(rbind, Map(cbind, ID = as.factor(seq_along(Trt_0)), Trt_0))
Trt90_data <-  do.call(rbind, Map(cbind, ID = as.factor(seq_along(Trt_90)), Trt_90))

#combine distribution and delete ID column -> save ID as vector 
Distribution_ID<-rbind(Trt0_data,Trt90_data)
Distribution<-subset(Distribution_ID, select=-c(ID))

#save ID as vectro
ID<-as.factor(Distribution_ID$ID)

#add group 
group_fact<-c(rep("g1",nrow(Trt0_data)),rep("g2",nrow(Trt90_data)))

#store an example for the distribution in a csv file  
Example_Dist <- data.frame(Distribution[,1],group_fact,ID)

#random sample 
random_sample<- sapply(1:rans, function(x)rcircmix(nrow(Distribution),model = 1))


####Moores - PAIRED - test - within group averaged by ID

#first split in two groups 
#split by group 
DistributionX<-data.frame(Distribution,group_fact)
Distribution1<-DistributionX[DistributionX$group_fact=="g1",];Distribution1<-Distribution1[,-ncol(Distribution1)]
Distribution2<-DistributionX[DistributionX$group_fact=="g2",];Distribution2<-Distribution2[,-ncol(Distribution2)]

#then average by ID 
Distribution_1<-sapply(1:rans,function(x) setNames(data.frame(Distribution1[,x],ID), c("ori", "ID")))
Distribution_1_IDmean<-as.data.frame(lapply(c(1:rans),FUN=function(x) setDT(Distribution_1 [,x])[, mean.circular(c(ori)), by=ID][,2]))

Distribution_2<-sapply(1:rans,function(x) setNames(data.frame(Distribution2[,x],ID), c("ori", "ID")))
Distribution_2_IDmean<-as.data.frame(lapply(c(1:rans),FUN=function(x) setDT(Distribution_2 [,x])[, mean.circular(c(ori)), by=ID][,2]))

Moores_paired <- mapply(function(x,y) {MooreRTestRand(x,y)},Distribution_1_IDmean,Distribution_2_IDmean)

#MANOVA approach added grouping factor
funManCirc_group<-function(x) summary(manova(cbind(cos(x),sin(x)) ~ group_fact+Error(0+ID)),tol=0,intercept=T)
MANOVA_approach_group <- apply(Distribution,2, FUN=funManCirc_group)
MANOVA_P_group_group <- sapply(c(1:rans),function(x) MANOVA_approach_group[[x]][["Error: Within"]][["stats"]][1,6]) #need to undertsand why covariate P is only in within... ahh wait there is only one ID per year!! 

#MANOVA approach Random used MANOVA_P from above
random_MANOVA_group<-apply(random_sample,2,function(x) funManCirc_group(x))
random_MANOVA_F_group_cov <- sapply(c(1:rans),function(x) random_MANOVA_group[[x]][["Error: Within"]][["stats"]][1,3])
MANOVA_F_group_cov <- sapply(c(1:rans),function(x) MANOVA_approach_group[[x]][["Error: Within"]][["stats"]][1,3])
if (list(NULL) %in% MANOVA_F_group_cov) {MANOVA_F_group_cov_2<-rep(0,length(MANOVA_F_group_cov))}else {MANOVA_F_group_cov_2=MANOVA_F_group_cov}
random_MANOVA_P_group_group<-sapply(1:rans,function(x) sum(1*(MANOVA_F_group_cov_2[x]< random_MANOVA_F_group_cov))/rans)


#using the LM function
variable<-rep(c("Cos","Sin"),times=c(length(ID),length(ID)))  
value<-rbind(cos(Distribution),sin(Distribution))
group_2<-c(group_fact,group_fact)
ID_<-c(ID,ID)

#placing ID nested in "variable" allows differing means of ID for each of the response variables  
#results for LMM a bit weird... not sure why need to double check
#guess when it is in both I cannot nest it in variable !!!
#actually didn't change too much 
#makes sense that it is only nested when it appears either in group 1 or group 2

funLimCirc2<-function(x) summary(lmer(value[,x] ~ variable+group_2+ (1|ID_)))
lmer_approach <- lapply(c(1:rans),FUN=funLimCirc2)
lmer_group_P_group <- sapply(c(1:rans),function(x) lmer_approach[[x]][["coefficients"]][3,5])


#####Calculate p-value and store in Power file 
Moores_paired_P<- sum(1*(Moores_paired< 0.05))/rans

MANOVA_group_group<- sum(1*(MANOVA_P_group_group < 0.05))/rans #group P value of group effects model
MANOVA_random_group_group<- sum(1*(random_MANOVA_P_group_group < 0.05))/rans

lmer_group_P_group_<- sum(1*(lmer_group_P_group < 0.05))/rans


names__<-c("Moores_paired", 
    "MANOVA_groupP","MANOVA_random_groupP",
    "LM_group_groupP") 

Power<- data.frame(matrix(nrow = 1, ncol = length(names__)))
names(Power)<-names__

Power[1,]<-c(Moores_paired_P,
             MANOVA_group_group,MANOVA_random_group_group,
             lmer_group_P_group_)
          
list(Power,Example_Dist)
}#e
stopCluster(cl)

Power_df_<-do.call(rbind,Power_df[,1])
Example_Dist_<-Power_df[,2]


text <- paste0(ID_number,"_",Name[i],"_n_",min(nseq),"-",max(nseq),"_its_",rans,"_Dist_",
               plist,"_",distt, "_m=",deg(m1), "_conc=",k1,".csv")[1]

text2 <- paste0("ExampleDist_",ID_number,"_",Name[i],"_n_",min(nseq),"-",max(nseq),"_its_",rans,"_Dist_",
                plist,"_",distt, "_m=",deg(m1), "_conc=",k1,".RDS")[1]

write.csv(data.frame(nseq,Power_df_), file = text,row.names=FALSE )
saveRDS(Example_Dist_,file=text2)

}#i

