######
#Script for hypothetical example of orientation experiments for 5 year retesting the same individuals every year 
#the Individuals either belong to group 1 OR group 2 (e.g., the sex of the animal) 
#in this example if there is an effect there is always a year effect, soemtimes a sex effect.  

library(compiler)
library(circular)
library(NPCirc)
library(CircMLE)
library(data.table)
library(lme4)
library(lmerTest)
library(foreach)
library(parallel)
library(permute) #for shuffle 
options("scipen" = 10)
enableJIT(1)

#set seed - in order to get repeatable results 
set.seed(1)

#load background stuff - including function for G test
source("Background_stuff_Manova_manuscript.R")

####
#start the run 
##########
rans = 10000

#nseq
nvseq_1=1:10
n_list_1=list(nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1)

#list with each c(m1,m2,m3,m4,m5)
dir<-rad(180)
mn=rep(dir,10)
mg=c(rep(dir,5),rep(dir+rad(90),5))
mg2=c(rep(dir,5),rep(dir+rad(180),5))
m_list_1=list(mn,mn,mn,mn,mg,mg,mg2,mg2)

#list with each c(k1, k2,k3,k4,k5)
k_more_clustered<-seq(1,2,length.out = 5)
k_less_clustered<-seq(0,1,length.out = 5)

k_equal_less<-c(k_less_clustered,k_less_clustered)
k_equal_more<-c(k_more_clustered,k_more_clustered)
k_unequal<-c(k_more_clustered,k_less_clustered)
k0<-rep(0,10)
k_list_1=list(k0,k_equal_less,k_equal_more,k_unequal,k_equal_less,k_unequal,k_equal_less,k_unequal)

#distt (list with c(5) )
type_vm=rep("vm",10)
type_unif=rep("unif",10)
type_list_1 = list(type_unif,type_vm,type_vm,type_vm,type_vm,type_vm,type_vm,type_vm)  

#plist (list with c(5) )
param_vm<-c("param_vm1","param_vm2","param_vm3","param_vm4","param_vm5","param_vm6","param_vm7","param_vm8","param_vm9","param_vm10")
param_unif<-rep("param_unif",10) 
param_list_1 = list(param_unif,param_vm,param_vm,param_vm,param_vm,param_vm,param_vm,param_vm)  

#again combining the chunks 
Name=c("Hypothetical_example_Type1error","Hypothetical_example_equal_less",
       "Hypothetical_example_equal_more","Hypothetical_example_unequal",
       "Hypothetical_example_equal_less_90degdiff","Hypothetical_example_unequal_90degdiff",
       "Hypothetical_example_equal_less_180degdiff","Hypothetical_example_unequal_180degdiff")

Example_Dist<-list()

#Add the number of IDs used 
ID_number="IDs_are_n_times2_" #double check

for (i in 1:length(Name)) {
  
  #grabbing the correct vectors for the simulation from the list
  nseq=n_list_1[[i]]
  k1=k_list_1[[i]]
  m1=m_list_1[[i]]
  distt =type_list_1[[i]]
  plist =param_list_1[[i]]
  
   ####### Prepare data frame for Power results 
  #ncore<-detectCores()
  cl <- parallel::makeCluster(3, setup_strategy = "sequential")#first number changes the cores used
  doParallel::registerDoParallel(cl)
  
Power_df<-foreach(e=1:length(nseq), .combine='rbind',.packages=c("circular","NPCirc","CircMLE","lme4","lmerTest","data.table","permute")) %dopar%{
  
  n = nseq[e]

  #might not need all of these distributions, but leave it here for flexibility   
param_vm1=function(x) {list(p=1,  mu=m1[1], con=k1[1])}
param_vm2=function(x) {list(p=1,  mu=m1[2], con=k1[2])}
param_vm3=function(x) {list(p=1,  mu=m1[3], con=k1[3])}
param_vm4=function(x) {list(p=1,  mu=m1[4], con=k1[4])}
param_vm5=function(x) {list(p=1,  mu=m1[5], con=k1[5])}
param_vm6=function(x) {list(p=1,  mu=m1[6], con=k1[6])}
param_vm7=function(x) {list(p=1,  mu=m1[7], con=k1[7])}
param_vm8=function(x) {list(p=1,  mu=m1[8], con=k1[8])}
param_vm9=function(x) {list(p=1,  mu=m1[9], con=k1[9])}
param_vm10=function(x) {list(p=1,  mu=m1[10], con=k1[10])}

param_unif=function(x) {list(p=1,  mu=m1[1], con=k1[1])}


#set the random effect 
#group1
random_1<-sapply(1:rans, function(x) Rfast::rvonmises(n=n, m=0, k=3))

#group2
random_2<-sapply(1:rans, function(x) Rfast::rvonmises(n=n, m=0, k=3))

### add random error underneath! 
###### set the distribution for each year and then combine  
Year1 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[1],
                                             param=get(plist[1])()));Year1 <- Year1+random_1

Year2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[2],
                                                    param=get(plist[2])()));Year2 <- Year2+random_1

Year3 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[3],
                                                    param=get(plist[3])()));Year3 <- Year3+random_1

Year4 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[4],
                                                    param=get(plist[3])()));Year4 <- Year4+random_1

Year5 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[5],
                                                    param=get(plist[5])()));Year5 <- Year5+random_1

Year1_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[6],
                                             param=get(plist[6])()));Year1_2 <- Year1_2+random_2

Year2_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[7],
                                             param=get(plist[7])()));Year2_2 <- Year2_2+random_2

Year3_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[8],
                                             param=get(plist[8])()));Year3_2 <- Year3_2+random_2

Year4_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[9],
                                             param=get(plist[9])()));Year4_2 <- Year4_2+random_2

Year5_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[10],
                                             param=get(plist[10])()));Year5_2 <- Year5_2+random_2

Distribution<-rbind(Year1,Year2,Year3,Year4,Year5,Year1_2,Year2_2,Year3_2,Year4_2,Year5_2)

year_cov<-c(rep(1,n),rep(2,n),rep(3,n),rep(4,n),rep(5,n),rep(1,n),rep(2,n),rep(3,n),rep(4,n),rep(5,n))
group_fact<-c(rep("g1",length(year_cov)/2),rep("g2",length(year_cov)/2))
ID_1<-c(1:n);ID_2<-c((n+1):(n+n))
ID<-as.factor(c(rep(ID_1,5),rep(ID_2,5)))

#store an example for the distribution in a csv file  
Example_Dist <- data.frame(Distribution[,1],year_cov,group_fact,ID)

#Distribution for HR and Rayleigh test (circular mean over IDs)

Distribution_1<-sapply(1:rans,function(x) setNames(data.frame(Distribution[,x],ID), c("ori", "ID")))
Distribution_IDmean<-as.data.frame(lapply(c(1:rans),FUN=function(x) setDT(Distribution_1 [,x])[, mean.circular(c(ori)), by=ID][,2]))

#random sample 
random_sample<- sapply(1:rans, function(x)rcircmix(nrow(Distribution),model = 1))
random_sampleHR<- sapply(1:rans, function(x)rcircmix(nrow(Distribution_IDmean),model = 1))

#Rayleigh test
Rayleigh <- (apply(Distribution_IDmean,2,rayleigh.test))
Ray <- array(as.matrix(unlist(Rayleigh)), dim=c(4, rans))
RayP<- Ray[2,]

#random T and HR Test
random_T<-apply(random_sampleHR,2,function(x) CircMLE:::HermansRasson2T(x)) #here change the function 
HR.res.T<-apply(Distribution_IDmean,2,function(x) CircMLE:::HermansRasson2T(x)) # here change the function
HR.res.p<-sapply(1:rans,function(x) sum(1*(HR.res.T[x])< random_T)/rans)

#model fitting based approach - comparing two models (unimodal versus uniform)
random_Z_list<-apply(random_sampleHR,2,function(x) circ_mle(circular(x,units = 'radians'),criterion = "AIC", nchains = 5, BadStart = 10^9,
                                                          niter = 5000, method = "BFGS", lambda.min = 0.25, q.diff =pi/4,
                                                          exclude = c("M2B","M2C","M3A","M3B","M4A","M4B","M5A","M5B"))[["results"]]["M1","deltaAIC"])

random_Z<-as.matrix(sort(random_Z_list, decreasing = TRUE))[rans*0.05,]

res_Z_list<-apply(Distribution_IDmean,2,function(x) circ_mle(circular(x,units = 'radians'),criterion = "AIC", nchains = 5, BadStart = 10^9,
                                                      niter = 5000, method = "BFGS", lambda.min = 0.25, q.diff =pi/4,
                                                      exclude = c("M2B","M2C","M3A","M3B","M4A","M4B","M5A","M5B"))[["results"]]["M1","deltaAIC"])


####Watson - within group averaged by ID
#split by group 
DistributionX<-data.frame(Distribution,group_fact)
Distribution1<-DistributionX[DistributionX$group_fact=="g1",];Distribution1<-Distribution1[,-ncol(Distribution1)]
Distribution2<-DistributionX[DistributionX$group_fact=="g2",];Distribution2<-Distribution2[,-ncol(Distribution2)]

#then average by ID 

ID1<-ID[1:nrow(Distribution1)]
ID2<-ID[(nrow(Distribution1)+1):length(ID)]

Distribution_1_<-sapply(1:rans,function(x) setNames(data.frame(Distribution1[,x],ID1), c("ori", "ID")))
Distribution_1_IDmean<-as.data.frame(lapply(c(1:rans),FUN=function(x) setDT(Distribution_1_ [,x])[, mean.circular(c(ori)), by=ID][,2]))

Distribution_2<-sapply(1:rans,function(x) setNames(data.frame(Distribution2[,x],ID2), c("ori", "ID")))
Distribution_2_IDmean<-as.data.frame(lapply(c(1:rans),FUN=function(x) setDT(Distribution_2 [,x])[, mean.circular(c(ori)), by=ID][,2]))

#add watson U2 for the group difference 
watson1 = mapply(function(x,y) {watson.two.test(x,y)},Distribution_1_IDmean,Distribution_2_IDmean)
watson1 <- array(as.matrix(unlist(watson1)), dim=c(5, rans))
watson1<- watson1[1,]  # critical valueCritical Value: 0.187 

#G-test 
G.p= sapply(1:rans,function(x) G_test(Distribution_1_IDmean[,x],Distribution_2_IDmean[,x]))

#MANOVA approach
funManCirc<-function(x) helpCatch(summary(manova(cbind(cos(x),sin(x)) ~ 1+Error(0+ID)),tol=0,intercept=T))
MANOVA_approach <- apply(Distribution,2, FUN=funManCirc)
MANOVA_P <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach[[x]][["Error: ID"]][["stats"]][1,6]))

#MANOVA approach Random used MANOVA_P from above
random_MANOVA<-apply(random_sample,2,function(x) funManCirc(x))
random_MANOVA_F <- sapply(c(1:rans),function(x) helpCatch(random_MANOVA[[x]][["Error: ID"]][["stats"]][1,3]))
MANOVA_F <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach[[x]][["Error: ID"]][["stats"]][1,3]))
if (list(NULL) %in% MANOVA_F) {MANOVA_F_2<-rep(0,length(MANOVA_F))} else {MANOVA_F_2=MANOVA_F}
random_MANOVA_P<-sapply(1:rans,function(x) sum(1*(MANOVA_F_2[x]< random_MANOVA_F))/rans)

#MANOVA approach
funManCirc_lin<-function(x) helpCatch(summary(manova(cbind(cos(x),sin(x)) ~ year_cov+Error(0+ID)),tol=0,intercept=T))
MANOVA_approach_lin <- apply(Distribution,2, FUN=funManCirc_lin)
MANOVA_P_lin <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach_lin[[x]][["Error: ID"]][["stats"]][1,6]))
MANOVA_P_lin_cov <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach_lin[[x]][["Error: Within"]][["stats"]][1,6]))

#MANOVA approach Random used MANOVA_P from above
random_MANOVA_lin<-apply(random_sample,2,function(x) funManCirc_lin(x))
random_MANOVA_F_lin <- sapply(c(1:rans),function(x)  helpCatch(random_MANOVA_lin[[x]][["Error: ID"]][["stats"]][1,3]))
MANOVA_F_lin <- sapply(c(1:rans),function(x)  helpCatch(MANOVA_approach_lin[[x]][["Error: ID"]][["stats"]][1,3]))
if (list(NULL) %in% MANOVA_F_lin) {MANOVA_F_lin_2<-rep(0,length(MANOVA_F_lin))}else {MANOVA_F_lin_2=MANOVA_F_lin}
random_MANOVA_P_lin<-sapply(1:rans,function(x) sum(1*(MANOVA_F_lin_2[x]< random_MANOVA_F_lin))/rans)

random_MANOVA_F_lin_cov <- sapply(c(1:rans),function(x)  helpCatch(random_MANOVA_lin[[x]][["Error: Within"]][["stats"]][1,3]))
MANOVA_F_lin_cov <- sapply(c(1:rans),function(x)  helpCatch(MANOVA_approach_lin[[x]][["Error: Within"]][["stats"]][1,3]))
if (list(NULL) %in% MANOVA_F_lin_cov) {MANOVA_F_lin_cov_2<-rep(0,length(MANOVA_F_lin_cov))}else {MANOVA_F_lin_cov_2=MANOVA_F_lin_cov}
random_MANOVA_P_lin_cov<-sapply(1:rans,function(x) sum(1*(MANOVA_F_lin_cov_2[x]< random_MANOVA_F_lin_cov))/rans)

#MANOVA approach added linear and grouping factor
funManCirc_lin_group<-function(x) helpCatch(summary(manova(cbind(cos(x),sin(x)) ~ year_cov+group_fact+Error(0+ID)),tol=0,intercept=T))
MANOVA_approach_lin_group <- apply(Distribution,2, FUN=funManCirc_lin_group)

MANOVA_P_lin_group <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach_lin_group[[x]][["Error: ID"]][["stats"]][1,6]))
MANOVA_P_lin_group_cov <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach_lin_group[[x]][["Error: Within"]][["stats"]][1,6])) #need to undertsand why covariate P is only in within... ahh wait there is only one ID per year!! 
MANOVA_P_lin_group_cov_group <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach_lin_group[[x]][["Error: ID"]][["stats"]][2,6]))

#MANOVA approach Random used MANOVA_P from above
random_MANOVA_lin_group<-apply(random_sample,2,function(x) funManCirc_lin_group(x))
random_MANOVA_F_lin_group <- sapply(c(1:rans),function(x) helpCatch(random_MANOVA_lin_group[[x]][["Error: ID"]][["stats"]][1,3]))
if (list(NULL) %in% random_MANOVA_F_lin_group) {random_MANOVA_F_lin_group_2<-rep(NA,length(random_MANOVA_F_lin_group))}else {random_MANOVA_F_lin_group_2=random_MANOVA_F_lin_group}

MANOVA_F_lin_group <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach_lin_group[[x]][["Error: ID"]][["stats"]][1,3]))
if (list(NULL) %in% MANOVA_F_lin_group) {MANOVA_F_lin_group_2<-rep(NA,length(MANOVA_F_lin_group))}else {MANOVA_F_lin_group_2=MANOVA_F_lin_group}

random_MANOVA_P_lin_group<-sapply(1:rans,function(x) sum(1*(MANOVA_F_lin_group_2[x]< random_MANOVA_F_lin_group_2))/rans)

random_MANOVA_F_lin_group_cov <- sapply(c(1:rans),function(x) helpCatch(random_MANOVA_lin_group[[x]][["Error: Within"]][["stats"]][1,3]))
if (list(NULL) %in% random_MANOVA_F_lin_group_cov) {random_MANOVA_F_lin_group_cov_2<-rep(NA,length(random_MANOVA_F_lin_group_cov))}else {random_MANOVA_F_lin_group_cov_2=random_MANOVA_F_lin_group_cov}

MANOVA_F_lin_group_cov <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach_lin_group[[x]][["Error: Within"]][["stats"]][1,3]))
if (list(NULL) %in% MANOVA_F_lin_group_cov) {MANOVA_F_lin_group_cov_2<-rep(NA,length(MANOVA_F_lin_group_cov))}else {MANOVA_F_lin_group_cov_2=MANOVA_F_lin_group_cov}

random_MANOVA_P_lin_group_cov<-sapply(1:rans,function(x) sum(1*(MANOVA_F_lin_group_cov_2[x]< random_MANOVA_F_lin_group_cov_2))/rans)

random_MANOVA_F_lin_group_cov <- sapply(c(1:rans),function(x) helpCatch(random_MANOVA_lin_group[[x]][["Error: ID"]][["stats"]][2,3]))
if (list(NULL) %in% random_MANOVA_F_lin_group_cov) {random_MANOVA_F_lin_group_cov_2<-rep(NA,length(random_MANOVA_F_lin_group_cov))}else {random_MANOVA_F_lin_group_cov_2=random_MANOVA_F_lin_group_cov}


MANOVA_F_lin_group_cov_group <- sapply(c(1:rans),function(x) helpCatch(MANOVA_approach_lin_group[[x]][["Error: ID"]][["stats"]][2,3]))
if (list(NULL) %in% MANOVA_F_lin_group_cov_group) {MANOVA_F_lin_group_cov_group_2<-rep(NA,length(MANOVA_F_lin_group_cov_group))}else {MANOVA_F_lin_group_cov_group_2=MANOVA_F_lin_group_cov_group}

random_MANOVA_P_lin_group_cov_group<-sapply(1:rans,function(x) sum(1*(MANOVA_F_lin_group_cov_group_2[x]< random_MANOVA_F_lin_group_cov_2))/rans)


#using the LM function
variable<-rep(c("Cos","Sin"),times=c(length(year_cov),length(year_cov)))  
value<-rbind(cos(Distribution),sin(Distribution))
year_2<-c(year_cov,year_cov)
group_2<-c(group_fact,group_fact)
ID_<-c(ID,ID)

#placing ID nested in "variable" allows differing means of ID for each of the response variables  
#need to decide if it makes sens nested in variable or better just 1|ID ??
funLimCirc<-function(x) helpCatch(summary(lmer(value[,x] ~ variable+ (1|ID_))))
lmer_approach <- lapply(c(1:rans),FUN=funLimCirc)
lmer_P_intercept <- sapply(c(1:rans),function(x) lmer_approach[[x]][["coefficients"]][1,5])

funLimCirc1<-function(x) helpCatch(summary(lmer(value[,x] ~ variable+year_2+ (1|ID_))))
lmer_approach <- lapply(c(1:rans),FUN=funLimCirc1)
lmer_lin_P_intercept <- sapply(c(1:rans),function(x) lmer_approach[[x]][["coefficients"]][1,5])
lmer_lin_P_lin <- sapply(c(1:rans),function(x) lmer_approach[[x]][["coefficients"]][3,5])

funLimCirc2<-function(x) helpCatch(summary(lmer(value[,x] ~ variable+year_2+group_2+ (1|ID_))))
lmer_approach <- lapply(c(1:rans),FUN=funLimCirc2)
lmer_lin_group_P_intercept <- sapply(c(1:rans),function(x) lmer_approach[[x]][["coefficients"]][1,5])
lmer_lin_group_P_lin <- sapply(c(1:rans),function(x) lmer_approach[[x]][["coefficients"]][3,5])
lmer_lin_group_P_group <- sapply(c(1:rans),function(x) lmer_approach[[x]][["coefficients"]][4,5])


#####Calculate p-value and store in Power file 
Rayleigh<- sum(1*(RayP< 0.05))/rans
HR<- sum(1*(HR.res.p< 0.05))/rans
AIC2.Z<-sum(1*(as.numeric(res_Z_list)>random_Z))/rans
WatsonTwoSamp<- (sum(1*(watson1>0.187)))/rans #added
G.test<- (sum(1*(G.p<0.05)))/rans

MANOVA<- sum(1*(MANOVA_P < 0.05))/rans
MANOVA_random<- sum(1*(random_MANOVA_P < 0.05))/rans
MANOVA_lin<- sum(1*(MANOVA_P_lin < 0.05))/rans
MANOVA_random_lin<- sum(1*(random_MANOVA_P_lin < 0.05))/rans
MANOVA_lin_cov<- sum(1*(MANOVA_P_lin_cov < 0.05))/rans
MANOVA_random_lin_cov<- sum(1*(random_MANOVA_P_lin_cov < 0.05))/rans

MANOVA_lin_group<- helpCatch(sum(1*(MANOVA_P_lin_group < 0.05))/rans)
MANOVA_random_lin_group<- sum(1*(random_MANOVA_P_lin_group < 0.05))/rans
MANOVA_lin_group_cov<- sum(1*(MANOVA_P_lin_group_cov < 0.05))/rans
MANOVA_random_lin_group_cov<- sum(1*(random_MANOVA_P_lin_group_cov < 0.05))/rans
MANOVA_lin_group_cov_group<- helpCatch(sum(1*(MANOVA_P_lin_group_cov_group < 0.05))/rans)
MANOVA_random_lin_group_cov_group<- sum(1*(random_MANOVA_P_lin_group_cov_group < 0.05))/rans

lmer_P_intercept_<- sum(1*(lmer_P_intercept < 0.05))/rans
lmer_lin_P_intercept_<- sum(1*(lmer_lin_P_intercept < 0.05))/rans
lmer_lin_P_lin_<- sum(1*(lmer_lin_P_lin < 0.05))/rans
lmer_lin_group_P_intercept_<- sum(1*(lmer_lin_group_P_intercept < 0.05))/rans
lmer_lin_group_P_lin_<- sum(1*(lmer_lin_group_P_lin < 0.05))/rans
lmer_lin_group_P_group_<- sum(1*(lmer_lin_group_P_group < 0.05))/rans


names__<-c("Rayleigh","HR","Model_comp","Watson_two_sample","G-test",
           "MANOVA_intercept","MANOVA_random_intercept",
    "MANOVA_linear_interceptP","MANOVA_random_linear_interceptP",
    "MANOVA_linear_covariateP","MANOVA_random_linear_covariateP",
    "MANOVA_linear_group_interceptP","MANOVA_random_linear_group_interceptP", 
    "MANOVA_linear_group_covariateP","MANOVA_random_linear_group_covariateP", 
    "MANOVA_linear_group_groupP","MANOVA_random_linear_group_groupP",
    "LM_P_intercept","LM_lin_P_intercept","LM_lin_P_lin","LM_lin_group_P_intercept",
    "LM_lin_group_P_lin","LM_lin_group_P_group") 

Power<- data.frame(matrix(nrow = 1, ncol = length(names__)))
names(Power)<-names__

Power[1,]<-c(Rayleigh,HR,AIC2.Z,WatsonTwoSamp,G.test,
             MANOVA,MANOVA_random,MANOVA_lin,MANOVA_random_lin,
             MANOVA_lin_cov,MANOVA_random_lin_cov,
             MANOVA_lin_group,MANOVA_random_lin_group,
             MANOVA_lin_group_cov,MANOVA_random_lin_group_cov,
             MANOVA_lin_group_cov_group,MANOVA_random_lin_group_cov_group,
             lmer_P_intercept_,lmer_lin_P_intercept_,lmer_lin_P_lin_,
             lmer_lin_group_P_intercept_,lmer_lin_group_P_lin_,lmer_lin_group_P_group_)
          
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

