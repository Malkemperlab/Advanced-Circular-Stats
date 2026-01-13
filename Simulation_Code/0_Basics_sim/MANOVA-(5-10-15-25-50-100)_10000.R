#Script to simulate circular distributions for power analyses (written by Lukas Landler, 08/Nov/2022)

#load libraries
library(compiler)
library(circular)
library(NPCirc)
library(CircMLE)
library(foreach)
library(parallel)
options("scipen" = 10)
enableJIT(1)

#set seed - in order to get repeatable results 
set.seed(1)

###
# start the runs 
##########
rans = 10000

#setting sample size for dist 1 - in two steps to reduce typing effort
nvseq_1=c(5,10,15,25,50,100)
nvseq_2=c(10,20,30,50,100,200)
nvseq_3=c(15,30,45,75,150,300)
n_list_1=list(nvseq_1,nvseq_1,nvseq_1,nvseq_2,nvseq_2,nvseq_3,nvseq_3) 

#preparing certain vectors of concentration parameters and direction - all 7 elements long (they need to be same length) 
k0=0
k1=1
kwsn=2

#preparing the kappa/conc sequences
k_matrix_1=c(k0,k1,kwsn,k1,k1,k1,k1)

#preparing the m sequence 
m0=0
m_matrix_1=c(m0,m0,m0,m0,m0,m0,m0)

#defining the distribution type, there are always 5 combinations of sample sizes 
#therefore I prepared "chunks" of distribution and then combined them, decreasing typing effort 
type_vm="vm"
type_unif="unif"
type_vm_2=c("vm","vm")
type_vm_3=c("vm","vm","vm")
type_wsn="wsn"
type_list_1 = list(type_unif,type_vm,type_wsn,type_vm_2,type_vm_2,type_vm_3,type_vm_3)  

##### these are the names of the functions 

param_matrix_1 = c("param_unif","param_vm","param_wsn","param_axial","param_bim","param_trim_sym","param_trim_asym")  


#again in chunks of 5: naming the file that is created (see text for write.csv)
Type_1_errorn=c("Type_1_error")
Powern=c("Power")

#again combining the chunks 
Name=c(Type_1_errorn,Powern,Powern,Powern,Powern,Powern,Powern)

Example_Dist<-list()

###########
#the outer loop is stored one csv file, this means one panel in the plot. 
for (i in 1:length(Name)) {
  
   #grabbing the correct vectors for the simulation from the list
  nseq=n_list_1[[i]]
  k1=k_matrix_1[i]
  m1=m_matrix_1[i]
  distt =type_list_1[[i]]
  plist =param_matrix_1[i]

############# critical values for Watson and Kuiper (P=0.05), indipendent from sample size 
####### Prepare data frame for Power results 
ncore<-detectCores()
cl <- parallel::makeCluster(ncore-1, setup_strategy = "sequential")#first number changes the cores used
doParallel::registerDoParallel(cl)

Power_df<-foreach(e=1:length(nseq), .combine='rbind',.packages=c("circular","NPCirc","CircMLE")) %dopar%{
  
  n = nseq[e]
  
  #set up the functions for the different params - need to do it in here, because of the foreach 
  param_unif=function(x) {list(p=1,  mu=m1[1], con=k1[1])}
  param_vm=function(x) {list(p=1,  mu=m1, con=k1)}
  param_wsn=function(x) {list(p=1,  mu=m1, con=k1,sk=c(30))}
  param_axial=function(x) {list(p=c(0.5,0.5), mu=c(m1,m1-pi), con=c(k1,k1))}
  param_bim=function(x) {list(p=c(0.5,0.5), mu=c(m1,m1-rad(120)), con=c(k1,k1))}
  param_trim_sym=function(x) {list(p=c(1/3,1/3,1/3), mu=c(m1,m1-rad(120),m1+rad(120)), con=c(k1,k1,k1))}
  param_trim_asym=function(x) {list(p=c(1/3,1/3,1/3), mu=c(m1,m1-rad(90),m1+rad(120)), con=c(k1,k1,k1))}
  
  ###### set the distribution 
  Distribution <- sapply(1:rans, function(x) rcircmix(n,dist = distt, param=get(plist)()))
  
  #store an example for the distribution in a csv file  
  Example_Dist <- Distribution[,1]
  
  #random sample 
  random_sample<- sapply(1:rans, function(x) circular(rcircmix(n,model = 1)))
  
  #Rayleigh test
  Rayleigh <- (apply(Distribution,2,rayleigh.test))
  Ray <- array(as.matrix(unlist(Rayleigh)), dim=c(4, rans))
  RayP<- Ray[2,]
  
  #random T and HR Test
  random_T<-apply(random_sample,2,function(x) CircMLE:::HermansRasson2T(x)) #here change the fucntion 
  HR.res.T<-apply(Distribution,2,function(x) CircMLE:::HermansRasson2T(x)) # here change the function
  HR.res.p<-vapply(1:rans,function(x) sum(1*(HR.res.T[x])< random_T)/rans, FUN.VALUE = numeric(1))
  
  #model fitting based approach - comparing two models (unimodal versus uniform)
 
   random_Z_list<-apply(random_sample,2,function(x) circ_mle(circular(x,units = 'radians'),criterion = "AIC", nchains = 5, BadStart = 10^9,
           niter = 5000, method = "BFGS", lambda.min = 0.25, q.diff =pi/4,
           exclude = c("M2B","M2C","M3A","M3B","M4A","M4B","M5A","M5B"))[["results"]]["M1","deltaAIC"])
  
    random_Z<-as.matrix(sort(random_Z_list, decreasing = TRUE))[rans*0.05,]
  
  res_Z_list<-apply(Distribution,2,function(x) circ_mle(circular(x,units = 'radians'),criterion = "AIC", nchains = 5, BadStart = 10^9,
                                                            niter = 5000, method = "BFGS", lambda.min = 0.25, q.diff =pi/4,
                                                            exclude = c("M2B","M2C","M3A","M3B","M4A","M4B","M5A","M5B"))[["results"]]["M1","deltaAIC"])
  
  #MANOVA approach
  funManCirc<-function(x) summary(manova(cbind(cos(x),sin(x)) ~ 1),intercept=T)
  MANOVA_approach <- apply(Distribution,2, FUN=funManCirc)
  MANOVA_P <- sapply(c(1:rans),function(x) MANOVA_approach[[x]][["stats"]][1,6])
  
  #MANOVA approach Random used MANOVA_P from above
  random_MANOVA<-apply(random_sample,2,function(x) funManCirc(x))
  random_MANOVA_F <- sapply(c(1:rans),function(x) random_MANOVA[[x]][["stats"]][1,3])
  MANOVA_F <- sapply(c(1:rans),function(x) MANOVA_approach[[x]][["stats"]][1,3])
  random_MANOVA_P<-vapply(1:rans,function(x) sum(1*(MANOVA_F[x]< random_MANOVA_F))/rans, FUN.VALUE = numeric(1))
  
  #using the LM function
  variable<-rep(c("Cos","Sin"),times=c(n,n))
  value<-rbind(cos(Distribution),sin(Distribution))
  
  funLimCirc<-function(x) summary(lm(value[,x] ~ variable))
  LM_approach <- lapply(c(1:rans),FUN=funLimCirc)
  LM_P <- sapply(c(1:rans),function(x) LM_approach[[x]][["coefficients"]][1,4])
  
  #####Calculate p-value and store in Power file 
  Rayleigh<- sum(1*(RayP< 0.05))/rans
  HR<- sum(1*(HR.res.p< 0.05))/rans
  AIC2.Z<-sum(1*(as.numeric(res_Z_list)>random_Z))/rans
  MANOVA<- sum(1*(MANOVA_P < 0.05))/rans
  MANOVA_random<- sum(1*(random_MANOVA_P < 0.05))/rans
  lm_<- sum(1*(LM_P < 0.05))/rans
  
  names_names<-c("Rayleigh","HR","Model_comp","MANOVA","MANOVA_random","LM")
  Power<- data.frame(matrix(nrow = 1, ncol = length(names_names)))
  names(Power)<-names_names
  Power[1,]<-c(Rayleigh,HR,AIC2.Z,MANOVA,MANOVA_random,lm_)
  list(Power,Example_Dist)

} #e
stopCluster(cl)

Power_df_<-do.call(rbind,Power_df[,1])
Example_Dist_<-Power_df[,2]

text <- paste0(Name[i],"_n_",min(nseq),"-",max(nseq),"_its_",rans,"_Dist_",
             plist,"_",distt, "_m=",deg(m1), "_conc=",k1,".csv")[1]
text2 <- paste0("ExampleDist_",Name[i],"_n_",min(nseq),"-",max(nseq),"_its_",rans,"_Dist_",
              plist,"_",distt, "_m=",deg(m1), "_conc=",k1,".RDS")[1]

write.csv(data.frame(nseq,Power_df_), file = text,row.names=FALSE )
saveRDS(Example_Dist_,file=text2)

}#i
