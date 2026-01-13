

###### FUNCTIONS for MANOVA analyses 

####the G-test 
#######function for G value 
Gvalue <- function(s1,s2,p,q){
  total <- 0 
  for(i in 1:p){
    for(j in 1:q){
      temp <- s1[i]-s2[j]
      if(temp<0){temp <- -temp}
      if(temp>pi){temp <- (2*pi)-temp}
      total <- total+temp}}
  return(total)}

G_test <-function(samp1,samp2){
  require(permute)
  NR <- 999
  n1 <- length(samp1)
  n2 <- length(samp2)
  Gstat <- Gvalue(samp1,samp2,n1,n2)
  nxtrm <-1 
  n <- n1+n2
  combsample <- c(samp1,samp2)
  for(r in 1:NR){
    perm <- permute::shuffle(1:n)
    randsamp1 <- combsample[perm[1:n1]]
    randsamp2 <- combsample[perm[n1+1:n]]
    randsamp2 <- randsamp2[1:n2]
    
    Grand <- Gvalue(randsamp1,randsamp2,n1,n2)
    if(Grand >= Gstat){nxtrm<-nxtrm+1}
    
  }
  return(nxtrm/(NR+1))
  
}


#defining two helper functions 
DoublingRad <- function (x) {x=(x*2)%%(2*pi);return(x)}

################
#MANOVA model selection for angular data _  AIC based  (pre-selection based on effect sizes possible)

####################
# input below for function testing only, commented out 
#RESPONSE_Mat<-RESPONSE_deer;MODEL_FULL<-MANOVA_deer_full

#input a response matrix with two columns (cosine and sine of the circular response variable in radians )
#  the full model (result of  summary function on manova, without intercept - see examples) 
# and the maximum number of terms to be evaluated (terms are ranked based on effect size 
# and the input order the lowest ones are deleted, in order to reach the desired number 
# - larger number of terms slows down the function by a lot)
#returns a list with all the models tested, a manova summary for the top ranked model, as well as the effect size (partial eta squared) for each of the terms.
#can be accessed using list[[1]] for the table with the AICs for each model,
#list[[2]] for the manova summary object of the "best" model
#and list [[3]] for the effect sizes 

#### important IF the model contains a random error term specifiy it in the function
#### this is done by setting random_error = T

#test:
# MODEL_FULL<-MANOVA_bianco_doubled
# RESPONSE_Mat<-cbind(cos(angle_rad*2),sin(angle_rad*2))

manova_AIC_selection_randomerror<-function(RESPONSE_Mat,MODEL_FULL,n=NULL) {
  
  #libraries that are required
  require(stringi)
  require(effectsize)
  
  #  here needs to be specified what happens then 
  
   # define the variables to be tested (also explicitly add interaction terms) and 
  # determine the order of the terms and delete terms which will not be evaluted based on "n" input 
  variable_<-paste("RESPONSE_Mat ~ ",paste(MODEL_FULL[[1]][["row.names"]][-length(MODEL_FULL[[1]][["row.names"]])],collapse="+"))
  random_term<-gsub("Error: ","",names(MODEL_FULL)[1])
  variable_1<-paste(variable_,"+ Error(0+",random_term,")")
  formula_1<-as.formula(variable_1)
  effect_1<-lapply(summary(aov(formula_1))[[1]], effectsize::eta_squared) #calculate effect size for each anova 
  effect_1<-effect_1$` Response Y1`$Eta2_partial+effect_1$` Response Y2`$Eta2_partial #calculate  effect size for the terms 
  
  # order after effect size 
  variable_effect<-cbind(MODEL_FULL[[1]][["row.names"]][-length(MODEL_FULL[[1]][["row.names"]])],effect_1)
  variable_effect_sorted <- variable_effect[order(-effect_1),] 
  
  #set the default if n is not given
  if(is.null(n))
    n <- nrow(variable_effect_sorted)
  
  #remove terms that are not used in AIC based model selection 
  
  n_u <-if (n>nrow(variable_effect_sorted)) {nrow(variable_effect_sorted)} else {n} #if one would put in a larger number than terms present in the model the number of model terms in the input model are used 
  variables<-variable_effect_sorted[c(1:n_u),1]
  
  #create all combinations and collapse adding "+" and store the text in the file forumlas 
  id <- unlist(lapply(1:length(variables), function(i)combn(1:length(variables),i,simplify=FALSE)),recursive=FALSE)
  formulas <- sapply(id,function(i) paste("RESPONSE_Mat[,x]~ ",paste(variables[i],collapse="+"),paste("+ Error(",random_term,")")))
  intercept_model<-paste("RESPONSE_Mat[,x]~ 1","+ Error(0+",random_term,")")
   formulas <- c(formulas,intercept_model) #add the intercept model 
  
  #prepare the data frame for the result 
  AIC_mean_df_collection<-data.frame(formulas,rep("NA",length(formulas)));colnames(AIC_mean_df_collection)<-c("formula","AIC")
  
  
  #test<-t(as.data.frame(formulas))

  AIC_mean_df_collection[,2]<-sapply (1:length(formulas), function (i) { mean(sapply(1:ncol(RESPONSE_Mat),
                                                                                     function(x) AIC(aov(as.formula(formulas[i]))[[2]])))})
  
  #order using the number of terms
  #first add column with termnumber 
  AIC_mean_df_collection$number.of.terms = stringi::stri_count(AIC_mean_df_collection$formula, fixed = "+")
   #order using AIC value and number of terms (best model has highest number of terms and lowest p-value)
  AIC_mean_df_collection<-AIC_mean_df_collection[order(AIC_mean_df_collection$AIC,-AIC_mean_df_collection$number.of.terms),]
  
  #use best model
  
  BEST_MODEL<-unlist(gsub(",x",",", AIC_mean_df_collection[1,1]))
  intercept_model_<-unlist(gsub(",x",",", intercept_model))
  
  #here if loop in case the best model is an intercept model!! 
  ########
  
  if (BEST_MODEL==intercept_model_) #is this really the same ? See line above 
  {Effect_size_final<-"intercept model -> no effect sizes of best model"
  intercept_model_<-paste("RESPONSE_Mat~ 1","+ Error(0+",random_term,")")
    MANOVA_summary_final<- summary(manova(as.formula(intercept_model_)),intercept = T)} 
  else 
    {
  
  MANOVA_summary<-summary(manova(as.formula(BEST_MODEL))) #after eliminating non significant variables in a step-wise manner 
  
  #calculate effect size for best model  
  #needs to be fixed underneath!
  effect_best<-lapply(summary(aov(as.formula(BEST_MODEL)))[[1]], effectsize::eta_squared) #calculate effect size for each anova 
  effect_best<-effect_best$` Response Y1`$Eta2_partial+effect_best$` Response Y2`$Eta2_partial #calcaulte  efefct size for the terms 
  
  #order after effect size
  ##### This still need to be fixed for random effect 
  variable_best<-paste("RESPONSE_Mat ~ ",paste(MANOVA_summary[[1]][["row.names"]][-length(MANOVA_summary[[1]][["row.names"]])],collapse="+"))
  variable_best_rand<-paste(variable_best,"+ Error(0+",random_term,")")
   formula_best<-as.formula(variable_best_rand)
  
  variable_effect_best<-cbind(MANOVA_summary[[1]][["row.names"]][-length(MANOVA_summary[[1]][["row.names"]])],effect_best)
  variable_effect_sorted_best <- variable_effect_best[order(-effect_best),]
  Effect_size_final<-variable_effect_sorted_best
  
  ####get the terms in right order  
  terms_sorted_best<- variable_effect_sorted_best[,1]
  
  variable_best_final<-paste("RESPONSE_Mat ~ ",paste(unlist(terms_sorted_best),collapse="+")) 
  variable_best_final_rand<-paste(variable_best_final,"+ Error(0+",random_term,")")
  MANOVA_summary_final<-summary(manova(as.formula(variable_best_final_rand)),intercept = T) }
  
  return(list(AIC_mean_df_collection,MANOVA_summary_final,Effect_size_final))
}



#all the test names 
name_vector<- c("Rayleigh","HR","MANOVA_intercept","MANOVA_random_intercept",
                "MANOVA_linear_interceptP","MANOVA_random_linear_interceptP",
                "MANOVA_linear_group_interceptP","MANOVA_random_linear_group_interceptP",
                "MANOVA_linear_covariateP","MANOVA_random_linear_covariateP",
                "MANOVA_linear_group_covariateP", "MANOVA_random_linear_group_covariateP",
                "MANOVA_linear_group_groupP","MANOVA_random_linear_group_groupP",
                "LM_P_intercept","LM_lin_P_intercept","LM_lin_P_lin",	"LM_lin_group_P_intercept",
                "LM_lin_group_P_lin",	"LM_lin_group_P_group")

#define colors 
Col <- rainbow(length(name_vector))
Col[c(3,4)] <-c("black","#800000")
Col[20] <-"purple"


#adding a "col" 
name_vector_col <- paste(name_vector, "col", sep="_")

#the name plu col is now associated with a color
colors_plot<-mapply(function(x,y) {assign(x, y)},name_vector_col,Col)



