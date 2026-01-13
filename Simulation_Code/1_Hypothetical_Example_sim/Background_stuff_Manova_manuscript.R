#background stuff for MANOVA paper 


################################################################################
# Moore's test for a common distribution for paired samples - slightly adapted from Pewsey et al. chapter 7
################################################################################

#helper function 1 
MooreRStats <- function(ldat1, ldat2) { 
  x <- cos(ldat1)-cos(ldat2); y <- sin(ldat1)-sin(ldat2)
  r <- sqrt((x*x)+(y*y)) ; Ranks <- rank(r)
  cosphi <- x/r; sinphi <- y/r 
  return(list(cosphi, sinphi, Ranks))
}

#helper function 2
MooreRTestStat <- function(cosphi, sinphi, Ranks){ 
  n <- length(cosphi)
  RbarC <- (1/n)*sum(Ranks*cosphi); RbarS <- (1/n)*sum(Ranks*sinphi)
  Rval <- sqrt(((RbarC*RbarC)+(RbarS*RbarS))/n) ; return(Rval)
}

#actual test - if 
#if R = T then R value and P value (in this order are given), defaults to only P-value
#defualt iteration is set to 999 can be changed by specifiying "NR" in the function 
MooreRTestRand <- function(ldat1, ldat2, NR = 999, R = F) {
  
  RoostingStats <- MooreRStats(ldat1, ldat2)
  cosphi <- RoostingStats[[1]] ; sinphi <- RoostingStats[[2]] ; Ranks <- RoostingStats[[3]]
  
  RObs <- MooreRTestStat(cosphi, sinphi, Ranks) ; nxtrm <- 1
  n <- length(cosphi)
  for (r in 1:NR) {
    cosphirand <- 0 ; sinphirand <- 0
    for (j in 1:n) {
      if (runif(1) < 0.5) {
        cosphirand[j] <- cosphi[j] ; sinphirand[j] <- sinphi[j] }
      else {
        cosphirand[j] <- -cosphi[j] ; sinphirand[j] <- -sinphi[j] } }
    RRand <- MooreRTestStat(cosphirand, sinphirand, Ranks)
    if (RRand >= RObs) { nxtrm <- nxtrm+1 }
  }
  pval <- nxtrm/(NR+1) ; if (R==T) {return(c(RObs, pval))} else {return(pval)}
}


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


#############################################################

#all the test names as they appear in the csv
#need to add watson and G-test and remove what is not needed 
name_vector<- c("Rayleigh","HR","MANOVA_intercept","MANOVA_random_intercept","MANOVA_linear_interceptP","MANOVA_random_linear_interceptP",
                "MANOVA_linear_covariateP","MANOVA_random_linear_covariateP","MANOVA_linear_group_interceptP","MANOVA_random_linear_group_interceptP",
                "MANOVA_linear_group_covariateP","MANOVA_random_linear_group_covariateP","MANOVA_linear_group_groupP","MANOVA_random_linear_group_groupP",
                "LM_P_intercept","LM_lin_P_intercept","LM_lin_P_lin",	"LM_lin_group_P_intercept",
                "LM_lin_group_P_lin",	"LM_lin_group_P_group",	"pMCMC_intercept",
                "pMCMC_lin_P_intercept",	"pMCMC_lin_P_lin",	"pMCMC_lin_group_P_intercept",	"pMCMC_lin_group_P_lin",
                "pMCMC_lin_group_P_group"
                )

#define colors 
Col <- rainbow(length(name_vector))
Col[c(3,4)] <-c("black","#800000")

#adding a "col" 
name_vector_col <- paste(name_vector, "col", sep="_")

#the name plu col is now associated with a color
colors_plot<-mapply(function(x,y) {assign(x, y)},name_vector_col,Col)
