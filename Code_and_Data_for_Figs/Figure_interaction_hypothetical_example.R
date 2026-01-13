###script for figure for interaction effects 

#load background stuff 
source("Background_stuff_Manova_manuscript_fig.R")

#Plotting MANOVA examples 

#load data - change to 10000 in script 
Figure1A=read.csv("Hypex_k_Int_less_n_1-10_its_10000_Dist_param_vm1_vm_m=180-180_conc=0-1.csv")

Figure1B=read.csv("Hypex_dir_Int_0_180_n_1-10_its_10000_Dist_param_vm1_vm_m=180-360_conc=1-1.csv")

Figure1C=read.csv("Hypex__k_Int_less_0_180_n_1-10_its_10000_Dist_param_vm1_vm_m=180-360_conc=0-1.csv")

list_fig1 <- list(Figure1A, Figure1B,Figure1C)
#list_fig2 <- list(Figure2A, Figure2B,Figure2C,Figure2D)

list_all<-list(list_fig1)

type<-c("Concentration interaction\n(g1: k=0-1, g2: k=1-0)",
        "Direction interaction\n(g1: 180°-360°, g2: 360°-180°)",
        "Concentration by direction interact.\n(g1: k=0-1 & 180°-360°,\n g2: k=1-0 & 360°-180°)")

list_type<-list(type[1:3])

specs_Fig1 <- c("Figure_interaction_examples_Oct2024.pdf","Power","Sample size",1)

specs <- rbind(specs_Fig1)

########

for (i in 1:nrow(specs)) {
  
  name_fig<-specs[,1][i]
  
  ylab<-specs[,2][i]; xlab<-specs[,3][i]; ymax=as.numeric(specs[,4][i])
  
  FigurexA <-list_all[[i]][[1]]; FigurexB <-list_all[[i]][[2]];FigurexC <-list_all[[i]][[3]]
 # FigurexD <-list_all[[i]][[4]]
  
  
  textH<-ymax+ymax*0.15
  textn<-ymax+ymax*0.1
  
  
  pdf(name_fig, width=8, height=6)
  
  layout_matrix <- matrix(seq(1:(4*4)), nrow = 4, byrow = TRUE)
  
  layout_widths <- c(1.2, 1, 1,1)  # First column wider
  layout_heights <- c(2,2.2,2.2,2.2,2.2)  # first row labels, the column heading
  
  layout(layout_matrix, widths = layout_widths, heights = layout_heights)
  
  
  par(cex.axis = 1,cex.lab=1.4,mar=c(3,3,2,0),
     mgp=c(1,0.1,0),tck=-0.015,cex=0.35)
  seglen=3;yesp=1;adjLet=-0.1;lineLet=1
  
  #########prepare legends 
  ################Legend_1 #########
  #col_legen_1=colors_plot[c(name_vector_col)]
  
  legend_1= function(){
    
    
    name_vector_col<-c("Rayleigh_col","HR_col","Model_comp_col","MANOVA_intercept_col","MANOVA_random_intercept_col" , 
                       "MANOVA_linear_group_interceptP_col","MANOVA_random_linear_group_interceptP_col",
                       "LM_P_intercept_col","LM_lin_group_P_intercept_col") 
    
    legend("bottomleft", legend=c("Rayleigh","HR","CircMLE","MANOVA (intercept)","simMANOVA (intercept)" , 
                                  "MANOVA interaction  (intercept)","simMANOVA interaction (intercept)",
                                  "LM (intercept)","LM interaction (intercept)"),
           
           
           col=colors_plot[c(name_vector_col)],
           
           cex = 1.3, lty=c(1:2,6,3:6,1:5),pch = 1,bty="n",
           lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}
  
  legend_2= function(){
    
    name_vector_col<-c(
                       "MANOVA_linear_group_covariateP_col","MANOVA_random_linear_group_covariateP_col",
                       "LM_lin_group_P_lin_col") 
    
    
    legend("bottomleft", legend=c(
                                  "MANOVA interaction (linear)","simMANOVA Interaction (linear)",
                                  "LM interaction (linear)"),
           
           col=colors_plot[c(name_vector_col)],
           cex = 1.3, lty=c(1:3),pch = 1,bty="n",
           lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}
  
  legend_3= function(){
    name_vector_col<-c("Rayleigh_col","HR_col","MANOVA_linear_group_groupP_col","MANOVA_random_linear_group_groupP_col",
                       "LM_lin_group_P_group_col")
    
    legend("bottomleft", legend=c("Watson","G-test","MANOVA interaction (group)","simMANOVA interaction (group)",
                                  "LM interaction (group)" ),
           col=colors_plot[name_vector_col],cex = 1.3, lty=c(1:5),pch = 1,bty="n",
           lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}
  
  legend_4= function(){
    name_vector_col<-c("MANOVA_linear_group_groupP_col","MANOVA_random_linear_group_groupP_col",
                       "LM_lin_group_P_group_col")
    
    legend("bottomleft", legend=c("MANOVA interaction (interaction)","simMANOVA interaction (interaction)",
                                  "LM interaction (interaction)" ),
           col=colors_plot[name_vector_col],cex = 1.3, lty=c(1:5),pch = 1,bty="n",
           lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}
  
  ######################## Plotting #################################
  par(mar=c(2,5,0,0))
  plot.new();legend_1()
  
  par(mar=c(2,4,0,0))
  plot.new();legend_2()
  plot.new();legend_3()
  plot.new();legend_4()
  
  ################
  #Figure1A - erster Panel
  ################
  par(mar=c(2,7,3,0))
  xmax<-max(FigurexA$nseq)
  xmin<-min(FigurexA$nseq)
  plot(FigurexA$nseq,FigurexA$Rayleigh,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexA$nseq,FigurexA$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

  
  lines(FigurexA$nseq,FigurexA$MANOVA_intercept,col=colors_plot["MANOVA_intercept_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$MANOVA_random_intercept,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$MANOVA_linear_group_int_interceptP,col=colors_plot["MANOVA_linear_group_interceptP_col"],lty=5,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_group_int_interceptP,lty=6,col=colors_plot["MANOVA_random_linear_group_interceptP_col"],type = "b",pch = 1,lwd = 0.6)
  
  lines(FigurexA$nseq,FigurexA$LM_P_intercept,lty=1,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$LM_lin_group_int_P_intercept,lty=2,col=colors_plot["LM_lin_group_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  
  text(x = xmin-2.2, y = textn-1.1, labels = list_type[[i]][1], adj = 0, xpd = NA, srt = 90, font = 2,xpd=NA,cex=1.4)
  text(x= xmin+2, y=textn+0.1,labels="Orientation effect",adj = 0,xpd=NA,font = 2,cex = 1.6)
  
  
  mtext('A',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  
  ################  Fig 1A - 2ter panel #####
  par(mar=c(2,2,3,0))
  xmax<-max(FigurexA$nseq)
  xmin<-min(FigurexA$nseq)
  plot(FigurexA$nseq,FigurexA$MANOVA_linear_group_int_covariateP,col=colors_plot["MANOVA_linear_group_covariateP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_group_int_covariateP,col=colors_plot["MANOVA_random_linear_group_covariateP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexA$nseq,FigurexA$LM_lin_group_int_P_lin,lty=3,col=colors_plot["LM_lin_group_P_lin_col"],type = "b",pch = 1,lwd = 0.6)
  
  text(x= xmin+2, y=textn+0.1,labels="Linear effect",adj = 0,xpd=NA,font = 2,cex = 1.6)
  

  ################ Fig 1A - 3ter panel #####
  xmax<-max(FigurexA$nseq)
  xmin<-min(FigurexA$nseq)
  plot(FigurexA$nseq,FigurexA$Watson_two_sample,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  
  lines(FigurexA$nseq,FigurexA$G.test,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexA$nseq,FigurexA$MANOVA_linear_group_int_groupP,col=colors_plot["MANOVA_linear_group_groupP_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_group_int_groupP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=4,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexA$nseq,FigurexA$LM_lin_group_int_P_group,col=colors_plot["LM_lin_group_P_group_col"],lty=5,type = "b",pch = 1,lwd = 0.6,)
  
  text(x= xmin+2, y=textn+0.1,labels="Group effect",adj = 0,xpd=NA,font = 2,cex = 1.6)
  
  
  ################ Fig 1A - 4ter panel #####
  xmax<-max(FigurexA$nseq)
  xmin<-min(FigurexA$nseq)

  plot(FigurexA$nseq,FigurexA$MANOVA_linear_group_int_intP,col=colors_plot["MANOVA_linear_group_groupP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  
  lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_group_int_intP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexA$nseq,FigurexA$LM_lin_group_int_P_int,col=colors_plot["LM_lin_group_P_group_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)
  
  text(x= xmin+2, y=textn+0.1,labels="Interaction effect",adj = 0,xpd=NA,font = 2,cex = 1.6)
  
  #########
  ###########
  ################
  #Figure1B - erster Panel
  ################
  xmax<-max(FigurexB$nseq)
  xmin<-min(FigurexB$nseq)
  par(mar=c(2,7,2,0))

  plot(FigurexB$nseq,FigurexB$Rayleigh,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexB$nseq,FigurexB$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)
  
  
  lines(FigurexB$nseq,FigurexB$MANOVA_intercept,col=colors_plot["MANOVA_intercept_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$MANOVA_random_intercept,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$MANOVA_linear_group_int_interceptP,col=colors_plot["MANOVA_linear_group_interceptP_col"],lty=5,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_group_int_interceptP,lty=6,col=colors_plot["MANOVA_random_linear_group_interceptP_col"],type = "b",pch = 1,lwd = 0.6)
  
  lines(FigurexB$nseq,FigurexB$LM_P_intercept,lty=1,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$LM_lin_group_int_P_intercept,lty=2,col=colors_plot["LM_lin_group_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  
  text(x = xmin-2.2, y = textn-1.1, labels = list_type[[i]][2], adj = 0, xpd = NA, srt = 90, font = 2,xpd=NA,cex=1.4)
  
  mtext('B',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  
  ################  Fig 1B - 2ter panel #####
  xmax<-max(FigurexB$nseq)
  xmin<-min(FigurexB$nseq)
  par(mar=c(2,2,2,0))
  plot(FigurexB$nseq,FigurexB$MANOVA_linear_group_int_covariateP,col=colors_plot["MANOVA_linear_group_covariateP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_group_int_covariateP,col=colors_plot["MANOVA_random_linear_group_covariateP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexB$nseq,FigurexB$LM_lin_group_int_P_lin,lty=3,col=colors_plot["LM_lin_group_P_lin_col"],type = "b",pch = 1,lwd = 0.6)
  
  
  ################ Fig 1B - 3ter panel #####
  xmax<-max(FigurexB$nseq)
  xmin<-min(FigurexB$nseq)
  plot(FigurexB$nseq,FigurexB$Watson_two_sample,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  
  lines(FigurexB$nseq,FigurexB$G.test,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexB$nseq,FigurexB$MANOVA_linear_group_int_groupP,col=colors_plot["MANOVA_linear_group_groupP_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_group_int_groupP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=4,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexB$nseq,FigurexB$LM_lin_group_int_P_group,col=colors_plot["LM_lin_group_P_group_col"],lty=5,type = "b",pch = 1,lwd = 0.6,)
  
  
  ################ Fig 1B - 4ter panel #####
  xmax<-max(FigurexB$nseq)
  xmin<-min(FigurexB$nseq)
  plot(FigurexB$nseq,FigurexB$MANOVA_linear_group_int_intP,col=colors_plot["MANOVA_linear_group_groupP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  
  lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_group_int_intP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexB$nseq,FigurexB$LM_lin_group_int_P_int,col=colors_plot["LM_lin_group_P_group_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)
  
  ##############
  #Figure1C - erster Panel
  ################
  xmax<-max(FigurexC$nseq)
  xmin<-min(FigurexC$nseq)
  par(mar=c(2,7,2,0))
  plot(FigurexC$nseq,FigurexC$Rayleigh,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexC$nseq,FigurexC$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
 
  lines(FigurexC$nseq,FigurexC$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)
  
  
  lines(FigurexC$nseq,FigurexC$MANOVA_intercept,col=colors_plot["MANOVA_intercept_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexC$nseq,FigurexC$MANOVA_random_intercept,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  lines(FigurexC$nseq,FigurexC$MANOVA_linear_group_int_interceptP,col=colors_plot["MANOVA_linear_group_interceptP_col"],lty=5,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_group_int_interceptP,lty=6,col=colors_plot["MANOVA_random_linear_group_interceptP_col"],type = "b",pch = 1,lwd = 0.6)
  
  lines(FigurexC$nseq,FigurexC$LM_P_intercept,lty=1,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  lines(FigurexC$nseq,FigurexC$LM_lin_group_int_P_intercept,lty=2,col=colors_plot["LM_lin_group_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  
  text(x = xmin-2.2, y = textn-1.1, labels = list_type[[i]][3], adj = 0, xpd = NA, srt = 90, font = 2,xpd=NA,cex=1.4)
  
  mtext('C',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  
  ################  Fig 1C - 2ter panel #####
  xmax<-max(FigurexC$nseq)
  xmin<-min(FigurexC$nseq)
  par(mar=c(2,2,2,0))
  plot(FigurexC$nseq,FigurexC$MANOVA_linear_group_int_covariateP,col=colors_plot["MANOVA_linear_group_covariateP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_group_int_covariateP,col=colors_plot["MANOVA_random_linear_group_covariateP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexC$nseq,FigurexC$LM_lin_group_int_P_lin,lty=3,col=colors_plot["LM_lin_group_P_lin_col"],type = "b",pch = 1,lwd = 0.6)
  
  

  ################ Fig 1C - 3ter panel #####
  xmax<-max(FigurexC$nseq)
  xmin<-min(FigurexC$nseq)
  plot(FigurexC$nseq,FigurexC$Watson_two_sample,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  
  lines(FigurexC$nseq,FigurexC$G.test,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexC$nseq,FigurexC$MANOVA_linear_group_int_groupP,col=colors_plot["MANOVA_linear_group_groupP_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_group_int_groupP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=4,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexC$nseq,FigurexC$LM_lin_group_int_P_group,col=colors_plot["LM_lin_group_P_group_col"],lty=5,type = "b",pch = 1,lwd = 0.6,)
  
  
  ################ Fig 1C - 4ter panel #####
  xmax<-max(FigurexC$nseq)
  xmin<-min(FigurexC$nseq)
  plot(FigurexC$nseq,FigurexC$MANOVA_linear_group_int_intP,col=colors_plot["MANOVA_linear_group_groupP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  
  lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_group_int_intP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
  
  lines(FigurexC$nseq,FigurexC$LM_lin_group_int_P_int,col=colors_plot["LM_lin_group_P_group_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)
  
  
  ##############

  
  dev.off()
  
}
