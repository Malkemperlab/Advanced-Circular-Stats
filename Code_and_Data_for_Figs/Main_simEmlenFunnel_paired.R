#Figure for "simulated Emlen Funnel" results

#load background stuff 
source("Background_stuff_Manova_manuscript_fig.R")


#the followinbg is for "with reusing animals" 
#Figure2_1000 is just a placeholder!!! this one is missing! 
#load data 
# FigureC_10=read.csv("_EmlenFunnel_both0deg_10subsamples_n_3-10_its_10000_Dist_param_vm1_vm_m=0-0_conc=0.2-0.2.csv")
# FigureC_100=read.csv("_EmlenFunnel_both0deg_100subsamples_n_3-10_its_10000_Dist_param_vm1_vm_m=0-0_conc=0.2-0.2.csv")
FigureC_1000=read.csv("_EmlenFunnel_both0deg_1000subsamples_n_3-10_its_10000_Dist_param_vm1_vm_m=0-0_conc=0.2-0.2.csv")

# FigureD_10=read.csv("_EmlenFunnel_example_090deg_10subsample_n_3-10_its_10000_Dist_param_vm1_vm_m=0-90_conc=0.2-0.2.csv")
# FigureD_100=read.csv("_EmlenFunnel_example_090deg_100subsample_n_3-10_its_10000_Dist_param_vm1_vm_m=0-90_conc=0.2-0.2.csv")
FigureD_1000=read.csv("IDs_are_equal_n_Hypothetical_EmlenFunnel_example_090deg_1000subsample_n_3-10_its_10000_Dist_param_vm1_vm_m=0-90_conc=0.2-0.2.csv")


list_fig1 <- list(FigureC_1000,
                  FigureD_1000)

list_all<-list(list_fig1)

#prepare a list for  labeling for each panel
type<-c("No group difference\n(Paired)",
        "90° group differences\n(Paired)")

list_type<-list(type)

specs_Fig1 <- c("Main_EmlenFunnel_paired.pdf","Type 1 error","Power","Sample size (number IDs = n)",0.10,1)

specs <- rbind(specs_Fig1)

########

for (i in 1:nrow(specs)) {
  
  name_fig<-specs[,1][i]
  
  
  xlab<-specs[,4][i]; 
  
   
  FigurexA <-list_all[[i]][[1]]; FigurexB <-list_all[[i]][[2]];
  
  #textH<-ymax+ymax*0.15
  textn<-function() {ymax+ymax*0.1}
  
  pdf(name_fig, width=4, height=2)
  
  par(cex.axis =1,cex.lab=1.4,mar=c(3,3,3,0),
      mfrow = c(1, 2),mgp=c(1.3,0.2,0),tck=-0.015,cex=0.40)
  seglen=3;yesp=1;adjLet=-0.1;lineLet=1
  
  #########prepare legends 
  ################Legend_1 #########
  #need to add legend in first plot not extra and only one legend for all 
  col_legen_1=colors_plot[c("Rayleigh_col","MANOVA_linear_group_groupP_col",
                            "LM_lin_group_P_group_col")]
  
  legend_1= function(){legend("topleft", legend=c("Moores paired (IDs averaged)","MANOVA re group (group)",
                                                     "LMM group (group)"),
                              col=col_legen_1,cex = 1.1, lty=c(1,2,4),pch = 1,bty="n", #lty needs to be corrected
                              lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}
  
 
  ######################## Plotting #################################
  xi<-1
  ylab<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {specs[,2][i]} else {specs[,3][i]}; 
  ymax<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {as.numeric(specs[,5][i])} else {as.numeric(specs[,6][i])}; 
  
  xmax<-max(FigurexA$nseq)
  xmin<-min(FigurexA$nseq)
  #need to change the y-axis for the type 1 erro 
  plot(FigurexA$nseq,FigurexA$Moores_paired ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$MANOVA_groupP,col=col_legen_1[2],lty=2,type = "b",pch = 1,lwd = 0.6,)
#  lines(FigurexA$nseq,FigurexA$MANOVA_random_groupP ,col=col_legen_1[3],lty=3,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$LM_group_groupP,col=col_legen_1[3],lty=4,type = "b",pch = 1,lwd = 0.6)
  
  #axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))
  abline(h=0.05,lty=2,col="grey")
  
  text(x= xmin, y=textn(),font=2,cex=1.3,labels=list_type[[i]][1],pos=4,xpd=NA)
  
  mtext('A',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  legend_1()
  
  #############
  xi<-2
  ylab<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {specs[,2][i]} else {specs[,3][i]}; 
  ymax<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {as.numeric(specs[,5][i])} else {as.numeric(specs[,6][i])}; 
  
  xmax<-max(FigurexB$nseq)
  xmin<-min(FigurexB$nseq)
  #need to change the y-axis for the type 1 erro 
  plot(FigurexB$nseq,FigurexB$Moores_paired ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$MANOVA_groupP,col=col_legen_1[2],lty=2,type = "b",pch = 1,lwd = 0.6,)
 # lines(FigurexB$nseq,FigurexB$MANOVA_random_groupP ,col=col_legen_1[3],lty=3,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$LM_group_groupP,col=col_legen_1[3],lty=4,type = "b",pch = 1,lwd = 0.6)
  
  #axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))
  #abline(h=0.05,lty=2,col="grey")
  
  text(x= xmin, y=textn(),font=2,cex=1.3,labels=list_type[[i]][xi],pos=4,xpd=NA)
  
  mtext('B',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)

  
dev.off()
}



