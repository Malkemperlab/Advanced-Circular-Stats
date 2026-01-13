#Figure for "simulated Emlen Funnel" results

#load background stuff 
source("Background_stuff_Manova_manuscript_fig.R")

FigureA_10=read.csv("IDs_are_n_times2_unpaired_TESTHypothetical_EmlenFunnel_both0deg_10subsamples_n_3-10_its_10000_Dist_param_vm1_vm_m=0_conc=0.2.csv")
FigureA_100=read.csv("IDs_are_n_times2_unpaired_TESTHypothetical_EmlenFunnel_both0deg_100subsamples_n_3-10_its_10000_Dist_param_vm1_vm_m=0_conc=0.2.csv")
FigureA_1000=read.csv("IDs_are_n_times2_unpaired_TESTHypothetical_EmlenFunnel_both0deg_1000subsamples_n_3-10_its_10000_Dist_param_vm1_vm_m=0_conc=0.2.csv")

FigureB_10=read.csv("IDs_are_n_times2_unpaired_TESTHypothetical_EmlenFunnel_example_090deg_10subsample_n_3-10_its_10000_Dist_param_vm1_vm_m=0_conc=0.2.csv")
FigureB_100=read.csv("IDs_are_n_times2_unpaired_TESTHypothetical_EmlenFunnel_example_090deg_100subsample_n_3-10_its_10000_Dist_param_vm1_vm_m=0_conc=0.2.csv")
FigureB_1000=read.csv("IDs_are_n_times2_unpaired_TESTHypothetical_EmlenFunnel_example_090deg_1000subsample_n_3-10_its_10000_Dist_param_vm1_vm_m=0_conc=0.2.csv")


list_fig1 <- list(FigureA_10, FigureA_100,FigureA_1000,
                  FigureB_10, FigureB_100,FigureB_1000)

list_all<-list(list_fig1)

#prepare a list for  labeling for each panel
type<-c("No group difference\n(10 subsamples)",
        "No group difference\n(100 subsamples)",
        "No group difference\n(1000 subsamples)",
        "90° group differences\n(10 subsamples)",
        "90° group differences\n(100 subsamples)",
        "90° group differences\n(1000 subsamples)")

list_type<-list(type)

specs_Fig1 <- c("Figure_EmlenFunnel_unpaired.pdf","Type 1 error","Power","Sample size per treatment",0.15,1)

specs <- rbind(specs_Fig1)

########

for (i in 1:nrow(specs)) {
  
  name_fig<-specs[,1][i]
  
  
  xlab<-specs[,4][i]; 
  
  
  FigurexA <-list_all[[i]][[1]]; FigurexB <-list_all[[i]][[2]];FigurexC <-list_all[[i]][[3]]
  FigurexD <-list_all[[i]][[4]]; FigurexE <-list_all[[i]][[5]]; FigurexF <-list_all[[i]][[6]]
  
  #textH<-ymax+ymax*0.15
  textn<-function() {ymax+ymax*0.1}
  
  pdf(name_fig, width=6, height=4)
  
  par(cex.axis =1,cex.lab=1.4,mar=c(3,3,3,0),
      mfrow = c(2, 3),mgp=c(1.3,0.2,0),tck=-0.015,cex=0.45)
  seglen=3;yesp=1;adjLet=-0.1;lineLet=1
  
  #########prepare legends 
  ################Legend_1 #########
  #need to add legend in first plot not extra and only one legend for all 
  col_legen_1=colors_plot[c("Rayleigh_col","HR_col","MANOVA_linear_group_groupP_col","MANOVA_random_linear_group_groupP_col",
                            "LM_lin_group_P_group_col")]
  
  legend_1= function(){legend("topright", legend=c("Watson","G-test","MANOVA re group (group)","simMANOVA re group (group)",
                                                  "LMM group (group)"),
                              col=col_legen_1,cex = 1.1, lty=c(1:5),pch = 1,bty="n",
                              lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}
  
  
  
  ######################## Plotting #################################
  xi<-1
  ylab<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {specs[,2][i]} else {specs[,3][i]}; 
  ymax<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {as.numeric(specs[,5][i])} else {as.numeric(specs[,6][i])}; 
  
  xmax<-max(FigurexA$nseq)
  xmin<-min(FigurexA$nseq)
  #need to change the y-axis for the type 1 erro 
  plot(FigurexA$nseq,FigurexA$Watson_two_sample ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$G.test,col=col_legen_1[2],lty=2,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexA$nseq,FigurexA$MANOVA_groupP,col=col_legen_1[3],lty=3,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexA$nseq,FigurexA$MANOVA_random_groupP ,col=col_legen_1[4],lty=4,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$LM_group_groupP,col=col_legen_1[5],lty=5,type = "b",pch = 1,lwd = 0.6)
  
  #axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))
  abline(h=0.05,lty=2,col="grey")
  
  text(x= xmin, y=textn(),font=2,cex=1.3,labels=list_type[[i]][xi],pos=4,xpd=NA)
  
  mtext('A',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  legend_1()
  
  ############
  xi<-2
  ylab<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {specs[,2][i]} else {specs[,3][i]}; 
  ymax<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {as.numeric(specs[,5][i])} else {as.numeric(specs[,6][i])}; 
  
  xmax<-max(FigurexB$nseq)
  xmin<-min(FigurexB$nseq)
  #need to change the y-axis for the type 1 erro 
  plot(FigurexB$nseq,FigurexB$Watson_two_sample ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$G.test,col=col_legen_1[2],lty=2,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexB$nseq,FigurexB$MANOVA_groupP,col=col_legen_1[3],lty=3,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexB$nseq,FigurexB$MANOVA_random_groupP ,col=col_legen_1[4],lty=4,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexB$nseq,FigurexB$LM_group_groupP,col=col_legen_1[5],lty=5,type = "b",pch = 1,lwd = 0.6)
  
  #axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))
  abline(h=0.05,lty=2,col="grey")
  
  text(x= xmin, y=textn(),font=2,cex=1.3,labels=list_type[[i]][xi],pos=4,xpd=NA)
  
  mtext('B',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)

  
  ##############
  
  xi<-3
  ylab<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {specs[,2][i]} else {specs[,3][i]}; 
  ymax<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {as.numeric(specs[,5][i])} else {as.numeric(specs[,6][i])}; 
  
  xmax<-max(FigurexC$nseq)
  xmin<-min(FigurexC$nseq)
  #need to change the y-axis for the type 1 erro 
  plot(FigurexC$nseq,FigurexC$Watson_two_sample ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexC$nseq,FigurexC$G.test,col=col_legen_1[2],lty=2,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexC$nseq,FigurexC$MANOVA_groupP,col=col_legen_1[3],lty=3,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexC$nseq,FigurexC$MANOVA_random_groupP ,col=col_legen_1[4],lty=4,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexC$nseq,FigurexC$LM_group_groupP,col=col_legen_1[5],lty=5,type = "b",pch = 1,lwd = 0.6)
  
  #axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))
  abline(h=0.05,lty=2,col="grey")
  
  text(x= xmin, y=textn(),font=2,cex=1.3,labels=list_type[[i]][xi],pos=4,xpd=NA)
  
  mtext('C',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)

  ##############
  
  xi<-4
  ylab<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {specs[,2][i]} else {specs[,3][i]}; 
  ymax<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {as.numeric(specs[,5][i])} else {as.numeric(specs[,6][i])}; 
  
  xmax<-max(FigurexD$nseq)
  xmin<-min(FigurexD$nseq)
  #need to change the y-axis for the type 1 erro 
  plot(FigurexD$nseq,FigurexD$Watson_two_sample ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexD$nseq,FigurexD$G.test,col=col_legen_1[2],lty=2,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexD$nseq,FigurexD$MANOVA_groupP,col=col_legen_1[3],lty=3,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexD$nseq,FigurexD$MANOVA_random_groupP ,col=col_legen_1[4],lty=4,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexD$nseq,FigurexD$LM_group_groupP,col=col_legen_1[5],lty=5,type = "b",pch = 1,lwd = 0.6)
  
  #axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))
  #abline(h=0.05,lty=2,col="grey")
  
  text(x= xmin, y=textn(),font=2,cex=1.3,labels=list_type[[i]][xi],pos=4,xpd=NA)
  
  mtext('D',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  
  ##############
  
  xi<-5
  ylab<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {specs[,2][i]} else {specs[,3][i]}; 
  ymax<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {as.numeric(specs[,5][i])} else {as.numeric(specs[,6][i])}; 
  
  xmax<-max(FigurexE$nseq)
  xmin<-min(FigurexE$nseq)
  #need to change the y-axis for the type 1 erro 
  plot(FigurexE$nseq,FigurexE$Watson_two_sample ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexE$nseq,FigurexE$G.test,col=col_legen_1[2],lty=2,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexE$nseq,FigurexE$MANOVA_groupP,col=col_legen_1[3],lty=3,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexE$nseq,FigurexE$MANOVA_random_groupP ,col=col_legen_1[4],lty=4,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexE$nseq,FigurexE$LM_group_groupP,col=col_legen_1[5],lty=5,type = "b",pch = 1,lwd = 0.6)
  
  #axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))
  #abline(h=0.05,lty=2,col="grey")
  
  text(x= xmin, y=textn(),font=2,cex=1.3,labels=list_type[[i]][xi],pos=4,xpd=NA)
  
  mtext('E',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  
  ##############
  
  xi<-6
  ylab<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {specs[,2][i]} else {specs[,3][i]}; 
  ymax<-if (grepl("No group difference", list_type[[i]][xi], fixed = TRUE)) {as.numeric(specs[,5][i])} else {as.numeric(specs[,6][i])}; 
  
  xmax<-max(FigurexF$nseq)
  xmin<-min(FigurexF$nseq)
  #need to change the y-axis for the type 1 erro 
  plot(FigurexF$nseq,FigurexF$Watson_two_sample ,col=col_legen_1[1],type = 'b',lty=1,pch = 1,bty="n" ,
       ylab = ylab,xlab=xlab,xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6)
  lines(FigurexF$nseq,FigurexF$G.test,col=col_legen_1[2],lty=2,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexF$nseq,FigurexF$MANOVA_groupP,col=col_legen_1[3],lty=3,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexF$nseq,FigurexF$MANOVA_random_groupP ,col=col_legen_1[4],lty=4,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexF$nseq,FigurexF$LM_group_groupP,col=col_legen_1[5],lty=5,type = "b",pch = 1,lwd = 0.6)
  
  #axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))
  #abline(h=0.05,lty=2,col="grey")
  
  text(x= xmin, y=textn(),font=2,cex=1.3,labels=list_type[[i]][xi],pos=4,xpd=NA)
  
  mtext('F',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  
  dev.off()
}



