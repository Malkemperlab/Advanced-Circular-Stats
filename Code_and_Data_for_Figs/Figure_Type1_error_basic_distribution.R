#need to be include all the type errors 

#load background stuff 
source("Background_stuff_Manova_manuscript_fig.R")

####Figure For simulation results 

Figure1A=read.csv("Type_1_error_n_5-100_its_10000_Dist_param_unif_unif_m=0_conc=0.csv")

#the panels are now arranged in a list (underneath thats done for each figure)
#list_fig1 <- list(Figure1A, Figure1B)
list_fig1 <- list(Figure1A)

#now all the list are again arranged in a list (a list of a list)
list_all<-list(list_fig1)

#prepare a list for  labeling for each panel
#type<-c("continous data","binned data")
#type<-c("continous data")
#list_type<-list(type)



#more text and parameters for each figure, the file name of the figure, the x-axis label, ...
# ... the y-axis label and the extent of the y-axis
specs_Fig1 <- c("Figure_type1_error_basic_stuff.pdf","Type 1 error (uniform distribution)","Sample size",0.15)

specs <- rbind(specs_Fig1) #binding the specs for the Figs


# 
# #the font type 
# library(extrafont) # in order to use Arial for figures 
# font_import() # run this line and type "yes", if fonts are not yet imported 
# loadfonts(device="pdf") 


######## now the loop can start - length of the loop depends on the number of figures we want to create


for (i in 1:nrow(specs)) {
  
  name_fig<-specs[,1][i]
  
  ylab<-specs[,2][i]; xlab<-specs[,3][i]; ymax=as.numeric(specs[,4][i])
  
  FigurexA <-list_all[[i]][[1]]; #FigurexB <-list_all[[i]][[2]]
  
  x<-FigurexA$nseq;xmax<-max(x);xmin<-max(x)-(0.2*max(x));xrealmin<-min(x)
  
  textH<-ymax+ymax*0.15
  textn<-ymax+ymax*0.06
  
  
  pdf(name_fig, width=3, height=2) #change with according to number of panels 

  par(cex.axis = 1,cex.lab=1.4,mar=c(3,3,3,0),
      #mfrow = c(1, 2),
      mgp=c(1.2,0.3,0),tck=-0.015,cex=0.38) #change mfrow accrding to number of panels 
  seglen=3;yesp=1;adjLet=-0.1;lineLet=1
  
  col_legen_1=colors_plot[c("Rayleigh_col","HR_col","MANOVA_intercept_col","MANOVA_random_intercept_col",
                            "LM_P_intercept_col", "Model_comp_col")]
  
  legend_1= function(){legend("topright", legend=c("Rayleigh","HR","MANOVA (intercept)",
                                                      "simMANOVA (intercept)","LM (intercept)", "CircMLE"
  ),
  col=col_legen_1,cex = 1, lty=c(1:6),pch = 1,bty="n",
  lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}
 
  # legend_2= function(){legend("topright", legend=c("MANOVA (intercept)","Rayleigh","HR",
  #                                                  "simMANOVA (intercept)","LM (intercept)",
  #                                                  "MCMC (intercept)"),
  # col=col_legen_1,cex = 1, lty=c(1:6,1,2),pch = 2,bty="n",
  # lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}
  # 
   ######################## Plotting #################################
  
  ############# identical Fig 1A##### 
  
  xmax<-max(FigurexA$nseq)
  xmin<-min(FigurexA$nseq)
  
   plot(FigurexA$nseq,FigurexA$MANOVA,col=colors_plot["MANOVA_intercept_col"],type = 'b',lty=3,pch = 1,bty="n" ,
       ylab = ylab,xlab="Sample size",xlim = c(5,xmax),ylim = c(0,ymax),lwd = 0.6,xaxp = c(10, 100,9))
  lines(FigurexA$nseq,FigurexA$Rayleigh,col=colors_plot["Rayleigh_col"],lty=1,type = "b",pch = 1,lwd = 0.6,)
  lines(FigurexA$nseq,FigurexA$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$MANOVA_random,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$LM,lty=5,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
  lines(FigurexA$nseq,FigurexA$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)
  
#  text(x=if (xmax>5) xrealmin else xmax, y=textH,labels="H0: Distributions are identical",pos=4,xpd=NA)
 # text(x= xmin, y=textn,labels=list_type[[i]][1],pos=4,xpd=NA)
  abline(h=0.05,lty=2,col="grey")
  
  #mtext('A',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  
  legend_1()
 # mtext('A',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  
  
  #############same mean Fig 1A#######
  ######tests for mean direction 
  # 
  # xmax<-max(FigurexB$nseq)
  # xmin<-min(FigurexB$nseq)
  # 
  # plot(FigurexB$nseq,FigurexB$MANOVA,col=colors_plot["MANOVA_intercept_col"],lty=1,type = "b",pch = 2,bty="n" ,
  #      xlab="Sample size",ylab="",xlim =c(5,xmax),ylim = c(0,ymax),lwd = 0.6,xaxp = c(10, 100,9))
  # lines(FigurexB$nseq,FigurexB$Rayleigh,col=colors_plot["Rayleigh_col"],lty=2,type = "b",pch = 2,lwd = 0.6)
  # lines(FigurexB$nseq,FigurexB$HR,col=colors_plot["HR_col"],lty=3,type = "b",pch = 2,lwd = 0.6)
  # lines(FigurexB$nseq,FigurexB$MANOVA_random,col=colors_plot["MANOVA_random_intercept_col"],lty=4,type = "b",pch = 2,lwd = 0.6)
  # 
  # legend_2()
  # text(x= xmin, y=textn,labels=list_type[[i]][2],pos=4,xpd=NA)
  # mtext('B',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
  # abline(h=0.05,lty=2,col="grey") 
 
# mtext('A',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)

  dev.off() #thsi wrotes the plot as a pdf 
  
}


