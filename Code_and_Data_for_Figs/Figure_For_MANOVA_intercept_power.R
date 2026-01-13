#Plotting Comp Dist
#load background stuff 
source("Background_stuff_Manova_manuscript_fig.R")

#Figure 1 (VM - identical distributions): A: (10,10), B: (20,20), C: (50,50),D: (20,30), E: (10,50), 

#load data 
Figure1A=read.csv("Power_n_5-100_its_10000_Dist_param_vm_vm_m=0_conc=1.csv")
Figure1B=read.csv("Power_n_5-100_its_10000_Dist_param_wsn_wsn_m=0_conc=2.csv")

Figure1C=read.csv("Power_n_10-200_its_10000_Dist_param_axial_vm_m=0_conc=1.csv")
Figure1D=read.csv("Power_n_10-200_its_10000_Dist_param_bim_vm_m=0_conc=1.csv")

Figure1E=read.csv("Power_n_15-300_its_10000_Dist_param_trim_sym_vm_m=0_conc=1.csv")
Figure1F=read.csv("Power_n_15-300_its_10000_Dist_param_trim_asym_vm_m=0_conc=1.csv")

list_fig1 <- list(Figure1A, Figure1B,Figure1C,Figure1D,Figure1E,Figure1F)

list_all<-list(list_fig1)

#prepare a list for  labeling for each panel
type<-c("unimodal VM","unimodal WSN","symmetrical bimodal",
        "asymmetrical bimodal",'symmetrical trimodal','asymmetrical trimodal')
# list_type<-list(type,type)
list_type<-list(type)

specs_Fig1 <- c("Figure_power_continous1.pdf","Power","Sample size",1)

specs <- rbind(specs_Fig1)


#########Arial ##########
# library(extrafont) # in order to use Arial for figures 
# font_import() # run this line and type "yes", if fonts are not yet imported 
# loadfonts(device="pdf") 
########

for (i in 1:nrow(specs)) {
  
name_fig<-specs[,1][i]

ylab<-specs[,2][i]; xlab<-specs[,3][i]; ymax=as.numeric(specs[,4][i])

FigurexA <-list_all[[i]][[1]]; FigurexB <-list_all[[i]][[2]];FigurexC <-list_all[[i]][[3]]
FigurexD <-list_all[[i]][[4]]; FigurexE <-list_all[[i]][[5]];FigurexF <-list_all[[i]][[6]]

textH<-ymax+ymax*0.15
textn<-ymax+ymax*0.04


pdf(name_fig, width=5, height=6)

par(cex.axis =1,cex.lab=1.4,mar=c(3,3,3,0),
    mfrow = c(3, 2),mgp=c(1.2,0.2,0),tck=-0.015,cex=0.45)
seglen=3;yesp=1;adjLet=-0.1;lineLet=1

#########prepare legends 
################Legend_1 #########

legend_1= function(){
  
  name_vector_col<-c("Rayleigh_col","HR_col","MANOVA_intercept_col","MANOVA_random_intercept_col" , 
                     "LM_P_intercept_col", "Model_comp_col") 
  
  
  legend("bottomright", legend=c("Rayleigh","HR","MANOVA (intercept)",
                                     "simMANOVA (intercept)","LM (intercept)","CircMLE"
                            ),
col=colors_plot[name_vector_col],cex = 1.3, lty=c(1:6),pch = 1,bty="n",
lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}

######################## Plotting #################################
# par(oma = c(4, 1, 1, 1))


############# identical Fig 1A##### 
#First test for identical
xmax<-max(FigurexA$nseq)
xmin<-min(FigurexA$nseq)
plot(FigurexA$nseq,FigurexA$MANOVA,col=colors_plot["MANOVA_intercept_col"],type = 'b',lty=3,pch = 1,bty="n" ,
     ylab = ylab,xlab="Sample size",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xaxt="n")
lines(FigurexA$nseq,FigurexA$Rayleigh,col=colors_plot["Rayleigh_col"],lty=1,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexA$nseq,FigurexA$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6)
lines(FigurexA$nseq,FigurexA$MANOVA_random,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexA$nseq,FigurexA$LM,lty=5,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexA$nseq,FigurexA$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))

text(x= xmin, y=textn,labels=list_type[[i]][1],pos=4,xpd=NA,cex=1.4,font=2)

mtext('A',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
legend_1()

################ identical Fig 1B #####
#First test for identical
xmax<-max(FigurexB$nseq)
xmin<-min(FigurexB$nseq)

plot(FigurexB$nseq,FigurexB$MANOVA,col=colors_plot["MANOVA_intercept_col"],type = 'b',lty=3,pch = 1,bty="n" ,
     ylab = ylab,xlab="Sample size",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xaxt="n")
lines(FigurexB$nseq,FigurexB$Rayleigh,col=colors_plot["Rayleigh_col"],lty=1,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexB$nseq,FigurexB$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6)
lines(FigurexB$nseq,FigurexB$MANOVA_random,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexB$nseq,FigurexB$LM,lty=5,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexB$nseq,FigurexB$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))

mtext('B',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
text(x= xmin, y=textn,labels=list_type[[i]][2],pos=4,xpd=NA,cex=1.4,font=2)


################ identical Fig 1C #####
#First test for identical
xmax<-max(FigurexC$nseq)
xmin<-min(FigurexC$nseq)

plot(FigurexC$nseq,FigurexC$MANOVA,col=colors_plot["MANOVA_intercept_col"],type = 'b',lty=3,pch = 1,bty="n" ,
     ylab = ylab,xlab="Sample size",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xaxt="n")
lines(FigurexC$nseq,FigurexC$Rayleigh,col=colors_plot["Rayleigh_col"],lty=1,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexC$nseq,FigurexC$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6)
lines(FigurexC$nseq,FigurexC$MANOVA_random,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexC$nseq,FigurexC$LM,lty=5,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexC$nseq,FigurexC$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))


mtext('C',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
text(x=xmin, y=textn,labels=list_type[[i]][3],pos=4,xpd=NA,cex=1.4,font=2)


################ identical Fig 1C #####
#First test for identical
xmax<-max(FigurexD$nseq)
xmin<-min(FigurexD$nseq)

plot(FigurexD$nseq,FigurexD$MANOVA,col=colors_plot["MANOVA_intercept_col"],type = 'b',lty=3,pch = 1,bty="n" ,
     ylab = ylab,xlab="Sample size",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xaxt="n")
lines(FigurexD$nseq,FigurexD$Rayleigh,col=colors_plot["Rayleigh_col"],lty=1,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexD$nseq,FigurexD$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6)
lines(FigurexD$nseq,FigurexD$MANOVA_random,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexD$nseq,FigurexD$LM,lty=5,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexD$nseq,FigurexD$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))


mtext('D',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
text(x=xmin, y=textn,labels=list_type[[i]][4],pos=4,xpd=NA,cex=1.4,font=2)


################ identical Fig 1C #####
#First test for identical

xmax<-max(FigurexE$nseq)
xmin<-min(FigurexE$nseq)

plot(FigurexE$nseq,FigurexE$MANOVA,col=colors_plot["MANOVA_intercept_col"],type = 'b',lty=3,pch = 1,bty="n" ,
     ylab = ylab,xlab="Sample size",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xaxt="n")
lines(FigurexE$nseq,FigurexE$Rayleigh,col=colors_plot["Rayleigh_col"],lty=1,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexE$nseq,FigurexE$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6)
lines(FigurexE$nseq,FigurexE$MANOVA_random,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexE$nseq,FigurexE$LM,lty=5,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexE$nseq,FigurexE$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))

mtext('E',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
text(x=xmin, y=textn,labels=list_type[[i]][5],pos=4,xpd=NA,cex=1.4,font=2)


################ identical Fig 1C #####
#First test for identical
xmax<-max(FigurexF$nseq)
xmin<-min(FigurexF$nseq)

plot(FigurexF$nseq,FigurexF$MANOVA,col=colors_plot["MANOVA_intercept_col"],type = 'b',lty=3,pch = 1,bty="n" ,
     ylab = ylab,xlab="Sample size",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xaxt="n")
lines(FigurexF$nseq,FigurexF$Rayleigh,col=colors_plot["Rayleigh_col"],lty=1,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexF$nseq,FigurexF$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6)
lines(FigurexF$nseq,FigurexF$MANOVA_random,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexF$nseq,FigurexF$LM,lty=5,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexF$nseq,FigurexF$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

axis(1, at = seq(xmin, xmax, by = round(xmin*2,-1)))

mtext('F',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
text(x=xmin, y=textn,labels=list_type[[i]][6],pos=4,xpd=NA,cex=1.4,font=2)



dev.off()

}




