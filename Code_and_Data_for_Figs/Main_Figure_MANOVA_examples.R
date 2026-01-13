
#load background stuff 
source("Background_stuff_Manova_manuscript_fig.R")

#Plotting MANOVA examples 

#load data - change to 10000 in script 
# Figure1A=read.csv("Hypothetical_example_LM_Type1error_n_1-10_its_10000_Dist_param_unif_unif_m=180_conc=0.csv")
# Figure1B=read.csv("Hypothetical_example_LM_equal_less_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=0.csv")
# Figure1C=read.csv("Hypothetical_example_LM_equal_more_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=1.csv")
# Figure1D=read.csv("Hypothetical_example_LM_unequal_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=1.csv")
# 
# Figure2A=read.csv("Hypothetical_example_LM_equal_less_90degdiff_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=0.csv")
# Figure2B=read.csv("Hypothetical_example_LM_unequal_90degdiff_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=1.csv")
# Figure2C=read.csv("Hypothetical_example_LM_equal_less_180degdiff_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=0.csv")
# Figure2D=read.csv("Hypothetical_example_LM_unequal_180degdiff_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=1.csv")
# 
# Figure3A=read.csv("IDs_are_n_times2__Hypothetical_example_LMM_Type1error_n_1-10_its_10000_Dist_param_unif_unif_m=180_conc=0.csv")
# Figure3B=read.csv("IDs_are_n_times2__Hypothetical_example_LMM_equal_less_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=0.csv")
# Figure3C=read.csv("IDs_are_n_times2__Hypothetical_example_equal_LMM_more_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=1.csv")
Figure3D=read.csv("IDs_are_n_times2__Hypothetical_example_LMM_unequal_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=1.csv")

# Figure4A=read.csv("IDs_are_n_times2__Hypothetical_example_equal_LMM_less_90degdiff_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=0.csv")
Figure4B=read.csv("IDs_are_n_times2__Hypothetical_example_LMM_unequal_90degdiff_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=1.csv")
# Figure4C=read.csv("IDs_are_n_times2__Hypothetical_example_equal_LMM_less_180degdiff_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=0.csv")
Figure4D=read.csv("IDs_are_n_times2__Hypothetical_example_LMM_unequal_180degdiff_n_1-10_its_10000_Dist_param_vm1_vm_m=180_conc=1.csv")


list_fig3 <- list(Figure3D,Figure4B,Figure4D)
#list_fig4 <- list(Figure4D)

list_all<-list(list_fig3)

#prepare a list for  labeling for each panel
# type<-c("No difference between groups and years","Concentration increase with years\n(k=0-1)",
#         "Concentration increase with years\n(k=1-2)","Concentration increase with years\n(group 1: k=0-1, group 2: k=1-2)",
#         "90° orientation group differences\n(both groups: k=0-1)",
#         '90° orientation group differences\n(group 1: k=0-1, group 2: k=1-2)',
#         "180° orientation group differences\n(both groups: k=0-1)",
#         '180° orientation group differences\n(group 1: k=0-1, group 2: k=1-2)')

type2<-c("No difference between groups and years","Concentration increase with years\n(k=0-1)",
        "Concentration increase with years\n(k=1-2)","Concentration increase with years\n(group 1: k=0-1, group 2: k=1-2)",
        "90° orientation group differences\n(both groups: k=0-1)",
        '90° orientation group differences\n(group 1: k=0-1, group 2: k=1-2)',
        "180° orientation group differences\n(both groups: k=0-1)",
        '180° orientation group differences\n(group 1: k=0-1, group 2: k=1-2)')

list_type<-list(type2[c(4,6,8)])

# ylab_vec1<-c("Type 1 error","Type 1 error","Type 1 error",
#              "Power","Power","Type 1 error",
#              "Power","Power","Type 1 error",
#              "Power","Power","Power")
# 
# ylab_vec2<-rep("Power",12)

#ylab_vec3<-c("Power","Power","Power")

ylab_vec3<-rep("Power",9)


specs_Fig3 <- list("Main_Figure_hypothetical_examples_k4ID_1_Oct2025.pdf",ylab_vec3,"Sample size",1)
#specs_Fig4 <- list("Main_Figure_hypothetical_examples_k4ID_2_Oct2025.pdf",ylab_vec4,"Sample size",1)


specs <- list(specs_Fig3)

########

for (i in 1:length(specs)) {
  
name_fig<-specs[[i]][[1]]

ylab<-specs[[i]][[2]]; xlab<-specs[[i]][[3]]; ymax=as.numeric(specs[[i]][[4]])

FigurexA <-list_all[[i]][[1]]; FigurexB <-list_all[[i]][[2]];FigurexC <-list_all[[i]][[3]]
#FigurexD <-list_all[[i]][[4]]


textH<-ymax+ymax*0.15
textn<-ymax+ymax*0.1


pdf(name_fig, width=6, height=8)

layout_matrix <- matrix(seq(1:(4*3)), nrow = 4, byrow = TRUE)

layout_widths <- c(1.2, 1, 1)  # First column wider
layout_heights <- c(2,2.2,2.2,2.2)  # first row labels, the column heading

layout(layout_matrix, widths = layout_widths, heights = layout_heights)


par(cex.axis = 1,cex.lab=1.4,
   mgp=c(1,0.1,0),tck=-0.015,cex=0.35)
seglen=3;yesp=1;adjLet=-0.1;lineLet=1


#########prepare legends 
################Legend_1 #########
#col_legen_1=colors_plot[c(name_vector_col)]

legend_4= function(){
  
  name_vector_col<-c("Rayleigh_col","HR_col","Model_comp_col","MANOVA_intercept_col", 
                     "MANOVA_linear_interceptP_col",
                     "MANOVA_linear_group_interceptP_col",
                     "LM_P_intercept_col","LM_lin_P_intercept_col","LM_lin_group_P_intercept_col") 
  
  
  legend("bottomleft", legend=c("Rayleigh (IDs averaged)","HR (IDs averaged)","CircMLE (IDs averaged)","MANOVA re (intercept)",
                                "MANOVA re linear (intercept)",
                                "MANOVA re linear & group (intercept)",
                                "LMM (intercept)","LMM linear (intercept)","LMM linear & group (intercept)"),
         col=colors_plot[c(name_vector_col)],cex = 1.3, lty=c(1:2,6,3,5,1,3:5),pch = 1,bty="n",
         lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}

legend_5= function(){
  
  name_vector_col<-c("MANOVA_linear_covariateP_col",
                     "MANOVA_linear_group_covariateP_col",
                     "LM_lin_P_lin_col","LM_lin_group_P_lin_col") 
  
  
  legend("bottomleft", legend=c("MANOVA re linear (linear)",
                                "MANOVA re linear & group (linear)",
                                "LMM linear (linear)","LMM linear & group (linear)"),
         col=colors_plot[c(name_vector_col)],cex = 1.3, lty=c(1,3,5,6),pch = 1,bty="n",
         lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}

legend_6= function(){
  
  name_vector_col<-c("Rayleigh_col","HR_col","MANOVA_linear_group_groupP_col",
                     "LM_lin_group_P_group_col")
  
  legend("bottomleft", legend=c("Watson (IDs averaged)","G-test (IDs averaged)","MANOVA re linear & group (group)",
                                "LMM linear & group (group)"),
         col=colors_plot[c(name_vector_col)],cex = 1.3, lty=c(1:3,5),pch = 1,bty="n",
         lwd = 0.6,y.intersp = yesp,seg.len = seglen,xjust = 0, x.intersp=0.1 )}



######################## Plotting #################################


# mar[1]: Bottom
# mar[2]: Left
# mar[3]: Top
# mar[4]: Right


par(mar=c(2,9,0,0))
plot.new(); legend_4()

par(mar=c(2,4,0,0))
plot.new();legend_5()

par(mar=c(2,4,0,0))
plot.new();legend_6()


#Figure1A - erster Panel
par(mar=c(2,7,3,0))
xmax<-max(FigurexA$nseq)
xmin<-min(FigurexA$nseq)
plot(FigurexA$nseq,FigurexA$Rayleigh,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[1],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)
lines(FigurexA$nseq,FigurexA$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexA$nseq,FigurexA$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexA$nseq,FigurexA$MANOVA_intercept,col=colors_plot["MANOVA_intercept_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexA$nseq,FigurexA$MANOVA_random_intercept,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexA$nseq,FigurexA$MANOVA_linear_interceptP,col=colors_plot["MANOVA_linear_interceptP_col"],lty=5,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_interceptP,lty=6,col=colors_plot["MANOVA_random_linear_interceptP_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexA$nseq,FigurexA$MANOVA_linear_group_interceptP,col=colors_plot["MANOVA_linear_group_interceptP_col"],lty=1,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_group_interceptP,lty=2,col=colors_plot["MANOVA_random_linear_group_interceptP_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexA$nseq,FigurexA$LM_P_intercept,lty=3,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexA$nseq,FigurexA$LM_lin_P_intercept,lty=4,col=colors_plot["LM_lin_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexA$nseq,FigurexA$LM_lin_group_P_intercept,lty=5,col=colors_plot["LM_lin_group_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)


text(x = xmin-2.2, y = textn-1.1, labels = list_type[[i]][1], adj = 0, xpd = NA, srt = 90, font = 2,xpd=NA,cex=1.4)
text(x= xmin+2, y=textn+0.1,labels="Orientation effect",adj = 0,xpd=NA,font = 2,cex = 1.6)

mtext('A',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)


################  Fig 1A - 2ter panel #####
par(mar=c(2,2,3,0))
xmax<-max(FigurexA$nseq)
xmin<-min(FigurexA$nseq)
plot(FigurexA$nseq,FigurexA$MANOVA_linear_covariateP,col=colors_plot["MANOVA_linear_covariateP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[2],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)
#lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_covariateP,col=colors_plot["MANOVA_random_linear_covariateP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexA$nseq,FigurexA$MANOVA_linear_group_covariateP,col=colors_plot["MANOVA_linear_group_covariateP_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_group_covariateP,lty=4,col=colors_plot["MANOVA_random_linear_group_covariateP_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexA$nseq,FigurexA$LM_lin_P_lin,lty=5,col=colors_plot["LM_lin_P_lin_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexA$nseq,FigurexA$LM_lin_group_P_lin,lty=6,col=colors_plot["LM_lin_group_P_lin_col"],type = "b",pch = 1,lwd = 0.6)


text(x= xmin+2, y=textn+0.1,labels="Linear effect",adj = 0,xpd=NA,font = 2,cex = 1.6)



################ Fig 1A - 3ter panel #####
xmax<-max(FigurexA$nseq)
xmin<-min(FigurexA$nseq)
plot(FigurexA$nseq,FigurexA$Watson_two_sample,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[3],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)

lines(FigurexA$nseq,FigurexA$G.test,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexA$nseq,FigurexA$MANOVA_linear_group_groupP,col=colors_plot["MANOVA_linear_group_groupP_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)

#lines(FigurexA$nseq,FigurexA$MANOVA_random_linear_group_groupP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=4,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexA$nseq,FigurexA$LM_lin_group_P_group,col=colors_plot["LM_lin_group_P_group_col"],lty=5,type = "b",pch = 1,lwd = 0.6,)


text(x= xmin+2, y=textn+0.1,labels="Group effect",adj = 0,xpd=NA,font = 2,cex = 1.6)

#Figure1A - erster Panel
xmax<-max(FigurexB$nseq)
xmin<-min(FigurexB$nseq)
par(mar=c(2,7,2,0))
plot(FigurexB$nseq,FigurexB$Rayleigh,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[4],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)
lines(FigurexB$nseq,FigurexB$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexB$nseq,FigurexB$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexB$nseq,FigurexB$MANOVA_intercept,col=colors_plot["MANOVA_intercept_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexB$nseq,FigurexB$MANOVA_random_intercept,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexB$nseq,FigurexB$MANOVA_linear_interceptP,col=colors_plot["MANOVA_linear_interceptP_col"],lty=5,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_interceptP,lty=6,col=colors_plot["MANOVA_random_linear_interceptP_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexB$nseq,FigurexB$MANOVA_linear_group_interceptP,col=colors_plot["MANOVA_linear_group_interceptP_col"],lty=1,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_group_interceptP,lty=2,col=colors_plot["MANOVA_random_linear_group_interceptP_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexB$nseq,FigurexB$LM_P_intercept,lty=3,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexB$nseq,FigurexB$LM_lin_P_intercept,lty=4,col=colors_plot["LM_lin_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexB$nseq,FigurexB$LM_lin_group_P_intercept,lty=5,col=colors_plot["LM_lin_group_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)


text(x = xmin-2.2, y = textn-1.1, labels = list_type[[i]][2], adj = 0, xpd = NA, srt = 90, font = 2,xpd=NA,cex=1.4)


mtext('B',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
#legend_1()

################  Fig 1A - 2ter panel #####
xmax<-max(FigurexB$nseq)
xmin<-min(FigurexB$nseq)
par(mar=c(2,2,2,0))
plot(FigurexB$nseq,FigurexB$MANOVA_linear_covariateP,col=colors_plot["MANOVA_linear_covariateP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[5],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)
#lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_covariateP,col=colors_plot["MANOVA_random_linear_covariateP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexB$nseq,FigurexB$MANOVA_linear_group_covariateP,col=colors_plot["MANOVA_linear_group_covariateP_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_group_covariateP,lty=4,col=colors_plot["MANOVA_random_linear_group_covariateP_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexB$nseq,FigurexB$LM_lin_P_lin,lty=5,col=colors_plot["LM_lin_P_lin_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexB$nseq,FigurexB$LM_lin_group_P_lin,lty=6,col=colors_plot["LM_lin_group_P_lin_col"],type = "b",pch = 1,lwd = 0.6)


#text(x=xmin, y=textn,labels=list_type[[i]][2],pos=4,xpd=NA)

#legend_2()

################ Fig 1A - 3ter panel #####
xmax<-max(FigurexB$nseq)
xmin<-min(FigurexB$nseq)
plot(FigurexB$nseq,FigurexB$Watson_two_sample,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[6],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)
lines(FigurexB$nseq,FigurexB$G.test,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexB$nseq,FigurexB$MANOVA_linear_group_groupP,col=colors_plot["MANOVA_linear_group_groupP_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)

#lines(FigurexB$nseq,FigurexB$MANOVA_random_linear_group_groupP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=4,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexB$nseq,FigurexB$LM_lin_group_P_group,col=colors_plot["LM_lin_group_P_group_col"],lty=5,type = "b",pch = 1,lwd = 0.6,)


#text(x=xmin, y=textn,labels=list_type[[i]][2],pos=4,xpd=NA)

#legend_3()

#Figure1A - erster Panel
xmax<-max(FigurexC$nseq)
xmin<-min(FigurexC$nseq)
par(mar=c(2,7,2,0))
plot(FigurexC$nseq,FigurexC$Rayleigh,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[7],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)
lines(FigurexC$nseq,FigurexC$HR,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexC$nseq,FigurexC$Model_comp,lty=6,col=colors_plot["Model_comp_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexC$nseq,FigurexC$MANOVA_intercept,col=colors_plot["MANOVA_intercept_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexC$nseq,FigurexC$MANOVA_random_intercept,lty=4,col=colors_plot["MANOVA_random_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexC$nseq,FigurexC$MANOVA_linear_interceptP,col=colors_plot["MANOVA_linear_interceptP_col"],lty=5,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_interceptP,lty=6,col=colors_plot["MANOVA_random_linear_interceptP_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexC$nseq,FigurexC$MANOVA_linear_group_interceptP,col=colors_plot["MANOVA_linear_group_interceptP_col"],lty=1,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_group_interceptP,lty=2,col=colors_plot["MANOVA_random_linear_group_interceptP_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexC$nseq,FigurexC$LM_P_intercept,lty=3,col=colors_plot["LM_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexC$nseq,FigurexC$LM_lin_P_intercept,lty=4,col=colors_plot["LM_lin_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexC$nseq,FigurexC$LM_lin_group_P_intercept,lty=5,col=colors_plot["LM_lin_group_P_intercept_col"],type = "b",pch = 1,lwd = 0.6)

text(x = xmin-2.2, y = textn-1.1, labels = list_type[[i]][3], adj = 0, xpd = NA, srt = 90, font = 2,xpd=NA,cex=1.4)

mtext('C',side=3,cex=0.8,las=1,line=lineLet,adj=adjLet)
#legend_1()

################  Fig 1A - 2ter panel #####
xmax<-max(FigurexC$nseq)
xmin<-min(FigurexC$nseq)
par(mar=c(2,2,2,0))
plot(FigurexC$nseq,FigurexC$MANOVA_linear_covariateP,col=colors_plot["MANOVA_linear_covariateP_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[8],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)
#lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_covariateP,col=colors_plot["MANOVA_random_linear_covariateP_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)
lines(FigurexC$nseq,FigurexC$MANOVA_linear_group_covariateP,col=colors_plot["MANOVA_linear_group_covariateP_col"],lty=3,type = "b",pch = 1,lwd = 0.6)
#lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_group_covariateP,lty=4,col=colors_plot["MANOVA_random_linear_group_covariateP_col"],type = "b",pch = 1,lwd = 0.6)

lines(FigurexC$nseq,FigurexC$LM_lin_P_lin,lty=5,col=colors_plot["LM_lin_P_lin_col"],type = "b",pch = 1,lwd = 0.6)
lines(FigurexC$nseq,FigurexC$LM_lin_group_P_lin,lty=6,col=colors_plot["LM_lin_group_P_lin_col"],type = "b",pch = 1,lwd = 0.6)


#text(x=xmin, y=textn,labels=list_type[[i]][3],pos=4,xpd=NA)

#legend_2()

################ Fig 1A - 3ter panel #####
xmax<-max(FigurexC$nseq)
xmin<-min(FigurexC$nseq)
plot(FigurexC$nseq,FigurexC$Watson_two_sample,col=colors_plot["Rayleigh_col"],type = 'b',lty=1,pch = 1,bty="n" ,
     ylab = ylab[9],xlab="Sample size per treat. comb.",xlim = c(xmin,xmax),ylim = c(0,ymax),lwd = 0.6,xpd=NA)
lines(FigurexC$nseq,FigurexC$G.test,col=colors_plot["HR_col"],lty=2,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexC$nseq,FigurexC$MANOVA_linear_group_groupP,col=colors_plot["MANOVA_linear_group_groupP_col"],lty=3,type = "b",pch = 1,lwd = 0.6,)

#lines(FigurexC$nseq,FigurexC$MANOVA_random_linear_group_groupP,col=colors_plot["MANOVA_random_linear_group_groupP_col"],lty=4,type = "b",pch = 1,lwd = 0.6,)

lines(FigurexC$nseq,FigurexC$LM_lin_group_P_group,col=colors_plot["LM_lin_group_P_group_col"],lty=5,type = "b",pch = 1,lwd = 0.6,)



dev.off()

}#i



