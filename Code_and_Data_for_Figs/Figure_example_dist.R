# example distributions 

#libraries 
library(circular)
library(NPCirc)

#plotting function 
plotting_dens<- function(x) {plot(density.circular(x,bw=10),main = "",xlab="",xpd = TRUE,
                                 ylab = "",axes=F,ticks=T,lty=2,template="geographics", offset = 1, shrink = 1.1)}


#random
dist_0<-rcircmix(n=1000,dist = "vm", param=list(p=1,  mu=0, con=0))

#von Mises k=0.2
dist_0_2<-rcircmix(n=1000,dist = "vm", param=list(p=1,  mu=0, con=0.2))

#von Mises k=4

dist_4<-rcircmix(n=1000,dist = "vm", param=list(p=1,  mu=0, con=4))

#wrapped skew normal 
dist_2wsn<-rcircmix(n=1000,dist = "wsn", param=list(p=1,  mu=0, con=2,sk=c(30)))

#axial von Mises 
dist_1__1<-rcircmix(n=1000,dist = c("vm","vm"), param=list(p=c(0.5,0.5), mu=c(0,0-pi), con=c(4,4)))

#bim von Mises 
dist_1_120_1<-rcircmix(n=1000,dist = c("vm","vm"), param=list(p=c(0.5,0.5), mu=c(0,0-rad(120)), con=c(4,4)))


#trim von Mises sym 

dist_1_120_3_sym<-rcircmix(n=1000,dist = c("vm","vm","vm"), 
                       param=list(p=c(1/3,1/3,1/3), mu=c(0,0-rad(120),0+rad(120)), con=c(4,4,4)))



#trim von Mises asym

dist_1_120_3_asym<-rcircmix(n=1000,dist = c("vm","vm","vm"), 
                       param=list(p=c(1/3,1/3,1/3), mu=c(0,0-rad(90),0+rad(120)), con=c(4,4,4)))



lineLet=-3;adjLet=0.1

#cex.axis = 1,cex.lab=1.2,tck=-0.015,cex=0.35

pdf("Example_Distribution_Densities.pdf", width=13, height=8.5)

par(mar=c(0,0,0,0.45),
    mfrow = c(2, 4),mgp=c(1,0.1,0))

plotting_dens(dist_0)
text(x= 0, y=0,labels="Uniform",pos=3,xpd=NA,cex=1.5)
mtext('A',side=3,cex=1.5,las=1,line=lineLet,adj=adjLet)

plotting_dens(dist_0_2)
text(x= 0, y=0,labels="VM\n(k = 0.2)",pos=3,xpd=NA,cex=1.5)
mtext('B',side=3,cex=1.5,las=1,line=lineLet,adj=adjLet)

plotting_dens(dist_4)
text(x= 0, y=0,labels="VM\n(k = 4)",pos=3,xpd=NA,cex=1.5)
mtext('C',side=3,cex=1.5,las=1,line=lineLet,adj=adjLet)

plotting_dens(dist_2wsn)
text(x= 0, y=0,labels="WSN",pos=3,xpd=NA,cex=1.5)
mtext('D',side=3,cex=1.5,las=1,line=lineLet,adj=adjLet)

plotting_dens(dist_1__1)
text(x= 0, y=0,labels="Axial VM",pos=3,xpd=NA,cex=1.5)
mtext('E',side=3,cex=1.5,las=1,line=lineLet,adj=adjLet)

plotting_dens(dist_1_120_1)
text(x= 0, y=0,labels="Bimodal VM\n(asymmetrical)",pos=3,xpd=NA,cex=1.5)
mtext('F',side=3,cex=1.5,las=1,line=lineLet,adj=adjLet)

plotting_dens(dist_1_120_3_sym)
text(x= 0, y=0,labels="Trimodal VM\n(symmetrical)",pos=3,xpd=NA,cex=1.5)
mtext('G',side=3,cex=1.5,las=1,line=lineLet,adj=adjLet)

plotting_dens(dist_1_120_3_asym)
text(x= 0, y=0,labels="Trimodal VM\n(asymmetrical)",pos=3,xpd=NA,cex=1.5)
mtext('H',side=3,cex=1.5,las=1,line=lineLet,adj=adjLet)

dev.off()

