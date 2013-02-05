

error2MHz <- 100*(((Linearity2MHz100[,3]/1555818.6)-Linearity2MHz100[,2])/Linearity2MHz100[,2])
error4MHz <- 100*(((Linearity4MHz100[,3]/1211355.4)-Linearity4MHz100[,2])/Linearity4MHz100[,2])
error6MHz <- 100*(((Linearity6MHz100[,3]/1209798.1)-Linearity6MHz100[,2])/Linearity6MHz100[,2])

par(xaxs="r", yaxs="i")#x軸とy軸がy=0で交わるように設定
par(mar=c(5,5.5,3,5))
plot(Linearity2MHz100$SG_output_dBm,error2MHz,ylim=c(-100,100),col='blue',cex=0.3,pch=19,axes=F,xlab='',ylab='')
lines(Linearity2MHz100$SG_output_dBm,error2MHz,ylim=c(-100,100),col='blue')
par(new=T)
plot(Linearity4MHz100$SG_output_dBm,error4MHz,ylim=c(-100,100),col='red',cex=0.3,pch=19,axes=F,xlab='',ylab='')
lines(Linearity4MHz100$SG_output_dBm,error4MHz,ylim=c(-100,100),col='red')
par(new=T)
plot(Linearity6MHz100$SG_output_dBm,error6MHz,ylim=c(-100,100),col='purple',cex=0.3,xlab='SG-OUTPUT[dBm]',ylab='Relative Error[%]',main='Relative Error of Linearity',pch=19,axes=F)
lines(Linearity6MHz100$SG_output_dBm,error6MHz,ylim=c(-100,100),col='purple')
axis(2,c(-100,-80,-60,-40,-20,-10,0,10,20,40,60,80,100),labels=c(-100,-80,-60,-40,-20,-10,0,10,20,40,60,80,100),las=2,cex.axis=0.8)
axis(1,c(-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10),labels=c(-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10),cex.axis=0.8)
box()
grid(ny=20)
legend("topright",inset=0.05,c("32751ch","65551ch","98351ch"),pch=c(19,19,19),lty=c(1,1,1),col=c("blue","red","purple"),cex=0.8)

###################################################################################################################################

######under3%plot

par(xaxs="r", yaxs="i")#x軸とy軸がy=0で交わるように設定
par(mar=c(5,5.5,3,5))
plot(Linearity2MHz100$SG_output_dBm,error2MHz,xlim=c(-35,10),ylim=c(-3,3),col='blue',cex=0.3,pch=19,axes=F,xlab='',ylab='')
lines(Linearity2MHz100$SG_output_dBm,error2MHz,xlim=c(-35,10),ylim=c(-3,3),col='blue')
par(new=T)
plot(Linearity4MHz100$SG_output_dBm,error4MHz,xlim=c(-35,10),ylim=c(-3,3),col='red',cex=0.3,pch=19,axes=F,xlab='',ylab='')
lines(Linearity4MHz100$SG_output_dBm,error4MHz,xlim=c(-35,10),ylim=c(-3,3),col='red')
par(new=T)
plot(Linearity6MHz100$SG_output_dBm,error6MHz,xlim=c(-35,10),ylim=c(-3,3),col='purple',cex=0.3,xlab='SG-OUTPUT[dBm]',ylab='Relative Error[%]',main='Relative Error of Linearity',pch=19,axes=F)
lines(Linearity6MHz100$SG_output_dBm,error6MHz,xlim=c(-35,10),ylim=c(-3,3),col='purple')
grid()
abline(h=c(1,0,-1),lty=c(3,3,3))
axis(1,c(-30,-25,-20,-15,-10,-5,0,5,10),labels=c(-30,-25,-20,-15,-10,-5,0,5,10),cex.axis=0.8)
axis(2,c(-5,-4,-3,-2,-1,0,1,2,3,3,4,5),labels=c(5,4,3,2,1,0,1,2,3,3,4,5),las=2,cex.axis=0.8)
box()
legend("topright",inset=0.05,c("32751ch","65551ch","98351ch"),pch=c(19,19,19),lty=c(1,1,1),col=c("blue","red","purple"),cex=0.8)




