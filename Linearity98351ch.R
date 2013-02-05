

source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
Linearity6MHz <- readPolariS(fname='~/R/Iriki_test025/Linearity6.00280762MHz.dat',stnum=1)
Linearity6MHz98351 <- Linearity6MHz[98351,1,9:412]
Linearity6MHz32751 <- Linearity6MHz[32751,1,9:412]
lino <- c(1:101)
liso <- c(-90:10)
lisomw <- 10^(liso/10)
noise6MHz <- mean((Linearity6MHz32751[4*lino-2]+Linearity6MHz32751[4*lino-1])/2)
Linearity6MHz100 <- data.frame(SG_output_dBm=liso, SG_output_mW=lisomw ,Power=(Linearity6MHz98351[4*lino-2]+Linearity6MHz98351[4*lino-1])/2-noise6MHz)

#x軸をdBmでプロット
#plot(Linearity6MHz100[,1],Linearity6MHz100[,3],log='y',ylab='OUTPUT-98351ch',xlab='SG-OUTPUT[dBm]',main='Linearity-98351ch',type='p',cex=0.5,col='purple')

#x軸をmWでプロット
plot(Linearity6MHz100[,2],Linearity6MHz100[,3],log='xy',ylab='Detected Power 98351ch',xlab='SG-OUTPUT[dBm]',main='Linearity-98351ch',type='p',cex=0.5,col='purple')


#Power=0+a0*SG_output_mWでフィッティング

linearfit6MHz <- lm(formula = Linearity6MHz100$Power ~ 0 + Linearity6MHz100$SG_output_mW)

#フィッティングの結果をみる
summary(linearfit6MHz)
anova(linearfit6MHz)

#グラフフィットはこのコマンド。
lines(Linearity6MHz100$SG_output_mW, predict(linearfit, data.frame(x=Linearity6MHz100$SG_output_mW)))

#求めた傾きで規格化したデータ列を作り、さらにx軸との比をとる
stPower6MHz <- Linearity6MHz100$Power/491485/Linearity6MHz100$SG_output_mW
#比を取った結果をプロット
plot(Linearity6MHz100$SG_output_dBm,stPower6MHz,ylim=c(0.1,3),col='purple',cex=0.5,ylab='the Rate of DetectedPower to SG_output',xlab='SG-OUTPUT[dBm]')
abline(h=1)




#最終的には以下の形でグラフにする。
#上段に(x,y)=(PolariS計測値,SG出力[mW])下段に(x,y)=(PolariS計測値を傾きとSG出力で規格化した値,SG出力[mW])のグラフをプロット
par(mfrow=c(2,1))
par(mar=c(0,5.5,4,4))
plot(Linearity6MHz100[,1],Linearity6MHz100[,3],log='y',ylab='Detected Power',xlab='SG-OUTPUT[dBm]',main='Linearity 98351ch',type='p',cex=0.5,col='purple',axes=F)
lines(Linearity6MHz100$SG_output_dBm, predict(linearfit, data.frame(x=Linearity6MHz100$SG_output_mW)))
axis(2,las=2,cex.axis=0.6)
box()
grid()
legend("topleft",inset=0.05,c("Analyzed Data","y=491485*x"),pch=c(1,-1),lty=c(-1,1),col=c("purple",1),cex=0.6)
par(mar=c(5,5.5,0,4))
plot(Linearity6MHz100$SG_output_dBm,stPower6MHz,ylim=c(0.1,3),col='purple',cex=0.5,ylab='Rate of Detected Power \n to SG-OUTPUT',xlab='SG-OUTPUT[dBm]',axes=F)
abline(h=1)
axis(2,las=2,cex.axis=0.6)
axis(1,cex.axis=0.6)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1"),pch=c(1,-1),lty=c(-1,1),col=c("purple",1),cex=0.6)
par(mfrow=c(1,1),mar=c(5,5,5,5)) #グラフを1つだけプロットする形式に戻す。

############################################################################################################################以下で外れ値を除いてplot


plot(Linearity6MHz100[,2],Linearity6MHz100[,3],log='xy',ylab='Detected Power 98351ch',xlab='SG-OUTPUT[dBm]',main='Linearity-98351ch',type='p',cex=0.5,col='purple')
linearfit6MHz <- lm(formula = Linearity6MHz100$Power[1:97] ~ 0 + Linearity6MHz100$SG_output_mW[1:97])
summary(linearfit6MHz)
anova(linearfit6MHz)
lines(Linearity6MHz100$SG_output_mW[1:97], predict(linearfit6MHz, data.frame(x=Linearity6MHz100$SG_output_mW[1:97])))
stPower6MHz <- Linearity6MHz100$Power/1209798.1/Linearity6MHz100$SG_output_mW

par(mfrow=c(2,1))
par(mar=c(0,5.5,4,4))
plot(Linearity6MHz100[,1],Linearity6MHz100[,3],log='y',ylab='Detected Power',xlab='SG-OUTPUT[dBm]',main='Linearity 98351ch',type='p',cex=0.5,col='purple',axes=F)
lines(Linearity6MHz100$SG_output_dBm[1:97], predict(linearfit6MHz, data.frame(x=Linearity6MHz100$SG_output_mW[1:97])))
axis(2,las=2,cex.axis=0.6)
box()
grid()
legend("topleft",inset=0.05,c("Analyzed Data","y=1209798.1*x"),pch=c(1,-1),lty=c(-1,1),col=c("purple",1),cex=0.6)
par(mar=c(5,5.5,0,4))
plot(Linearity6MHz100$SG_output_dBm,stPower6MHz,ylim=c(0,3),xlim=c(-90,10),col='purple',cex=0.5,ylab='Rate of Detected Power \n to SG-OUTPUT',xlab='SG-OUTPUT[dBm]',axes=F)
abline(h=1)
axis(2,las=2,cex.axis=0.6)
axis(1,cex.axis=0.6)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1"),pch=c(1,-1),lty=c(-1,1),col=c("purple",1),cex=0.6)
par(mfrow=c(1,1),mar=c(4,4,4,4)) #グラフを1つだけプロットする形式に戻す。







