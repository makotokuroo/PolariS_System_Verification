source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
Linearity2MHz <- readPolariS(fname='~/R/Iriki_test025/Linearity1.99890137MHz.dat',stnum=1)
Linearity2MHz32751 <- Linearity2MHz[32751,1,7:410]
Linearity2MHz65551 <- Linearity2MHz[65551,1,7:410]
lino <- c(1:101)
liso <- c(-90:10)
lisomw <- 10^(liso/10)
noise2MHz <- mean((Linearity2MHz65551[4*lino-2]+Linearity2MHz65551[4*lino-1])/2)
Linearity2MHz100 <- data.frame(SG_output_dBm=liso, SG_output_mW=lisomw ,Power=(Linearity2MHz32751[4*lino-2]+Linearity2MHz32751[4*lino-1])/2-noise2MHz)

#x軸をdBmでプロット
#plot(Linearity2MHz100[,1],Linearity2MHz100[,3],log='y',ylab='OUTPUT-32751ch',xlab='SG-OUTPUT[dBm]',main='Linearity-32751ch',type='p',cex=0.5,col='blue')

#x軸をmWでプロット
plot(Linearity2MHz100[,2],Linearity2MHz100[,3],log='xy',ylab='Detected Power 32751ch',xlab='SG-OUTPUT[dBm]',main='Linearity-32751ch',type='p',cex=0.5,col='blue')


#Power=a1+a0*SG_output_mWでフィッティング(切片あり)
linearfit2MHz0 <- lm(formula = Linearity2MHz100$Power ~ Linearity2MHz100$SG_output_mW)


#Power=0+a0*SG_output_mWでフィッティング(切片なし)

linearfit2MHz <- lm(formula = Linearity2MHz100$Power ~ 0 + Linearity2MHz100$SG_output_mW)




#フィッティングの結果をみる
summary(linearfit2MHz)
anova(linearfit2MHz)

#グラフフィットはこのコマンド。
lines(Linearity2MHz100$SG_output_mW, predict(linearfit, data.frame(x=Linearity2MHz100$SG_output_mW)))

#求めた傾きで規格化したデータ列を作り、さらにx軸との比をとる
stPower2MHz <- Linearity2MHz100$Power/1318132/Linearity2MHz100$SG_output_mW
#比を取った結果をプロット
plot(Linearity2MHz100$SG_output_dBm,stPower2MHz,ylim=c(0.1,3),col='blue',cex=0.5,ylab='the Rate of DetectedPower to SG_output',xlab='SG-OUTPUT[dBm]')
abline(h=1)




#最終的には以下の形でグラフにする。
#上段に(x,y)=(PolariS計測値,SG出力[mW])下段に(x,y)=(PolariS計測値を傾きとSG出力で規格化した値,SG出力[mW])のグラフをプロット
par(mfrow=c(2,1))
par(mar=c(0,5.5,4,4))
plot(Linearity2MHz100[,1],Linearity2MHz100[,3],log='y',ylab='Detected Power',xlab='SG-OUTPUT[dBm]',main='Linearity 32751ch',type='p',cex=0.5,col='blue',axes=F)
lines(Linearity2MHz100$SG_output_dBm, predict(linearfit, data.frame(x=Linearity2MHz100$SG_output_mW)))
axis(2,las=2,cex.axis=0.6)
box()
grid()
legend("topleft",inset=0.05,c("Analyzed Data","y=1318132*x"),pch=c(1,-1),lty=c(-1,1),col=c("blue",1),cex=0.6)
par(mar=c(5,5.5,0,4))
plot(Linearity2MHz100$SG_output_dBm,stPower2MHz,ylim=c(0.1,3),col='blue',cex=0.5,ylab='Rate of Detected Power \n to SG-OUTPUT',xlab='SG-OUTPUT[dBm]',axes=F)
abline(h=1)
axis(2,las=2,cex.axis=0.6)
axis(1,cex.axis=0.6)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1"),pch=c(1,-1),lty=c(-1,1),col=c("blue",1),cex=0.6)
par(mfrow=c(1,1),mar=c(5,5,5,5)) #グラフを1つだけプロットする形式に戻す。








#################################################外れ値を除外してもう一度同じ手順を行う。
plot(Linearity2MHz100[,2],Linearity2MHz100[,3],log='xy',ylab='Detected Power 32751ch',xlab='SG-OUTPUT[dBm]',main='Linearity-32751ch',type='p',cex=0.5,col='blue')
linearfit2MHz <- lm(formula = Linearity2MHz100$Power[1:93] ~ 0 + Linearity2MHz100$SG_output_mW[1:93])
summary(linearfit2MHz)
anova(linearfit2MHz)
lines(Linearity2MHz100$SG_output_mW[1:93], predict(linearfit2MHz, data.frame(x=Linearity2MHz100$SG_output_mW[1:93])))
stPower2MHz <- Linearity2MHz100$Power/1551469.4/Linearity2MHz100$SG_output_mW


par(mfrow=c(2,1))
par(mar=c(0,5.5,4,4))
plot(Linearity2MHz100[,1],Linearity2MHz100[,3],log='y',ylab='Detected Power',xlab='SG-OUTPUT[dBm]',main='Linearity 32751ch',type='p',cex=0.5,col='blue',axes=F)
lines(Linearity2MHz100$SG_output_dBm[1:93], predict(linearfit2MHz, data.frame(x=Linearity2MHz100$SG_output_mW[1:93])))
axis(2,las=2,cex.axis=0.6)
box()
grid()
legend("topleft",inset=0.05,c("Analyzed Data","y=1551469.4*x"),pch=c(1,-1),lty=c(-1,1),col=c("blue",1),cex=0.6)
par(mar=c(5,5.5,0,4))
plot(Linearity2MHz100$SG_output_dBm,stPower2MHz,ylim=c(0,3),xlim=c(-90,10),col='blue',cex=0.5,ylab='Rate of Detected Power \n to SG-OUTPUT',xlab='SG-OUTPUT[dBm]',axes=F)
abline(h=1)
axis(2,las=2,cex.axis=0.6)
axis(1,cex.axis=0.6)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1"),pch=c(1,-1),lty=c(-1,1),col=c("blue",1),cex=0.6)
par(mfrow=c(1,1),mar=c(4,4,4,4)) #グラフを1つだけプロットする形式に戻す。






