source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
Linearity4MHz <- readPolariS(fname='~/R/Iriki_test025/Linearity4.00085450MHz.dat',stnum=1)
Linearity4MHz65551 <- Linearity4MHz[65551,1,8:411]
Linearity4MHz98351 <- Linearity4MHz[98351,1,8:411]
lino <- c(1:101)
liso <- c(-90:10)
lisomw <- 10^(liso/10)
noise4MHz <- mean((Linearity4MHz98351[4*lino-2]+Linearity4MHz98351[4*lino-1])/2)
Linearity4MHz100 <- data.frame(SG_output_dBm=liso, SG_output_mW=lisomw ,Power=(Linearity4MHz65551[4*lino-2]+Linearity4MHz65551[4*lino-1])/2-noise4MHz)

#x軸をdBmでプロット
#plot(Linearity4MHz100[,1],Linearity4MHz100[,3],log='y',ylab='OUTPUT-65551ch',xlab='SG-OUTPUT[dBm]',main='Linearity-65551ch',type='p',cex=0.5,col='red')

#x軸をmWでプロット
plot(Linearity4MHz100[,2],Linearity4MHz100[,3],log='xy',ylab='Detected Power 65551ch',xlab='SG-OUTPUT[dBm]',main='Linearity-65551ch',type='p',cex=0.5,col='red')


#Power=0+a0*SG_output_mWでフィッティング(切片あり)

linearfit4MHz <- lm(formula = Linearity4MHz100$Power ~ Linearity4MHz100$SG_output_mW)
summary(linearfit4MHz)

#Power=0+a0*SG_output_mWでフィッティング(切片なし)

linearfit4MHz <- lm(formula = Linearity4MHz100$Power ~ 0 + Linearity4MHz100$SG_output_mW)

#フィッティングの結果をみる
summary(linearfit4MHz)
anova(linearfit4MHz)

#グラフフィットはこのコマンド。
lines(Linearity4MHz100$SG_output_mW, predict(linearfit, data.frame(x=Linearity4MHz100$SG_output_mW)))

##################################################外れ値を除外しないまま解析

#求めた傾きで規格化したデータ列を作り、さらにx軸との比をとる
stPower4MHz <- Linearity4MHz100$Power/532289/Linearity4MHz100$SG_output_mW
#比を取った結果をプロット
plot(Linearity4MHz100$SG_output_dBm,stPower4MHz,ylim=c(0.1,3),col='red',cex=0.5,ylab='the Rate of DetectedPower to SG_output',xlab='SG-OUTPUT[dBm]')
abline(h=1)




#最終的には以下の形でグラフにする。
#上段に(x,y)=(PolariS計測値,SG出力[mW])下段に(x,y)=(PolariS計測値を傾きとSG出力で規格化した値,SG出力[mW])のグラフをプロット
par(mfrow=c(2,1))
par(mar=c(0,5.5,4,4))
plot(Linearity4MHz100[,1],Linearity4MHz100[,3],log='y',ylab='Detected Power',xlab='SG-OUTPUT[dBm]',main='Linearity 65551ch',type='p',cex=0.5,col='red',axes=F)
lines(Linearity4MHz100$SG_output_dBm, predict(linearfit, data.frame(x=Linearity4MHz100$SG_output_mW)))
axis(2,las=2,cex.axis=0.6)
box()
grid()
legend("topleft",inset=0.05,c("Analyzed Data","y=532289*x"),pch=c(1,-1),lty=c(-1,1),col=c("red",1),cex=0.6)
par(mar=c(5,5.5,0,4))
plot(Linearity4MHz100$SG_output_dBm,stPower4MHz,ylim=c(0.1,3),col='red',cex=0.5,ylab='Rate of Detected Power \n to SG-OUTPUT',xlab='SG-OUTPUT[dBm]',axes=F)
abline(h=1)
axis(2,las=2,cex.axis=0.6)
axis(1,cex.axis=0.6)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1"),pch=c(1,-1),lty=c(-1,1),col=c("red",1),cex=0.6)
par(mfrow=c(1,1),mar=c(4,4,4,4)) #グラフを1つだけプロットする形式に戻す。

#################################################外れ値を除外してもう一度同じ手順を行う。
plot(Linearity4MHz100[,2],Linearity4MHz100[,3],log='xy',ylab='Detected Power 65551ch',xlab='SG-OUTPUT[dBm]',main='Linearity-65551ch',type='p',cex=0.5,col='red')
linearfit4MHz <- lm(formula = Linearity4MHz100$Power[1:96] ~ 0 + Linearity4MHz100$SG_output_mW[1:96])
summary(linearfit4MHz)
anova(linearfit4MHz)
lines(Linearity4MHz100$SG_output_mW[1:96], predict(linearfit4MHz, data.frame(x=Linearity4MHz100$SG_output_mW[1:96])))
stPower4MHz <- Linearity4MHz100$Power/1211355.4/Linearity4MHz100$SG_output_mW

par(mfrow=c(2,1))
par(mar=c(0,5.5,4,4))
plot(Linearity4MHz100[,1],Linearity4MHz100[,3],log='y',ylab='Detected Power',xlab='SG-OUTPUT[dBm]',main='Linearity 65551ch',type='p',cex=0.5,col='red',axes=F)
lines(Linearity4MHz100$SG_output_dBm[1:96], predict(linearfit4MHz, data.frame(x=Linearity4MHz100$SG_output_mW[1:96])))
axis(2,las=2,cex.axis=0.6)
box()
grid()
legend("topleft",inset=0.05,c("Analyzed Data","y=1211355.4*x"),pch=c(1,-1),lty=c(-1,1),col=c("red",1),cex=0.6)
par(mar=c(5,5.5,0,4))
plot(Linearity4MHz100$SG_output_dBm,stPower4MHz,ylim=c(0,3),xlim=c(-90,10),col='red',cex=0.5,ylab='Rate of Detected Power \n to SG-OUTPUT',xlab='SG-OUTPUT[dBm]',axes=F)
abline(h=1)
axis(2,las=2,cex.axis=0.6)
axis(1,cex.axis=0.6)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1"),pch=c(1,-1),lty=c(-1,1),col=c("red",1),cex=0.6)
par(mfrow=c(1,1),mar=c(4,4,4,4)) #グラフを1つだけプロットする形式に戻す。



