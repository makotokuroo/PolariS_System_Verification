

source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
Resolution4 <- readPolariS(fname='~/R/Iriki_test025/Resolution1.99890137MHz4.dat',stnum=1)
Resolution32751 <- Resolution4[32751,1,9:812]
Resolution92751 <- Resolution4[92751,1,9:812]
reno <- c(1:201)
resohz <- c(4*no-404)
resnoise <- mean((Resolution92751[4*reno-2]+Resolution92751[4*reno-1])/2)

resPower <- (((Resolution32751[4*reno-2])+(Resolution32751[4*reno-1]))/2-resnoise)


stPower<- resPower/max(resPower)
Resolution201 <- data.frame(Frecency=resohz, Power=stPower)

#x=frec-1.99890137MHz,y=Detected Powerでプロット
par(mar=c(5.5,5.5,4,4))
plot(Resolution201$Frecency,Resolution201$Power,ylab='Amplitude',xlab='Frecency[Hz]-1.99890137MHz',main='Resolution-32751ch',type='p',cex=0.5,col='red')

#y=a*(sin(πbx-c)/(πbx-c))^2でフィッティング
sinc_fit <- nls(data=Resolution201, formula = Power ~ (a * sin(pi*b*Frecency-c)/(pi*b*Frecency-c))^2, start = list(a=1.01, b=0.00724, c=0.00000001))

summary(sinc_fit)

# a=9.96e-01 ±　5.365e-04, b=1.631e-02 ± 1.771e-05, c=1.395e-01 ± 1.472e-03 ということがわかる。

#フィット結果をグラフプロット
lines(Resolution201$Frecency,predict(sinc_fit, data.frame(x=Resolution201$Frecency)),col='orange')


#y=(sinc(x))^2の半値半幅(HWHM)=0.442946である。 →0.5=(sinc(x))^2 をx=で解けば確認できる
FWHMpolaris <- 0.442946*2/1.631e-02


##############################################################
#メインローブに対する最大のサイドローブの大きさ(比)を求める。

Power_dB <- 10*log(Resolution201$Power)
plot(Resolution201$Frecency,Power_dB,ylab='Amplitude[dB]',xlab='Frecency[Hz]-1.99890137MHz',main='Resolution-32751ch',type='p',cex=0.5,col='red')
lines(Resolution201$Frecency,10*log(predict(sinc_fit, data.frame(x=Resolution201$Frecency))),col='orange')
rateMSdB <- (-30.67-30.61)/2
rateMS100 <- 100*10^(rateMSdB/10)
rateMSdB
rateMS100

######最終的には以下のようにplotする
par(mfrow=c(1,2))
par(mar=c(5.5,5.5,3,2))
plot(Resolution201$Frecency,Resolution201$Power,ylab='Amplitude',xlab='Frecency[Hz]-1.99890137MHz',main='Resolution-32751ch-a',type='p',cex=0.5,col='red',axes=F)
lines(Resolution201$Frecency,predict(sinc_fit, data.frame(x=Resolution201$Frecency)),col='orange')
axis(1,cex.axis=0.6,c(-400,-300,-200,-100,0,100,200,300,400))
axis(2,las=2,cex.axis=0.6)
box()
grid()
legend("topleft",inset=0.05,c("Analyzed Data","fit-result"),pch=c(1,-1),lty=c(-1,1),col=c("red","orange"),cex=0.6)
par(mar=c(5.5,5.5,3,2))
plot(Resolution201$Frecency,Power_dB,ylab='Amplitude[dB]',xlab='Frecency[Hz]-1.99890137MHz',main='Resolution-32751ch-b',type='p',cex=0.5,col='red',axes=F)
lines(Resolution201$Frecency,10*log(predict(sinc_fit, data.frame(x=Resolution201$Frecency))),col='orange')
axis(1,cex.axis=0.6,c(-400,-300,-200,-100,0,100,200,300,400))
axis(2,las=2,cex.axis=0.6,c(-100,-50,-30,-10,0))
box()
grid()
legend("topleft",inset=0.05,c("Analyzed Data","fit-result"),pch=c(1,-1),lty=c(-1,1),col=c("red","orange"),cex=0.6)
par(mfrow=c(1,1),mar=c(5,5,5,5)) #グラフを1つだけプロットする形式に戻す。
