source('asd.R')
source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
gc()
sens1 <- readPolariS(fname='~/R/sensitivity1.dat',stnum=1)
gc()
gc()
sens2 <- readPolariS(fname='~/R/sensitivity2.dat',stnum=1)
gc()
gc()

##############################
maxch <-2^17

##############################

nch <- c(1:maxch)
noise_max <- matrix(nrow=maxch,ncol=7200)
gc()
gc()
for(n in 1:3600)noise_max[,n] <- sens1[nch,1,n]
gc()
gc()
for(n in 1:3600)noise_max[,(n+3600)] <-sens2[nch,1,n]
gc()
gc()


##############################
selectch <-2^10
###############################


noise <- matrix(bunch(noise_max,(maxch/selectch)),nrow=selectch,ncol=7200)
nch <- c(1:selectch)
tint <- c(1:7200)
gc()
gc()
chmeans <- rowSums(noise[,2*(tint[1:3600])]/(ncol(noise)/2))
gc()
gc()
noise_chara_off <- matrix(nrow=selectch,ncol=3600)
gc()
gc()
noise_chara_off[nch,] <- noise[nch,(2*(tint[1:3600])-1)]/chmeans[nch]
gc()
gc()
sumnoise <- matrix(nrow=selectch,ncol=3600)
gc()
gc()
sumnoise[,1] <- noise_chara_off[,1]
gc()
gc()
for(n in 2:600)sumnoise[,n] <- apply(noise_chara_off[,1:n],1,mean)
gc()
gc()
for(n in 601:1200)sumnoise[,n] <- apply(noise_chara_off[,1:n],1,mean)
gc()
gc()
for(n in 1201:1800)sumnoise[,n] <- apply(noise_chara_off[,1:n],1,mean)
gc()
gc()
for(n in 1801:2400)sumnoise[,n] <- apply(noise_chara_off[,1:n],1,mean)
gc()
gc()
for(n in 2401:3000)sumnoise[,n] <- apply(noise_chara_off[,1:n],1,mean)
gc()
gc()
for(n in 3001:3600)sumnoise[,n] <- apply(noise_chara_off[,1:n],1,mean)
gc()
gc()


#################################################
#for(n in 2:3600)sumnoise[,n] <- apply(noise_chara_off[,1:n],1,mean)
#gc()
#gc()
##################################################
sumnoise1023 <- matrix(nrow=selectch-1,ncol=3600)
sumnoise1023 <- sumnoise[2:selectch,]
tint <- tint[1:3600]
sdnoise <- apply(sumnoise1023[,tint],2,sd)
gc()
gc()
rootfit <- nls(formula = sdnoise ~ a*tint^b,start=list(a=0.01,b=-0.5))
gc()
gc()
summary(rootfit)

par(mar=c(4.5,5.5,5.5,4))
plot(tint,sdnoise,log='xy',ylab='R.M.S of White-Noise',xlab='Integeration Time[sec]',main='Sensitivity(Tint= 1~3600sec)',type='p',cex=0.4,col='orange',axes=F,ylim=c(0.0014,0.0715))
par(new=T)
plot(tint,predict(rootfit),type='l',log='xy',xlab='',ylab='',main='',col='red',axes=F,yli=c(0.0014,0.0715))
lines(tint,0.0635079*tint^(-0.5),ylim=c(0.0014,0.0075),type='l',lty=2,col='purple')
axis(1)
axis(2,las=2,cex.axis=0.8)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","fit-result  y=0.0635079x^-0.4663939","Conventional Line  y=0.0635079x^-0.5"),pch=c(1,-1,-1),lty=c(-1,1,2),col=c("orange","red","purple"),cex=0.7)
gc()
gc()




##バンドキャラクタをplot
plot(chmeans,type='l',xlab='Frequency[ch]',ylab='Band Character')
##バンドキャラクタをplot(Zoom)
plot(chmeans,type='l',xlab='Frequency[ch]',ylab='Band Character',ylim=c(3.19,3.30))

##バンドキャラクタ補正後のスペクトルをplot(1sec目のデータ)
plot(noise_chara_off[,1],type='l',ylim=c(0.5,1.5),xlab='Frequency[ch]',ylab='Nomalized Amplitude(1sec)')
##バンドキャラクタ補正後のスペクトルをplot(3600sec目のデータ)
plot(noise_chara_off[,3600],type='l',ylim=c(0.5,1.5),xlab='Frequency[ch]',ylab='Nomalized Amplitude(3600sec)')


###########モデル式と計測値の相対誤差を計算する
Ttest <- 1:3600
model <- 0.0635079*Ttest^(-0.5)
plot(Ttest,100*(model-sdnoise)/model,type='l',col='red',xlab='Integration Time[sec]',ylab='Rerative Error[%]',main='Rerative Error of Sensitivity',xlim=c(1,300),ylim=c(-25,0))





###############################
summary(rootfit)

Formula: sdnoise ~ a * tint^b

Parameters:
    Estimate Std. Error t value Pr(>|t|)    
a  0.0635079  0.0001656   383.6   <2e-16 ***
b -0.4663939  0.0005299  -880.1   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.0002847 on 3598 degrees of freedom

Number of iterations to convergence: 6 
Achieved convergence tolerance: 2.644e-06 













################################summary(root fit) @sensitivity1T and sensitivity2T
> summary(rootfit)

Formula: sdnoise ~ a * tint^b

Parameters:
    Estimate Std. Error t value Pr(>|t|)    
a  0.0670048  0.0001146   584.8   <2e-16 ***
b -0.4594444  0.0003427 -1340.6   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.0001994 on 3598 degrees of freedom

Number of iterations to convergence: 5 
Achieved convergence tolerance: 2.27e-06 




