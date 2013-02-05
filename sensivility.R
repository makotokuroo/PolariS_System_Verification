


source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
#取得した1時間分のデータ×2をそれぞれ行列に読み込む(sens1がはじめの1時間、sens2が次の1時間)
sens1 <- readPolariS(fname='~/R/Iriki_test025/sensitivity1.dat',stnum=1)
gc()
gc()
sens2 <- readPolariS(fname='~/R/Iriki_test025/sensitivity2.dat',stnum=1)
gc()
gc()

#プログラムに用いるパラメータの設定#############################################

maxch <- 2^17
ch16 <- 2^16
ch15 <- 2^15
ch14 <- 2^14
ch13 <- 2^13
ch12 <- 2^12
ch11 <- 2^11
ch10 <- 2^10
ch9 <- 2^9

#########################################################################
#131072×7200の行列noiseを作成
noise <- matrix(ncol=2^17,nrow=7200) 
#noiseに、取得した1~131072chの1~3600秒のデータを入れる。
n <- c(1:3600)
nch <- c(1:(2^17))
for(n in 1:3600)noise[n,] <- sens1[nch,1,n]
#noiseの続きに、取得した1024ch(90001~91024ch)の3601~7200秒のデータを入れる。
for(n in 1:3600)noise[(n+3600),] <-sens2[nch,1,n]

#noise[t,ch]になっている。
##########################################################################
#隣り合う2列(1~2,3~4,.....,(2^17)-1~(2^17))の周波数方向の平均した結果を行列に入れる
noise2_16 <- matrix(mapply(function(a,b) mean(c(a,b)),noise[,(2*(1:16ch)-1)],noise[,(2*(1:16ch)]),nrow=7200)
noise2_15 <- matrix(mapply(function(a,b) mean(c(a,b)),noise2_16[,(2*(1:15ch)-1)],noise2_16[,(2*(1:15ch)]),nrow=7200)
noise2_14 <- matrix(mapply(function(a,b) mean(c(a,b)),noise2_15[,(2*(1:14ch)-1)],noise2_15[,(2*(1:14ch)]),nrow=7200)
noise2_13 <- matrix(mapply(function(a,b) mean(c(a,b)),noise2_14[,(2*(1:13ch)-1)],noise2_14[,(2*(1:13ch)]),nrow=7200)
noise2_12 <- matrix(mapply(function(a,b) mean(c(a,b)),noise2_13[,(2*(1:12ch)-1)],noise2_13[,(2*(1:12ch)]),nrow=7200)
noise2_11 <- matrix(mapply(function(a,b) mean(c(a,b)),noise2_12[,(2*(1:11ch)-1)],noise2_12[,(2*(1:11ch)]),nrow=7200)
noise2_10 <- matrix(mapply(function(a,b) mean(c(a,b)),noise2_11[,(2*(1:10ch)-1)],noise2_11[,(2*(1:10ch)]),nrow=7200)








################バンドキャラクタを補正する。ch毎に7200秒分のデータを平均し、全てのデータから平均値を引く。
#積分時間1~7200を表す変数を作成
tint <- c(1:7200)
#ch数1~chnoを表す変数を作成
ch <- c(1:chno)
#各チャンネル毎の平均値を計算しchmeansに入れる
chmeans <- colSums(noise)/nrow(noise)
#chno×7200の行列noise_chara_offを作成
noise_chara_off <- matrix(ncol=chno,nrow=7200)
#各チャンネルのデータから各チャンネルの平均値を差し引く。
noise_chara_off[,ch] <- noise[,ch]-chmeans[ch]






#131072×7200の行列sumnoiseを作成
sumnoise <- matrix(ncol=100,nrow=7200)
#sumnoiseの1行目に、バンキャラ補正した1秒目のデータを入れる。
sumnoise[1,] <- noise_chara_off[1,]
#sumnoiseのn行目に、取得した2^17chのn秒目までの平均値を入れる。
n <- c(2:7200)
for(n in 2:7200)sumnoise[n,] <- apply(noise[1:n,],2,mean)   #長時間かかる


#積分時間毎に、1024chの取得データの標準偏差を作成
sdnoise <- apply(sumnoise[tint,],1,sd)
#縦軸に標準偏差、横軸に積分時間で結果をplot
plot(tint,sdnoise,log='xy')
#結果をy=a*x^bでフィッティング
rootfit <- nls(formula = sdnoise ~ a*tint^b,start=list(a=-0.01,b=0.5))
フィッティング結果を確認
summary(rootfit)
#重ねてplotするための宣言
par(new=T)
#フィッティング結果を重ねてplot
plot(tint,predict(rootfit),type='l',log='xy',xlab='',ylab='')


#最終的には以下の形でplotする。

par(mar=c(4.5,5.5,5.5,4))
plot(tint,sdnoise,log='xy',ylab='R.M.S of White-Noise',xlab='Integeration Time',main='Sensitivity(Tint= 1~7200)',type='p',cex=0.5,col='skyblue',axes=F)
par(new=T)
plot(tint,predict(rootfit),type='l',log='xy',xlab='',ylab='',main='',col='blue',axes=F)
axis(1)
axis(2,las=2,cex.axis=0.8)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1.270x^-0.5076"),pch=c(1,-1),lty=c(-1,1),col=c("skyblue","blue"),cex=0.6)



##131072chの計算用

#131072×7200の行列sumnoiseを作成
sumnoise <- matrix(ncol=100,nrow=7200)
#sumnoiseの1行目に、バンキャラ補正した1秒目のデータを入れる。
sumnoise[1,] <- noise_chara_off[1,]
#sumnoiseのn行目に、取得した2^17chのn秒目までの平均値を入れる。
n <- c(2:7200)

#################################################################################
for(n in 2:7200)sumnoise[n,] <- apply(noise_chara_off[1:n,],2,mean)   #長時間かかる
#################################################################################

#積分時間毎に、1024chの取得データの標準偏差を作成
sdnoise <- apply(sumnoise[tint,],1,sd)
#縦軸に標準偏差、横軸に積分時間で結果をplot
plot(tint,sdnoise,log='xy')
#結果をy=a*x^bでフィッティング
rootfit <- nls(formula = sdnoise ~ a*tint^b,start=list(a=-0.01,b=0.5))
フィッティング結果を確認
summary(rootfit)
#重ねてplotするための宣言
par(new=T)
#フィッティング結果を重ねてplot
plot(tint,predict(rootfit),type='l',log='xy',xlab='',ylab='')


#最終的には以下の形でplotする。

par(mar=c(4.5,5.5,5.5,4))
plot(tint,sdnoise,log='xy',ylab='R.M.S of White-Noise',xlab='Integeration Time',main='Sensitivity(Tint= 1~7200)',type='p',cex=0.5,col='skyblue',axes=F)
par(new=T)
plot(tint,predict(rootfit),type='l',log='xy',xlab='',ylab='',main='',col='blue',axes=F)
axis(1)
axis(2,las=2,cex.axis=0.8)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1.270x^-0.5076"),pch=c(1,-1),lty=c(-1,1),col=c("skyblue","blue"),cex=0.6)











##########################################以下はテスト用。ch数を100個のみで行う。

source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
#取得した1時間分のデータ×2をそれぞれ行列に読み込む(sens1がはじめの1時間、sens2が次の1時間)
sens1 <- readPolariS(fname='~/R/Iriki_test025/sensitivity1.dat',stnum=1)
sens2 <- readPolariS(fname='~/R/Iriki_test025/sensitivity2.dat',stnum=1)

#131072×7200の行列noiseを作成
noise <- matrix(ncol=100,nrow=7200)  #全部でとる
#noiseに、取得した1~131072chの1~3600秒のデータを入れる。
n <- c(1:3600)
nch <- c(90001:90100)
for(n in 1:3600)noise[n,] <- sens1[nch,1,n]
#noiseの続きに、取得した1024ch(90001~91024ch)の3601~7200秒のデータを入れる。
for(n in 1:3600)noise[(n+3600),] <-sens2[nch,1,n]

#noise[t,ch]になっている。
################バンドキャラクタを補正する。ch毎に7200秒分のデータを平均し、全てのデータから平均値を引く。
#積分時間1~7200を表す変数を作成
tint <- c(1:7200)
#ch数1~2^17を表す変数を作成
ch <- c(1:100)
#各チャンネル毎の平均値を計算しchmeansに入れる
chmeans <- colSums(noise)/nrow(noise)
#131072×7200の行列noise_chara_offを作成
noise_chara_off <- matrix(ncol=100,nrow=7200)
#各チャンネルのデータから各チャンネルの平均値を差し引く。
noise_chara_off[,ch] <- noise[,ch]-chmeans[ch]






#131072×7200の行列sumnoiseを作成
sumnoise <- matrix(ncol=100,nrow=7200)
#sumnoiseの1行目に、バンキャラ補正した1秒目のデータを入れる。
sumnoise[1,] <- noise_chara_off[1,]
#sumnoiseのn行目に、取得した2^17chのn秒目までの平均値を入れる。
n <- c(2:7200)
for(n in 2:7200)sumnoise[n,] <- apply(noise_chara_off[1:n,],2,mean)   #長時間かかる


#積分時間毎に、1024chの取得データの標準偏差を作成
sdnoise <- apply(sumnoise[tint,],1,sd)
#縦軸に標準偏差、横軸に積分時間で結果をplot
plot(tint,sdnoise,log='xy')
#結果をy=a*x^bでフィッティング
rootfit <- nls(formula = sdnoise ~ a*tint^b,start=list(a=-0.01,b=0.5))
フィッティング結果を確認
summary(rootfit)
#重ねてplotするための宣言
par(new=T)
#フィッティング結果を重ねてplot
plot(tint,predict(rootfit),type='l',log='xy',xlab='',ylab='')


#最終的には以下の形でplotする。

par(mar=c(4.5,5.5,5.5,4))
plot(tint,sdnoise,log='xy',ylab='R.M.S of White-Noise',xlab='Integeration Time',main='Sensitivity(Tint= 1~7200)',type='p',cex=0.5,col='skyblue',axes=F)
par(new=T)
plot(tint,predict(rootfit),type='l',log='xy',xlab='',ylab='',main='',col='blue',axes=F)
axis(1)
axis(2,las=2,cex.axis=0.8)
box()
grid()
legend("topright",inset=0.05,c("Analyzed Data","y=1.270x^-0.5076"),pch=c(1,-1),lty=c(-1,1),col=c("skyblue","blue"),cex=0.6)
