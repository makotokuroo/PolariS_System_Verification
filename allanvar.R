
source('~/R/asd.R')
source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
gc()
gc()
allan1 <- readPolariS(fname='~/R/Iriki_test025/allan1.dat',stnum=1)
gc()
gc()
allan2 <- readPolariS(fname='~/R/Iriki_test025/allan2.dat',stnum=1)
gc()
gc()
allan3 <- readPolariS(fname='~/R/Iriki_test025/allan3.dat',stnum=1)
gc()
gc()
allan4 <- readPolariS(fname='~/R/Iriki_test025/allan4.dat',stnum=1)
gc()
gc()

################TAVの計算###############################################

avnoise <- matrix(ncol=3,nrow=14400)
t1 <- c(1:3600)
t2 <- c(3601:7200)
t3 <- c(7201:10800)
t4 <- c(10801:14400)

#30000chのデータを抽出
avnoise[t1,1] <- allan1[30000,1,]
avnoise[t2,1] <- allan2[30000,1,]
avnoise[t3,1] <- allan3[30000,1,]
avnoise[t4,1] <- allan4[30000,1,]

#60000chのデータを抽出
avnoise[t1,2] <- allan1[60000,1,]
avnoise[t2,2] <- allan2[60000,1,]
avnoise[t3,2] <- allan3[60000,1,]
avnoise[t4,2] <- allan4[60000,1,]

#90000chのデータを抽出
avnoise[t1,3] <- allan1[90000,1,]
avnoise[t2,3] <- allan2[90000,1,]
avnoise[t3,3] <- allan3[90000,1,]
avnoise[t4,3] <- allan4[90000,1,]

#バンドキャラクタを補正する
tint <- c(1:14400)
ch <- c(1:3)
avchmeans <- apply(avnoise,2,mean)
avnoise_chara_off <- matrix(ncol=3,nrow=14400)
avnoise_chara_off[,ch] <- avnoise[,ch]-avchmeans[ch]

#各チャンネル毎に時間積分する
#3×14400の行列sumavnoiseを作成
sumavnoise <- matrix(ncol=3,nrow=14400)
#sumavnoiseの1行目に、バンキャラ補正した1秒目のデータを入れる。
sumavnoise[1,] <- avnoise_chara_off[1,]
#sumnoiseのn行目に、取得した2^17chのn秒目までの平均値を入れる。
n <- c(2:14400)
for(n in 2:14400)sumavnoise[n,] <- apply(avnoise[1:n,],2,mean) 


#アラン分散を計算

tav30000ch <-allanvar(avnoise_chara_off[,1])
tav60000ch <-allanvar(avnoise_chara_off[,2])
tav90000ch <-allanvar(avnoise_chara_off[,3])

plot(tint[1:7199],tav30000ch,type='p',col='red',cex=0.4,xlim=c(1,8000),axes=F,log='xy')
par(new=T)
plot(tint[1:7199],tav60000ch,type='p',col='blue',cex=0.4,xlim=c(1,8000),axes=F,log='xy')
par(new=T)
plot(tint[1:7199],tav90000ch,type='p',col='green',cex=0.4,xlim=c(1,8000),axes=F,log='xy')
axis(1)
axis(2,las=2,cex.axis=0.8)
box()
grid()





###############################################################################
source('~/R/asd.R')
source('http://milkyway.sci.kagoshima-u.ac.jp/~kameno/Programs/PolariSData.R')
gc()
gc()
allan1 <- readPolariS(fname='~/R/Iriki_test025/allan1.dat',stnum=1)
gc()
gc()
allan2 <- readPolariS(fname='~/R/Iriki_test025/allan2.dat',stnum=1)
gc()
gc()
allan3 <- readPolariS(fname='~/R/Iriki_test025/allan3.dat',stnum=1)
gc()
gc()
allan4 <- readPolariS(fname='~/R/Iriki_test025/allan4.dat',stnum=1)
gc()
gc()

################SAVの計算#######################################################
#131072×7200の行列savnoiseを作成
savnoise <- matrix(nrow=2^17,ncol=14400)
gc()
gc()
#savnoiseに、取得した1~131072chの1~3600秒のデータを入れる。savnoise[ch,t]になっている。
n <- c(1:3600)
gc()
gc()
nch <- c(1:(2^17))
gc()
gc()
for(n in 1:3600)savnoise[,n] <- allan1[nch,1,n]
gc()
gc()
rm(allan1)
gc()
gc()
#noiseの続きに、取得した131072chの3601~14400秒のデータを入れる。
for(n in 1:3600)savnoise[,(n+3600)] <-allan2[nch,1,n]
gc()
gc()
rm(allan2)
gc()
gc()
for(n in 1:3600)savnoise[,(n+7200)] <-allan3[nch,1,n]
gc()
gc()
rm(allan3)
gc()
gc()
for(n in 1:3600)savnoise[,(n+10800)] <-allan4[nch,1,n]
gc()
gc()
rm(allan4)
gc()
gc()









#積分時間1~14400を表す変数を作成
tint <- c(1:14400)
gc()
gc()

#ch数1~2^17を表す変数を作成
chno <- c(1:2^17)
gc()
gc()

#各チャンネル毎の平均値を計算しsavchmeansに入れる
savchmeans <- rowSums(savnoise)/ncol(savnoise)
gc()
gc()

#131072×7200の行列noise_chara_offを作成
savnoise_chara_off <- matrix(nrow=2^17,ncol=14400)
gc()
gc()

#各チャンネルのデータから各チャンネルの平均値を差し引く。
chno <- 1:9000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 9001:18000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 18001:27000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 27001:36000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 36001:45000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 45001:54000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 54001:63000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 63001:72000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 72001:81000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 81001:90000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 90001:99000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 99001:108000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 108001:117000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 117001:126000
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()
chno <- 126001:131072
savnoise_chara_off[chno,] <- savnoise[chno,]-savchmeans[chno]
gc()
gc()







rm(savnoise)
gc()
gc()

#131072×15の行列sumsavnoiseを作成
sumsavnoise <- matrix(nrow=2^17,ncol=15)
gc()
gc()

#sumsavnoiseの1行目に、バンキャラ補正した1秒目のデータを入れる。
sumsavnoise[,1] <- savnoise_chara_off[,1]
gc()
gc()

#sumsavnoiseのn行目に、取得した2^17chのn秒目までの平均値を入れる。(用はここで積分してる。)
#積分時間は1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 14400
sumsavnoise[,2] <- apply(savnoise_chara_off[,1:2],1,mean)
gc()
gc() 
sumsavnoise[,3] <- apply(savnoise_chara_off[,1:4],1,mean)
gc()
gc() 
sumsavnoise[,4] <- apply(savnoise_chara_off[,1:8],1,mean) 
gc()
gc() 
sumsavnoise[,5] <- apply(savnoise_chara_off[,1:16],1,mean) 
gc()
gc() 
sumsavnoise[,6] <- apply(savnoise_chara_off[,1:32],1,mean) 
gc()
gc() 
sumsavnoise[,7] <- apply(savnoise_chara_off[,1:64],1,mean) 
gc()
gc() 
sumsavnoise[,8] <- apply(savnoise_chara_off[,1:128],1,mean) 
gc()
gc() 
sumsavnoise[,9] <- apply(savnoise_chara_off[,1:256],1,mean) 
gc()
gc() 
sumsavnoise[,10] <- apply(savnoise_chara_off[,1:512],1,mean) 
gc()
gc() 
sumsavnoise[,11] <- apply(savnoise_chara_off[,1:1024],1,mean) 
gc()
gc() 
sumsavnoise[,12] <- apply(savnoise_chara_off[,1:2048],1,mean) 
gc()
gc() 
sumsavnoise[,13] <- apply(savnoise_chara_off[,1:4096],1,mean) 
gc()
gc() 
sumsavnoise[,14] <- apply(savnoise_chara_off[,1:8192],1,mean) 
gc()
gc() 
sumsavnoise[,15] <- apply(savnoise_chara_off[,1:14400],1,mean) 
gc()
gc() 


#SAVを計算。
sav <- matrix(nrow=65535,ncol=15)
gc()
gc()
for(t in 1:15)sav[,t]  <-allanvar(sumsavnoise[,t])
gc()
gc()



 














