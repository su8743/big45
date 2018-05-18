### PriceDr ###
cat('\f');rm(list=ls())
tdy<-'180510';load(paste("Data/sise_cls_",tdy,".rdata",sep=""))
ind<-which(sise_cls$date>'2015-01-00')
sise<-sise_cls[ind,]

table(duplicated(sise_cls[,c("date","code")]))

library(doParallel)
registerDoParallel(6)

library(quantmod) # install.packages('quantmod')
load("Data/ft.rdata")

table(table(sise$code));(m<-max(as.numeric(names(table(table(sise$code)))))-4)
(cd_sltd<-names(table(sise$code)[table(sise$code)>=m]))

ind<-which(sise$code %in% cd_sltd);length(ind);fl<-T
if (length(ind)>0 & fl==T) sise<-sise[ind,];fl<-F

nrow(sise)
mycode<-unique(sise$code)
mycode_bkup<-mycode
# mycode<-mycode_bkup

### BGN of Exception
# (n<-length(mycode_bkup))

# ind<-which(sise$Volume==0)
# unique(sise$code[ind])

# rownames(sise)<-NULL

# (a<-1324)
# (ex_cd_a<-mycode[a:(a+1)])

ex_cd<-c("000180","000230","001000","001260","001440","001470","001800","002870","003090","003460","004200","005740","005960","005990","006580","007280","007540","008800","009310","009540","010640","011200","011330","011810","012030","012320","014200","014470","014530","014940","016380","016920","017650","018290","019570","023440","023530","024840","025620","028040","028670","029460","030530","031860","032860","033430","035480","037560","042660","042700","042940","043220","043710","045660","047440","048870","049180","050320","054340","056730","060900","066110","066430","069140","071970","072470","073010","073110","077970","078070","080440","081970","082920","083380","084180","088390","089230","090370","090710","094840","096040","097780","106080","115530","122800","129260","145270","155960","184230","193250","194510","196450","198440","200230","203650","204840","205100","205470","206400","208350","208370","208640","900080","900090","900100","900180")
(k<-which(mycode_bkup %in% ex_cd))
mycode<-mycode_bkup[-k]
# (ind<-which(mycode==ex_cd_a[2]));mycode[ind];ex_cd_a
# mycode_i<-mycode[ind:length(mycode)]
### END of Exception

# mycode<-mycode_i
gc()
# 84,241+84
system.time(st_dt<-foreach (i=84:length(mycode), .combine='rbind') %dopar% {
  require(quantmod)
  ind<-which(sise$code==mycode[i])
  pv_tmp<-sise[sise$code==mycode[i],c("date","Open","High","Low","Close","Volume")]
  pv_tmp$Adjusted <- pv_tmp$Close
  row.names(pv_tmp) <- pv_tmp$date
  pv_tmp <- as.xts(pv_tmp[,-1],dateFormat="POSIXct")
  # str(stock.data)
  pv_tmp_f <- addFewerFeatures(pv_tmp)
  colnames(pv_tmp_f)<-c("ROC1","ROC2","ROC3","ROC5","VROC","BBands_pctB","SMI","SMI_sig","SMI3MA","SMI3MA_sig","MACD","MACD_sig","Stoch_fastK","Stoch_1fastD","Stoch_2fastD","Stoch_3fastD","Stoch_4fastD","Stoch_slowD","Stoch2MA.fastK","Stoch2MA.fastD","Stoch2MA.slowD","StochRSI_fastK","StochRSI_fastD","StochRSI_slowD","RSI","ATR.tr","ATR.atr","ATR.trueHigh","ATR.trueLow","Aroon.up","Aroon.down","Aroon.osci","Volatility","EMA7","EMA50","EMA120","EMA7to50","EMA7to120","EMA50to120","SMA7","SMA50","SMA120","SMA7to50","SMA7to120","SMA50to120","CMO15","DEMA20")
  pv_tmp_f <- as.data.frame(na.omit(pv_tmp_f))
  pv_tmp_f$date<-row.names(pv_tmp_f)
  pv_tmp_f$code<-mycode[i]
  return(pv_tmp_f)
}) # 1622개 주식 138 sec

head(st_dt)
system.time(st_pv<-merge(sise,st_dt,by=c("code","date"))) # 16 sec
length(unique(st_pv$code))
head(st_pv)
st_pv[1:6,1:10]
st_pv[(nrow(st_pv)-6):nrow(st_pv),1:10]
weekdays(as.Date(st_pv$date[1:5]))

st_pv$day<-factor(weekdays(as.Date(st_pv$date)),levels=c("Monday","Tuesday","Wednesday","Thursday","Friday"))
head(st_pv$day,10)

(fn_fnl<-paste("Data/st_pv_",tdy,".rdata",sep=""))
save(st_pv,mycode,cd_sltd,file=fn_fnl)
