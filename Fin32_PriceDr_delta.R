### PriceDr ###
cat('\f');rm(list=ls())
tdy<-'180510';load(paste("Data/sise_cls_",tdy,".rdata",sep=""));ydy<-'180504'
ysday<-paste('20',substr(ydy,1,2),'-',substr(ydy,3,4),'-',substr(ydy,5,6),sep='')
today<-paste('20',substr(tdy,1,2),'-',substr(tdy,3,4),'-',substr(tdy,5,6),sep='')

dt<-unique(sise_cls$date)
dt<-dt[order(dt,decreasing=T)]
dt[180]

ind<-which(sise_cls$date>dt[180])
sise<-sise_cls[ind,];rm(sise_cls)

table(duplicated(sise[,c("date","code")]))

library(doParallel)
registerDoParallel(6)

library(quantmod) # install.packages('quantmod')
load("Data/ft.rdata")

table(table(sise$code));(m<-max(as.numeric(names(table(table(sise$code)))))-2)
cd_sltd<-names(table(sise$code)[table(sise$code)>=m]);length(cd_sltd)

ind<-which(sise$code %in% cd_sltd);length(ind);fl<-T
if (length(ind)>0 & fl==T) sise<-sise[ind,];fl<-F

nrow(sise)
mycode<-unique(sise$code)
mycode_bkup<-mycode

### BGN of Exception
(n<-length(mycode_bkup))

ind<-which(sise$Volume==0)
unique(sise$code[ind])

rownames(sise)<-NULL

(a<-1592)
(ex_cd_a<-mycode[a:(a+1)])

ex_cd<-c("000040","005450","021880","026260","032040","038340","043580","053660","065150","083370","096690","101680","117670","131100","187790","192400","192410","","","","","")
(k<-which(mycode_bkup %in% ex_cd));length(k)
mycode<-mycode_bkup[-k]
(ind<-which(mycode==ex_cd_a[2]));mycode[ind];ex_cd_a
mycode_i<-mycode[ind:length(mycode)]
mycode<-mycode_i
### END of Exception

gc()
for (i in 1593:length(mycode)) { # Ser
# system.time(st_dt<-foreach (i=1:length(mycode), .combine='rbind') %dopar% {
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
  # return(pv_tmp_f) # doPar
  if (i==1) st_dt<-pv_tmp_f else st_pv<-rbind(st_pv,pv_tmp_f) # Ser
  print(paste(i,mycode[i])) # Ser
} # 1622개 주식 138 sec

system.time(st_pv<-merge(sise,st_dt,by=c("code","date"))) # 16 sec
length(unique(st_pv$code))
length(unique(st_pv$date))

tail(st_pv)

st_pv[1:6,1:10]
st_pv[(nrow(st_pv)-6):nrow(st_pv),1:10]
Sys.setlocale("LC_ALL", "C")

weekdays(as.Date(st_pv$date[1:5]))

st_pv$day<-factor(weekdays(as.Date(st_pv$date)),levels=c("Monday","Tuesday","Wednesday","Thursday","Friday"))
head(st_pv$day,10)
st_pv_dlt<-st_pv
table(st_pv_dlt$date)

# merge dleta
load(paste("Data/st_pv_",ydy,".rdata",sep=""))
unique(st_pv_dlt$date)
st_pv_dlt$date[1]
ind<-which(st_pv$date<st_pv_dlt$date[1])

st_pv_tmp<-rbind(st_pv[ind,],st_pv_dlt)
st_pv_tmp<-st_pv_tmp[order(st_pv_tmp$date,decreasing=T),]
st_pv_tmp<-st_pv_tmp[order(st_pv_tmp$code),]
row.names(st_pv_tmp)<-NULL

head(st_pv_tmp[,c("date","code")]);tail(st_pv_tmp[,c("date","code")])
ind<-which(st_pv_tmp$date>=st_pv_dlt$date[1])
table(st_pv_tmp$date[ind])
table(st_pv_tmp$date)

st_pv<-st_pv_tmp

# Backup
(fn_fnl<-paste("Data/st_pv_",tdy,".rdata",sep=""))
save(st_pv,file=fn_fnl)
