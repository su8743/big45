rm(list=ls());cat('\f')
library(doParallel)
registerDoParallel(6)

tdy<-'180510';ydy='180504'
ysday<-paste('20',substr(ydy,1,2),'-',substr(ydy,3,4),'-',substr(ydy,5,6),sep='')
today<-paste('20',substr(tdy,1,2),'-',substr(tdy,3,4),'-',substr(tdy,5,6),sep='')

load(file=paste("Data/st_pv_",tdy,".rdata",sep=""))
load(file=paste("Data/volume_",tdy,".rdata",sep=""))

head(st_pv,2);head(volume)
pv_tmp<-merge(st_pv,volume,by=c("code","date","exch_mkt"))

ind<-which(st_pv$date=='2018-03-26')

dt<-unique(pv_tmp$date)
dt<-dt[order(dt,decreasing=T)]
ind<-which(pv_tmp$date>dt[110]);fl<-T
pv<-pv_tmp[ind,]

pv$org_tot<-0
pv$prv_tot<-0
pv$total<-0
pv$for_r<-0
pv$org_r<-0
pv$prv_r<-0
pv$close_r<-0
pv$vol_r<-0
pv$vol_var<-0
pv$o_net_cont<-0 #

mycode<-unique(pv$code)
(t_pv<-table(pv$date))
max(t_pv);min(t_pv)

system.time(stock_pv<-foreach (i = 1:length(mycode), .combine=rbind) %dopar% {
  pv_tmp<-pv[pv$code==mycode[i],]
  # head(pv_tmp,2);tail(pv_tmp,2)
  for (k in nrow(pv_tmp):1) {
    if(k==nrow(pv_tmp)) {
      if(pv_tmp$o_net[k]>0) pv_tmp$o_net_cont[k]<-1
      pv_tmp$org_tot[k]<-pv_tmp$o_net[k]
      pv_tmp$prv_tot[k]<--(pv_tmp$f_net[k]+pv_tmp$o_net[k])
    } else {
      if(pv_tmp$o_net[k]>0) { #
        pv_tmp$o_net_cont[k]<-pv_tmp$o_net_cont[k+1]+1 #
      } else pv_tmp$o_net_cont[k]<-0 #
      pv_tmp$org_tot[k]<-pv_tmp$org_tot[k+1]+pv_tmp$o_net[k]
      pv_tmp$prv_tot[k]<-pv_tmp$prv_tot[k+1]-(pv_tmp$f_net[k]+pv_tmp$o_net[k])
    }
  }
  pv_tmp$org_tot<-pv_tmp$org_tot-min(pv_tmp$org_tot)
  pv_tmp$prv_tot<-pv_tmp$prv_tot-min(pv_tmp$prv_tot)
  pv_tmp$total<-pv_tmp$f_tot+pv_tmp$org_tot+pv_tmp$prv_tot
  pv_tmp$for_r<-pv_tmp$f_tot/pv_tmp$total*100
  pv_tmp$org_r<-pv_tmp$org_tot/pv_tmp$total*100
  pv_tmp$prv_r<-pv_tmp$prv_tot/pv_tmp$total*100
  pv_tmp$close_r<-pv_tmp$Close/max(pv_tmp$Close)*100
  pv_tmp$vol_r<-pv_tmp$Volume/max(pv_tmp$Volume)*100
  pv_tmp$vol_var<-100-min(pv_tmp$Volume)/max(pv_tmp$Volume)
  print(paste(i,"/",length(mycode)))
  return(pv_tmp)
}) # 82 sec / 1621 주식

head(stock_pv)

head(stock_pv,1)
mycode<-unique(stock_pv$code)
# i<-1
gc()
# setdiff(1:length(mycode),c(3,236,537,596,952,1205,1328,1369,1436,1488,1582,1587,1588))
# a<-596
# (a<-a+1)

system.time(stock_pv_dr<-foreach (i = setdiff(1:length(mycode),c(3,236,537,596,952,1205,1328,1369,1436,1488,1582,1587,1588)), .combine=rbind) %dopar% {
  gc()
  ind<-which(stock_pv$code==mycode[i])
  pv_tmp<-stock_pv[ind,]
  pv_tmp$N_sp<-0
  pv_tmp$p_df_d1<-0
  pv_tmp$p_df_d2<-0
  pv_tmp$p_df_d3<-0
  pv_tmp$p_df_d5<-0
  pv_tmp$p_df_d10<-0
  pv_tmp$p_df_d20<-0
  pv_tmp$p_df_d30<-0
  pv_tmp$p_df_d50<-0
  pv_tmp$r_df_d1<-0
  pv_tmp$r_df_d2<-0
  pv_tmp$r_df_d3<-0
  pv_tmp$r_df_d5<-0
  pv_tmp$r_df_d10<-0
  pv_tmp$r_df_d20<-0
  pv_tmp$r_df_d30<-0
  pv_tmp$r_df_d50<-0
  pv_tmp$for_r_d1<-0
  pv_tmp$for_r_d2<-0
  pv_tmp$for_r_d3<-0
  pv_tmp$for_r_d5<-0
  pv_tmp$for_r_d10<-0
  pv_tmp$for_r_d20<-0
  pv_tmp$for_r_d30<-0
  pv_tmp$for_r_d50<-0
  pv_tmp$org_r_d1<-0
  pv_tmp$org_r_d2<-0
  pv_tmp$org_r_d3<-0
  pv_tmp$org_r_d5<-0
  pv_tmp$org_r_d10<-0
  pv_tmp$org_r_d20<-0
  pv_tmp$org_r_d30<-0
  pv_tmp$org_r_d50<-0
  pv_tmp$prv_r_d1<-0
  pv_tmp$prv_r_d2<-0
  pv_tmp$prv_r_d3<-0
  pv_tmp$prv_r_d5<-0
  pv_tmp$prv_r_d10<-0
  pv_tmp$prv_r_d20<-0
  pv_tmp$prv_r_d30<-0
  pv_tmp$prv_r_d50<-0
  pv_tmp$close_r_d1<-0
  pv_tmp$close_r_d2<-0
  pv_tmp$close_r_d3<-0
  pv_tmp$close_r_d5<-0
  pv_tmp$close_r_d10<-0
  pv_tmp$close_r_d20<-0
  pv_tmp$close_r_d30<-0
  pv_tmp$close_r_d50<-0
  pv_tmp<-pv_tmp[order(pv_tmp$date,decreasing=T),]
  # j<-1
  # head(pv_tmp);tail(pv_tmp)
  for (j in 1:(nrow(pv_tmp)-50)) {
    if (j==1) {
      pv_tmp$N_sp[j]<-0
    } else {
      pv_tmp$N_sp[j]<-pv_tmp$High[j-1]-pv_tmp$Open[j-1]
    }
    pv_tmp$p_df_d1[j]<-pv_tmp$Close[j]-pv_tmp$Close[j+1]
    pv_tmp$p_df_d2[j]<-pv_tmp$Close[j]-pv_tmp$Close[j+2]
    pv_tmp$p_df_d3[j]<-pv_tmp$Close[j]-pv_tmp$Close[j+3]
    pv_tmp$p_df_d5[j]<-pv_tmp$Close[j]-pv_tmp$Close[j+5]
    pv_tmp$p_df_d10[j]<-pv_tmp$Close[j]-pv_tmp$Close[j+10]
    pv_tmp$p_df_d20[j]<-pv_tmp$Close[j]-pv_tmp$Close[j+20]
    pv_tmp$p_df_d30[j]<-pv_tmp$Close[j]-pv_tmp$Close[j+30]
    pv_tmp$p_df_d50[j]<-pv_tmp$Close[j]-pv_tmp$Close[j+50]
    pv_tmp$r_df_d1[j]<-pv_tmp$Close[j]/pv_tmp$Close[j+1]-1
    pv_tmp$r_df_d2[j]<-pv_tmp$Close[j]/pv_tmp$Close[j+2]-1
    pv_tmp$r_df_d3[j]<-pv_tmp$Close[j]/pv_tmp$Close[j+3]-1
    pv_tmp$r_df_d5[j]<-pv_tmp$Close[j]/pv_tmp$Close[j+5]-1
    pv_tmp$r_df_d10[j]<-pv_tmp$Close[j]/pv_tmp$Close[j+10]-1
    pv_tmp$r_df_d20[j]<-pv_tmp$Close[j]/pv_tmp$Close[j+20]-1
    pv_tmp$r_df_d30[j]<-pv_tmp$Close[j]/pv_tmp$Close[j+30]-1
    pv_tmp$r_df_d50[j]<-pv_tmp$Close[j]/pv_tmp$Close[j+50]-1
    pv_tmp$for_r_d1[j]<-pv_tmp$for_r[j]-pv_tmp$for_r[j+1] ##
    pv_tmp$for_r_d2[j]<-pv_tmp$for_r[j]-pv_tmp$for_r[j+2]
    pv_tmp$for_r_d3[j]<-pv_tmp$for_r[j]-pv_tmp$for_r[j+3]
    pv_tmp$for_r_d5[j]<-pv_tmp$for_r[j]-pv_tmp$for_r[j+5]
    pv_tmp$for_r_d10[j]<-pv_tmp$for_r[j]-pv_tmp$for_r[j+10]
    pv_tmp$for_r_d20[j]<-pv_tmp$for_r[j]-pv_tmp$for_r[j+20]
    pv_tmp$for_r_d30[j]<-pv_tmp$for_r[j]-pv_tmp$for_r[j+30]
    pv_tmp$for_r_d50[j]<-pv_tmp$for_r[j]-pv_tmp$for_r[j+50]
    pv_tmp$org_r_d1[j]<-pv_tmp$org_r[j]-pv_tmp$org_r[j+1]
    pv_tmp$org_r_d2[j]<-pv_tmp$org_r[j]-pv_tmp$org_r[j+2]
    pv_tmp$org_r_d3[j]<-pv_tmp$org_r[j]-pv_tmp$org_r[j+3]
    pv_tmp$org_r_d5[j]<-pv_tmp$org_r[j]-pv_tmp$org_r[j+5]
    pv_tmp$org_r_d10[j]<-pv_tmp$org_r[j]-pv_tmp$org_r[j+10]
    pv_tmp$org_r_d20[j]<-pv_tmp$org_r[j]-pv_tmp$org_r[j+20]
    pv_tmp$org_r_d30[j]<-pv_tmp$org_r[j]-pv_tmp$org_r[j+30]
    pv_tmp$org_r_d50[j]<-pv_tmp$org_r[j]-pv_tmp$org_r[j+50]
    pv_tmp$prv_r_d1[j]<-pv_tmp$prv_r[j]-pv_tmp$prv_r[j+1]
    pv_tmp$prv_r_d2[j]<-pv_tmp$prv_r[j]-pv_tmp$prv_r[j+2]
    pv_tmp$prv_r_d3[j]<-pv_tmp$prv_r[j]-pv_tmp$prv_r[j+3]
    pv_tmp$prv_r_d5[j]<-pv_tmp$prv_r[j]-pv_tmp$prv_r[j+5]
    pv_tmp$prv_r_d10[j]<-pv_tmp$prv_r[j]-pv_tmp$prv_r[j+10]
    pv_tmp$prv_r_d20[j]<-pv_tmp$prv_r[j]-pv_tmp$prv_r[j+20]
    pv_tmp$prv_r_d30[j]<-pv_tmp$prv_r[j]-pv_tmp$prv_r[j+30]
    pv_tmp$prv_r_d50[j]<-pv_tmp$prv_r[j]-pv_tmp$prv_r[j+50]
    pv_tmp$close_r_d1[j]<-pv_tmp$close_r[j]-pv_tmp$close_r[j+1]
    pv_tmp$close_r_d2[j]<-pv_tmp$close_r[j]-pv_tmp$close_r[j+2]
    pv_tmp$close_r_d3[j]<-pv_tmp$close_r[j]-pv_tmp$close_r[j+3]
    pv_tmp$close_r_d5[j]<-pv_tmp$close_r[j]-pv_tmp$close_r[j+5]
    pv_tmp$close_r_d10[j]<-pv_tmp$close_r[j]-pv_tmp$close_r[j+10]
    pv_tmp$close_r_d20[j]<-pv_tmp$close_r[j]-pv_tmp$close_r[j+20]
    pv_tmp$close_r_d30[j]<-pv_tmp$close_r[j]-pv_tmp$close_r[j+30]
    pv_tmp$close_r_d50[j]<-pv_tmp$close_r[j]-pv_tmp$close_r[j+50]
  }
  return(pv_tmp)
}) # Parallel 2min 13sec

head(stock_pv_dr)
stock_pv_dr[1:6,c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp")]
stock_pv_dr$RN_sp<-as.numeric(sprintf("%.1f",stock_pv_dr$N_sp/stock_pv_dr$Close*100))
stock_pv_dr$Targ<-as.numeric(stock_pv_dr$RN_sp>=4)
stock_pv_dr[1:6,c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]

stock_pv_dr_dlt<-stock_pv_dr
fn<-paste('data/stock_pv_dr_dlt_',tdy,'.rdata',sep='')
save(stock_pv_dr_dlt,file=fn)
###################################
load(file=fn)
# stock_pv_dr<-stock_pv_dr_bkup

head(stock_pv_dr,1)
str(stock_pv_dr)
attr_int<-c("r_df_d","for_r_d","org_r_d","prv_r_d","close_r_d")
days_int<-c(1,2,3,5,10,20,30,50)
hdr_r<-c("r_diff","f_rate","close_r","vol_r","vol_var","for_r","org_r","prv_r")
for (i in 1:length(attr_int)){
  hdr_r<-c(hdr_r,paste(attr_int[i],days_int,sep=""))
  print(c(i, hdr_r))
}

(ind<-which(names(stock_pv_dr) %in% hdr_r))
names(stock_pv_dr)[ind]
head(stock_pv_dr,1)

# i<-1
head(stock_pv_dr[,ind[i]]);head(as.numeric(sprintf("%.1f",stock_pv_dr[,ind[i]])))
for (i in 1:length(ind)) {
  stock_pv_dr[,ind[i]]<-as.numeric(sprintf("%.1f",stock_pv_dr[,ind[i]]))
} # 60 sec

head(stock_pv_dr,2)
str(stock_pv_dr)
stock_pv_dr[1:6,c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]
stock_pv_dr[(nrow(stock_pv_dr)-6):nrow(stock_pv_dr),c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]
head(stock_pv_dr,2)
stock_pv_dr[59:61,]
tail(stock_pv_dr)

ind<-which(stock_pv_dr$N_sp==0&stock_pv_dr$date<today)
stock_pv_dr[ind[1:6],c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]

stock_pv_dr_dlt<-stock_pv_dr

(fn<-paste('data/stck_pv_dr_',ydy,'.rdata',sep=''))
load(file=fn)

head(stock_pv_dr,2)
head(stock_pv_dr_dlt,2)
stock_pv_dr_dlt[59:61,]

ind<-which(stock_pv_dr$date>='2018-03-26')
table(stock_pv_dr$date[ind])
ind_dlt<-which(stock_pv_dr_dlt$date>='2018-03-26')
table(stock_pv_dr_dlt$date[ind_dlt])

st_tmp<-rbind(stock_pv_dr[-ind,],stock_pv_dr_dlt[ind_dlt,])
stock_pv_dr<-st_tmp

(fn<-paste('data/stck_pv_dr_',tdy,'.rdata',sep=''))
save(stock_pv_dr,file=fn)

selected<-unique(stock_pv_dr[,c("code","exch_mkt")])
save(selected,file=paste('Data/selected_',tdy,'.rdata',sep=''))

