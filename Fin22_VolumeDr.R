rm(list=ls());cat('\f')
library(doParallel)
registerDoParallel(6)

tdy<-'180510'
load(file=paste("Data/st_pv_",tdy,".rdata",sep=""))
load(file=paste("Data/volume_",tdy,".rdata",sep=""))

head(st_pv);head(volume)
st_pv<-merge(st_pv,volume,by=c("code","date","exch_mkt"))

str(st_pv[,1:10])
st_pv$code<-as.character(st_pv$code)
st_pv$exch_mkt<-as.character(st_pv$exch_mkt)

pv<-st_pv

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
# i<-1
system.time(stock_pv<-foreach (i = 1:length(mycode), .combine=rbind) %dopar% {
  pv_tmp<-pv[pv$code==mycode[i],]
  # head(pv_tmp,2);tail(pv_tmp,2)
  # k<-1
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
  summary(pv_tmp$org_tot)
  pv_tmp$org_tot<-pv_tmp$org_tot-min(pv_tmp$org_tot)
  summary(pv_tmp$prv_tot)
  pv_tmp$prv_tot<-pv_tmp$prv_tot-min(pv_tmp$prv_tot)
  summary(pv_tmp$total)
  pv_tmp$total<-pv_tmp$f_tot+pv_tmp$org_tot+pv_tmp$prv_tot
  pv_tmp$for_r<-pv_tmp$f_tot/pv_tmp$total*100
  pv_tmp$org_r<-pv_tmp$org_tot/pv_tmp$total*100
  pv_tmp$prv_r<-pv_tmp$prv_tot/pv_tmp$total*100
  pv_tmp$close_r<-pv_tmp$Close/max(pv_tmp$Close)*100
  pv_tmp$vol_r<-pv_tmp$Volume/max(pv_tmp$Volume)*100
  summary(pv_tmp$vol_var)
  pv_tmp$vol_var<-100-min(pv_tmp$Volume)/max(pv_tmp$Volume)
  print(paste(i,"/",length(mycode)))
  return(pv_tmp)
}) # 207 sec / 1621 주식

head(stock_pv)

head(stock_pv,1)
mycode<-unique(stock_pv$code)
# i<-1
gc()
system.time(stock_pv_dr<-foreach (i = 1:length(mycode), .combine=rbind) %dopar% {
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
}) # Parallel 12min 34sec # Serial 1hr 27min / 1621 주식

head(stock_pv_dr)
stock_pv_dr[1:6,c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp")]

stock_pv_dr$RN_sp<-as.numeric(sprintf("%.1f",stock_pv_dr$N_sp/stock_pv_dr$Close*100))
stock_pv_dr$Targ<-as.numeric(stock_pv_dr$RN_sp>=2)
stock_pv_dr[1:6,c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]

stock_pv_dr_bk<-stock_pv_dr
fn<-paste('data/stock_pv_dr_bk_',tdy,'.rdata',sep='')

save(stock_pv_dr_bk,file=fn)
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

(fn<-paste('data/stck_pv_dr_',tdy,'.rdata',sep=''))
save(stock_pv_dr,file=fn)

selected<-unique(stock_pv_dr[,c("code","exch_mkt")])
save(selected,file=paste('Data/selected_',tdy,'.rdata',sep=''))

