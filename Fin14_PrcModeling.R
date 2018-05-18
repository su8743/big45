cat("\f");rm(list=ls())
library(doParallel)
registerDoParallel(6)

tdy<-'180504'
(fn<-paste("Data/st_pv_",tdy,".rdata",sep=""))
load(file=fn)

head(st_pv);tail(st_pv)

mycode<-unique(st_pv$code)
(t_pv<-table(st_pv$date))
max(t_pv);min(t_pv)

# Next Day Spread (High - Open)
system.time(stock_pv_dr<-foreach (i=1:length(mycode), .combine='rbind') %dopar% {
  ind<-which(st_pv$code==mycode[i])
  pv_tmp<-st_pv[ind,]
  pv_tmp$N_sp<-0
  pv_tmp<-pv_tmp[order(pv_tmp$date,decreasing=T),]
  for (j in 1:nrow(pv_tmp)) {
    if (j==1) pv_tmp$N_sp[j]<-0 else pv_tmp$N_sp[j]<-pv_tmp$High[j-1]-pv_tmp$Open[j-1]
  }
  return(pv_tmp)
}) # 1620개 주식 108 SEC

head(stock_pv_dr);tail(stock_pv_dr)
stock_pv_dr$RN_sp<-as.numeric(sprintf("%.1f",stock_pv_dr$N_sp/stock_pv_dr$Close*100))
stock_pv_dr$Targ<-as.numeric(stock_pv_dr$RN_sp>=4)

### Set Target
T_Rvn<-4;d<-5
tt_e<-'2018-04-27'
(tt_s<-as.character(as.Date(tt_e)-d))
(tr_s<-as.character(as.Date(tt_e)-130));(tr_e<-as.character(as.Date(tt_e)-d-1))
(tt_days<-as.numeric(as.Date(tt_e)-as.Date(tt_s)))

prc_l<-3000;prc_h<-30000;mrk<-'DAQ' # DAQ Model

### BGN of Mdl with Price
table(stock_pv_dr$RN_sp>=T_Rvn)
stock_pv_dr$Targ<-as.numeric(stock_pv_dr$RN_sp>=T_Rvn)
table(stock_pv_dr$Targ)[2]/table(stock_pv_dr$Targ)[1]
# Eye Check (Validation)
ind<-which(stock_pv_dr$Targ==1)
stock_pv_dr[(ind[length(ind)]-1):(ind[length(ind)]+2),c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]
stock_pv_dr[(ind[10]-1):(ind[10]+2),c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]
head(stock_pv_dr,2)
hdr_ex<-which(names(stock_pv_dr) %in% c("code","date","Open","High","Low","Close","p_diff","Volume","exch_mkt","N_sp","RN_sp")) # Variables not used to create model

stock_pv_dr$date[1];prc_l;prc_h
ind<-which(stock_pv_dr$date==stock_pv_dr$date[1] & stock_pv_dr$Close>=prc_l & stock_pv_dr$Close<prc_h);length(ind)
s_code<-unique(stock_pv_dr$code[ind]);length(ind)
table(stock_pv_dr[stock_pv_dr$code %in% s_code,"exch_mkt"])

if (mrk=='DAQ') {
  ind_tr<-which(stock_pv_dr$date>=tr_s & stock_pv_dr$date<=tr_e & stock_pv_dr$code %in% s_code & stock_pv_dr$exch_mkt=="KOSDAQ")
  ind_tt<-which(stock_pv_dr$date>=tt_s & stock_pv_dr$date<=tt_e & stock_pv_dr$code %in% s_code & stock_pv_dr$exch_mkt=="KOSDAQ")
} else if (mrk=='PI') {
  ind_tr<-which(stock_pv_dr$date>=tr_s & stock_pv_dr$date<=tr_e & stock_pv_dr$code %in% s_code & stock_pv_dr$exch_mkt=="KOSPI")
  ind_tt<-which(stock_pv_dr$date>=tt_s & stock_pv_dr$date<=tt_e & stock_pv_dr$code %in% s_code & stock_pv_dr$exch_mkt=="KOSPI")
} else {
  ind_tr<-which(stock_pv_dr$date>=tr_s & stock_pv_dr$date<=tr_e & stock_pv_dr$code %in% s_code)
  ind_tt<-which(stock_pv_dr$date>=tt_s & stock_pv_dr$date<=tt_e & stock_pv_dr$code %in% s_code)
}


for (i in 0:tt_days) if (i==0) tt_dates<-tt_s else tt_dates<-c(tt_dates,as.character(as.Date(tt_s)+i))
tt_dates
tail(stock_pv_dr[ind_tr,-hdr_ex])
tail(stock_pv_dr[ind_tr,])

library(party) # install.packages("party")
stock_pv_dr$Targ<-factor(stock_pv_dr$Targ)
table(stock_pv_dr$Targ)
table(stock_pv_dr$Targ)[2]/table(stock_pv_dr$Targ)[1]*100
table(stock_pv_dr$Targ[ind_tr])[2]/table(stock_pv_dr$Targ[ind_tr])[1]*100

V_mdl<-ctree(Targ~., data=stock_pv_dr[ind_tr,-hdr_ex])
table(predict(V_mdl),stock_pv_dr[ind_tr,"Targ"])
table(predict(V_mdl,newdata=stock_pv_dr[ind_tt,]),stock_pv_dr[ind_tt,"Targ"])

### Predict
if (mrk=='DAQ') {
  ind_tt<-which(stock_pv_dr$date>=tt_s & stock_pv_dr$date<=tt_e & stock_pv_dr$code %in% s_code & stock_pv_dr$exch_mkt=="KOSDAQ")
} else if (mrk=='PI') {
  ind_tt<-which(stock_pv_dr$date>=tt_s & stock_pv_dr$date<=tt_e & stock_pv_dr$code %in% s_code & stock_pv_dr$exch_mkt=="KOSPI")
} else {
  ind_tt<-which(stock_pv_dr$date>=tt_s & stock_pv_dr$date<=tt_e & stock_pv_dr$code %in% s_code)
}

# node와 확률 찾기
prd_r<-as.numeric(predict(V_mdl,newdata=stock_pv_dr[ind_tt,],type="response"))-1
class(prd_r);table(prd_r)
prd_p<-sapply(predict(V_mdl,newdata=stock_pv_dr[ind_tt,],type="prob"), function(l) l[[2]])
prd_n<-predict(V_mdl,newdata=stock_pv_dr[ind_tt,],type="node")

res1<-cbind(stock_pv_dr[ind_tt,],prd_r,prd_p,prd_n)

V_mdl@tree

table(res1$date)
tail(res1)
ind<-which(res1$prd_r==1);length(ind)
hdr<-c("code","date","exch_mkt","Open","High","Low","Close","p_diff","r_diff","N_sp","RN_sp","Targ","prd_r","prd_p","prd_n")
res_1<-res1[ind,hdr]
res_o1<-res_1[order(res_1$date),]
res_o1$rvn<-(as.numeric(res_o1$Targ)-1)*(res_o1$prd_r)*(T_Rvn-1)-(1-(as.numeric(res_o1$Targ)-1))*(res_o1$prd_r)-0.3

table(res_o1$RN_sp>T_Rvn-1)
sum(res_1$RN_sp)
sum(res_o1$rvn)

# 결과 Eye Check
res_o1
res_o1[order(res_o1$code,res_o1$date),]

# 일자별 수익률
i<-1
tt_dates[i];(res_td01<-res_o1[res_o1$date==tt_dates[i],]);sum(res_td01$rvn)
i<-i+1

# 노드별 갯수
table(res_o1$prd_n)
nd<-unique(res_o1$prd_n)
ind<-NA
for (i in 1:length(nd)){
  ind_t<-which(res_o1$prd_n==nd[i])
  if (sum(res_o1[res_o1$prd_n==nd[i],"rvn"])>0) {ind<-c(ind,ind_t)}
  print(paste(nd[i],sum(res_o1[res_o1$prd_n==nd[i],"rvn"])))
}

table(res_o1$prd_n[ind[-1]])
table(res_o1$date[ind[-1]])
sum(res_o1$rvn[ind[-1]])

save(list=ls(),file='data/FNL_20180504.rdata')
load(file='data/FNL_20180504.rdata')
head(st_pv)
selected<-unique(st_pv[,c("code","exch_mkt")])
save(selected,file=paste("Data/slt_v_",tdy,".rdata",sep=""))

save(st_pv,stock_pv_dr,file=paste('Data/st_p_',tdy,'.rdata',sep=''))
