cat("\f");rm(list=ls())
library(doParallel)
registerDoParallel(6)

tdy<-'180510'
fn<-paste('data/stck_pv_dr_',tdy,'.rdata',sep='')
load(file=fn)

st_pv<-stock_pv_dr
head(st_pv);tail(st_pv)
st_pv[1:10,c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]

##########################
# Set Target
##########################
T_Rvn<-4
st_pv$Targ<-as.numeric(st_pv$RN_sp>=T_Rvn)
table(st_pv$Targ)
table(st_pv$exch_mkt)

tt_e<-'2018-04-27'
(tt_s<-as.character(as.Date(tt_e)-4))
(tr_s<-as.character(as.Date(tt_e)-125));(tr_e<-as.character(as.Date(tt_e)-5))
(tt_days<-as.numeric(as.Date(tt_e)-as.Date(tt_s)))

# mrk<-'PI' 
# mrk<-'ALL'

#### PI Model (BGN) ####
########################
prc_l<-3000;prc_h<-30000;mrk<-'PI' # PI Model
# prc_l<-3000;prc_h<-30000;mrk<-'PI' 4:4
# prc_l<-3000;prc_h<-10000;mrk<-'PI' 1:6
# prc_l<-1000;prc_h<-15000;mrk<-'PI' 3:7
#### PI Model (END) ####

#### DAQ Model (BGN) ####
prc_l<-3000;prc_h<-30000;mrk<-'DAQ' # DAQ Model
# prc_l<-1000;prc_h<-90000  23 (16:7)
# prc_l<-2000~4000;prc_h<-60000~100000 DAQ 9 (3:6)
# prc_l<-3000;prc_h<-60000 DAQ 10
#### DAQ Model (END) ####

#### Sample bin (BGN) ####
class(st_pv$date)
today<-'2018-05-09'
st_pv_tdy_D<-st_pv[st_pv$date==today&st_pv$exch_mkt=="KOSDAQ",]
table(st_pv_tdy_D$Targ)

st_pv_tdy_D$prc_bn<-cut(st_pv_tdy_D$Close,breaks=c(0,1000,2000,3000,5000,10000,20000,30000,50000,100000,200000,300000,500000,1000000,2000000))
# head(st_pv_tdy_D)
table(st_pv_tdy_D$Targ,st_pv_tdy_D$prc_bn)
table(st_pv_tdy_D$Targ)

st_pv_tdy_P<-st_pv[st_pv$date==today&st_pv$exch_mkt=="KOSPI",]
st_pv_tdy_P$prc_bn<-cut(st_pv_tdy_P$Close,breaks=c(0,1000,2000,3000,5000,10000,20000,30000,50000,100000,200000,300000,500000,1000000,2000000))
# head(st_pv_tdy_P)
table(st_pv_tdy_P$Targ,st_pv_tdy_P$prc_bn)
#### Sample Bin (END) ####

#################################
### BGN of Mdl with PV
#################################
st_pv$Targ<-as.numeric(st_pv$RN_sp>=T_Rvn)
table(st_pv$Targ)[2]/sum(table(st_pv$Targ))
ind<-which(st_pv$Targ==1)
st_pv[(ind[10]-1):(ind[10]+2),c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]
st_pv[(ind[length(ind)]-1):(ind[length(ind)]+2),c("code","date","exch_mkt","Open","High","Low","Close","p_diff","N_sp","RN_sp","Targ")]

head(st_pv,2)
hdr_ex<-which(names(st_pv) %in% c("date","code","exch_mkt","Open","High","Low","Close","p_diff","Volume","N_sp","RN_sp","f_tot","f_net","o_net","org_tot","prv_tot","total","p_df_d1","p_df_d2","p_df_d3","p_df_d5","p_df_d10","p_df_d20","p_df_d30","p_df_d50"))

ind<-which(st_pv$date==st_pv$date[1] & st_pv$Close>=prc_l & st_pv$Close<prc_h);length(ind)
s_code<-unique(st_pv$code[ind]);length(ind)
table(st_pv$exch_mkt)

if (mrk=='DAQ') {
  ind_tr<-which(st_pv$date>=tr_s & st_pv$date<=tr_e & st_pv$code %in% s_code & st_pv$exch_mkt=="KOSDAQ")
  ind_tt<-which(st_pv$date>=tt_s & st_pv$date<=tt_e & st_pv$code %in% s_code & st_pv$exch_mkt=="KOSDAQ")
} else if (mrk=='PI') {
  ind_tr<-which(st_pv$date>=tr_s & st_pv$date<=tr_e & st_pv$code %in% s_code & st_pv$exch_mkt=="KOSPI")
  ind_tt<-which(st_pv$date>=tt_s & st_pv$date<=tt_e & st_pv$code %in% s_code & st_pv$exch_mkt=="KOSPI")
} else {
  ind_tr<-which(st_pv$date>=tr_s & st_pv$date<=tr_e & st_pv$code %in% s_code)
  ind_tt<-which(st_pv$date>=tt_s & st_pv$date<=tt_e & st_pv$code %in% s_code)
}


for (i in 0:tt_days) if (i==0) tt_dates<-tt_s else tt_dates<-c(tt_dates,as.character(as.Date(tt_s)+i))
tt_dates
tail(st_pv[ind_tr,-hdr_ex])
tail(st_pv[ind_tr,])

library(party)
st_pv$Targ<-factor(st_pv$Targ)
table(st_pv$Targ)
table(st_pv$Targ)[2]/table(st_pv$Targ)[1]*100
table(st_pv$Targ[ind_tr])[2]/table(st_pv$Targ[ind_tr])[1]*100

V_mdl<-ctree(Targ~., data=st_pv[ind_tr,-hdr_ex])
table(predict(V_mdl),st_pv[ind_tr,"Targ"])
table(predict(V_mdl,newdata=st_pv[ind_tt,]),st_pv[ind_tt,"Targ"])


#################################
# Predict
#################################
# tt_s<-'2018-04-22';tt_e<-'2018-04-27'
if (mrk=='DAQ') {
  ind_tt<-which(st_pv$date>=tt_s & st_pv$date<=tt_e & st_pv$code %in% s_code & st_pv$exch_mkt=="KOSDAQ")
} else if (mrk=='PI') {
  ind_tt<-which(st_pv$date>=tt_s & st_pv$date<=tt_e & st_pv$code %in% s_code & st_pv$exch_mkt=="KOSPI")
} else {
  ind_tt<-which(st_pv$date>=tt_s & st_pv$date<=tt_e & st_pv$code %in% s_code)
}
prd_r<-as.numeric(predict(V_mdl,newdata=st_pv[ind_tt,],type="response"))-1
class(prd_r);table(prd_r)
prd_p<-sapply(predict(V_mdl,newdata=st_pv[ind_tt,],type="prob"), function(l) l[[2]])
prd_n<-predict(V_mdl,newdata=st_pv[ind_tt,],type="node")

res1<-cbind(st_pv[ind_tt,],prd_r,prd_p,prd_n)

# V_mdl@tree

table(res1$date)
tail(res1)
ind<-which(res1$prd_r==1);length(ind)
hdr<-c("code","date","exch_mkt","Open","High","Low","Close","p_diff","r_diff","N_sp","RN_sp","Targ","prd_r","prd_p","prd_n")
res_1<-res1[ind,hdr]
res_o1<-res_1[order(res_1$date),]
res_o1$rvn<-(as.numeric(res_o1$Targ)-1)*(res_o1$prd_r)*(T_Rvn-1)-(1-(as.numeric(res_o1$Targ)-1))*(res_o1$prd_r)-0.3

table(res_o1$RN_sp>T_Rvn-1)
mean(res_1$RN_sp)
mean(res_o1$rvn)

res_o1
res_o1[order(res_o1$code,res_o1$date),]

i<-1
tt_dates[i];(res_td01<-res_o1[res_o1$date==tt_dates[i],]);mean(res_td01$rvn)
i<-i+1

table(res_o1$prd_n)
nd<-unique(res_o1$prd_n)
ind<-NA
for (i in 1:length(nd)){
  ind_t<-which(res_o1$prd_n==nd[i])
  if (sum(res_o1[res_o1$prd_n==nd[i],"rvn"])>0) {ind<-c(ind,ind_t)}
  print(paste(nd[i],mean(res_o1[res_o1$prd_n==nd[i],"rvn"])))
}

table(res_o1$prd_n[ind[-1]])
table(res_o1$date[ind[-1]])
sum(res_o1$rvn[ind[-1]])

save(list=ls(),file='data/FNL_20180504.rdata')
load(file='data/FNL_20180814.rdata')
