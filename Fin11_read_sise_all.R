cat("\f");rm(list=ls())
# For Windows
# install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=6)
# install.packages("doMC")
library(doMC)
registerDoMC(cores=6)

load(file='./Data/code_all_20180319.rdata')

selected<-code_all[,c('code','exch_mkt')]
table(code_all$exch_mkt)

system.time(sise<-foreach(i=c(1501:1510), .combine=rbind) %dopar% {
  require(XML);Sys.setlocale("LC_ALL", "C")
  for (k in 1:30){
    theurl<-paste("http://stock.daum.net/item/quote_yyyymmdd_sub.daum?page=",k,"&code=",selected$code[i], "&modify=0",sep="")
    sise_html<-readHTMLTable(theurl)
    if (length(sise_html[[1]])>0) {
      if (k==1) sise_table_tmp<-sise_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:40),] else sise_table_tmp<-sise_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),]
      if (k==1) sise_table<-sise_table_tmp else sise_table<-rbind(sise_table,sise_table_tmp)
    }
  }
  sise_table$code<-selected$code[i]
  sise_table$exch_mkt<-selected$exch_mkt[i]
  return(sise_table)
})

head(sise);tail(sise)
sise<-na.omit(sise)

names(sise)<-c("date","Open","High","Low","Close","p_diff","r_diff","Volume")
sise$date<-paste("20",sise$date,sep="")
sise[,2]<-as.numeric(gsub(",","",as.character(sise[,2])))
sise[,3]<-as.numeric(gsub(",","",as.character(sise[,3])))
sise[,4]<-as.numeric(gsub(",","",as.character(sise[,4])))
sise[,5]<-as.numeric(gsub(",","",as.character(sise[,5])))
sise[,6]<-gsub("▼","-",as.character(sise[,6]))
sise[,6]<-gsub("↓","-",sise[,6])
sise[,6]<-gsub("▲","",sise[,6])
sise[,6]<-as.numeric(gsub(",","",sise[,6]))
sise[,7]<-as.numeric(gsub("%","",as.character(sise[,7])))
sise[,8]<-as.numeric(gsub(",","",as.character(sise[,8])))
sise$date <- gsub("\\.","-",sise$date)

head(sise);tail(sise)
table(duplicated(sise))

ind<-which(duplicated(sise))
sise_cls<-sise
if (length(ind)>0) sise_cls<-sise[-ind,]

table(duplicated(sise_cls))

table(table(sise_cls$code))

sise_cls<-sise_cls[order(sise_cls$date,decreasing=T),]
sise_cls<-sise_cls[order(sise_cls$code),]
sise_cls<-sise_cls[order(sise_cls$exch_mkt,decreasing=T),]
row.names(sise_cls)<-NULL


# Data Backup
tdy<-"180323"
save(sise_cls,file=paste("Data/sise_cls_",tdy,".rdata",sep=""))
save(selected,file=paste("Data/selected_",tdy,".rdata",sep=""))
save(selected,sise_cls,sise_fnl,file=paste("Data/sise_bkup",tdy,".rdata",sep=""))

# Alternative
# For MaC
install.packages("doMC")
library(doMC)
registerDoMC(6)

n<-1;m<-20

gc()
library(doParallel)
registerDoParallel(cores=6)
system.time(sise<-foreach(i=n:m, .combine='rbind') %dopar% {
  require(XML);Sys.setlocale("LC_ALL", "C")
  for (k in 1:30){
    theurl<-paste("http://stock.daum.net/item/quote_yyyymmdd_sub.daum?page=",k,"&code=",selected$code[i], "&modify=0",sep="")
    sise_html<-readHTMLTable(theurl)
    if (length(sise_html[[1]])>0) {
      if (k==1) sise_table_tmp<-sise_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:40),] else sise_table_tmp<-sise_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),]
      if (k==1) sise_table<-sise_table_tmp else sise_table<-rbind(sise_table,sise_table_tmp)
    }
  }
  sise_table$code<-selected$code[i]
  sise_table$exch_mkt<-selected$exch_mkt[i]
  return(sise_table)
}) # 10개주식 10 SEC 

if (n==1) sise_fnl<-sise
if (n>1 & length(unique(sise_fnl$code))==m) sise_fnl<-rbind(sise_fnl,sise)
if (length(unique(sise_fnl$code))==m) (l<-length(unique(sise_fnl$code)));(n<-l+1);(m<-min(l+20,nrow(selected)))

length(unique(sise_fnl$code))

(fn<-paste("sise_",length(unique(sise_fnl$code)),".rdata",sep=""))
save(sise_fnl,file=fn)
save(list=ls(),file="bkup.rdata")
