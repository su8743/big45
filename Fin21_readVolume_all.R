cat("\f");rm(list=ls())
# For Windows
library(doParallel)
registerDoParallel(cores=6)

ydy<-"180504"
load(file=paste("Data/selected_",ydy,".rdata",sep=""))

n<-101;m<-140
i<-101
system.time(volume<-foreach(i=n:m, .combine='rbind') %dopar% {
  require(XML);Sys.setlocale("LC_ALL", "C")
  for (k in 1:30) {
    theurl<-paste("http://stock.daum.net/item/foreign_yyyymmdd.daum?page=",k,"&code=",selected$code[i],sep="")
    volume_html<-readHTMLTable(theurl)
    if (length(volume_html[[1]])>0) {
      volume_table_tmp<-volume_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),c(1:5)]
      volume_table_tmp$code<-selected$code[i]
      volume_table_tmp$exch_mkt<-selected$exch_mkt[i]
      if (k==1) volume_table<-volume_table_tmp else volume_table<-rbind(volume_table,volume_table_tmp)
    }
  }
  head(volume_table)
  volume_table<-na.omit(volume_table)
  volume_table
})

if(n==1) volume_fnl<-volume
if(n>1 & length(unique(volume_fnl$code))==m) volume_fnl<-rbind(volume_fnl,volume)
if(length(unique(volume_fnl$code))==m) (l<-length(unique(volume_fnl$code)));(n<-l+1);(m<-min(l+20,nrow(selected)))

volume_fnl_bkup<-volume_fnl

head(volume_fnl)
names(volume_fnl)[1:5]<-c("date","f_tot","f_rate","f_net","o_net")
volume_fnl$date<-paste("20",volume_fnl$date,sep="")
volume_fnl[,1]<-gsub("\\.","-",volume_fnl[,1])
volume_fnl[,2]<-as.numeric(gsub(",","",as.character(volume_fnl[,2])))
volume_fnl[,3]<-as.numeric(gsub("%","",as.character(volume_fnl[,3])))
volume_fnl[,4]<-as.numeric(gsub(",","",as.character(volume_fnl[,4])))
volume_fnl[,5]<-as.numeric(gsub(",","",as.character(volume_fnl[,5])))

volume_fnl<-volume_fnl[order(volume_fnl$date,decreasing=T),]
volume_fnl<-volume_fnl[order(volume_fnl$code),]
volume_fnl<-volume_fnl[order(volume_fnl$exch_mkt,decreasing=T),]

table(duplicated(volume_fnl))
ind<-which(duplicated(volume_fnl))
volume_cls_t<-volume_fnl
if (length(ind)>0) volume_cls_t<-volume_fnl[-ind,]

volume_cls<-na.omit(volume_cls_t)
row.names(volume_cls)<-NULL

head(volume_cls)
table(table(volume_cls$code))

tdy<-"180323"
(fn<-paste("Data/volume_",tdy,".rdata",sep=""))
save(volume_cls,file=fn)
load(file=fn)

n<-1;m<-20;l<-0

gc()
library(doParallel)
registerDoParallel(cores=6)
system.time(volume<-foreach(i=c(n:m), .combine='rbind') %dopar% {
  require(XML);Sys.setlocale("LC_ALL", "C")
  for (k in 1:30) {
    theurl<-paste("http://stock.daum.net/item/foreign_yyyymmdd.daum?page=",k,"&code=",selected$code[i],sep="")
    volume_html<-readHTMLTable(theurl)
    if (length(volume_html[[1]])>0) {
      volume_table_tmp<-volume_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),c(1:5)]
      volume_table_tmp$code<-selected$code[i]
      volume_table_tmp$exch_mkt<-selected$exch_mkt[i]
      if (k==1) volume_table<-volume_table_tmp else volume_table<-rbind(volume_table,volume_table_tmp)
    }
  }
  volume_table<-na.omit(volume_table)
  volume_table
}) # 30 sec / 50개 주식

if(n==1) volume_fnl<-volume
if(n>1 & length(unique(volume_fnl$code))==m) volume_fnl<-rbind(volume_fnl,volume)
if(length(unique(volume_fnl$code))==m) (l<-length(unique(volume_fnl$code)));(n<-l+1);(m<-min(l+20,nrow(selected)))

(fn<-paste("volume_",length(unique(volume_fnl$code)),".rdata",sep=""))
save(volume_fnl,file=fn)
