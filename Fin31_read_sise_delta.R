cat("\f");rm(list=ls())
# sise_cls Data load
ydy<-'180510'
tdy<-'180515'
(ysday<-paste('20',substr(ydy,1,2),'-',substr(ydy,3,4),'-',substr(ydy,5,6),sep=''))

load(file=paste("Data/sise_cls_",ydy,".rdata",sep=""))

selected<-unique(sise_cls[sise_cls$date>'2018-04-27',c("code","exch_mkt")])

library(XML)
Sys.setlocale("LC_ALL", "C")

library(doParallel)
registerDoParallel(6)

# 620,986,990,993,1211,1294
# selected[1294,] #008000, 221200, 223040, 221950, 099830, 087220, 
# system.time(sise<-foreach(i = 1:nrow(selected), .combine=rbind) %dopar% {

system.time(s_dlt<-foreach(i = setdiff(1:nrow(selected),c(1354, 1476, 1881, 1884, 1893)), .combine=rbind) %dopar% {  # Ser
  require(XML);Sys.setlocale("LC_ALL", "C") # doPar
  theurl<-paste("http://stock.daum.net/item/quote_yyyymmdd_sub.daum?page=1&code=",selected$code[i], "&modify=0",sep="")
  sise_html<-readHTMLTable(theurl)
  sise_table<-sise_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),]
  sise_table$code<-selected$code[i]
  sise_table$exch_mkt<-selected$exch_mkt[i]
  # if (i==1) sise<-sise_table else sise<-rbind(sise,sise_table) # Ser
  # print(i) # Ser
  return(sise_table) # doPar
}) # [doPar # 120 sec 2045 stocks]
sise<-s_dlt

head(sise);tail(sise)
sise<-na.omit(sise)

names(sise)<-c("date","Open","High","Low","Close","p_diff","r_diff","Volume","code","exch_mkt")
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
table(duplicated(sise[,c("date","code")]))

ind<-which(sise$date>ysday)
length(ind);nrow(selected)*length(unique(sise$date[ind]))
sise_dlt<-sise[ind,]

table(sise_dlt$date)

# Data Upload
library(RMySQL)
con <- dbConnect(dbDriver("MySQL"), dbname = "fin", user = "root", password = "1111")
dbWriteTable(con,"sise_cls",sise_dlt,append=T,overwrite=FALSE)
dbListTables(con)  #DB fin에 있는 테이블목록 확인
dbDisconnect(con)
