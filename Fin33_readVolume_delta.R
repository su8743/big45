################################
# Read Delta & save
################################
cat('\f');rm(list=ls())
library(doParallel)
registerDoParallel(6)
library(XML)
Sys.setlocale("LC_ALL", "C")

tdy<-"180514";ydy<-'180510'
ysday<-paste('20',substr(ydy,1,2),'-',substr(ydy,3,4),'-',substr(ydy,5,6),sep='')

load(paste("Data/volume_",ydy,".rdata",sep=""))
selected<-unique(volume[,c("code","exch_mkt")])

### Delta Crawling
system.time(v_dlt<-foreach (i = 1:nrow(selected), .combine=rbind) %dopar% {
  require(XML);Sys.setlocale("LC_ALL", "C")
  theurl<-paste("http://stock.daum.net/item/foreign_yyyymmdd.daum?page=1&code=",selected$code[i],sep="")
  volume_html<-readHTMLTable(theurl)
  if (length(volume_html[[1]])>0) {
    volume_table<-volume_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),c(1:5)]
    volume_table$code<-selected$code[i]
    volume_table$exch_mkt<-selected$exch_mkt[i]
  }
  volume_table<-na.omit(volume_table)
  return(volume_table)
}) # 42 sec 1621개 주식
table(v_dlt$V1)

# Begin of Fixing Errors
# a<-153
# selected$code[a]
# ind<-which(selected_bkup$code==selected$code[a]);ind
# selected_bkup$code[ind]
# selected<-selected_bkup[-c(592),]
# End of Fixing Errors

volume_dlt<-na.omit(v_dlt)
names(volume_dlt)[1:5]<-c("date","f_tot","f_rate","f_net","o_net")
volume_dlt$date<-paste("20",volume_dlt$date,sep="")
volume_dlt[,2]<-as.numeric(gsub(",","",as.character(volume_dlt[,2])))
volume_dlt[,3]<-as.numeric(gsub("%","",as.character(volume_dlt[,3])))
volume_dlt[,4]<-as.numeric(gsub(",","",as.character(volume_dlt[,4])))
volume_dlt[,5]<-as.numeric(gsub(",","",as.character(volume_dlt[,5])))
volume_dlt$date <- gsub("\\.","-",volume_dlt$date)
head(volume_dlt)
str(volume_dlt)

ind<-which(volume_dlt$date>ysday)

table(volume_dlt$date[ind])

# DB Upload
library(RMySQL)
con <- dbConnect(dbDriver("MySQL"), dbname = "fin", user = "root", password = "1111")
# dbRemoveTable(con,"volume_cls")
dbWriteTable(con,"volume_cls",volume_dlt[ind,],append=T,overwrite=FALSE)
dbListTables(con)  #DB fin에 있는 테이블목록 확인
dbDisconnect(con)
