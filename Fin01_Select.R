cat('\f')
install.packages("XML")
library(XML)

# Visit the following URL
theurl<-'http://bigdata-trader.com/itemcodehelp.jsp'
code_all<-readHTMLTable(theurl,header = F)[[1]]
head(code_all)

code_all<-read.csv("C:/Users/user/Documents/stck_Fin03/Results",colClasses=rep("character",10))

head(code_all)
str(code_all)
names(code_all)[c(3,2,1)]<-c('code','name','exch_mkt')
save(code_all,file='data/code_all_20180319.rdata')
load('Data/code_all_20180319.rdata')
head(code_all)
table(code_all$exch_mkt)

selected<-code_all

Sys.setlocale("LC_ALL", "C") # multibyte error ë°œìƒ?‹œ ?‹¤?–‰

system.time(for (i in 1:nrow(selected)){
  theurl<-paste("http://stock.daum.net/item/quote_yyyymmdd_sub.daum?page=1&code=",selected$code[i],"&modify=0",sep="")
  sise_html<-readHTMLTable(theurl)
  sise_table_tmp<-sise_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:40),]
  # head(sise_table_tmp)
  names(sise_table_tmp)<-c("date","Open","High","Low","Close","p_diff","r_diff","Volume")
  sise_table_tmp$date<-paste("20",sise_table_tmp$date,sep="")
  sise_table_tmp[,2]<-as.numeric(gsub(",","",as.character(sise_table_tmp[,2])))
  sise_table_tmp[,3]<-as.numeric(gsub(",","",as.character(sise_table_tmp[,3])))
  sise_table_tmp[,4]<-as.numeric(gsub(",","",as.character(sise_table_tmp[,4])))
  sise_table_tmp[,5]<-as.numeric(gsub(",","",as.character(sise_table_tmp[,5])))
  sise_table_tmp[,6]<-gsub("?–¼","-",as.character(sise_table_tmp[,6]))
  sise_table_tmp[,6]<-gsub("?†“","-",sise_table_tmp[,6])
  sise_table_tmp[,6]<-gsub("?–²","",sise_table_tmp[,6])
  sise_table_tmp[,6]<-as.numeric(gsub(",","",sise_table_tmp[,6]))
  sise_table_tmp[,7]<-as.numeric(gsub("%","",as.character(sise_table_tmp[,7])))
  sise_table_tmp[,8]<-as.numeric(gsub(",","",as.character(sise_table_tmp[,8])))
  sise_table_tmp$code<-selected$code[i]
  sise_table_tmp$name<-selected$name[i]
  sise_table<-sise_table_tmp
  rownames(sise_table)<-NULL
  sise_table<-na.omit(sise_table)
  if (i==1) sise_s<-sise_table[1,] else sise_s<-rbind(sise_s,sise_table[1,])
  
  print(paste(i, selected$code[i], selected$name[i],nrow(sise_s)))
  # head(sise);tail(sise)
}) # 300 sec 2046 stocks

head(sise_s)
tail(sise_s)
sise_s<-merge(sise_s,code_all[,c(1,3)],by='code',all.x=T)
head(sise_s)

table(sise_s$exch_mkt)
str(sise_s)

save(sise_s,file='Data/sise_s_180319.rdata')
