install.packages('RMySQL')
library(RMySQL)

tdy<-'180510'

load('Data/code_all_20180319.rdata')
load(paste('Data/sise_cls_',tdy,'.rdata',sep=''))
load(paste('Data/volume_',tdy,'.rdata',sep=''))

str(code_all)
code<-code_all[,c("code","exch_mkt")]
str(sise_cls)
str(volume)

# Create
con <- dbConnect(dbDriver("MySQL"), dbname = "fin", user = "root", password = "1111")
# dbRemoveTable(con,"volume_cls")
dbWriteTable(con,"code_all",code)
dbWriteTable(con,"sise_cls",sise_cls)
dbWriteTable(con,"volume_cls",volume)
dbListTables(con)  #DB da_kdata에 있는 테이블목록 확인
sise_tdy <- dbGetQuery(con, "SELECT * FROM sise_cls where date = '2018-05-10'")
head(sise_tdy)
dbDisconnect(con)

#Append
con <- dbConnect(dbDriver("MySQL"), dbname = "fin", user = "root", password = "1111")
dbWriteTable(con,"sise_cls",sise_dlt,append=T,overwrite=FALSE)
dbWriteTable(con,"volume_cls",volume_dlt,append=T,overwrite=FALSE)
dbListTables(con)  #DB fin에 있는 테이블목록 확인
dbDisconnect(con)

