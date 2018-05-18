### Upload DB
library(RODBC)
YM<-'2018-03-21'

##############################
# READ DATA from Existing DB #
##############################
# setup odbc connection before using this con.
ch<-odbcConnect("odbcConName",uid="sqlId",pwd="password")
system.time(sise_org<-sqlFetch(ch,"TB_sise",as.is=T))
close(ch)

##############################
# APPEND DATA to Existing DB #
##############################
ch<-odbcConnect("odbcConName",uid="sqlId",pwd="password")
system.time(sqlSave(ch, sise, tablename = "TB_sise", append=T, rownames=F))
close(ch)

#######################################
# DELETE DATA FROM THE EXISTING TABLE #
#######################################
ch<-odbcConnect("odbcConName",uid="sqlId",pwd="password")
(sql_del<-paste("delete from TB_sise where YM=","\'",YM,"\'",sep=""))  # 특수문자는 맨아래 참조
sqlQuery(ch, sql_del, errors = TRUE)
close(ch)

#############################
# CREATE FOR THE FIRST TIME #
#############################
# load(file='volume_180321.rdata')
# ch<-odbcConnect("odbcConName",uid="sqlId",pwd="password")
# system.time(sqlSave(ch, volume, tablename = "TB_volume", rownames=F)) # 312
# close(ch)

##############
# DROP TABLE #
##############
# ch<-odbcConnect("odbcConName",uid="sqlId",pwd="password")
# sql_drop<-"drop table TB_volume"
# sqlQuery(ch, sql_drop, errors = TRUE)
# close(ch)


# Single and double quotes delimit character constants. They can be used interchangeably but double quotes are preferred (and character constants are printed using double quotes), <<<so single quotes are normally only used to delimit character constants containing double quotes>>>. 

# \n newline 
# \r carriage return 
# \t tab 
# \b backspace 
# \a alert (bell) 
# \f form feed 
# \v vertical tab 
# \\ backslash \ 
# \' ASCII apostrophe ' 
# \" ASCII quotation mark " 
# \` ASCII grave accent (backtick) ` 
# \nnn character with given octal code (1, 2 or 3 digits) 
# \xnn character with given hex code (1 or 2 hex digits) 
# \unnnn Unicode character with given code (1--4 hex digits) 
# \Unnnnnnnn Unicode character with given code (1--8 hex digits) 
