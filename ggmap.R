# http://lumiamitie.github.io/r/geocoding-with-r-02/

library(ggmap) #install.packages("ggmap")

cust_addr = data.frame(addr=c('서울시 강동구 상일로10길46','상일로10길 46','잠실로62','잠실로 62'), stringsAsFactors = F)
cust_addr$addr = enc2utf8(cust_addr$addr)
geo_cst = mutate_geocode(cust_addr, addr, source = 'google')

geo_cst

library(ggplot2)
seoul_map <- qmap('Seoul', zoom = 11)
seoul_map + geom_point(data = geo_cst, aes(lon, lat), size = 3, colour='red')

seoul_map <- qmap('Seoul', zoom = 11, source = 'stamen', maptype = 'toner')
