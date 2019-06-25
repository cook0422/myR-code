library(REmap)
library(dplyr)
library(maptools)
library(rgdal)
library(ggplot2)
#devtools::install_github("lchiffon/REmap")
options(remap.ak = '3c8860f7c10b603a54c8327c194bbf85')
get_city_coord("天津")

order.csv <- read.csv('C:/Users/lenovo/Desktop/R/order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
order_3.csv <- read.csv('C:/Users/lenovo/Desktop/R/3order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
order.csv <- plyr::rbind.fill(order.csv,order_3.csv)
order.csv$打款商家金额 <- as.numeric(gsub("元","",order.csv$打款商家金额))
order.csv$订单编号 <- as.character(order.csv$订单编号)
order.csv$订单年份 <- as.character(year(order.csv$订单创建时间))
order.csv$订单月份 <- month(order.csv$订单创建时间)
order.csv <- order.csv %>% separate(收货地址.,c("省份","城市"),seq = " ",remove =TRUE)
location <- duplicated(order.csv$城市)
location <- order.csv[!location,c("省份","城市")]
sale_map <- order.csv %>% filter(省份!="") %>% group_by(省份,订单年份) %>% summarise(销售金额=sum(打款商家金额)) %>% filter(销售金额 >0)
sale_map$省份 <- gsub("北京","北京市",sale_map$省份)
sale_map$省份 <- gsub("天津","天津市",sale_map$省份)
sale_map$省份 <- gsub("重庆","重庆市",sale_map$省份)
sale_map$省份 <- gsub("上海","上海市",sale_map$省份)
names(sale_map)[1] <- "NAME"


china_map = readOGR(dsn="bou2_4p.shp",layer="bou2_4p")
#读取行政信息
x <- china_map@data
#含岛屿共925个形状
xs <- data.frame(x,id=seq(0:924)-1)  
#转化为数据框
china_map1 <- fortify(china_map)
#合并两个数据框
china_map_data <- plyr::join(china_map1, xs, type = "full")     

ggplot(china_map,aes(x=long,y=lat,group=group)) +
  geom_polygon (fill="white",colour="grey") +
  coord_map("polyconic")
unique(china_map@data$NAME)
#合并两个数据框
china_data <- plyr::join(china_map_data, sale_map, type="full")


ggplot(china_data, aes(x = long, y = lat, group = group, fill = 销售金额)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="steelblue") +  #指定渐变填充色，可使用RGB
  coord_map("polyconic")        #指定投影方式为polyconic，获得常见视角中国地图

