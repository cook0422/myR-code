library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(magick)
library(ggplot2)
library(rvest)
library(rjson)
library(ggthemr)
library(gganimate)

#https://madlogos.github.io/recharts/index_cn.html#-en
#devtools::install_github("madlogos/recharts")
#devtools::install_github('cttobin/ggthemr')



ggthemr('flat')
items.xls <- read_xlsx("不锈钢菜板淘宝搜索.xlsx")

#去重 items.xls <- items.xls[!duplicated(items.xls[,"shopname"],fromLast =TRUE),]
items.xls$价格 <- as.numeric(items.xls$价格)
items.xls$付款数 <- as.numeric(items.xls$付款数)
rank <- quantile(items.xls[which(items.xls$价格<100&items.xls$付款数>1),]$付款数,seq(0.2,0.8,0.2),right = F)
items.xls <- items.xls %>% mutate(销售排名等级=cut(items.xls$付款数,labels=c('1级','2级','3级','4级','5级'),c(-Inf,rank,Inf),right=F))
items.xls$买家数 <- as.numeric(gsub("人付款","",items.xls$付款数))
#items.xls <-items.xls[grep("item.taobao",x = items.xls$标题链接),] %>% mutate(店铺类型 = "淘宝")
items.xls$店铺类型 <- ifelse(str_detect(items.xls$宝贝ID,"item.taobao"),"淘宝",
                         ifelse(str_detect(items.xls$宝贝ID,"detail.tmall"),"天猫",
                                ifelse(str_detect(items.xls$店铺名,"旗舰店"),"天猫","未知")))
items.xls <- items.xls %>% separate(地区,c("省份","城市"),seq = " ",remove = TRUE)
items.xls[is.na(items.xls[,"城市"]),"城市"]  <- items.xls %>% filter(is.na(城市)) %>% select(省份)



ggplot(filter(items.xls,价格 < 100 & 付款数 > 1),mapping = aes(x=价格,y=付款数)) + 
  geom_point(size = 1.0, shape = 16) +geom_smooth()

ggplot(filter(items.xls,付款数 > 1),aes(x=销售排名等级,fill=店铺类型)) +
  geom_histogram(stat = 'count') + labs(title='销售等级',y='卖家数量') +
  facet_wrap(~ 店铺类型, ncol = 1)

ggplot(filter(items.xls,付款数 > 1),aes(x=销售排名等级)) +
  geom_histogram(stat = 'count') + labs(title='销售等级',y='卖家数量')


ggplot(filter(items.xls,付款数 > 1),aes(x=factor(1),fill=factor(店铺类型))) +
  geom_bar(width = 1,stat = 'count') + coord_polar(theta = "y")

#销量分位数（≈市场竞争度）
quantile((items.xls[which(items.xls$付款数>10),])$付款数,seq(0,1,0.1))
items.xls %>% filter(付款数>450) %>% group_by(店铺类型) %>% 
  summarise(店铺数=length(店铺名),总销量=sum(付款数)) %>%
  mutate(平均销量=总销量/店铺数)

#商家排行
items.xls %>% filter(付款数 > 1) %>% arrange(desc(付款数)) %>% select(店铺名,付款数,店铺类型)
#地区排行
items.xls %>% filter(买家数 > 1) %>% group_by(城市) %>% 
  summarise(销量 = sum(买家数),店铺数 = length(shopname)) %>% 
  mutate(平均销量 = 销量/店铺数) %>% arrange(desc(销量))
#淘宝天猫比列
items.xls %>% filter(买家数 > 1) %>% group_by(店铺类型) %>% 
  summarise(销量 = sum(买家数)) %>% arrange(desc(销量))
unique(items.xls$shopname)


a <- read_html('https://rate.taobao.com/detailCommon.htm?auctionNumId=592313833043')
c <- html_text(a)
c <- gsub("\\(","",c)
c <- gsub("\\)","",c)
json <- fromJSON(c)
print(json$data$impress)


#str_detect  包含
a <- image_read(as.character(items.xls[1,"缩略图"]))
b <- image_read(as.character(items.xls[2,"缩略图"]))
c <- image_read(as.character(items.xls[3,"缩略图"]))
img <- c(a,b,c)
image_append(img)


