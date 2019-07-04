#library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(fpc)
library(readxl)
library(ggthemr)
library(gganimate)
library(gifski)
library(splines)
options(scipen = 200) #取消科学计数

ggthemr('fresh')

#***************订单导入
#order.csv <- read_csv('D:\\备份\\来尔佳昵\\旗舰店\\销售数据报表-下载\\订单数据\\ExportOrderList201905151650.csv',col_names = T)
order.csv <- read.csv('C:/Users/lenovo/Desktop/R/order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
item.csv <- read.csv('C:/Users/lenovo/Desktop/R/item.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
product.xlsx <- read_xlsx('C:/Users/lenovo/Desktop/R/商品上新.xlsx')
#cor(order.csv$买家应付货款,order.csv$打款商家金额)
order_3.csv <- read.csv('C:/Users/lenovo/Desktop/R/3order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
order_3.csv <- order_3.csv[,colnames(order.csv)]
order.csv <- plyr::rbind.fill(order.csv,order_3.csv)
order.csv$订单编号 <- as.character(gsub("=","",order.csv$订单编号))
order.csv$联系手机 <- gsub("'","",order.csv$联系手机)
order.csv$打款商家金额 <- as.numeric(gsub("元","",order.csv$打款商家金额))
item_3.csv <- read.csv('C:/Users/lenovo/Desktop/R/3item.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
item.csv <-  plyr::rbind.fill(item.csv,item_3.csv)


item.csv$订单编号 <- gsub("=","",item.csv$订单编号)
order.csv$订单编号 <- as.character(order.csv$订单编号)
order.csv$订单年份 <- as.character(year(order.csv$订单创建时间))
order.csv$订单月份 <- month(order.csv$订单创建时间)
order.csv <- order.csv %>% mutate(订单小时 = hour(订单创建时间))
order.csv$订单日 <- as.character(day(order.csv$订单创建时间))
order.csv$订单创建时间 <- as.Date(order.csv$订单创建时间)
order.csv$订单详细状态<- ifelse(order.csv$订单付款时间. != ""&order.csv$订单状态 == "交易关闭"&order.csv$物流单号=="" ,"未发货退款",
                          ifelse(order.csv$订单付款时间. != ""&order.csv$订单状态 == "交易关闭"&order.csv$物流单号!="" ,"发货全退款",
                                 ifelse(order.csv$订单状态 == "交易成功"&order.csv$买家应付货款 != order.csv$打款商家金额,"发货部分退款",
                                        ifelse(order.csv$订单付款时间. == "","未付款","其他"))))
order.csv <- order.csv %>% separate(收货地址.,c("省份","城市","市区"),seq = " ",remove =FALSE)
product.xlsx$折扣 <- product.xlsx$折扣价/product.xlsx$吊牌价
product.xlsx$折扣区间 <- cut(product.xlsx$折扣,seq(0,0.95,0.05),
                         labels =  paste(seq(0,0.9,0.05),seq="-",seq(0.05,0.95,0.05)),right = F)
product.xlsx$价格区间 <- cut(product.xlsx$折扣价,c(seq(0,1000,50),Inf),
                         labels =  paste(seq(0,1000,50),seq="-",seq(50,1050,50)),right = F)
item.csv$订单编号 <- as.character(item.csv$订单编号)
item.csv$款号 <- substr(item.csv$商家编码,1,9)
item.csv <- item.csv %>% left_join(y = order.csv %>% select(订单编号,订单年份,订单月份,订单创建时间),by = "订单编号")
item.csv$订单状态 <- gsub("，","",item.csv$订单状态)
item.csv$商品年份 <- substring(item.csv$款号,3,3)
item.csv$商品季节 <- substring(item.csv$款号,4,4)


order.csv %>% filter(订单付款时间.!='',城市=='广州市') %>% 
  group_by(市区,订单年份) %>% summarise(买家数= length(unique(买家会员名))) %>% 
  ggplot(aes(x=市区,y=买家数)) + geom_bar(stat = "identity", position = 'stack')  +
  transition_time(as.integer(订单年份))+
  labs(title='年份:{frame_time}')

#生成实际购买数量
order.csv <- order.csv %>% left_join(x = order.csv,
                                     y = item.csv %>% filter(价格>100) %>% 
                                       group_by(订单编号,订单状态) %>% 
                                       summarise(实际件数=sum(购买数量)) %>% 
                                       spread(订单状态,实际件数,fill=0),by ="订单编号")
#生成交易成功吊牌额
order.csv <- order.csv %>% left_join(x = order.csv,
                                     y = item.csv %>% filter(价格>100&订单状态=="交易成功") %>% 
                                       group_by(订单编号) %>% 
                                       summarise(实际吊牌额 = sum(价格*购买数量)),by="订单编号")
#替换所有NA值
order.csv[is.na(order.csv)] <- 0
#生成订单价格段
order.csv$订单价格段  = cut(order.csv$打款商家金额,c(-Inf,seq(0,20000,50),Inf),
                       labels =  paste(c(-Inf,seq(0,20000,50)),seq="-",c(seq(0,20000,50),Inf)),right = F)




#箱线图
#boxplot(打款商家金额 ~ 订单年份,subset(order.csv,打款商家金额 <= 5800&打款商家金额 >= 30),xlab = "年份",ylab = "实付金额", main = "TEST")


#***************订单级数据
#***************订单级数据
#***************订单级数据

order.csv %>% filter(订单付款时间. !=""&订单年份=="2019") %>% group_by(订单月份) %>% 
  summarise(付款金额 = sum(买家应付货款),回款金额 = sum(打款商家金额)) %>%
  mutate(金额退款率=1-回款金额/付款金额)

############################交易金额概览######################
#各年度销售额(去重)
order.csv %>% filter(打款商家金额 > 0) %>%  group_by(订单年份) %>% summarise(成交金额 = sum(打款商家金额),成交人数 = length(unique(买家会员名))) %>% 
  select(订单年份,成交金额,成交人数) %>%  mutate(客单价 = round(成交金额/成交人数,0))
#月度销售额
order.csv  %>% filter(打款商家金额 > 0) %>% group_by(订单年份,订单月份) %>% 
  summarise(订单数 = length(订单编号),成交金额 = sum(打款商家金额),
               成交人数 = length(unique(买家会员名)),成交件数 = sum(交易成功),
               吊牌总额 =sum(实际吊牌额)) %>% 
  select(订单年份,订单月份,订单数,成交金额,成交人数,成交件数,吊牌总额) %>%
  mutate(客单价 = round(成交金额/成交人数,0),件单价 = 成交金额/成交件数,
            客单件 = 成交件数/成交人数,平均折扣=成交金额/吊牌总额) %>% 
  ggplot(aes(x=订单月份,y=成交金额,color=订单年份)) + geom_point(size=4,shape=16) + 
  geom_line(size=2)



#销售TOP日
order.csv  %>% filter(打款商家金额 > 0 & 订单年份 == "2019" & 订单月份 =="5")  %>%  group_by(订单创建时间) %>% 
  summarise(成交金额 = sum(打款商家金额),成交人数 = length(unique(买家会员名)),成交件数=sum(交易成功)) %>% 
  select(订单创建时间,成交金额,成交人数,成交件数) %>% arrange(desc(成交金额)) %>% slice(1:20) %>%  
  mutate(客单价 = round(成交金额/成交人数,0))
#指定时段查询
order.csv %>% filter("2018-05-18" < 订单创建时间 & 订单创建时间< "2018-05-25") %>% group_by(订单创建时间) %>% 
  summarise(成交金额 = sum(打款商家金额),成交人数 = length(unique(买家会员名))) %>% 
  select(订单创建时间,成交金额,成交人数) %>%  mutate(客单价 = round(成交金额/成交人数,0)) %>% slice(1:20)
#下单次数&会员 分布
order.csv %>% filter(订单付款时间.!=""&订单年份=="2019"&订单月份=="5") %>% 
  group_by(买家会员名) %>%  summarise(订单数 = length(买家会员名)) %>% arrange(desc(订单数)) %>% 
  group_by(订单数) %>% summarise(买家数 = length(订单数))
#实付金额五分位概括
aggregate(打款商家金额 ~ 订单年份,data = subset(order.csv,打款商家金额 > 20),
                FUN = quantile,probs = seq(0,1,0.2))  
#订单金额五分位概括
aggregate(买家应付货款 ~ 订单年份,data = subset(order.csv,买家应付货款 > 88&order.csv$订单付款时间.!=""),
                FUN = quantile,probs = seq(0,1,0.2))
#订单均价
order.csv %>% filter(打款商家金额 > 20) %>% group_by(订单年份) %>%  
  summarise(订单均价 = sum(打款商家金额)/length(订单编号))  
#买家平均消费（年度、月度 二维）
order.csv %>% filter(打款商家金额 > 20) %>% group_by(订单年份,订单月份) %>% 
  summarise(全年平均消费 = sum(打款商家金额)/length(unique(买家会员名))) %>%spread(订单月份,全年平均消费,fill = 0)  
#订单价格段统计（实付金额）
order.csv %>% filter(打款商家金额 > 0) %>% group_by(订单价格段) %>% 
  summarise(订单数=length(订单编号),订单总金额=sum(打款商家金额)) %>%
  mutate(金额占比 = 订单总金额/sum(order.csv[which(order.csv$打款商家金额 > 0),"打款商家金额"]),
             数量占比 = 订单数/length(order.csv[which(order.csv$打款商家金额 > 0),"打款商家金额"]))
#订单价格段统计（下单金额）
order.csv %>% filter(订单详细状态 != "未付款") %>% group_by(订单价格段) %>% 
  summarise(订单数=length(订单编号),订单总金额=sum(买家应付货款)) %>%
  mutate(金额占比 = 订单总金额/sum(order.csv[which(order.csv$订单详细状态 != "未付款"),"买家应付货款"]),
             数量占比 = 订单数/length(order.csv[which(order.csv$订单详细状态 != "未付款"),"买家应付货款"]))



###########################商品统计###########################
#商品折扣价格段分布
product.xlsx %>% group_by(价格区间) %>% summarise(length(价格区间))
#商品销售价格区间
item.csv %>% filter(订单年份 == "2019" & 订单月份 == "5") %>% select(款号,购买数量,订单状态) %>%
  group_by(款号,订单状态) %>% summarise(总件数 = sum(购买数量)) %>% 
  left_join( y= product.xlsx %>% select(款号,价格区间),by="款号") %>%
  group_by(订单状态,价格区间) %>% summarise(件数 = sum(总件数)) %>%
  spread(订单状态,件数,fill = 0)
#详细销售数量
item.csv %>% filter(订单年份 == "2019" & 订单月份 == "5") %>% select(款号,购买数量,订单状态) %>% 
  group_by(款号,订单状态) %>% summarise(总件数 = sum(购买数量))  %>% spread(订单状态,总件数,fill = 0)
#商品销售折扣区间
item.csv %>% filter(订单年份 == "2019" & 订单月份 == "5") %>% select(款号,购买数量,订单状态) %>%
  group_by(款号,订单状态) %>% summarise(总件数 = sum(购买数量)) %>% 
  left_join( y= product.xlsx %>% select(款号,折扣区间),by="款号") %>%
  group_by(订单状态,折扣区间) %>% summarise(件数 = sum(总件数)) %>%
  spread(订单状态,件数,fill = 0)
#上新7天内销量
上新7日销量 <- item.csv %>% select(订单创建时间,款号,购买数量,订单状态) %>% 
  left_join(y=product.xlsx %>% select(款号,上新时间),by="款号") %>%
  filter(!is.na(上新时间),difftime(订单创建时间,上新时间,units = "days") <= 6) %>%
  group_by(款号,订单状态) %>% summarise (下单件数 = sum(购买数量)) %>% spread(订单状态,下单件数,fill = 0) %>%
  left_join(product.xlsx %>% select(款号,上新时间),by="款号")
#上新商品单日销量
上新商品当日销量 <- item.csv %>% select(订单创建时间,款号,购买数量,订单状态) %>% 
  left_join(y=product.xlsx %>% select(款号,上新时间),by="款号") %>%
  filter(!is.na(上新时间),difftime(订单创建时间,上新时间,units = "days") == 0) %>%
  group_by(款号,订单状态) %>% summarise (下单件数 = sum(购买数量)) %>% spread(订单状态,下单件数,fill = 0) %>%
  left_join(product.xlsx %>% select(款号,上新时间),by="款号")
上新当日销量 <- item.csv %>% select(订单创建时间,款号,购买数量,订单状态) %>% 
  left_join(y=product.xlsx %>% select(款号,上新时间),by="款号") %>%
  filter(!is.na(上新时间),difftime(订单创建时间,上新时间,units = "days") == 0) %>% group_by(上新时间,订单状态) %>%
  summarise(下单件数=sum(购买数量)) %>% spread(订单状态,下单件数,fill=0)
上新当日销量 %>% select(-contains("交易关闭")) %>% select(-contains("交易关闭"))



###########################退款数据###########################
#商品级退款率（年度）
order.csv %>% filter(物流单号.!="") %>% group_by(订单年份) %>% 
  summarise(成交件数=sum(交易成功),退款件数=sum(交易关闭)) %>% 
  mutate(退款率 = 退款件数/(退款件数+成交件数))
#发货商品级退款率（月度）
order.csv %>% filter(订单年份 == "2019") %>% group_by(订单月份) %>% 
  summarise(成交件数=sum(交易成功),退款件数=sum(交易关闭)) %>% 
  mutate(退款率 = 退款件数/(退款件数+成交件数))
#商品级退款率（日度）
order.csv %>% filter(订单年份 == "2019"&物流单号.!=""&订单月份=="5") %>% group_by(订单日) %>% 
  summarise(成交件数=sum(交易成功),退款件数=sum(交易关闭)) %>% 
  mutate(退款率 = 退款件数/(退款件数+成交件数)) %>% arrange(desc(as.numeric(退款件数))) %>% slice(1:20)
#商品级退款率（商品级）
order.csv %>% filter(订单年份 == "2019"&物流单号.!=""&订单月份=="5") %>% select(订单编号,订单年份) %>% 
  left_join(y = item.csv,by ="订单编号") %>% filter(!is.na(商家编码)) %>% 
  group_by(款号,订单状态) %>% summarise(成交件数=sum(购买数量)) %>% spread(订单状态,成交件数,fill = 0) %>%
  mutate(退款率 = 交易关闭/(交易成功+交易关闭)) %>% arrange(desc(交易成功))
#订单退款率（年度）
order.csv %>% filter(订单付款时间.!=""&订单月份=="5") %>% group_by(订单年份,订单状态) %>% 
  summarise(订单数= length(订单编号)) %>% spread(订单状态,订单数,fill=0) %>% 
  mutate(退款率 = 交易关闭/(交易成功+交易关闭)) %>% slice(1:20) %>% arrange(订单年份)
#销售TOP日(商品级)
order.csv  %>% filter(订单年份 == "2018" & 订单月份 =="5")  %>%  group_by(订单创建时间) %>% 
  summarise(成交金额 = sum(打款商家金额),成交件数=sum(交易成功),退款件数=sum(交易关闭)) %>% 
  select(订单创建时间,成交金额,成交件数,退款件数) %>% slice(1:20)  %>% arrange(desc(成交金额))%>%  
  mutate(退款率 = 退款件数/(退款件数+成交件数))
#订单退款率（包含未发货退款）
order.csv %>% filter(订单月份=="5") %>% group_by(订单年份,订单详细状态) %>%  summarise(订单数 = length(订单编号)) %>% 
  spread(订单详细状态,订单数,fill=0) %>% 
  mutate(总订单数 = length(order.csv[which(order.csv$订单年份 == 订单年份&order.csv$订单月份=="5"),1]),
             支付率 = round(1 - 未付款/总订单数,2),
             订单退款率 = round((未发货退款+发货全退款+发货部分退款)/(未发货退款+发货全退款+发货部分退款+其他),2))


###########################订单视图###########################
#消费时段折线图（by年份）
ggplot(data = order.csv %>% filter(订单年份 == "2019"&订单日!="11") %>% group_by(订单小时,订单年份) %>% 
         summarise(成交金额 = sum(打款商家金额)) , mapping = aes(x = as.integer(订单小时), y = 成交金额, colour = 订单年份)) + 
  geom_line()+scale_linetype_manual(values=c(12,18)) + expand_limits(x=1) 
#消费时段柱状图
ggplot(
  order.csv %>% filter(订单年份 == 2019) %>% group_by(订单小时,订单年份) %>% summarise(成交金额 = sum(打款商家金额)) , 
  aes(x = factor(订单小时),y=成交金额,fill=订单年份)) + 
  geom_bar(stat='identity', position='stack') +
  labs(x="成交时段",y="成交金额",title="下单时段概览") +
  guides(fill = guide_legend(reverse= TRUE)) +
  scale_fill_brewer(palette = 'Accent')
#h<-aggregate(cbind(打款商家金额,买家应付货款)  ~ 买家会员名,data = order.csv, FUN = sum)
#订单价格段概览
ggplot(order.csv %>% group_by(订单价格段) %>%  summarise(订单数 = length(订单价格段),成交金额 = sum(打款商家金额)) %>%
          #  spread(订单价格段,订单总金额) %>% 
         arrange(desc(成交金额)) %>% slice(1:20) %>%
         mutate(金额占比 = round(成交金额/sum(order.csv$打款商家金额),2),订单平均支付 = round(成交金额/订单数,2)) %>%
         arrange(订单价格段), 
       aes(x = factor(订单价格段),y=成交金额)) + geom_bar(stat='identity', position='stack') +
  labs(x="成交时段",y="成交金额",title="下单时段概览") +
  guides(fill = guide_legend(reverse= TRUE)) + scale_fill_brewer(palette = 'Accent')

#省份销售额【动图MOVING】
order.csv %>% filter(打款商家金额>0) %>% group_by(订单年份,省份) %>% 
  summarise(成交金额 = sum(打款商家金额),成交数量=sum(交易成功)) %>%
  ggplot(aes(成交金额,成交数量,color=省份,size=成交数量)) + geom_point() + scale_x_log10() +
  transition_time(as.integer(订单年份))+
  labs(title='年份:{frame_time}')

#省份成交额【年月动图MOVING】  dataFrame$xxx 默认 drop=true
order.csv %>% filter(打款商家金额>0&省份 %in% 
                             (order.csv %>% group_by(省份) %>% summarise(总额=sum(打款商家金额)) %>% 
                             arrange(desc(总额)) %>% slice(1:9) %>% select(省份))$省份) %>% 
  group_by(订单年份,订单月份,省份) %>% 
  summarise(成交金额 = sum(打款商家金额),成交数量=sum(交易成功)) %>%
  ggplot(aes(y=成交金额,x=factor(订单月份))) + 
  geom_histogram(stat = "identity", position = 'dodge')+ facet_wrap(~省份,nrow = 3,as.table = TRUE) +
  transition_time(as.integer(订单年份)) +
  labs(title='年份:{frame_time}')

#单个省份成交额【年月动图MOVING】  dataFrame$xxx 默认 drop=true
order.csv %>% filter(打款商家金额>0&省份=='江苏省') %>% 
  group_by(订单年份,订单月份,城市) %>% 
  summarise(成交金额 = sum(打款商家金额),成交数量=sum(交易成功)) %>%
  ggplot(aes(y=成交金额,x=factor(订单月份))) + 
  geom_histogram(stat = "identity", position = 'dodge')+ facet_wrap(~城市,ncol = 3,as.table = TRUE) +
  transition_time(as.integer(订单年份)) +
  labs(title='年份:{frame_time}')

#商品季节销售趋势
item.csv %>% filter(订单年份 != "2019" &订单创建时间 > '2014-12-31'&订单状态 == "交易成功" & 价格 > 100 & 商品季节 %in% c('1','2','3','5')) %>%
  group_by(订单年份,订单月份,商品季节) %>% summarise(销售件数 = sum(购买数量)) %>%
  ggplot(aes(x=factor(订单月份),y=销售件数,group=订单年份)) +
  geom_histogram(stat = "identity", position = 'stack') + 
  stat_smooth(aes(color=订单年份),method='loess',se=FALSE) +
  facet_wrap(~商品季节,ncol = 2,as.table = TRUE,scales = 'free_x')+
  labs(x='月份',title='各季节货品销售概览')

#商品季节销售趋势【动图MOVING】
item.csv %>% filter(订单状态 == "交易成功" & 价格 > 100 & 商品季节 %in% c('1','2','3','5')) %>%
  group_by(订单年份,订单月份,商品季节) %>% summarise(销售件数 = sum(购买数量)) %>%
  ggplot(aes(x=factor(订单月份),y=销售件数)) +
  geom_bar(stat = "identity", position = 'stack') + 
  facet_wrap(~商品季节,ncol = 2,as.table = TRUE,scales = 'free_x')+
  labs(x='月份',title='各季节货品销售概览')+
  transition_time(as.integer(订单年份)) +
  labs(title='年份:{frame_time}')


orderTemp <- order.csv %>% filter(打款商家金额>0) %>% select(省份,打款商家金额,交易成功,交易关闭,实际吊牌额) %>%
  group_by(省份) %>% summarise(总金额 =sum(打款商家金额),成交件数 = sum(交易成功),退款件数 = sum(交易关闭),吊牌总额 = sum(实际吊牌额))
#k-means买家聚类

#VIP.Temp[,3] <- as.numeric(VIP.Temp[,3])
orderTemp2 <- orderTemp[,2:3]
orderTemp.kmeans <- kmeans(orderTemp2,3)
orderTemp.kmeans
#用table函数查看分类结果情况
#table(VIP.Temp$买家会员名,VIP.Temp.kmeans$cluster)
#下边我们将分类以及中心点打印出来
plot(orderTemp$总金额,orderTemp$成交件数,col=orderTemp.kmeans$cluster,pch="*")
points(orderTemp.kmeans$centers,pch="X",cex=1.5,col=4)
str(orderTemp.kmeans$centers)
###
orderTemp$分类 <- c(orderTemp.kmeans$cluster)


#使用K-Mediods 进行聚类分析
orderTemp.pamk<-pamk(orderTemp[,2:3])
table(orderTemp.pamk$pamobject$clustering,orderTemp$省份)
#每页显示两个图
layout(matrix(c(1,2),1,2)) 
plot(orderTemp.pamk$pamobject)
layout(matrix(1))

#层次聚类（千万不要超过100行数据）
result <- dist(orderTemp, method = "euclidean")
#产生层次结构
result_hc <- hclust(d = result, method = "ave")
orderTemp$分类 <- c(result_hc$order)
#这里的hang=-1使得树的节点在下方对齐
plot(result_hc,hang=-1,labels=orderTemp$省份)
#将树分为3块
rect.hclust(result_hc,k=4)
groups<-cutree(result_hc,k=4)


#密度聚类
xx <- order.csv %>% filter(打款商家金额>0) %>% select(买家会员名,打款商家金额,交易成功,交易关闭,实际吊牌额) %>%
  group_by(买家会员名) %>% summarise(总金额 =sum(打款商家金额),成交件数 = sum(交易成功),退款件数 = sum(交易关闭),吊牌总额 = sum(实际吊牌额))
xx <- scale(xx[,2:5], center=T,scale=T)
ds<-dbscan(xx,eps=0.45,MinPts = 5)
#打印出ds和iris2的聚类散点图
plot(ds,xx)
#打印出orderTemp第一列和第四列为坐标轴的聚类结果
plot(ds,xx[,c(3,4)])
plotcluster(xx,ds$cluster)
plot(xx[,2], xx[,3], col=ds$cluster)


iris2<-iris[-5]
ds<-dbscan(iris2,eps=0.42,MinPts = 5)
table(ds$cluster,iris$Species)
#打印出ds和iris2的聚类散点图
plot(ds,iris2)

##############################会员表##########################
#***************会员表
#***************会员表
rm(VIP.table)

VIP.table <- order.csv %>%
  #filter(订单详细状态 == "交易成功" | 订单详细状态 == "发货有退款" ) %>% 
  group_by(买家会员名) %>% 
  summarise(
    购买件数 = sum(交易成功),
    退款件数 = sum(交易关闭),
    消费金额 = sum(打款商家金额), 
    首次下单时间 = min(订单创建时间), 
    最后下单时间 = max(订单创建时间),
    省份 = 省份[1],
    城市 = 城市[1],
    姓名 = 收货人姓名[1],
    总订单数 = length(订单编号),
    #订单退款率 = 发货退款订单数/ (发货退款订单数 + 无退款订单数) * 100,
    手机号 = 联系手机[1]) %>%
  mutate(商品退款率 = 退款件数/(退款件数+购买件数))
VIP.table <- order.csv %>% group_by(买家会员名,订单详细状态) %>%  summarise(订单数 = length(订单编号)) %>% 
  spread(订单详细状态,订单数,fill = 0) %>%   #逆向透视--长转宽
  left_join(x = VIP.table,by = "买家会员名")  #vlookup合并
VIP.table <- VIP.table %>% mutate(订单退款率 = round((发货部分退款+发货全退款+未发货退款)/(发货部分退款+未发货退款+发货全退款+其他),2))
VIP.table$会员等级 <- cut(VIP.table$消费金额,c(-Inf,399,1699,3699,Inf),right = F,
                      labels = c("店铺会员","普通会员","高级会员","VIP会员"))
VIP.table$沉睡天数 <- as.numeric(Sys.Date() - as.Date(VIP.table$最后下单时间))
VIP.table$活跃天数 <- as.numeric(as.Date(VIP.table$最后下单时间) - as.Date(VIP.table$首次下单时间))
#length(VIP.table[which(VIP.table$消费金额 > 20&VIP.table$消费金额 <= 148),"消费金额"])   价格检测
#VIP.payRank <- quantile(unlist(VIP.table[which(VIP.table$会员等级!="店铺会员"),"消费金额"]),c(0,0.2,0.4,0.6,0.8,1)) #查询比列金额段
VIP.payRank <- quantile(unlist(VIP.table[which(VIP.table$消费金额 >=20),"消费金额"]),seq(0.1,0.9,0.1)) #查询比列金额段
#cut 区分
VIP.table <- VIP.table %>% mutate(消费等级 = cut(VIP.table$消费金额,c(-Inf,VIP.payRank,Inf),right = T))



# 保存
write.csv(VIP.table ,file = 'C:/Users/lenovo/Desktop/所有会员.csv')

#会员消费金额段比例
VIP.table %>% filter(消费金额 >=20) %>% group_by(消费等级) %>% 
  summarise(会员数 = length(买家会员名),
               消费总金额 = sum(消费金额),
               金额占比 = sum(消费金额)/sum(VIP.table[which(VIP.table$消费金额 >=20),4]),
               数量占比 = length(买家会员名)/nrow(VIP.table[which(VIP.table$消费金额>=20),1]))
#退款率会员等级
write.csv(VIP.table %>% filter(最后下单时间 < '2019-01-01',最后下单时间 > '2018-06-01',消费金额 > 1699,商品退款率<0.45) %>% 
            select(买家会员名,消费金额,最后下单时间),file = 'C:/Users/lenovo/Desktop/千牛唤醒客户.csv')

VIP.table %>% ggplot(aes(活跃天数,消费金额)) + geom_point() + stat_smooth() + facet_wrap(~会员等级)

#计算第2次下单间隔时间
VIP.table <- merge(
  VIP.table,
  VIP.table %>% filter(总订单数 - 未付款 >= 2) %>% group_by(买家会员名) %>% 
    summarise(消费间隔 = as.Date(sort(order.csv[which(order.csv$买家会员名 == 买家会员名&order.csv$订单付款时间.!= ""),"订单付款时间."])[2]) - as.Date(首次下单时间)),
  all.x=T)

#计算第3次下单间隔时间
VIP.table <- merge(
  VIP.table,
  VIP.table %>% filter(总订单数 - 未付款 >= 3) %>% group_by(买家会员名) %>% 
    summarise(消费间隔3 = as.Date(sort(order.csv[which(order.csv$买家会员名 == 买家会员名&order.csv$订单付款时间.!= ""),"订单付款时间."])[3]) - as.Date(sort(order.csv[which(order.csv$买家会员名 == 买家会员名&order.csv$订单付款时间.!= ""),"订单付款时间."])[2])),
  all.x=T)

VIP.table[is.na(VIP.table)] <- 0

#k-means买家聚类
VIP.Temp <- VIP.table %>% filter(消费金额>0&消费金额<20000) %>% select(买家会员名,消费金额,总订单数,活跃天数)
VIP.Temp[,4] <- as.numeric(VIP.Temp[,4])
#VIP.Temp[,2:4] <- scale(VIP.Temp[,2:4],center = T,scale = T)
VIP.Temp2 <- VIP.Temp[,2:4]
VIP.Temp.kmeans <- kmeans(VIP.Temp2,5)
VIP.Temp.kmeans
#用table函数查看分类结果情况
#table(VIP.Temp$买家会员名,VIP.Temp.kmeans$cluster)
#下边我们将分类以及中心点打印出来
plot(VIP.Temp$消费金额,VIP.Temp$总订单数,col=VIP.Temp.kmeans$cluster,pch="*")
points(VIP.Temp.kmeans$centers,pch="X",cex=1.5,col=VIP.Temp.kmeans$cluster)

###
###可以尝试用  购买频次+购买件数  分类出 非正常消费买家

ggplot(VIP.Temp, aes(x=消费金额, y=总订单数))+ 
  geom_point(col="steelblue",alpha=0.4,size = 2) +
  geom_smooth()

write.csv(VIP.table, file="D:/备份/来尔佳昵/旗舰店/销售数据报表-下载/订单数据/VipTable.csv")
