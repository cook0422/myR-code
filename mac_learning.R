library(dplyr)
library(ggplot2)
library(ggthemr)
library(gganimate)
library(C50)
library(e1071)
library(readxl)
library(lubridate)
library(tidyr)

options(scipen = 200) #取消科学计数

ggthemr('fresh')

#***************订单导入
order.csv <- read.csv('C:/Users/lenovo/Desktop/R/order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
item.csv <- read.csv('C:/Users/lenovo/Desktop/R/item.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
product.xlsx <- read_xlsx('C:/Users/lenovo/Desktop/R/商品上新.xlsx')
#order_3.csv <- read.csv('C:/Users/lenovo/Desktop/R/3order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
#order.csv <- plyr::rbind.fill(order.csv,order_3.csv)


order.csv$打款商家金额 <- as.numeric(gsub("元","",order.csv$打款商家金额))
order.csv$订单编号 <- as.character(order.csv$订单编号)
order.csv$订单年份 <- as.character(year(order.csv$订单创建时间))
order.csv$订单月份 <- month(order.csv$订单创建时间)
order.csv <- order.csv %>% mutate(订单小时 = hour(订单创建时间))
order.csv$订单日 <- as.character(day(order.csv$订单创建时间))
order.csv$订单创建时间 <- as.Date(order.csv$订单创建时间)
order.csv$订单付款时间. <- as.Date(order.csv$订单付款时间.)
order.csv$订单详细状态<- ifelse(!is.na(order.csv$订单付款时间.)&order.csv$订单状态 == "交易关闭"&order.csv$物流单号=="" ,"未发货退款",
                          ifelse(!is.na(order.csv$订单付款时间.)&order.csv$订单状态 == "交易关闭"&order.csv$物流单号!="" ,"发货全退款",
                                 ifelse(order.csv$订单状态 == "交易成功"&order.csv$买家应付货款 != order.csv$打款商家金额,"发货部分退款",
                                        ifelse(is.na(order.csv$订单付款时间.),"未付款","其他"))))
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
                                       summarise(实际吊牌额 = sum(价格)),by="订单编号")
#替换所有NA值
order.csv[is.na(order.csv)] <- 0

#生成订单价格段
order.csv$订单价格段  = cut(order.csv$打款商家金额,c(-Inf,seq(0,20000,50),Inf),
                       labels =  paste(c(-Inf,seq(0,20000,50)),seq="-",c(seq(0,20000,50),Inf)),right = F)

##############################会员表##########################
#***************会员表
#***************会员表
##构建训练数据：时间 2016-01-01至2017-10-31，整合2017-11-11数据作为训练
VIP.table <- order.csv %>%
  filter(订单付款时间. < '2018-11-01'&订单付款时间. >= '2017-11-01' & 打款商家金额>10) %>% 
  group_by(买家会员名) %>% 
  summarise(
    购买件数 = sum(交易成功),
    #退款件数 = sum(交易关闭),
    #消费金额 = sum(打款商家金额), 
    #首次下单距今 =as.numeric(as.Date('2018-11-10') - min(订单付款时间.)), 
    最后下单距今 =as.numeric(as.Date('2018-11-10') - max(订单付款时间.)),
    活跃天数 = as.numeric(max(订单付款时间.) - min(订单付款时间.)))
    #省份 = 省份[1],
    #城市 = 城市[1],
    #姓名 = 收货人姓名[1],
    #总订单数 =  length(订单编号),
    #订单退款率 = 发货退款订单数/ (发货退款订单数 + 无退款订单数) * 100,
    #手机号 = 联系手机[1]) 
VIP_11 <- order.csv %>% filter(订单付款时间. == '2018-11-11') %>% group_by(买家会员名) %>% summarise(双11下单='YES')
VIP.table <- VIP.table %>% left_join(y=VIP_11,by='买家会员名')%>% select(-买家会员名)
VIP.table[is.na(VIP.table$双11下单),'双11下单'] <- 'NO'
VIP.table[is.na(VIP.table)] <- 0
VIP.table$双11下单 <- factor(VIP.table$双11下单)
VIP.table<-VIP.table[order(runif(nrow(VIP.table))),]
summary(VIP.table)

#生成数据集
set.seed(666)  ##设置随机可重复
train_sample <- sample(1821,1350)  ##设置训练集抽取随机因子，训练集包含2461条记录
vip_train <- VIP.table[train_sample,]  ##抽取训练集
vip_test <- VIP.table[-train_sample,]  ##以提出训练集的形式，抽取测试集
#查看训练集和测试集中的记录是否符合随机分布要求
prop.table(table(vip_train$双11下单))
prop.table(table(vip_test$双11下单))
#基本符合随机分布要求

#C5.0决策树
vip_model <- C5.0(vip_train[,-4],vip_train$双11下单,trials=3)  ## 要剔除类变量因子
summary(vip_model) ##查看树 col:预测   row:实际
vip_t_model <- predict(vip_model,vip_test)
table(vip_test$双11下单,vip_t_model) 
plot(vip_model)
#引入代价矩阵
#matrix_deminsions<-list(c("no","yes"),c("no","yes")) ##确定矩阵的维度
#names(matrix_deminsions)<-c("predicted","actual") ##命名矩阵的维度
#error_cost<-matrix(c(0,1,4,0),nrow=2,dimnames=matrix_deminsions) ##得到代价矩阵如下所示
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
vip_cost <- C5.0(vip_train[,-4],vip_train$双11下单,costs=error_cost)
summary(vip_cost)
vip_t_pre <- predict(vip_cost,vip_test)
table(vip_test$双11下单,vip_t_pre) 
plot(vip_cost)


#支持向量机SVM
svm_model <- svm(双11下单~.,data=vip_train)
summary(svm_model)
pred<-predict(svm_model,vip_test,decision.values=TRUE)
table(vip_test$双11下单,pred) 
summary(pred)
#准确率计算
sum(pred==vip_test$双11下单)/dim(vip_test)[1]











