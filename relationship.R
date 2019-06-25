library(dplyr)
library(lubridate)
library(readxl)

options(scipen = 200) #取消科学计数


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
order.csv <- order.csv %>% tidyr::separate(收货地址.,c("省份","城市","市区"),seq = " ",remove =FALSE)
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
                                       tidyr::spread(订单状态,实际件数,fill=0),by ="订单编号")
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
  tidyr::spread(订单详细状态,订单数,fill = 0) %>%   #逆向透视--长转宽
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
VIP.table <- as.data.frame(VIP.table)
VIP.table[is.na(VIP.table)] <- 0

VIP <- VIP.table

VIP.table <- VIP %>% filter(消费金额 > 0)  %>% select(活跃天数,购买件数,沉睡天数,商品退款率,消费金额)
VIP.table <- scale(VIP.table)

######主成分分析
  #R中作为主成分分析最主要的函数是princomp()函数
  #princomp()主成分分析   可以从相关阵或者从协方差阵做主成分分析
  #summary()提取主成分信息 
  #loadings()显示主成分分析或因子分析中载荷的内容
  #predict()预测主成分的值 
  #screeplot()画出主成分的碎石图 
  #biplot()画出数据关于主成分的散点图和原坐标在主成分下的方向
  #cor是逻辑变量 当cor=TRUE表示用样本的相关矩阵R做主成分分析
###当cor=FALSE表示用样本的协方差阵S做主成分分析
vip_pr <- princomp(VIP.table,cor = TRUE)
###loading是逻辑变量 当loading=TRUE时表示显示loading 的内容
#loadings的输出结果为载荷 是主成分对应于原始变量的系数即Q矩阵
summary(vip_pr,loadings=TRUE)
#分析结果含义
#----Standard deviation 标准差   其平方为方差=特征值
#----Proportion of Variance  方差贡献率
#----Cumulative Proportion  方差累计贡献率
#由结果显示 前两个主成分的累计贡献率已经达到96% 可以舍去另外两个主成分 达到降维的目的
#因此可以得到函数表达式 Z1=-0.497X'1-0.515X'2-0.481X'3-0.507X'4
#Z1=  0.543X'1-0.210X'2-0.725X'3-0.368X'4
###4.画主成分的碎石图并预测
screeplot(vip_pr,type="lines")
p<-predict(vip_pr)
plot(p[,1:4])

######因子分析
vip_cor <- cor(scale(VIP.table))
cov(VIP.table)
cor.test(VIP.table[,3],VIP.table[,4])
corr.test(VIP.table)
library(psych)
#碎石图
fa.parallel(vip_cor,n.obs = 112,fa="both",n.iter = 100)
m <- fa.parallel(VIP.table)#确定因子数量
m$nfact
#因子分析
vip_fa <- fa(vip_cor, nfactors = 2, rotate = "none", fm = "ml")
vip_fa

# 因子旋转：正交旋转法
vip_fa2 <- fa(vip_cor, nfactors = 2, rotate = "varimax", fm = "ml")
vip_fa2
factor.plot(vip_fa2)
fa.diagram(vip_fa2, simple = FALSE)
