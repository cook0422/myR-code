library(arules)
library(arulesViz)
item.csv <- read.csv("3item.csv",header=T,sep=',',stringsAsFactors=TRUE,nrows=100000)
item.csv$款号 <- substr(item.csv$商家编码,1,9)
item.csv$订单编号 <- gsub("=","",item.csv$订单编号)
item.csv <- item.csv %>% select(订单编号,款号)
write.csv(item.csv[,1:2], file="购物车.csv",row.names = FALSE)
gou1 <- read.transactions("购物车.csv", format="single", sep=",",skip=0,cols=c(1, 2),rm.duplicates=FALSE)
# 参数说明：
# format=c("basket", "single")用于注明源数据的格式。如果源数据每行内容就是一条交易购买的商品列表（类似于一行就是一个购物篮）那么使用basket；如果每行内容是交易号+单个商品，那么使用single。
# cols=c("transId", "ItemId") 对于single格式，需要指定cols，二元向量（数字或字符串）。如果是字符串，那么文件的第一行是表头（即列名）。第一个元素是交易号的字段名，第二个元素是商品编号的字段名。如果是数字，那么无需表头。对于basket，一般设置为NULL，缺省也是NULL，所以不用指定。
# signle format的数据格式如下所示，与此同时，需要设定cols=c(1, 2)
# 1001,Fries
# 1001,Coffee
# 1001,Milk
# 1002,Coffee
# 1002,Fries
# rm.duplicates=FALSE：表示对于同一交易，是否需要删除重复的商品

summary(gou1)

# 每个购物list里包含商品的数量 length in each basket(row) 
basketSize<-size(gou1)
table(basketSize)

# 单个商品的出现频率 Support of each item 
itemFreq <- itemFrequency(gou1)
(head(itemFreq[order(-itemFreq)]))


#按最小支持度 itemfreq >= 0.2
itemFrequencyPlot(gou1, support= quantile(itemFreq,0.99))  

# head(itemFreq[order(-itemFreq)],10)
itemFrequencyPlot(gou1, topN=10, horiz=T) 

#筛选购买两件商品以上的交易
groceries_use <- gou1[basketSize > 1]  
dim(gou1)
dim(groceries_use)

#查看交易数据
inspect(gou1[1:3])

#关联规则挖掘
groceryrules <- apriori(gou1, 
                        parameter = list(support = 0.001, 
                                         confidence = 0.05, 
                                         minlen = 2))
#这里需要说明下parameter：
# 支持度 Support  置信度 Confidence  提升度 Lift
#默认的support=0.1, confidence=0.8, minlen=1, maxlen=10
#对于minlen，maxlen这里指规则的LHS+RHS的并集的元素个数
#如果minlen=1，意味着 {} => {beer}是合法的规则
#我们往往不需要这种规则，所以需要设定minlen=2

#保存到文件。可以与外部程序进行交换
write(groceryrules, file="D:/备份/来尔佳昵/旗舰店/销售数据报表-下载/订单数据/TOUMI购物篮分析.csv", sep=",", quote=TRUE, row.names=FALSE)
#转换为data frame，然后再进行进一步的处理。处理完的结果可以保存到外部文件或者数据库。
groceryrules_df <- as(groceryrules, "data.frame")  
str(groceryrules_df)  

top.vegie.rules <- sort(groceryrules,
                        by=c('support','lift'))[1:10]
plot(top.vegie.rules, measure="support", 
     method="graph",
     shading = "lift")


#规则统计汇总信息
summary(groceryrules)
#规则查看
inspect(groceryrules)

#按照 lift 对规则进行排序
ordered_groceryrules <- sort(groceryrules, by="support")
inspect(ordered_groceryrules[1:5])

groceryrules@quality$lift2 <- floor(groceryrules@quality$lift*100)/100
ordered_groceryrules2 <- sort(groceryrules, by=c("lift2",'support'))
inspect(ordered_groceryrules2[1:6])

# 规则搜索
yogurtrules <- subset(groceryrules, items %pin% c("8182")) 
inspect(sort(yogurtrules,by=c('support','confidence')))

#通过 条件运算符(&, |, !) 添加 support, confidence, lift的过滤条件
fruitrules <- subset(groceryrules, items %pin% c("8182"))  
inspect(sort(yogurtrules[1:1],by=c('lift2','support')))

#限制挖掘的item
#可以控制规则的左手边或者右手边出现的item，即appearance。但尽量要放低支持度和置信度
berriesInLHS <- apriori(gou1, 
                        parameter = list( support = 0.001, 
                                          confidence = 0.05,
                                          minlen=2), 
                        appearance = list(lhs = c("818201278"), 
                                          default="rhs"))
summary(berriesInLHS)
inspect(berriesInLHS)
# 既然lhs都是"818201078"，那么只查看rhs的itemset即可
inspect(rhs(berriesInLHS))

# 使用subset进行进一步过滤. 如,不希望看到rhs包含"root vegetables" 或 "whole milk"
berrySub <- subset(groceryrules, subset = !(rhs %pin% c("8181", "8175")))
inspect(sort(berrySub, by="confidence")[1:2])

#获取出support,confidence,lift外的其他评价标准
qualityMeasures <- interestMeasure(groceryrules, 
                                   measure = c("coverage","fishersExactTest",
                                               "conviction", "chiSquared"), 
                                   transactions=groceries_use)
summary(qualityMeasures)

quality(groceryrules) <- cbind(quality(groceryrules), qualityMeasures)  
inspect(head(sort(groceryrules, by = "conviction", decreasing = F))) 