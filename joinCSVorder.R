
library(XLConnect)
library(xlsx)
library(rJava)
library(dplyr)
options(scipen = 200) #取消科学计数
#***************订单导入
#order.csv <- read_csv('D:\\备份\\来尔佳昵\\旗舰店\\销售数据报表-下载\\订单数据\\ExportOrderList201905151650.csv',col_names = T)
order.csv <- read.csv('C:/Users/lenovo/Desktop/R/order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
order_3.csv <- read.csv('C:/Users/lenovo/Desktop/R/3order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
order.csv$订单编号 <- as.character(order.csv$订单编号)
order_3.csv$订单编号 <- as.character(order_3.csv$订单编号)
order_3.csv <- order_3.csv[,colnames(order.csv)]
names(order.csv) <- gsub("\\."," ",names(order.csv))
names(order_3.csv) <- gsub("\\."," ",names(order_3.csv))
order.csv <- plyr::rbind.fill(order.csv,order_3.csv)
order.csv$店铺Id <- ""
order.csv$订单编号 <- gsub("=","",order.csv$订单编号)	
order.csv$联系手机 <- gsub("'","",order.csv$联系手机)
order.csv$打款商家金额 <- as.numeric(gsub("元","",order.csv$打款商家金额))
duplicate <- duplicated(order.csv[,"订单编号"]) #重复项
write.csv(order.csv[!duplicate,],file = "D:/备份/来尔佳昵/旗舰店/销售数据报表-下载/订单数据/销售/all_orders.csv",row.names = FALSE,fileEncoding = "UTF-8")

item.csv <- read.csv('C:/Users/lenovo/Desktop/R/item.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
item_3.csv <- read.csv('C:/Users/lenovo/Desktop/R/3item.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
item.csv <-  plyr::rbind.fill(item.csv,item_3.csv)
item.csv$订单编号 <- gsub("=","",as.character(item.csv$订单编号))
duplicate <- duplicated(item.csv) #重复项
write.csv(item.csv[!duplicate,],file = "D:/备份/来尔佳昵/旗舰店/销售数据报表-下载/订单数据/商品/all_items.csv",row.names = FALSE,fileEncoding = "UTF-8")

#write.xlsx(item.csv,file = "C:/Users/lenovo/Desktop/R/item1.xlsx", sheetName="Sheet1",col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)