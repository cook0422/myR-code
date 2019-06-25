library(jiebaR)
library(wordcloud2)
order <- read.csv('C:/Users/lenovo/Desktop/R/order.csv',header=T,sep=',',stringsAsFactors=FALSE,nrows=100000)
order <- unique(order$买家会员名)
txt <- paste(order,collapse = "") #拼接词语

wk = worker(type="mix") #启动分词引擎（默认混合模型）
words <- segment(txt,wk) #分词
words <- words[nchar(words)>1]
words_count_table <- freq(words)  #统计词频 table(words)
words_count_table <- words_count_table[order(-words_count_table[,2]),]  #排序
wordcloud2(words_count_table[1:200,],size = 1,shape = 'star')
