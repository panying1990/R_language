#author:panying
#本代码块用于解决用户关联规则算法 
#最后编辑时间：2017年07月28日
#加载数据样式模块
------------------------------------------------------------------------------
#设置工作路径及工作环境
workdir<-getwd()
if(!is.null(workdir))
  setwd(workdir)

library(Matrix)
library(arules)
#通过统一处理将数据转为为交易数据形式
groceries <- read.transactions("groceries.csv", sep = ",") 
#导入数据 data
library(DBI)
library(RMySQL)
conn<-dbConnect(MySQL(),dbname="tag_explore",host="192.168.111.251",username="root",password="P#y20bsy17")
dbSendQuery(conn,"SET NAMES gbk")
# read_data<-dbSendQuery(conn, "SELECT * from kd_trade_brusher180")
# kd_trade_brusher<-dbFetch(read_data,-1)

#检查数据类型
summary(groceries)
inspect(groceries[1:5])

# 检查数据集中各产品的支持度情况
itemFrequency(groceries[, 1:3])
     
#数据集中不同产品支持度可视化
#1.支持度>0.1的的产品可视化
itemFrequencyPlot(groceries, support = 0.1)
#2.支持度排名前20的可视化
itemFrequencyPlot(groceries, topN = 20)
#3.可视化交易数据-绘制稀疏矩阵
image(groceries[1:25])
#4.随机抽样进行交易数据可视化
image(sample(groceries, 100))

#第三步：基于数据训练模型
apriori(groceries)


# 根据需要调试支持度、置信度、交易包含项
# 支持度support：某产品在交易集中出现的频率；
# 置信度confidence: 类似条件概率
# 交易订单项目：一般设置为>=2;
groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))


#第四步：评估规则
summary(groceryrules)
inspect(groceryrules[1:5])

#第五步：提高模型性能
# 根据提升度情况，逐一检查规则的使用性
inspect(sort(groceryrules, by = "lift")[1:5],decreasing=FALSE)
# 将符合某一产品来源的规则进行检查
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)


# 将规则以CSV格式进行输出
write(groceryrules, file = "groceryrules.csv",sep = ",", quote = TRUE, row.names = FALSE)

# 将规则作为数据框的格式进行输出
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

#将数据写入到目标数据库方便之后做进一步整合
dbListTables(conn)  
dbWriteTable(conn,"groceryrules_df",groceryrules_df)  
dbListTables(conn)
