# author:panying1990
# 本代码块采用随机抽样，抽取用于检测数据，并对抽样数据执行区间<5%;
# 最后编辑时间：2017年09月15日
# 加载数据样式模块
# ------------------------------------------------------------------------------
# 设置工作路径
workdir<-getwd()
if(!is.null(workdir))
  setwd(workdir)

# 导入数据 data
library(DBI)
library(RMySQL)
# 导入数据库
conn1<-dbConnect(MySQL(),dbname="tag_explore",host="192.168.111.251",username="root",password="P#y20bsy17")
# 导出数据库
conn2<-dbConnect(MySQL(),dbname="tag_text",host="192.168.111.251",username="root",password="P#y20bsy17")
dbSendQuery(conn1,"SET NAMES gbk")
read_data<-dbSendQuery(conn1, "SELECT * from fristdrug_check_data")
fristdrug_check_data<-dbFetch(read_data,-1)
