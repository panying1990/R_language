
#author:panying
#本代码块用于解决用户聚类算法 
#最后编辑时间：2017年09月10日
#加载数据样式模块
------------------------------------------------------------------------------
#设置工作路径及工作环境
  workdir<-getwd()
  if(!is.null(workdir))
    setwd(workdir)

  
  #导入数据 data
  library(DBI)
  library(RMySQL)
  conn<-dbConnect(MySQL(),dbname="tag_explore",host="192.168.111.251",username="root",password="P#y20bsy17")
  dbSendQuery(conn,"SET NAMES gbk")
  # read_data<-dbSendQuery(conn, "SELECT * from kd_trade_brusher180")
  # kd_trade_brusher<-dbFetch(read_data,-1)
  #采用csv格式导入
  teens <- read.csv("snsdata.csv")
  str(teens)
  
  #第二步：探索数据
  #1、缺失值处理,极端值处理
  table(teens$gender)
  table(teens$gender, useNA = "ifany")
  summary(teens$age)
  #1.1 对于关键信息，如性别等，对于未知状态用户信息可以将其设置为第三状态编码
  # reassign missing gender values to "unknown"
  teens$female <- ifelse(teens$gender == "F" &!is.na(teens$gender), 1, 0)
  teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
  #查询设置结果
  table(teens$gender, useNA = "ifany")
  table(teens$female, useNA = "ifany")
  table(teens$no_gender, useNA = "ifany")
  #1.2 插补法
  #2、极端值处理，将不符合基本逻辑的极端值设置为NA
  teens$age <- ifelse(teens$age >= 13 & teens$age < 20,teens$age, NA)
