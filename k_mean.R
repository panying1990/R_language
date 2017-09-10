
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
  # 寻找字段分布状态，以age为例
  mean(teens$age) # 因为有NA值，故无法进行
  mean(teens$age, na.rm = TRUE) 
  
  # 利用毕业时间计算年龄
  aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
  # 为每个实例计算年龄
  ave_age <- ave(teens$age, teens$gradyear,FUN = function(x) mean(x, na.rm = TRUE))
  teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
  # 检查插补后的数据，无NA值
  summary(teens$age)
  #2、极端值处理，将不符合基本逻辑的极端值设置为NA
  teens$age <- ifelse(teens$age >= 13 & teens$age < 20,teens$age, NA)
  
  ## 第三步: 基于数据训练模型 ----
  interests <- teens[5:40]
  interests_z <- as.data.frame(lapply(interests, scale))
  
  teen_clusters <- kmeans(interests_z, 5)
  
  ## Step 4: Evaluating model performance ----
  # look at the size of the clusters
  teen_clusters$size
  
  # look at the cluster centers
  teen_clusters$centers
  
  ## Step 5: Improving model performance ----
  # apply the cluster IDs to the original data frame
  teens$cluster <- teen_clusters$cluster
  
  # look at the first five records
  teens[1:5, c("cluster", "gender", "age", "friends")]
  
  # mean age by cluster
  aggregate(data = teens, age ~ cluster, mean)
  
  # proportion of females by cluster
  aggregate(data = teens, female ~ cluster, mean)
  
  # mean number of friends by cluster
  aggregate(data = teens, friends ~ cluster, mean)
