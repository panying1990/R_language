# author:panying1990
# 本代码块采用朴素贝叶斯算法，基于国人取名特征规律，对用户性别进行识别
# 最后编辑时间：2017年09月07日
# 加载数据样式模块
# ------------------------------------------------------------------------------
# 设置工作路径
workdir<-getwd()
if(!is.null(workdir))
    setwd(workdir)

# 设置工作环境

  
# 导入数据 data
library(DBI)
library(RMySQL)
conn1<-dbConnect(MySQL(),dbname="tag_explore",host="192.168.111.251",username="root",password="P#y20bsy17")
conn2<-dbConnect(MySQL(),dbname="tag_text",host="192.168.111.251",username="root",password="P#y20bsy17")
dbSendQuery(conn1,"SET NAMES gbk")
read_data<-dbSendQuery(conn1, "SELECT * from sex_tag_Classifier")
kd_customer_sex<-dbFetch(read_data,-1)

## 第一步：准备分析数据--------------------------------------------------------------------

library(dplyr)
library(stringr)
#去除文本中的数字
kd_customer_sex$customer_name_t=gsub(pattern = "[a-zA-Z]+","",kd_customer_sex$customer_name)
#去除文本中的字母
kd_customer_sex$customer_name_t=gsub(pattern = "[0-9 0 1 2 3 4 5 6 7 8 9]+","",kd_customer_sex$customer_name_t)
#去除文本中的特殊号
kd_customer_sex$customer_name_t=gsub(pattern = "[.]","",kd_customer_sex$customer_name_t)
kd_customer_sex$customer_name_t=gsub(pattern = "[# @ ! % ^ & * -]","",kd_customer_sex$customer_name_t)
kd_customer_sex$customer_name_t=gsub(pattern = "[, / ? \ |]","",kd_customer_sex$customer_name_t)
kd_customer_sex$customer_name_t=gsub(pattern = "[， 。 （ ） ( ) 一]","",kd_customer_sex$customer_name_t)
kd_customer_sex$customer_name_t=gsub(pattern = "[ ]","",kd_customer_sex$customer_name_t)

typeof(kd_customer_sex)
  
#获得整理后数据情况
kd_customer_sex_f<-filter(kd_customer_sex,kd_customer_sex$customer_name_t!='')
str(kd_customer_sex_f)
prop.table(table(kd_customer_sex_f$sex))

kd_customer_sex_f$sex<-as.factor(kd_customer_sex_f$sex)

#文本数据处理-构建语料库
#将用户姓名向量化，将名字拆分成姓 名等适应词频统计软件使
library(NLP)
library(tm)
library(tmcn)
library(stringr)
library(Rcpp)
library(jiebaRD)
library(jiebaR)

#将用户姓名分好词
customer.firstname<-substr(kd_customer_sex_f$customer_name_t,0,1)
custname<-str_trim(substr(kd_customer_sex_f$customer_name_t,2,4),side = "both")
#customer.secondname<-lapply(X=customer.secondname, FUN=str_split,"")
worker()
segmentCN(customer.secondname, package = "jiebaR", nature = FALSE,nosymbol = TRUE, returnType = "tm")
 
          
#customer.name_matrix[1]<-(unlist(customer.secondname[1]))
#customer.sname$text<-lapply(X=customer.secondname,FUN=)
customer.sname<-list()
for(i in 1:length(customer.secondname))
{
  customer.sname[i]=as.array(unlist(customer.secondname[1]))
  }
   
  
customer.namefreq<-as.data.frame(sort(table(unlist(customer.secondname)),decreasing = TRUE))
write.csv(customer.secondname2,"customer_secondname.csv")


## 第二步：探索数据特征，特征工程--------------------------------------------------------------------
# 创建训练和测试数据集，将数据集合随机设置

custname_raw_train <- custname_raw[1:4169, ]
custname_raw_test<- custname_raw[4170:5559, ]

custname_dtm_train <- custname_dtm[1:4169, ]
custname_dtm_test<- custname_dtm[4170:5559, ]

custname_corpus_train <- custname_corpus_clean[1:4169]
custname_corpus_test  <- custname_corpus_clean[4170:5559]

# 检查测试与训练与测试数据集合的比例是否相似
prop.table(table(custname_raw_train$sex))
prop.table(table(custname_raw_test$type))

# 创造词云，选择合适的用户名声
library(wordcloud)

wordcloud(custname_corpus_train, min.freq = 10, random.order = FALSE)

# 将模型训练数据分别分成男组，女组
male <- subset(custname_raw_train, sex == "male")
female<- subset(custname_raw_train, sex == "female")

wordcloud(male$sname, max.words = 40, scale = c(3, 0.5))
wordcloud(female$sname, max.words = 40, scale = c(3, 0.5))

# 将词频较高的名单字作为特征指标
findFreqTerms(custname_dtm_train, 5)
custname_dict <- Dictionary(findFreqTerms(custname_dtm_train, 5))
custname_train <- DocumentTermMatrix(custname_corpus_train, list(dictionary = custname_dict))
custname_test  <- DocumentTermMatrix(custname_corpus_test, list(dictionary = custname_dict))

# 将数值技术转化为一个逻辑判别银子
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# 应用convert_counts() 函数转换训练数据/测试数据的列名
custname_train <- apply(custname_train, MARGIN = 2, convert_counts)
custname_test  <- apply(custname_train, MARGIN = 2, convert_counts)

## 第三步: 利用训练数据训练模型 -----------------------------------------------------
library(e1071)
custname_classifier <- naiveBayes(custname_train, custname_raw_train$sex)
custname_classifier

## 第四步:  评估模型有效性 ------------------------------------------------------------
custname_test_pred <- predict(custname_classifier, custname_test)

library(gmodels)
CrossTable(custname_test_pred, custname_raw_test$sex,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## 第五步: 优化提升模型有效性 ---------------------------------------------------------
custname_classifier2 <- naiveBayes(custname_train, custname_raw_train$sex, laplace = 1)
custname_test_pred2 <- predict(custname_classifier2, custname_test)
CrossTable(custname_test_pred2, custname_raw_test$sex,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


#将数据写入到目标数据库方便之后做进一步整合
dbListTables(conn2)  
dbWriteTable(conn2,"customer_se",xxxxx)  
dbListTables(conn2)
