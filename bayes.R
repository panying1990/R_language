# author:panying1990
# 本代码块采用朴素贝叶斯算法，基于国人取名特征规律，对用户性别进行识别
# 最后编辑时间：2017年09月07日
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

#typeof(kd_customer_sex)
  
#获得整理后数据情况
kd_customer_sex_f<-filter(kd_customer_sex,kd_customer_sex$customer_name_t!='')
str(kd_customer_sex_f)
prop.table(table(kd_customer_sex_f$sex_type))*100
# 将数据修改成为因子型
kd_customer_sex_f$sex_type<-as.factor(kd_customer_sex_f$sex_type)

#文本数据处理-构建语料库
#将用户姓名向量化，将名字拆分成姓 名等适应词频统计软件使
library(NLP)
library(tm)
library(tmcn)
library(stringr)
library(Rcpp)
library(jiebaRD)
library(jiebaR)
#vignette("tmCN")  
custsname_text<-str_trim(paste(substr(kd_customer_sex_f$customer_name_t,2,2),' ',substr(kd_customer_sex_f$customer_name_t,3,3)),side = "both")
custsname_sex_type<-kd_customer_sex_f$sex_type
custsname_raw<-data.frame(name_text=custsname_text, sex_type=custsname_sex_type)
write.table(custsname_raw$name_text,file = "custsname_raw.txt", row.names = FALSE)
texts = readLines("custsname_raw.txt", encoding="UTF-8")
custsplit = worker(bylines = TRUE) 
custsname_split = segment(texts, custsplit)
合并各行分词结果 =sapply(custsname_split, function(x){ paste(x, collapse = " ")})
writeLines(合并各行分词结果, "./某个文件.txt")
file.remove("./某个文件.txt")

#custsname_raw1<-read.csv("custsname_raw.csv")

#修饰名字构成，使名字两边无空格,并将全部数据编码修改成为UTF-8形式
custsname_raw1<-as.data.frame(lapply(X=custsname_raw, FUN=strstrip))
custsname_raw1$sex_type<-iconv(custsname_raw1$sex_type, "GBK", 'UTF-8')
custsname_raw1$name_text<-iconv(custsname_raw1$name_text, "GBK", 'UTF-8')
#custsname_raw1<-lapply(X=custsname_raw1, FUN=strstrip)
#worker()
#segmentCN(custsname, package = "jiebaR", nature = FALSE,nosymbol = TRUE, returnType = "tm")
VectorSource(custsname_raw1$name_text)
docs <- c("This is a text.", "This another one.")
(vs <- VectorSource(docs))
inspect(VCorpus(vs))

custsname_corpus<-Corpus(VectorSource(custsname_raw1$name_text))
custsname_corpus<-tm_map(custsname_corpus,stripWhitespace)
custsname_dtm<-DocumentTermMatrix(custsname_corpus)

## 第二步：探索数据特征，特征工程--------------------------------------------------------------------
# 设置随机取数装置
sample0<-1:nrow(custsname_raw1)
sample1<-sample(x=sample0,size=round(nrow(custsname_raw1)*0.9,0),replace = FALSE)
sample2<-array()
j<-1
for(i in 1:2075)
{
  if(i %in% sample1) j<-j+0
  else
  {sample2[j]<-i
    j<-j+1
  }
}
# 创建训练和测试数据集，
custsname_raw_train <- custsname_raw1[sample1, ]
custsname_raw_test<- custsname_raw1[sample2, ]

custsname_dtm_train <- custsname_dtm[sample1 ]
custsname_dtm_test<- custsname_dtm[sample2, ]

custsname_corpus_train <- custsname_corpus[sample1]
custsname_corpus_test  <- custsname_corpus[sample2]

# 检查测试与训练与测试数据集合的比例是否相似
prop.table(table(custsname_raw_train$sex_type))
prop.table(table(custsname_raw_test$sex_type))

# 创造词云，选择合适的用户名声
library(RColorBrewer)
library(wordcloud)

wordcloud(custsname_corpus_train, min.freq = 10, random.order = FALSE)

# 将模型训练数据分别分成男组，女组
male <- subset(custsname_raw_train, sex_type == "male")
female<- subset(custsname_raw_train, sex_type == "female")

wordcloud(male$name_text, max.words = 1, scale = c(1, 0.5))
wordcloud(female$name_text, max.words = 3, scale = c(3, 0.5))

# 将词频较高的名单字作为特征指标
toUTF8(findFreqTerms(custsname_dtm_train, 5))
custname_dict <- findFreqTerms(custsname_dtm_train, 5)
custname_train <- DocumentTermMatrix(custsname_corpus_train, list(dictionary = custname_dict))
custname_test  <- DocumentTermMatrix(custsname_corpus_test, list(dictionary = custname_dict))

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
custname_classifier <- naiveBayes(custname_train, custsname_raw_train$sex_type)
custname_classifier

## 第四步:  评估模型有效性 ------------------------------------------------------------
custname_test_pred <- predict(custname_classifier, custname_test)

library(gmodels)
CrossTable(custname_test_pred, custsname_raw_test$sex,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## 第五步: 优化提升模型有效性 ---------------------------------------------------------
custname_classifier2 <- naiveBayes(custname_train, custsname_raw_train$sex, laplace = 1)
custname_test_pred2 <- predict(custname_classifier2, custsname_test)
CrossTable(custname_test_pred2, custsname_raw_test$sex,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


#将数据写入到目标数据库方便之后做进一步整合
dbListTables(conn2)  
dbWriteTable(conn2,"customer_se",xxxxx)  
dbListTables(conn2)

#新R包探索
data(GBK)
head(GBK)
Encoding(custsname_raw1$name_text[2])
toUTF8(txt1) 
txt1 <- c("\u4E2D\u56FDR\u8BED\u8A00\u4F1A\u8BAE")  #UTF-8编码  
txt2 <- iconv(txt1, "UTF-8", "GBK")   
toPinyin(txt1, capitalize = TRUE)

custsname_raw1$sex_type<-iconv(custsname_raw1$sex_type, "GBK", "UTF-8")
custsname_raw1$name_text<-iconv(custsname_raw1$name_text, "GBK", "UTF-8")
