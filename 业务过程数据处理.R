# _author_ = panying
# 模块目的：快速处理电商市场推广及运营数据导入
# 最后编辑时间："2017-11-29"
# Sys.Date()

# 数据环境================================================================ 
library(DBI)
library(RMySQL)

# 数据库环境设置
conn1<-dbConnect(MySQL(),dbname="master_data",host="192.168.111.251",username="root",password="P#y20bsy17")
dbSendQuery(conn1,"SET NAMES gbk")
read_word<-dbSendQuery(conn1,"select tbk_date from mkt_tbk_check")
tbk_date<-dbFetch(read_word, n = -1)
tbk_tag_date<-max(tbk_date$tbk_date)

# 设置工作目录
dir<-"V:/更新数据/数据转化中转文件夹"
setwd(dir)
getwd()

# 设置当天时间函数
now_date<-Sys.Date()
month_date<-substr(now_date,6,7)
day_date<-substr(now_date,9,10)

## mkt_tbk_data======================================
tbk_filename<-"mkt_tbk_data"
file_names<-paste0(tbk_filename,month_date,".",day_date,".csv")
file_names_data<-read.csv(file_names,stringsAsFactors = FALSE)
file_names_date<-min(file_names_data$确认收货时间)
file_names_data$添加时间<-now_date
names(file_names_data)<-c('确认收货时间','结算时间','创建时间','计划名称','商品名称','实际成交价格','成交商品数',
                          '佣金比例','佣金','淘宝订单编号','来源或淘客昵称','添加时间')
for(i in 0:nrow(file_names_data))
{
  file_names_data$淘宝订单编号[i]<-format(file_names_data$淘宝订单编号[i], scientific = FALSE)
}


if (tbk_tag_date<file_names_date)
  {dbWriteTable(conn1,"mkt_tbk_data",file_names_data,append = TRUE,row.names = FALSE)
  print("success!")}else{print("wrong!!")}

## mkt_zz_data======================================
zz_word<-dbSendQuery(conn1,"select zz_date from mkt_zz_check")
zz_date<-dbFetch(zz_word, n = -1)
zz_tag_date<-max(zz_date$zz_date)
zz_filename<-"mkt_zz_data"
file_names<-paste0(zz_filename,month_date,".",day_date,".csv")
file_names_data<-read.csv(file_names,stringsAsFactors = FALSE)
file_names_data$时间<-as.Date(file_names_data$时间,origin = "1899-12-30")
file_names_date<-min(file_names_data$时间)
file_names_data$添加时间<-now_date
names(file_names_data)<-c('计划基本信息','时间','展现','点击','消耗','点击率(%)','点击单价(元)','千次展现成本(元)','访客',
                                '深度进店量','访问时长','访问页面数','收藏宝贝量','收藏店铺量','添加购物车量','拍下订单量',
                          '拍下订单金额','成交订单量','成交订单金额','点击转化率(%)','投资回报率','添加时间')


if (zz_tag_date<file_names_date)
{
  dbWriteTable(conn1,"mkt_zz_data",file_names_data,append = TRUE,row.names = FALSE)
  print("success!")
}else{print("wrong!!")}

## mkt_pzbrand_data======================================
pzbrand_word<-dbSendQuery(conn1,"select pzbrand_date from mkt_pzbrand_check")
pzbrand_date<-dbFetch(pzbrand_word, n = -1)
pzbrand_tag_date<-max(pzbrand_date$pzbrand_date)
pzbrand_filename<-"mkt_pzbrand_data"
file_names<-paste0(pzbrand_filename,month_date,".",day_date,".csv")
file_names_data<-read.csv(file_names,stringsAsFactors = FALSE)
file_names_data$日期<-as.Date(file_names_data$日期,origin = "1899-12-30")
file_names_date<-min(file_names_data$日期)
file_names_data$添加时间<-now_date
names(file_names_data)<-c('日期','所属计划','所属单元','流量包名称','展现量','点击量','点击率(%)','宝贝收藏数',
                            '店铺收藏数','宝贝加购数','宝贝浏览数','成交笔数','转化率','回搜展现量','回搜点击量','店铺浏览数',
                            '访问时长','访客数','新访客数','店铺收藏访客数','店铺收藏新访客数','宝贝收藏访客数','宝贝收藏新访客数',
                            '宝贝加购访客数','宝贝加购新访客数','宝贝浏览访客数','宝贝浏览新访客数','成交访客数','成交新访客数',
                            '回搜触达访客数','回搜触达新访客数','回搜点击访客数','回搜点击新访客数','添加时间')


if (pzbrand_tag_date<file_names_date)
{
  dbWriteTable(conn1,"mkt_pzbrand_data",file_names_data,append = TRUE,row.names = FALSE)
  print("success!")
}else{print("wrong!!")}

## mkt_pzperson_data======================================
pzperson_word<-dbSendQuery(conn1,"select pzperson_date from mkt_pzperson_check")
pzperson_date<-dbFetch(pzperson_word, n = -1)
pzperson_tag_date<-max(pzperson_date$pzperson_date)
pzperson_filename<-"mkt_pzperson_data"
file_names<-paste0(pzperson_filename,month_date,".",day_date,".csv")
file_names_data<-read.csv(file_names,stringsAsFactors = FALSE)
file_names_data$日期<-as.Date(file_names_data$日期,origin = "1899-12-30")
file_names_date<-min(file_names_data$日期)
file_names_data$添加时间<-now_date
names(file_names_data)<-c('日期','所属计划','所属单元','创意名称','定向名称','展现量','点击量','点击率(%)','宝贝收藏数'
                          ,'店铺收藏数','宝贝加购数','宝贝浏览数','成交笔数','转化率','回搜展现量','回搜点击量','店铺浏览数','访问时长'
                          ,'访客数','新访客数','店铺收藏访客数','店铺收藏新访客数','宝贝收藏访客数','宝贝收藏新访客数'
                          ,'宝贝加购访客数','宝贝加购新访客数','宝贝浏览访客数','宝贝浏览新访客数','成交访客数','成交新访客数','回搜触达访客数'
                          ,'回搜触达新访客数','回搜点击访客数','回搜点击新访客数','添加时间')


if (pzperson_tag_date<file_names_date)
{
  dbWriteTable(conn1,"mkt_pzperson_data",file_names_data,append = TRUE,row.names = FALSE)
  print("success!")
}else{print("wrong!!")}
