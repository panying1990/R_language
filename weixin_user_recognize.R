## coding：UTF-8
## AUTHOR:Pan Ying.

## 对电商会员营销通过拆包裹有奖加微信的客户进行打标
## 本部分数据即对该类用户的特定进行统一分析以进行统一导入
## 最近一次编辑时间

# 数据导入--------------------------------
source_dir="V:/【需求】电商数据需求/拆包裹优秀奖微信好友"
setwd(source_dir)
weixin_data<-read.csv("target_data.csv",stringsAsFactors = FALSE, encoding = 'gbk')

# 数据基本情况---------------
## 得到数据唯一的用户手机号
library(dplyr)
mobilephone<-weixin_data[1:2]%>%
  group_by(手机或其他信息)%>%
  summarise(n = n())

## 将数据导入到数据库中，与CRM中户手机号做匹配
library(DBI)
library(RMySQL)

conn<-dbConnect(MySQL(),dbname="crm_kd",host="192.168.111.251",username="root",password="55555555")
dbSendQuery(conn,"SET NAMES gbk")
## query<-dbSendQuery(conn1, "SELECT * from address_unnorm_data")
## address_unnorm_data<-dbFetch(anal_data,-1)

# 数据标准化处理==============================
target_phone<-data.frame(tellphone =rep(1,nrow(weixin_data)),taobaoid =rep(1,nrow(weixin_data)),tag =rep(1,nrow(weixin_data)))
# 原始数据中淘宝id与手机号为唯一可识别用户信息，该部分对该数据进行以下判定
# 1、当两个字段皆不为空，且手机匹配手机格式则将手机作为该微信好友的唯一识别，若手机不符合标准则将淘宝ID作为唯一识别；
# 2、当两字段其中某一列为空，则将其中不为空部分，为手机号则符合标识则判定为唯一标识，否则采用淘宝ID作为唯一识别；

for(i in 1:nrow(weixin_data)){
  if((is.null(weixin_data$淘宝id[i])&is.null(weixin_data$手机或其他信息[i]))==FALSE){
    if(nchar(weixin_data$手机或其他信息[i])==11){
      target_phone$tellphone[i]<-weixin_data$手机或其他信息[i]
      target_phone$tag[i]<-"手机号"
    }else{
      target_phone$taobaoid[i]<-weixin_data$淘宝id[i]
      target_phone$tag[i]<-"淘宝ID"}
  }
  else if((is.null(weixin_data$手机或其他信息[i])==FALSE)&(is.null(weixin_data$淘宝id[i])==TRUE)){
    if(nchar(weixin_data$手机或其他信息[i])==11){
      target_phone$tellphone[i]<-weixin_data$手机或其他信息[i]
      target_phone$tag[i]<-"手机号"
    }else{
      target_phone$tellphone[i]<-null
      target_phone$tag[i]<-"手机号不合格"
      }
  }
  else{
    if(is.null(weixin_data$淘宝id[i])==FALSE){
      target_phone$taobaoid[i]<-weixin_data$淘宝id[i]
      target_phone$tag[i]<-"淘宝ID"
    }else{
      target_phone$淘宝id[i]<-null
      target_phone$tag[i]<-"淘宝ID不合格"
    }
    
  }
}


