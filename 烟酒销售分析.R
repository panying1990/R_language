# coding = utf-8 #
# author = panyiing 

# 内存使用空间调整
memory.limit(102400)
a<-memory.size()

# 用于分析烟草产品活动前后效果分析
# 原始数据结果描述

saledata <- read.csv(file = "数据分析法0311_20180312.csv",stringsAsFactors=FALSE)

library(DBI)
library(RMySQL)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stats)
library(base)
library(stringr)
# 数据库环境设置
conn1<-dbConnect(MySQL(),dbname="tag_text",host="192.168.111.251",username="root",password="P#y20bsy17")
dbSendQuery(conn1,"SET NAMES gbk")
query<-dbSendQuery(conn1, "SELECT * from 结果数据")
saledata<-dbFetch(query,-1)

# 数据结构初始分析
# 缺失值分析
a<-sum(complete.cases(saledata))
a<-sum(!complete.cases(saledata))
a<-mean(!complete.cases(saledata))


# 数据特征重新命名
names(saledata)

# 数据值特征可视化
sp<-boxplot(saledata$全国A小包数,boxwex=0.7)
title("A产品销量异常值检测箱型图")
xi<-1.1
sd.s<-sd(saledata[complete.cases(saledata),]$)
mn.s<-mean(saledata[complete.cases(saledata),]$全国A小包数)
points(xi,mn.s,col="red",pch=18)
arrows(xi,mn.s-sd.s,xi,mn.s+sd.s,code=3,col="pink",angle = 75,length = .1)
text(rep(c(1.05,0.95),length(sp$out)/2),sp$out,col="red")
 

# 将时间替换成序列号

saledata$时间序列[(saledata$年份==2017)&(saledata$月份=='07')]<-1
saledata$时间序列[(saledata$年份==2017)&(saledata$月份=='08')]<-2
saledata$时间序列[(saledata$年份==2017)&(saledata$月份=='09')]<-3
saledata$时间序列[(saledata$年份==2017)&(saledata$月份==10)]<-4
saledata$时间序列[(saledata$年份==2017)&(saledata$月份==11)]<-5
saledata$时间序列[(saledata$年份==2017)&(saledata$月份==12)]<-6
saledata$时间序列[(saledata$年份==2018)&(saledata$月份=='01')]<-7 
saledata$时间序列[(saledata$年份==2018)&(saledata$月份=='02')]<-8


# 衍生定义数据
saledata$小包扫码率<-round(100*(saledata$全国A小包数_人次+saledata$重庆A小包数_人次)/saledata$全国A小包数,2)
saledata$条盒扫码率<-round(100*(saledata$全国A条盒数_人次+saledata$重庆A条盒数_人次)/saledata$全国A条盒数,2)
saledata$小包扫码数<-round(saledata$全国A小包数_人次+saledata$重庆A小包数_人次,0)
saledata$条盒扫码数<-round(saledata$全国A条盒数_人次+saledata$重庆A条盒数_人次,0)


# 测算不同城市产品A与产品B销量的情况
write.csv(saledata,"基础销售分析2.csv")

# 筛选数据
city<-subset(saledata,小包扫码率>0,select =c("城市"))
city_abc<-base::unique(city)
sale_ana_data<-filter(saledata,!is.na(str_match(saledata$城市, "成都|德阳|绵阳|宜宾|重庆")))
sale_ana_data$省份城市<-paste0(sale_ana_data$省份,sale_ana_data$城市)


# 数据值特征可视化
sp<-boxplot(sale_ana_data$全国A小包数,boxwex=0.7)
title("目标数据异常值检测箱型图")
xi<-1.1
sd.s<-sd(sale_ana_data[complete.cases(sale_ana_data),]$)
mn.s<-mean(sale_ana_data[complete.cases(sale_ana_data),]$全国A小包数)
points(xi,mn.s,col="red",pch=18)
arrows(xi,mn.s-sd.s,xi,mn.s+sd.s,code=3,col="pink",angle = 75,length = .1)
text(rep(c(1.05,0.95),length(sp$out)/2),sp$out,col="red")



sale_ana_data_temp<-subset(sale_ana_data,全国A小包数>0&全国B小包数>0,select=c("省份","城市","全国A条盒数","全国A小包数","小包扫码率","条盒扫码率","时间序列","省份城市"))

names(sale_ana_data_temp)<-c("省份","城市","五粮醇香and渝金香条盒销量","五粮醇香and渝金香小包销量","小包扫码率","条盒扫码率","时间序列","省份城市")
# 

sale_ana_data_temp$时间日期[sale_ana_data_temp$时间序列==1]<-as.Datepaste0("2017","-07")
sale_ana_data_temp$时间日期[sale_ana_data_temp$时间序列==2]<-paste0("2017","-08")
sale_ana_data_temp$时间日期[sale_ana_data_temp$时间序列==3]<-paste0("2017","-09")
sale_ana_data_temp$时间日期[sale_ana_data_temp$时间序列==4]<-paste0("2017","-10")
sale_ana_data_temp$时间日期[sale_ana_data_temp$时间序列==5]<-paste0("2017","-11")
sale_ana_data_temp$时间日期[sale_ana_data_temp$时间序列==6]<-paste0("2017","-12")
sale_ana_data_temp$时间日期[sale_ana_data_temp$时间序列==7]<-paste0("2018","-01")
sale_ana_data_temp$时间日期[sale_ana_data_temp$时间序列==8]<-paste0("2018","-02")
# sale_city_scan<-melt(saledata,id=c("省份","城市"))
# 
# # is.infinite(melt(saledata,id=c("省份","城市")))
# sale_city_data<-dcast(sale_city_scan,省份+城市~variable,sum)


city_abc<-base::unique(sale_ana_data$)%>% top_n(-2)

ggplot(sale_ana_data_temp,aes(x=时间日期,y=五粮醇香and渝金香条盒销量,group=省份城市,colour=省份城市))+
  geom_line(linetype="dashed", lineend = "round",size=0.5) + 
  geom_point(size=3, shape=21, fill="white")+
  ggtitle("五粮醇香and渝金香条盒销量BY省份城市")+
  theme_bw() +                         
  theme(legend.position = "bottom") +
  ylab("条盒数")+
  xlab("2017年07月-2018年02月")+
  scale_x_date(date_labels = "%Y-%m")

ggplot(sale_ana_data,aes(x=时间序列,y=条盒扫码率,group=省份城市,colour=省份城市))+
  geom_line(linetype="dashed", lineend = "round",size=0.5) + 
  geom_point(size=3, shape=21, fill="white")+
  ggtitle("五粮醇香and渝金香条盒扫码率BY省份城市")+
  theme_bw() +                         
  theme(legend.position = "bottom")+
  ylab("百分比（%）")+
  xlab("2017年07月-2018年02月")+
  geom_smooth(method="lm",se=FALSE,size = 0.5)+
  scale_x_date(limits = as.Date(c("2017-07-01","2018-02-01")),labels = date_format("%Y-%m-%d"))

  
ggplot(sale_ana_data_temp,aes(x=时间日期,y=五粮醇香and渝金香小包销量,group=省份城市,colour=省份城市))+
  geom_line(linetype="dashed", lineend = "round",size=0.5) + 
  geom_point(size=3, shape=21, fill="white")+
  ggtitle("五粮醇香and渝金香小包销量BY省份城市")+
  theme_bw() +                         
  theme(legend.position = "bottom")+
  ylab("小包数")+
  xlab("2017年07月-2018年02月")+
  geom_smooth(method="lm",se=FALSE,size = 0.5)

ggplot(sale_ana_data_temp,aes(x=时间日期,y=小包扫码率,group=省份城市,colour=省份城市))+
  geom_line(linetype="dashed", lineend = "round",size=0.5) + 
  geom_point(size=3, shape=21, fill="white")+
  ggtitle("五粮醇香and渝金香小包扫码率BY省份城市")+
  theme_bw() +                         
  theme(legend.position = "bottom")+
  ylab("百分比（%）")+
  xlab("2017年07月-2018年02月")+
  geom_smooth(method="lm",se=FALSE,size = 0.5)



# 计算增长率
sale_ana_data$A增长率[sale_ana_data$时间序列==8]<-round(sale_ana_data$全国A条盒数[sale_ana_data$时间序列==8]/sale_ana_data$全国A条盒数[sale_ana_data$时间序列==7]-1,3)*100
sale_ana_data$A增长率[sale_ana_data$时间序列==7]<-round(sale_ana_data$全国A条盒数[sale_ana_data$时间序列==7]/sale_ana_data$全国A条盒数[sale_ana_data$时间序列==6]-1,3)*100
sale_ana_data$A增长率[sale_ana_data$时间序列==6]<-round(sale_ana_data$全国A条盒数[sale_ana_data$时间序列==6]/sale_ana_data$全国A条盒数[sale_ana_data$时间序列==5]-1,3)*100
sale_ana_data$A增长率[sale_ana_data$时间序列==5]<-round(sale_ana_data$全国A条盒数[sale_ana_data$时间序列==5]/sale_ana_data$全国A条盒数[sale_ana_data$时间序列==4]-1,3)*100
sale_ana_data$A增长率[sale_ana_data$时间序列==4]<-round(sale_ana_data$全国A条盒数[sale_ana_data$时间序列==4]/sale_ana_data$全国A条盒数[sale_ana_data$时间序列==3]-1,3)*100
sale_ana_data$A增长率[sale_ana_data$时间序列==3]<-round(sale_ana_data$全国A条盒数[sale_ana_data$时间序列==3]/sale_ana_data$全国A条盒数[sale_ana_data$时间序列==2]-1,3)*100
sale_ana_data$A增长率[sale_ana_data$时间序列==2]<-round(sale_ana_data$全国A条盒数[sale_ana_data$时间序列==2]/sale_ana_data$全国A条盒数[sale_ana_data$时间序列==1]-1,3)*100

sale_ana_data$A增长率[sale_ana_data$A增长率==Inf]<-0


# 计算扫码增长率
# 计算增长率
sale_ana_data$扫码增长率[sale_ana_data$时间序列==8]<-round(sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==8]/sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==7]-1,3)*100
sale_ana_data$扫码增长率[sale_ana_data$时间序列==7]<-round(sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==7]/sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==6]-1,3)*100
sale_ana_data$扫码增长率[sale_ana_data$时间序列==6]<-round(sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==6]/sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==5]-1,3)*100
sale_ana_data$扫码增长率[sale_ana_data$时间序列==5]<-round(sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==5]/sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==4]-1,3)*100
sale_ana_data$扫码增长率[sale_ana_data$时间序列==4]<-round(sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==4]/sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==3]-1,3)*100
sale_ana_data$扫码增长率[sale_ana_data$时间序列==3]<-round(sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==3]/sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==2]-1,3)*100
sale_ana_data$扫码增长率[sale_ana_data$时间序列==2]<-round(sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==2]/sale_ana_data$全国A条盒数_人次[sale_ana_data$时间序列==1]-1,3)*100

sale_ana_data$扫码增长率[sale_ana_data$扫码增长率==Inf]<-0


# 计算扫码增长率
# 计算增长率
sale_ana_data$小包扫码增长率[sale_ana_data$时间序列==8]<-round(sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==8]/sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==7]-1,3)*100
sale_ana_data$小包扫码增长率[sale_ana_data$时间序列==7]<-round(sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==7]/sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==6]-1,3)*100
sale_ana_data$小包扫码增长率[sale_ana_data$时间序列==6]<-round(sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==6]/sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==5]-1,3)*100
sale_ana_data$小包扫码增长率[sale_ana_data$时间序列==5]<-round(sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==5]/sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==4]-1,3)*100
sale_ana_data$小包扫码增长率[sale_ana_data$时间序列==4]<-round(sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==4]/sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==3]-1,3)*100
sale_ana_data$小包扫码增长率[sale_ana_data$时间序列==3]<-round(sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==3]/sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==2]-1,3)*100
sale_ana_data$小包扫码增长率[sale_ana_data$时间序列==2]<-round(sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==2]/sale_ana_data$全国A小包数_人次[sale_ana_data$时间序列==1]-1,3)*100

sale_ana_data$小包扫码增长率[sale_ana_data$小包扫码增长率==Inf]<-0


# 计算增长率
sale_ana_data$B增长率[sale_ana_data$时间序列==8]<-round(sale_ana_data$全国B条盒数[sale_ana_data$时间序列==8]/sale_ana_data$全国B条盒数[sale_ana_data$时间序列==7]-1,3)*100
sale_ana_data$B增长率[sale_ana_data$时间序列==7]<-round(sale_ana_data$全国B条盒数[sale_ana_data$时间序列==7]/sale_ana_data$全国B条盒数[sale_ana_data$时间序列==6]-1,3)*100
sale_ana_data$B增长率[sale_ana_data$时间序列==6]<-round(sale_ana_data$全国B条盒数[sale_ana_data$时间序列==6]/sale_ana_data$全国B条盒数[sale_ana_data$时间序列==5]-1,3)*100
sale_ana_data$B增长率[sale_ana_data$时间序列==5]<-round(sale_ana_data$全国B条盒数[sale_ana_data$时间序列==5]/sale_ana_data$全国B条盒数[sale_ana_data$时间序列==4]-1,3)*100
sale_ana_data$B增长率[sale_ana_data$时间序列==4]<-round(sale_ana_data$全国B条盒数[sale_ana_data$时间序列==4]/sale_ana_data$全国B条盒数[sale_ana_data$时间序列==3]-1,3)*100
sale_ana_data$B增长率[sale_ana_data$时间序列==3]<-round(sale_ana_data$全国B条盒数[sale_ana_data$时间序列==3]/sale_ana_data$全国B条盒数[sale_ana_data$时间序列==2]-1,3)*100
sale_ana_data$B增长率[sale_ana_data$时间序列==2]<-round(sale_ana_data$全国B条盒数[sale_ana_data$时间序列==2]/sale_ana_data$全国B条盒数[sale_ana_data$时间序列==1]-1,3)*100

sale_ana_data$B增长率[sale_ana_data$B增长率==Inf]<-0
sale_ana_data$增长极差<-round(sale_ana_data$A增长率-sale_ana_data$B增长率,1)

# A产品销量增长率与扫码率的关系
sale_ana_data_temp<-subset(sale_ana_data,全国A小包数>0,select=c("省份城市","时间序列","A增长率","小包扫码率"))
names(sale_ana_data_temp)<-c("省份城市","时间序列","五粮醇香增长率","小包扫码率")
sale_ana_data_temp<-melt(sale_ana_data_temp,id=c("省份城市","时间序列"))
names(sale_ana_data_temp)<-c("省份城市","时间序列","比率类型","值")



ggplot(sale_ana_data_temp,aes(x=时间序列,y=值,group=比率类型,colour=比率类型))+
  geom_line(linetype="dashed", lineend = "round",size=0.5) + 
  geom_point(size=3, shape=21, fill="white")+
  ggtitle("各城市五粮醇香销量增长率与小包扫码率关系")+
  theme_bw() +                         
  theme(legend.position = "bottom")+
  facet_wrap(~省份城市)+
  ylab("百分比（%）")+
  xlab("2017年07月-2018年02月")+
  geom_smooth(method="lm",se=FALSE,size = 0.5)


# 销量增长率与扫码增长率之间的关系

sale_ana_data_temp<-subset(sale_ana_data,全国A小包数>0,select=c("省份城市","时间序列","A增长率","扫码增长率",))
names(sale_ana_data_temp)<-c("省份城市","时间序列","五粮醇香销量增长率","条盒扫码量增长率")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==1]<-paste0("07月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==2]<-paste0("08月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==3]<-paste0("09月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==4]<-paste0("10月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==5]<-paste0("11月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==6]<-paste0("12月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==7]<-paste0("18年01月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==8]<-paste0("18年02月")
sale_ana_data_temp<-melt(sale_ana_data_temp,id=c("省份城市","时间序列"))
names(sale_ana_data_temp)<-names(sale_ana_data_temp)

# 
ggplot(sale_ana_data_temp,aes(x=时间序列,y=value,group=variable,colour=variable))+
  geom_line(linetype="dashed", lineend = "round",size=0.5) + 
  geom_point(size=3, shape=21, fill="white")+
  ggtitle("五粮醇香增长率与条盒扫码增长率之间的关系")+
  theme_bw() +                         
  theme(legend.position = "bottom")+
  facet_wrap(~省份城市)+
  ylab("百分比（%）")+
  xlab("2017年07月-2018年02月")


# 销售增长量与扫码率之间的关系
sale_ana_data_temp<-subset(sale_ana_data,全国A小包数>0,select=c("省份城市","时间序列","A增长率","B增长率"))
names(sale_ana_data_temp)<-c("省份城市","时间序列","五粮香醇销量增长率","云烟销量增长率")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==1]<-paste0("07月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==2]<-paste0("08月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==3]<-paste0("09月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==4]<-paste0("10月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==5]<-paste0("11月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==6]<-paste0("12月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==7]<-paste0("18年01月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==8]<-paste0("18年02月")
sale_ana_data_temp<-melt(sale_ana_data_temp,id=c("省份城市","时间序列"))
names(sale_ana_data_temp)<-c("省份城市","时间序列","比率类型","值")

ggplot(sale_ana_data_temp,aes(x=时间序列,y=值,group=比率类型,colour=比率类型))+
  geom_line(linetype="dashed", lineend = "round",size=0.5) + 
  geom_point(size=3, shape=21, fill="white")+
  ggtitle("五粮醇香销量增长率与云烟销量增长率之间的关系")+
  theme_bw() +                         
  theme(legend.position = "bottom")+
  facet_wrap(~省份城市)+
  ylab("百分比（%）")+
  xlab("2017年07月-2018年02月")+
  scale_x_date(limits = as.Date(c("2017-07-01","2018-02-01")),labels = date_format("%Y-%m-%d"))
  

# 销售增长量与销售增长量的关系
sale_ana_data_temp<-subset(sale_ana_data,全国A小包数>0,select=c("省份城市","时间序列","增长极差"))
names(sale_ana_data_temp)<-c("省份城市","时间序列","五粮香醇销量增长率与云烟销量增长率之差")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==1]<-paste0("07月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==2]<-paste0("08月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==3]<-paste0("09月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==4]<-paste0("10月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==5]<-paste0("11月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==6]<-paste0("12月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==7]<-paste0("18年01月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==8]<-paste0("18年02月")
sale_ana_data_temp<-melt(sale_ana_data_temp,id=c("省份城市","时间序列"))
names(sale_ana_data_temp)<-c("省份城市","时间序列","比率类型","值")

ggplot(sale_ana_data,aes(x=时间序列,y=值,group=比率类型,colour=比率类型,fill=比率类型))+
  geom_bar(stat="identity") +
  geom_abline()+
  ggtitle("各城市五粮香醇销量增长率与云烟销量增长率之差")+
  theme_bw() +                         
  theme(legend.position = "bottom")+
  facet_wrap(~省份城市)+
  ylab("增长率之差-百分比（%）")+
  xlab("2017年07月-2018年02月")


# 销售增长量与销售增长量的关系
sale_ana_data_temp<-subset(sale_ana_data,全国A小包数>0,select=c("省份城市","时间序列","增长极差"))
names(sale_ana_data_temp)<-c("省份城市","时间序列","五粮香醇销量增长率与云烟销量增长率之差")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==1]<-paste0("07月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==2]<-paste0("08月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==3]<-paste0("09月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==4]<-paste0("10月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==5]<-paste0("11月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==6]<-paste0("12月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==7]<-paste0("18年01月")
sale_ana_data_temp$时间序列[sale_ana_data_temp$时间序列==8]<-paste0("18年02月")
sale_ana_data_temp<-melt(sale_ana_data_temp,id=c("省份城市","时间序列"))
names(sale_ana_data_temp)<-c("省份城市","时间序列","比率类型","值")

ggplot(sale_ana_data,aes(x=时间序列,y=值,group=比率类型,colour=比率类型,fill=比率类型))+
  geom_bar(stat="identity") +
  geom_abline()+
  ggtitle("各城市五粮香醇销量增长率与云烟销量增长率之差")+
  theme_bw() +                         
  theme(legend.position = "bottom")+
  facet_wrap(~省份城市)+
  ylab("增长率之差-百分比（%）")+
  xlab("2017年07月-2018年02月")+
  annotate("rect", xmin="07月", xmax="11月", ymin=-1, ymax=1, alpha=.1,fill="blue")
  scale_y_continuous(limits = c(0,100),breaks = c(seq(0,90,10))，scale_y_continuous(limits = c(0,100),breaks = c(seq(0,90,10))))

# 相关性分析
cor_data<-cor(sale_ana_data)
