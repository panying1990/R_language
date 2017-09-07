
#author:panying
#本代码块用于解决刷单用户及类似黄牛党用户的筛选
#最后编辑时间：2017年07月28日
#加载数据样式模块
------------------------------------------------------------------------------
#设置工作路径及工作环境
workdir<-getwd()
if(!is.null(workdir))
  setwd(workdir)
# base environment
library(gridExtra)
library(shiny)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(boot)
mytheme1<-theme(plot.title = element_text(face="italic",size = "12",color = "black"),legend.title=element_text(face="bold",size = "8",color = "black"),panel.background=element_rect(fill="white",color = "darkblue"),legend.position = "bottom",axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
mytheme2<-theme(plot.title = element_text(face="bold",size = "14",color = "black"),plot.subtitle= element_text(face="italic",size = "12",color = "black"),legend.title=element_text(face="bold",size = "8",color = "black"),panel.background=element_rect(fill="white",color = "darkblue"),legend.position = "left")
font_title<-list( fontFamily='sans-serif', fontSize=14, fontWeight='normal', fontStyle='normal', color='black')
font_lengend<-list( fontFamily='sans-serif', fontSize=8, fontWeight='normal', fontStyle='normal', color='black')
font_label<-list( fontFamily='sans-serif', fontSize=6, fontWeight='normal', fontStyle='normal', color='black')
yaxis_y2<-list(tickfont=list(color="red"),overlaying="y",side="right",title = "累计百分比(%)")
#x轴时间设置标签
x_day<-seq(1,31,length.out=16)

#导入数据 data
library(DBI)
library(RMySQL)
conn<-dbConnect(MySQL(),dbname="tag_explore",host="192.168.111.251",username="root",password="P#y20bsy17")
dbSendQuery(conn,"SET NAMES gbk")
read_data<-dbSendQuery(conn, "SELECT * from kd_trade_brusher180")
kd_trade_brusher<-dbFetch(read_data,-1)

kd_brusher_temp<-select(kd_trade_brusher,date_month,month_day,order_cunt,buyer_id)
kd_brusher_temp$user_type<-rep('',nrow(kd_brusher_temp))
for(i in 1:nrow(kd_brusher_temp))
{
  if(as.numeric(kd_brusher_temp$order_cunt[i])==1) kd_brusher_temp$user_type[i]<-'new' else kd_brusher_temp$user_type[i]<-'old'
}
#用户交易次数的分布情况：
kd_brusher_cunt<-kd_brusher_temp %>%
  dplyr::group_by(date_month,month_day,user_type) %>%
  dplyr::summarise( usertype_cunt = n())
kd_brusher_cunt<-ddply(kd_brusher_cunt,c("date_month","month_day"),transform,usertype_rat=round(100*usertype_cunt/sum(usertype_cunt),1))

kd_brusher_old<-filter(kd_brusher_cunt,user_type=="old",usertype_rat<60)
jd_meanoldrat<-round(100*sum(jd_usertype_old$usertype_cunt)/sum(jd_usertype_cunt$usertype_cunt),1)
#用户复购率-数据可视化
ggplot(jd_usertype_old,aes(x=month_day,y=usertype_rat,group=date_month))+
  geom_point(colour="red",size=1)+
  geom_line(colour="greenyellow",size=1)+
  geom_hline(yintercept = jd_meanoldrat,color = "red4",size=0.4,linetype=2)+
  annotate("text",x=20,y=jd_meanoldrat+2,label=paste("京东店铺每日复购率:",jd_meanoldrat,"%"),size=2.6)+
  facet_wrap(~date_month)+
  scale_x_continuous(breaks = x_day,label = x_day)+
  labs(title="京东店铺每日复购率的月分布情况",subtitle="---根据月份进行分类",x="用户订货日期(日)",y="京东店铺每日复购率（%）")+
  mytheme2

#1.1同一买家ID号短时间多次购买行为

#将-6.18-6.19期间的用户订单数据进行移除，从而降低刷单用户的可能性，对全部用户的判定方式做归集，最后给予用户刷单权重
kd_trade_brusher0<-filter(kd_trade_brusher,paste0(kd_trade_brusher$date_month,'-',kd_trade_brusher$month_day)!='6月-18'|paste0(kd_trade_brusher$date_month,'-',kd_trade_brusher$month_day)!='6月-18')
#判定个订单为疑似刷单
i<-1
kd_trade_brusher0$brusher_tag<-rep('',nrow(kd_trade_brusher0))
for(i in 1:nrow(kd_trade_brusher0))
{
  if((as.numeric(kd_trade_brusher0$order_payamout[i])>137)&(as.numeric(kd_trade_brusher0$order_cunt[i])>3)&(as.numeric(kd_trade_brusher0$repurchase[i])<4)&(as.numeric(kd_trade_brusher0$ordr_diff[i]<15))) kd_trade_brusher0$brusher_tag[i]<-'brusher' else kd_trade_brusher0$brusher_tag[i]<-'norm'
}

#将结果数据写入MySQL数据库
#筛选出疑似刷单的订单并进行判定
brusher_suspected<-filter(kd_trade_brusher0,brusher_tag=='brusher')
brusher_suspected$order_fee<-round(brusher_suspected$order_payamout,0)

y_price<-seq(0,round(max(brusher_suspected$order_payamout/100,0))*100,length.out=6)
#同一买家ID号短时间多次购买行为-数据可视化
ggplot(brusher_suspected,aes(x=month_day,y=order_payamout,group=date_month))+
    geom_point(colour="red",size=1)+
    facet_wrap(~date_month)+
    scale_x_continuous(breaks = x_day,label = x_day)+
    scale_y_continuous(breaks = y_price,label = y_price)+
    labs(title="假设1：短时同ID多次疑似刷单订单分布情况",subtitle="---根据月份进行分类",x="订单下单日期(日)",y="疑似刷单订单价格")+
    mytheme2
#将数据写入到目标数据库方便之后做进一步整合
dbListTables(conn)  
dbWriteTable(conn,"brusher_tag01",brusher_suspected)  
dbListTables(conn)

#2.相同购买金额且有多次购买ID号的嫌疑客户
#数据读取
library(DBI)
library(RMySQL)
dbSendQuery(conn,"SET NAMES gbk")
read_data<-dbSendQuery(conn, "SELECT * FROM kd_trade_brushtime180")
kd_trade_brushtime<-dbFetch(read_data,-1)

#控制流循环判定,当数据
#判定疑似订单循环
kd_trade_brushtime<-dplyr::arrange(kd_trade_brushtime, desc(rownum))
tag_num<-4
kd_trade_brushtime$tag_tip[1:nrow(kd_trade_brushtime)]<-rep('',nrow(kd_trade_brushtime))
i<-tag_num
j<-1
brusher_tag<-""
for(i in tag_num:as.numeric(nrow(kd_trade_brushtime)-tag_num))
{
  for(j in 1:tag_num)
    {
    if(kd_trade_brushtime$order_payamout[as.numeric(i-round(tag_num/2,0)+j)]==kd_trade_brushtime$order_payamout[as.numeric(i)]) brusher_tag[j]<-1 else brusher_tag[j]<-0 
    }
  if(sum(as.numeric(brusher_tag))==tag_num&as.numeric(kd_trade_brushtime$order_payamout[i])>137&as.numeric(kd_trade_brushtime$order_payamout[i])<1125) kd_trade_brushtime$tag_tip[i]<-'brusher' else kd_trade_brushtime$tag_tip[i]<-'norm'
}
  
#筛选出疑似刷单的订单并进行判定
kd_brushtime_suspected<-filter(kd_trade_brushtime,tag_tip=='brusher')
kd_brushtime_suspected$order_payamout<-round(kd_brushtime_suspected$order_payamout,0)

y_timeprice<-seq(0,round(max(kd_brushtime_suspected$order_payamout/100,0))*100,length.out=6)
#短时连续价格刷单数据-数据可视化
ggplot(kd_brushtime_suspected,aes(x=month_day,y=order_payamout,group=date_month))+
  geom_point(colour="red",size=1)+
  facet_wrap(~date_month)+
  scale_x_continuous(breaks = x_day,label = x_day)+
  scale_y_continuous(breaks = y_timeprice,label = y_price)+
  labs(title="假设2：疑似刷单订单分布情况",subtitle="---根据月份进行分类",x="订单下单日期(日)",y="疑似刷单订单价格")+
  mytheme2

#结果数据的描述性统计
jd_time_suspected07<-filter(jd_time,tag_tip=='brusher',date_month=="7月")
sum(jd_time_suspected07$order_fee)
write.table(jd_time_suspected07, file ="jd_time_suspected07.csv",sep =",",row.names = FALSE)

#将数据写入到目标数据库方便之后做进一步整合
dbListTables(conn)  
dbWriteTable(conn,"brusher_tag02",kd_brushtime_suspected)  
dbListTables(conn)


#3.相似地址的买家
#input data
library(DBI)
library(RMySQL)
conn<-dbConnect(MySQL(),dbname="tag_explore",host="192.168.111.251",username="root",password="P#y20bsy17")
dbSendQuery(conn,"SET NAMES gbk")
read_data<-dbSendQuery(conn, "SELECT * FROM kd_trade_brushadd180")
kd_trade_brushadd<-dbFetch(read_data,-1)

#初步数据清洗
#去除文本中的数字
kd_trade_brushadd$order_add=gsub(pattern = "[a-zA-Z]+","",kd_trade_brushadd$order_add)
#去除文本中的字母
kd_trade_brushadd$order_add=gsub(pattern = "[0-9 0 1 2 3 4 5 6 7 8 9]+","",kd_trade_brushadd$order_add)
#去除文本中的特殊号
kd_trade_brushadd$order_add=gsub(pattern = "[.]","",kd_trade_brushadd$order_add)
kd_trade_brushadd$order_add=gsub(pattern = "[# @ ! % ^ & * -]","",kd_trade_brushadd$order_add)
kd_trade_brushadd$order_add=gsub(pattern = "[, / ? \ |]","",kd_trade_brushadd$order_add)
kd_trade_brushadd$order_add=gsub(pattern = "[， 。 （ ） ( ) 一]","",kd_trade_brushadd$order_add)
kd_trade_brushadd$order_add=gsub(pattern = "[ ]","",kd_trade_brushadd$order_add)
#将全部数据按照订单地址+用户id号进行统计group_by 统计
kd_brushadd_cunt<-kd_trade_brushadd %>%
  group_by(order_add,buyer_id)%>%
  summarise(add_buy_cunt = n())%>%
  arrange(order_add,add_buy_cunt)

kd_brushadd_value<-kd_trade_brushadd%>%
  group_by(order_add)%>%
  summarise(add_buy_cunt = n())%>%
  arrange(order_add,add_buy_cunt)

#将平均每个地址出现的次数匹配上地址+买家ID的出现次数
kd_brushadd_outcome<-dplyr::left_join(kd_brushadd_cunt,kd_brushadd_value,by="order_add")

#判定疑似刷单用户
i<-1
kd_brushadd_outcome$brusher_tag<-rep('',nrow(kd_brushadd_outcome))
for(i in 1:nrow(kd_brushadd_outcome))
{
  if(as.numeric(kd_brushadd_outcome$add_buy_cunt.x[i])==as.numeric(kd_brushadd_outcome$add_buy_cunt.y[i])) kd_brushadd_outcome$brusher_tag[i]<-'norm' else kd_brushadd_outcome$brusher_tag[i]<-'brusher'
}

#获得疑似刷单用户全部信息
add_brushertag<-data.frame(kd_brushadd_outcome$buyer_id[kd_brushadd_outcome$brusher_tag=='brusher'])
add_brushertag$brusher_tag<-rep('brusher',nrow(add_brushertag))
names(add_brushertag)[1]<-'buyer_id'
kd_brushadd_brusher<-dplyr::left_join(kd_trade_brushadd,add_brushertag,by="buyer_id")

#筛出疑似刷单的订单并进行判定
kd_brushadd_suspected<-filter(kd_brushadd_brusher,brusher_tag=='brusher',order_payamout>137)
kd_brushadd_suspected$order_fee<-round(kd_brushadd_suspected$order_payamout,0)

y_addprice<-seq(0,round(max(kd_brushadd_suspected$order_fee/100,0))*100,length.out=6)
#同一买家ID号短时间多次购买行为-数据可视化
ggplot(kd_brushadd_suspected,aes(x=month_day,y=order_payamout,group=date_month))+
  geom_point(colour="red",size=1)+
  facet_wrap(~date_month)+
  scale_x_continuous(breaks = x_day,label = x_day)+
  scale_y_continuous(breaks = y_addprice,label = y_price)+
  labs(title="假设3：相同地址多ID疑似刷单订单分布情况",subtitle="---根据月份进行分类",x="订单下单日期(日)",y="疑似刷单订单价格")+
  mytheme2


#将数据写入到目标数据库方便之后做进一步整合
dbListTables(conn)  
dbWriteTable(conn,"brusher_tag03",kd_brushadd_suspected)  
dbListTables(conn)
