# author:panying1990
# 本代码块采用随机抽样，抽取用于检测数据，并对抽样数据执行区间<5%;
# 最后编辑时间：2017年09月19日
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

fristdrug_check_data$user_price<-round(fristdrug_check_data$user_amount/fristdrug_check_data$order_count,1)
#随机抽样

library(dplyr)
library(stats)
random_prob_meta<-0.0005
samplt_p_value<-data.frame(seril_num=0,order_count=0,user_price=0,first_order_month=0)
for(i in 1:100)
  {
  random_sample<-sample(1:nrow(fristdrug_check_data),nrow(fristdrug_check_data)*random_prob_meta*i,replace = FALSE)
  fristdrug_sample<-fristdrug_check_data[random_sample,]
  #订单数分布
  sample_prop_1<-data.frame(round(prop.table(table(fristdrug_sample$order_count))*100,1))
  reality_prop_1<-data.frame(round(prop.table(table(fristdrug_check_data$order_count))*100,1))
  prop_table_1<-reality_prop_1%>% left_join(sample_prop_1,by='Var1')%>%arrange(Var1)
  prop_table_1$Freq.y[is.na(prop_table_1$Freq.y)]<-0
  prop_data_1<-rbind(data.frame(variable=rep('reality',nrow(prop_table_1)),prop=prop_table_1$Freq.x),data.frame(variable=rep('sample',nrow(prop_table_1)),prop=prop_table_1$Freq.y))
  samplt_temp_1<-t.test(prop~variable,data =prop_data_1 )
#客单价分布
  sample_prop_2<-data.frame(round(prop.table(table(fristdrug_sample$user_price))*100,1))
  reality_prop_2<-data.frame(round(prop.table(table(fristdrug_check_data$user_price))*100,1))
  prop_table_2<-reality_prop_2%>% left_join(sample_prop_2,by='Var1')%>%arrange(Var1)
  prop_table_2$Freq.y[is.na(prop_table_2$Freq.y)]<-0
  prop_data_2<-rbind(data.frame(variable=rep('reality',nrow(prop_table_2)),prop=prop_table_2$Freq.x),data.frame(variable=rep('sample',nrow(prop_table_2)),prop=prop_table_2$Freq.y))
  samplt_temp_2<-t.test(prop~variable,data =prop_data_2 )

 #首次下单时间分布
  sample_prop_3<-data.frame(round(prop.table(table(fristdrug_sample$first_order_month))*100,1))
  reality_prop_3<-data.frame(round(prop.table(table(fristdrug_check_data$first_order_month))*100,1))
  prop_table_3<-reality_prop_3%>% left_join(sample_prop_3,by='Var1')%>%arrange(Var1)
  prop_table_3$Freq.y[is.na(prop_table_3$Freq.y)]<-0
  prop_data_3<-rbind(data.frame(variable=rep('reality',nrow(prop_table_3)),prop=prop_table_3$Freq.x),data.frame(variable=rep('sample',nrow(prop_table_3)),prop=prop_table_3$Freq.y))
  samplt_temp_3<-t.test(prop~variable,data =prop_data_3 )

}


#根据短板理论，分布合理主要依靠额单价合理分布进行确定分布特点，故将变成
library(dplyr)
library(stats)
random_prob_meta<-0.0001
sample_temp<-data.frame(index=1,p_value=0)
for(i in 1:100)
{
  random_sample<-sample(1:nrow(fristdrug_check_data),nrow(fristdrug_check_data)*random_prob_meta*i,replace = FALSE)
  fristdrug_sample<-fristdrug_check_data[random_sample,]
  sample_prop_2<-data.frame(round(prop.table(table(fristdrug_sample$user_price))*100,1))
  reality_prop_2<-data.frame(round(prop.table(table(fristdrug_check_data$user_price))*100,1))
  prop_table_2<-reality_prop_2%>% left_join(sample_prop_2,by='Var1')%>%arrange(Var1)
  prop_table_2$Freq.y[is.na(prop_table_2$Freq.y)]<-0
  prop_data_2<-rbind(data.frame(variable=rep('reality',nrow(prop_table_2)),prop=prop_table_2$Freq.x),data.frame(variable=rep('sample',nrow(prop_table_2)),prop=prop_table_2$Freq.y))
  sample_temp_2<-(t.test(prop~variable,data =prop_data_2 ))
  sample_temp[i,]<-c(i,sample_temp_2$p.value)
}
library(graphics)
plot(sample_temp$index,sample_temp$p_value)


#根据短板理论，分布合理主要依靠额单价合理分布进行确定分布特点，故将变成
library(dplyr)
library(stats)
random_prob_meta<-0.0001
sample_temp<-data.frame(index=1,p_value=0)
for(i in 1:100)
{ i<-20
  random_sample<-sample(1:nrow(fristdrug_check_data),nrow(fristdrug_check_data)*random_prob_meta*i,replace = FALSE)
  fristdrug_sample<-fristdrug_check_data[random_sample,]
  sample_prop_2<-data.frame(round(prop.table(table(fristdrug_sample$user_amount))*100,1))
  reality_prop_2<-data.frame(round(prop.table(table(fristdrug_check_data$user_amount))*100,1))
  prop_table_2<-reality_prop_2%>% left_join(sample_prop_2,by='Var1')%>%arrange(Var1)
  prop_table_2$Freq.y[is.na(prop_table_2$Freq.y)]<-0
  prop_data_2<-rbind(data.frame(variable=rep('reality',nrow(prop_table_2)),prop=prop_table_2$Freq.x),data.frame(variable=rep('sample',nrow(prop_table_2)),prop=prop_table_2$Freq.y))
  sample_temp_2<-t.test(prop~variable,data =prop_data_2)
  sample_temp[i,]<-c(i,sample_temp_2$)
}
library(graphics)
plot(sample_temp$index,sample_temp$p_value)


library(sampling)
for(i in 1:100)
sub_sample=strata(fristdrug_check_data,stratanames=("user_price"),size=rep(1,3),method="systematic")
head(sub_train)

#随机抽样，从1:100(3:300),用户的客单价p_value值区间未8.8-9.0,且不再会有增加，故部分查看均值与标准差偏离总体最小的取值
library(dplyr)
library(stats)
random_prob_meta<-0.0001
sample_temp<-data.frame(index=1,mean_t=round(mean(na.omit(fristdrug_check_data$user_price)),1),std=sd(na.omit(fristdrug_check_data$user_price)),mean_std=round(mean(na.omit(fristdrug_check_data$user_price))/sd(na.omit(fristdrug_check_data$user_price)),0))
for(i in 2:100)
{
  random_sample<-sample(1:nrow(fristdrug_check_data),nrow(fristdrug_check_data)*random_prob_meta*i,replace = FALSE)
  fristdrug_sample<-fristdrug_check_data[random_sample,]
  mean<-round(mean(na.omit(fristdrug_sample$user_price)),0)
  std<-round(sd(na.omit(fristdrug_sample$user_price)),0)
  mean_std<-round(mean/std,0)
  sample_temp[i,]<-c(i,mean,std,mean_std)
}

library(graphics)
plot(sample_temp$index,sample_temp$mean_std)



#获得抽样数据
sample_temp<-200
random_sample<-sample(1:nrow(fristdrug_check_data),sample_temp,replace = FALSE)
fristdrug_sample<-fristdrug_check_data[random_sample,]
  
#将数据写入到目标数据库方便之后做进一步整合
dbListTables(conn2)  
dbWriteTable(conn2,"fristdrug_sample1",fristdrug_sample)  
dbListTables(conn2)
