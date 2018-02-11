# coding = UTF-8
# AUTHOR = PANYING 
# THE LAST EDIT TIME: 2018-02-09


# ANALYSIS ENVIRONMENT 
library(stats)
library(base)
library(dplyr)
library(stringr)
library(ggplot2)


# 员工使用情况
work_file<-"V:\\【需求】新手机系统\\使用情况盘点"
setwd(work_file)
# getwd()
waiqin_employee<-read.csv("员工数据.csv",stringsAsFactors = FALSE)
waiqin_product<-read.csv("商品数据.csv",stringsAsFactors = FALSE)
names(waiqin_employee)
ncol(waiqin_employee)
names(waiqin_employee)[1:ncol(waiqin_employee)]<-c("姓名","登录帐号","性别","手机","部门","部门全路径","上级领导","是否为部门领导","电话","邮箱",          
                                                   "职务","岗位","生日","身份证号","地址","角色","是否认证","创建日期","最近登录时间","客户端版本")


# 全国账号最近一次数据处理(不含测试号)
waiqin_employee$最近登录时间<-as.Date(waiqin_employee$最近登录时间)
waiqin_employee_use<-filter(waiqin_employee,!is.na(waiqin_employee$最近登录时间))
waiqin_employee_use<-filter(waiqin_employee_use,is.na(str_match(waiqin_employee_use$姓名, "365|珠海|停用")))

# 不同职务类型账号登录
temp_use_date<-waiqin_employee_use%>%group_by(最近登录时间,职务,add= TRUE)%>%summarise(n=n())
names(temp_use_date)[1:ncol(temp_use_date)]<-c("recent_login_date","position","login_num")
temp_use_date<-temp_use_date[ ,1:3]
temp_use_date%>%arrange(desc(recent_login_date))
# temp_use_date$cum_login_num<-cumsum(temp_use_date$login_num[1:nrow(temp_use_date)])
# temp_use_date$cum_login_ratio<-round(100*temp_use_date$cum_login_num/sum(temp_use_date$login_num),1)

#相关数据导出
temp_use_date.to_excel("各职位最近一次登录情况.xlsx")
write.csv(temp_use_date,file="各职位最近一次登录情况.csv",quote=F,row.names = F)
# 不同职位登录情况

temp_data<-filter(temp_use_date,position=="业务代表")
ggplot(temp_data,aes(x=recent_login_date,y=login_num, group=position))+
  geom_point(aes(colour = factor(position)),stat = "identity",position = "identity",alpha = 1/2)+
  geom_line(aes(colour = factor(position)))+
  theme_bw()


# 1.连续30天未登录业务员数据

sales_man<-filter(waiqin_employee_use,职务=="业务代表"&最近登录时间<as.Date(43107,origin = "1899-12-30"))



# 账号权限角色分配情况=============
# 选择用户划分的原始数据：姓名、部门、部门全路径、职务、角色
# 将部门全路径进行拆分最小级别，角色同上;
name_department<-select(waiqin_employee,"姓名","职务","部门","部门全路径")
name_role<-select(waiqin_employee,"姓名","职务","部门","角色")
name_role_now<-filter(name_role,is.na(str_match(姓名, "365|珠海|停用")))
name_department_now<-filter(name_department,is.na(str_match(姓名, "365|珠海|停用")))


# 将部门全路径进行拆分作为该人员部门号
department_all<-str_split(name_department_now$部门全路径,"/")
department_dataframe<-data.frame("员工号"=rep(1,642),"姓名"=rep(as.character("不知道"),642),"职务"=rep(as.character("不知道"),642),"区域"=rep(as.character(""),642),"事业部"=rep(as.character(""),642),"地区"=rep(as.character(""),642),stringsAsFactors=FALSE)
for(i in 1:nrow(name_department_now)){
  role_name<-name_department_now$姓名[i]
  role_position<-name_department_now$职务[i]
  role_department<-name_department_now$部门[i]
  for(j in 1:lengths(department_all[i])){
    department_dataframe$员工号[i]<-i
    department_dataframe$姓名[i]<-role_name
    department_dataframe$职务[i]<-role_position
    department_dataframe$部门[i]<-role_department
    department<-department_all[[i]][j]
    if(j==3){department_dataframe$区域[i]<-department}
    else if(j==4){department_dataframe$事业部[i]<-department}
    else if(j==5){department_dataframe$地区[i]<-department}
    else{}
  }
}


# 将角色权限进行拆分。
role_prtmissions_data<-str_split(name_role_now$角色, ",")
temp<-0
role_dataframe<-data.frame("员工号"=rep(1,1181),"姓名"=rep(as.character("不知道"),1181),"职务"=rep(as.character("不知道"),1181),"部门"=rep(as.character("不知道"),1181),"角色"=rep(as.character("不知道"),1181),stringsAsFactors=FALSE)
for(i in 1:nrow(name_role_now)){
  role_name<-name_role_now$姓名[i]
  role_position<-name_role_now$职务[i]
  role_department<-name_role_now$部门[i]
  for(j in 1:lengths(role_prtmissions_data[i])){
    temp<-as.integer(temp+1)
    role_permissions<-role_prtmissions_data[[i]][j]
    print(paste0("",role_name,"：第",role_permissions,"个角色,累计共",temp,"角色+姓名"))
    role_dataframe$员工号[temp]<-i
    role_dataframe$姓名[temp]<-role_name
    role_dataframe$职务[temp]<-role_position
    role_dataframe$部门[temp]<-role_department
    role_dataframe$角色[temp]<-role_permissions
  }
}


# 2018年组织结构
new_organization<-read.csv("2018年组织架构.csv",stringsAsFactors = FALSE)
names(new_organization)[1:nrow(new_organization)]<-c("全国","事业部","地区","姓名")
new_organization$district<-str_r
