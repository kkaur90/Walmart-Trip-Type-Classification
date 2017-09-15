#Hypothesis: Trips with return product (scan <0) categorized into a single trip type
summary(train$ScanCount)

##Table with visit count across ScanCount and TripType 
train_returns<-data.frame(table(train[train$ScanCount<0,c(1,5)]))

write.csv(train_returns,'hypothesis_returns.csv')


#Hypothesis: TripTypes have different weekday preferences
weekday_triptype<-data.frame(table(train$TripType,train$Weekday))

write.csv(weekday_triptype, "weekday_triptype.csv")

barplot(weekday_triptype, main="Visit Distribution by Trip and weekdays",
        xlab="weekday",names.arg =colnames(weekday_triptype))


##Table with visit count across ScanCount and TripType 
returns_triptype<-table(train_returns$ScanCount,train_returns$TripType[train_returns$TripType])

barplot(returns_triptype, main="Visit Distribution by Trip and ScanCount",
        xlab="Trip type", col=c("darkblue","green"),names.arg =colnames(returns_triptype))



a=write.csv(hypothesis_upc_max2,"hypothesis_result.csv")

#Hypothesis: Department preferences can differentiate trip types

##Finding frequency of departments
dept_freq<-data.frame(table(train$DepartmentDescription))


dept_freq$bucket<-ifelse(dept_freq$Freq>=quantile(dept_freq$Freq,0.75),"Often",
                         ifelse(dept_freq$Freq<quantile(dept_freq$Freq,0.75) & dept_freq$Freq>quantile(dept_freq$Freq,0.25),"Moderate",
                                "Occasional"))

colnames(dept_freq)<-c("DepartmentDescription","frequency","freq_bucket")


write.csv(dept_freq,"dept_freq_buckets_yash2.csv")

##Add frequency bucket column to train dataset

train_freq_bucket<-merge(train,dept_freq,by =c("DepartmentDescription"))

train_freq_bucket<-train_freq_bucket[,c(3,6,9)]


train_freq_bucket_items= train_freq_bucket%>% 
  group_by(VisitNumber,freq_bucket) %>% 
  summarise(sum(ScanCount))

colnames(train_freq_bucket_items)=c("VisitNumber","freq_bucket","cart_size")


train_freq_bucket_items2=melt(train_freq_bucket_items, id.vars = c("VisitNumber", "freq_bucket"))



visit_dept_dept_pivot=dcast(train_freq_bucket_items2, VisitNumber ~ freq_bucket, value.var = "value")


#Get cart-size for each visit

visit_cartsize=aggregate(train$ScanCount,by=list(train$TripType, train$VisitNumber),sum)

View(visit_cartsize)

colnames(visit_cartsize)<-c('TripType','VisitNumber','CartSize')

write.csv(visit_cartsize,"visit_cartsize.csv")

#Get the number of products bought under each frequency bucket in a visit

write.csv(visit_dept_dept_pivot,"visit_dept_freq_pivot.csv")




#Create a function to count distinct set of records
CountDistinct=function(x){
  length(unique(x))
}
  
# Get the number of unique/different products bought per each visit
visit_unique_pdts=aggregate(train$Upc,by=list(train$TripType, train$VisitNumber),CountDistinct)


write.csv(visit_unique_pdts,"visit_unique_pdts.csv")


write.csv(train,"train_cleaned.csv")

attach(train)

#Hypothesis: product bundles in a transaction differentiate trip types

## Aggregate to visit, department level

train_vud=train[,c(1,2,4,6)]

Train_visit_dept= train_vud%>% 
  group_by(TripType,VisitNumber,DepartmentDescription) %>% 
  summarise(length(Upc))


colnames(Train_visit_dept) = c("TripType","VisitNumber", "DepartmentDesc", "Count")

rm(train_vud)

Train_visit_dept_1=melt(Train_visit_dept, id.vars = c("TripType","VisitNumber", "DepartmentDesc"))

View(Train_visit_dept_1)


Train_visit_dept_final=dcast(Train_visit_dept_1,TripType+VisitNumber ~ DepartmentDesc, value.var = "value")

Train_visit_dept_final[is.na(Train_visit_dept_final)]=0

View(Train_visit_dept_final)
rm(Train_visit_dept_1)
rm(Train_visit_dept)

#Using Association rules (refer to different code), we found that this is the most frequent item-set in the transactions
Train_visit_dept_final$Asscn_col1 = ifelse(Train_visit_dept_final$`COMM BREAD`>0 & 
                                             Train_visit_dept_final$DAIRY >0 & 
                                             Train_visit_dept_final$`GROCERY DRY GOODS`>0, 1, 0)

Train_visit_dept_final$Asscn_col2 = ifelse(Train_visit_dept_final$`DSD GROCERY`>0 & 
                                             Train_visit_dept_final$`GROCERY DRY GOODS` >0 & 
                                             Train_visit_dept_final$PRODUCE>0, 1, 0)

Train_visit_dept_final$Asscn_col3 = ifelse(Train_visit_dept_final$`COMM BREAD`>0 & 
                                             Train_visit_dept_final$`FROZEN FOODS` >0 & 
                                             Train_visit_dept_final$`PRE PACKED DELI`>0, 1, 0)


Train_visit_dept_final$Asscn_col4 = ifelse(Train_visit_dept_final$DAIRY>0 & 
                                             Train_visit_dept_final$`MEAT - FRESH & FROZEN` >0 & 
                                             Train_visit_dept_final$`PRE PACKED DELI`>0, 1, 0)


hypothesis_association=Train_visit_dept_final%>% 
  group_by(TripType) %>% 
  summarise(mean(Asscn_col1),mean(Asscn_col2),mean(Asscn_col3),mean(Asscn_col4))

hypothesis_association$TripType=as.factor(hypothesis_association$TripType)

plot(x=hypothesis_association$TripType,y=hypothesis_association$`mean(Asscn_col2)`)
plot(x=hypothesis_association$TripType,y=hypothesis_association$`mean(Asscn_col1)`)

write.csv(Train_visit_dept_final[,c(2,72,73)], "G:/MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train/01 Ready for model/visit_association_cols.csv")

write.csv(hypothesis_association,"G://MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train/02 Hypothesis tests/hypothesis_association.csv")

#

write.csv(Train_visit_dept_final[,c(2,72,73,74,75)],
          "G:/MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train/01 Ready for model/visit_association_cols2.csv")
