#Setting working directory
setwd( "G:/MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train/01 Ready for model/")


#Import all the datasets created in Preprocessing step

visit_cartsize=read.csv("visit_cartsize.csv")
visit_count_distinct_upc=read.csv("visit_count_distinct_upc.csv")
visit_dept_freq_pivot=read.csv("visit_dept_freq_pivot.csv")
Visit_ctgy_Dept=read.csv("Visit_Dept_ctgy.csv")
visit_association_rules=read.csv("visit_association_cols2.csv")

#Join all datasets

a=merge(visit_cartsize,visit_count_distinct_upc)


b=merge(a,visit_dept_freq_pivot)

c=merge(b,visit_association_rules)

final=merge(c,Visit_ctgy_Dept)

rm(list = c("a","b","c",
            "visit_association_rules","Visit_ctgy_Dept","visit_cartsize",
            "visit_count_distinct_upc","visit_dept_freq_pivot"))

#Replace NAs with 0

final[is.na(final)] <- 0

final$TripType=as.factor(final$TripType)
write.csv(final,"walmart_cleaned_25var.csv")


# Create a train and validation dataset
#For reproducibility, set seed for sampling
set.seed(123)

#sample random row numbers
index_train=sample(1: nrow(final), floor(0.75*nrow(final)))


library(MASS)


lda.fit=lda(TripType~CartSize+Weekday+distinct_upcs+Moderate+Occasional+Often+Asscn_col1+Asscn_col2,
            data=final ,subset =index_train)


lda.fit































## Discard trip type 999

final_exclude_999trip=final[final$TripType!=999,]

write.csv(final_exclude_999trip,"walmart_cleaned_excl_999.csv")
