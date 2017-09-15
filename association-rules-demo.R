setwd("G://MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train")

#Loading required libraries
library(arules)
library(arulesViz)


trans = read.transactions("transaction_dept_for_association.csv", 
                          format = "single", 
                          sep = ",", 
                          cols = c("VisitNumber", "DepartmentDescription"),rm.duplicates=FALSE)



rules = apriori(trans, parameter=list(support=0.01, confidence=0.8));

inspect(head(sort(rules, by="lift"),1));


rules2 = apriori(trans, parameter=list(support=0.05, confidence=0.8));

inspect(head(sort(rules2, by="lift"),1));

rules3 = apriori(trans, parameter=list(support=0.05, confidence=0.6));

inspect(head(sort(rules3, by="lift"),1));

rules4 = apriori(trans, parameter=list(support=0.01, confidence=0.7));

inspect(head(sort(rules4, by="lift"),1));

rules5 = apriori(trans, parameter=list(support=0.01, confidence=0.8));

inspect(head(sort(rules5, by="lift"),1));



plot(rules);

head(quality(rules));

plot(rules, measure=c("support","lift"), shading="confidence");

plot(rules, shading="order", control=list(main ="Two-key plot"))


sel = plot(rules, measure=c("support","lift"), shading="confidence", interactive=TRUE);

subrules = rules[quality(rules)$confidence > 0.8];

subrules

subrules2 = head(sort(rules, by="lift"), 20);

plot(subrules2, method="graph");

plot(subrules2, method="graph", control=list(type="items"));


plot(rules, method="grouped");

plot(rules, method="grouped", control=list(k=50));

sel = plot(rules, method="grouped", interactive=TRUE);

plot(subrules2, method="paracoord");

plot(subrules2, method="paracoord", control=list(reorder=TRUE));

oneRule = sample(rules, 1);

inspect(oneRule)


inspect(rules)
