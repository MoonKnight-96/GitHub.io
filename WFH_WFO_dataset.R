summary(WFH_WFO_dataset)

Data <- as.data.frame(WFH_WFO_dataset)

split <- sample.split(Data, SplitRatio = 0.8)
split

train <- subset(Data, split =="TRUE")
test <- subset(Data, split == "FALSE")

model <- glm(`Target` ~ `Age`+ `Occupation` +`Same_ofiice_home_location`+ `kids`+ `RM_save_money`+ `RM_quality_time`+ `RM_better_sleep`+ `calmer_stressed`	+`RM_professional_growth`+ `RM_lazy` + `RM_productive` + `digital_connect_sufficient` + `RM_better_work_life_balance` + `RM_improved_skillset` + `RM_job_opportunities`, data = train)

summary(model)

predict <- predict(model, test, type = "response")
predict

predict<- ifelse(predict>0.5, 1, 0)
predict

tab= table(test$`Target`, predict)
tab
sum(diag(tab))/sum(tab)

ROCPred <- prediction(predict, test$`Target`)
ROCPer <- performance(ROCPred, measure = "tpr",
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
             