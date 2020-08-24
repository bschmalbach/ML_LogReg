df <- read.csv("adult_sal.csv")

#randomly split data 70/30
split <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7, 0.3))
df_train <- df[split,]
df_test <- df[!split,]

model <- glm(factor(income) ~ age + factor(education) + factor(marital) + factor(occupation) + factor(relationship) + factor(race) + factor(sex) + capital_gain + capital_loss + hr_per_week + factor(country), 
             data=df_train, family = "binomial")
summary(model)

df_train$pred <- predict(model, df_train, type="response")
df_test$pred <- predict(model, df_test, type="response")

conf <- table(df_test$income, df_test$pred > 0.5)

paste("Accuracy:", (conf[1,1]+conf[2,2])/sum(conf))
paste("Misspecification:",(conf[1,2]+conf[2,1])/sum(conf))

#ROC Curve for test
pROC::roc(factor(df_test$income), df_test$pred,
                 smoothed = TRUE, ci=T,
                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                 print.auc=TRUE, show.thres=TRUE)

