################### HR attrition case study################

sessionInfo()  
## for getting the working directory(where data is storing)
getwd()
## for setting the working directory (where data will store)
setwd("F:/r")

## forcing r not to use exponential term 
options(scipen = 999)


### packages required 
install.packages("sqldf", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("caTools", dependencies = TRUE)
install.packages("rpart", dependencies = TRUE)
install.packages("rpart.plot", dependencies = TRUE)
install.packages("ROCR", dependencies = TRUE)
install.packages("pROC", dependencies = TRUE)
install.packages("magrittr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)

# loading required packages
library(sqldf)
library(ggplot2)
library(caTools)   
library(rpart)
library(rpart.plot)
library(ROCR)
library(pROC)
library(magrittr)
library(dplyr)

## reading the comma separated file and forming the dataset
DataSet=read.csv("HR_comma_sep.csv")
datasetoriginal<-read.csv("HR_comma_sep.csv")
## changing the  incorrect column name from sales to department 
colnames(DataSet)[9] = "department" 
## display the first six observations 
head(DataSet)
## for getting the info about variables and type of data
str(DataSet)

levels(DataSet$time_spend_company)

# ___ decriptive statistics_______#####################################################################################################

mystats=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,UC1=UC1,LC1=LC1,UC2=UC2,LC2=LC2))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

num_var= sapply(DataSet,is.numeric)
Other_var= !sapply(DataSet,is.numeric)

num_diag_stats<-t(data.frame(apply(DataSet[num_var], 2, FUN=mystats)))
othr_diag_stats<-data.frame(t(apply(DataSet[Other_var], 2, FUN=mystats)))

write.csv(num_diag_stats, file = "numericfile.csv")
write.csv(othr_diag_stats, file = "categoryfile.csv")

View(num_diag_stats)
View(othr_diag_stats)
########################################################################################################################################


## converting required integer variable into factor variable ##########################
DataSet$promotion_last_5years<-as.factor(DataSet$promotion_last_5years)
DataSet$left<-as.factor(DataSet$left)
DataSet$Work_accident<-as.factor(DataSet$Work_accident)
DataSet$time_spend_company<-as.factor(DataSet$time_spend_company)
DataSet$number_project<-as.factor(DataSet$number_project)

str(DataSet)
########################################################################################

### ANOVA Analysis ###########################################################

##function to capture anova and boxplot
  af<-function(a){
  boxplot(a~DataSet$left)
  aov(a~DataSet$left)
}
avona_for_monthyhours<-af(DataSet$average_montly_hours)
avona_for_satisfaction_level<-af(DataSet$satisfaction_level)
avona_for_last_evaluation<-af(DataSet$last_evaluation)
summary(avona_for_monthyhours)
summary(avona_for_satisfaction_level)
summary(avona_for_last_evaluation)

##############################################################################

######### Chi_square test ####################################################

####Cross Table
table_for_number_project<-xtabs(~left+number_project, data = DataSet)
table_for_time_spend_company<-xtabs(~left+time_spend_company,data = DataSet)
table_for_Work_accident<-xtabs(~left+Work_accident,data = DataSet)
table_for_promotion_last_5years<-xtabs(~left+promotion_last_5years,data = DataSet)
table_for_department<-xtabs(~left+department,data = DataSet)
table_for_salary<-xtabs(~left+salary,data = DataSet)

###barplot 
barplot(table_for_number_project,beside = T,legend.text = T,xlab ="number of project")
barplot(table_for_time_spend_company,beside = T,legend.text = T,xlab ="time spend in company")
barplot(table_for_Work_accident,beside = T,legend.text = T,xlab ="work accident")
barplot(table_for_promotion_last_5years,beside = T,legend.text = T,xlab ="promotion last 5 year")
barplot(table_for_department,beside = T,legend.text = T,xlab ="department")
barplot(table_for_salary,beside = T,legend.text = T,xlab ="salary")

## chi-square test
chisq.test(table_for_number_project, correct = T)
chisq.test(table_for_time_spend_company, correct = T)
chisq.test(table_for_Work_accident, correct = T)
chisq.test(table_for_promotion_last_5years, correct = T)
chisq.test(table_for_department, correct = T)
chisq.test(table_for_salary, correct = T)


##############################################################################

## for getting the rough probability of % of employee left the organisation ###############
round(prop.table(table(DataSet$left))*100)



## new dataset is formed having only those employee who left the organisation #######################
Employee_left=subset(DataSet,DataSet$left==1)

print(mean(Employee_left$satisfaction_level))
print(median(Employee_left$satisfaction_level))

## formimg a function which will plot the graph for varaible 
tr <- function(a){
  ggplot(data = Employee_left, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
    geom_density()
}
tr(Employee_left$satisfaction_level)

print(mean(Employee_left$last_evaluation))
print(median(Employee_left$last_evaluation))

tr(Employee_left$last_evaluation)

ggplot(subset(DataSet,left==1), aes(x = factor('Salary'), fill = factor(salary))) +
  geom_bar(width = 1, position = "fill", color = "black") + theme_bw()+
  labs(title="Salary")


ggplot(subset(DataSet,left==1), aes(time_spend_company))+
  geom_histogram(binwidth=0.5,fill='lightgreen')+
  labs(x="Time spent company", title="Time Spend in company")+
  theme_bw()

ggplot(subset(DataSet,left==1), aes(department))+
  geom_bar(fill='lightgreen',width=0.5)+
  labs(x="Department", title="Department")+
  theme_bw()
ggplot(subset(DataSet,left==1), aes(number_project))+
  geom_bar(fill='lightgreen',width=0.5)+
  labs(x="number_project", title="number_project")+
  theme_bw()


## getting the % of employee in particular department 
round(prop.table(table(DataSet$department))*100)

## % of employee left from particular department 
left_dept=subset(DataSet,DataSet$left==1)
(table(left_dept$department))/(table(DataSet$department))



#SELECTING PREDICTIVE MODEL

# Spliting data into train and test set######################################################################################
# By using the sample.split() we are actually creating a vector with two values TRUE and FALSE. 
# By setting the SplitRatio to 0.7, we are splitting the original dataset to 70% training and 30% testing data.


set.seed(300)

DataSet$sp <- sample.split(DataSet$left, SplitRatio=0.7)
# where DataSet$sp== TRUE means to add only those rows that have value true for sp in the training dataset
train <- subset(DataSet, DataSet$sp==TRUE)
# where DataSet$sp== FALSE means to add only those rows that have value false for sp in the testing dataset
test <- subset(DataSet, DataSet$sp==FALSE)
#############################################################################################################################

# let us first start with logistic regression############################################################
# Train the model using the training sets and check score
model_glm <- glm(left ~ ., data = train, family='binomial')
#Output of Logistic Regression
summary(model_glm)
# Predict Output of test data
predicted_glm <- predict(model_glm, test, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)
# Confusion matrix of Logistic regression
table(test$left, predicted_glm)
# Accuracy of model
mean(predicted_glm==test$left)
# Logistic regression is 90.33% accurate, which is  good  ##################################################


# check summarization for those with 7 projects
summary(DataSet[DataSet$number_project == 7,])

## getting the % of employee in particular department 
round(prop.table(table(DataSet$department))*100)

## % of employee left from particular department 
left_dept=subset(DataSet,DataSet$left==1)
(table(left_dept$department))/(table(DataSet$department))  ## shows the significance of department

table_for_time_spend_company<-xtabs(~left+time_spend_company,data = DataSet)
barplot(table_for_time_spend_company,beside = T,legend.text = T)   ## time spend more than 6 year has no effect  as they dont left the org

# Let us try decision trees################################################################################
model_dt <- rpart(left ~ ., data=train, method="class", minbucket=25)
#Output of decision tree
summary(model_dt)
# View decision tree plot
prp(model_dt)
# Predict Output of test data
predicted_dt <- predict(model_dt, test, type="class")
# Confusion matrix of decision tree
table(test$left, predicted_dt)
# Accuracy of decision tree
mean(predicted_dt==test$left)
# Decision tree has an astonishing accuracy of 97.48%  #####################################################

# Let's plot the ROC curves for all the models  ######################################################################################
# For Logistic regression
predict_glm_ROC <- predict(model_glm, test, type="response")
pred_glm <- prediction(predict_glm_ROC, test$left)
perf_glm <- performance(pred_glm, "tpr", "fpr")

# For Decision tree
predict_dt_ROC <- predict(model_dt, test)
pred_dt <- prediction(predict_dt_ROC[,2], test$left)
perf_dt <- performance(pred_dt, "tpr", "fpr")
# Area under the ROC curves
auc_glm <- performance(pred_glm,"auc")
auc_glm <- round(as.numeric(auc_glm@y.values),3)
auc_dt <- performance(pred_dt,"auc")
auc_dt <- round(as.numeric(auc_dt@y.values),3)


###ploting the roc curve 
roc(test$left,predict_glm_ROC,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="false positive percentage",
    ylab="true positive percentage",col="#377eb8",lwd=3,print.auc=TRUE)

plot.roc(test$left,predict_dt_ROC[,2],percent = TRUE,col="#4daf4a",lwd=3,print.auc=TRUE,add=TRUE,print.auc.y=40)

legend("bottomright", legend = c("logistic regression","decision tree"), col = c("#377eb8","#4daf4a"),lwd = 4)
########################################################################################################################################



# Filter Employees which are still in the Company to get the Employees who are on the risk of leaving
DataSetStayed <- DataSet[DataSet$left == 0,]
DataSetStayed[, c(1,2,4)] <- scale(DataSetStayed[,c(1,2,4)])
DataSetStayed$prediction <- predict(model_glm, DataSetStayed)
DataSetStayed <- DataSetStayed %>% arrange(desc(prediction))


head(DataSetStayed)
nrow(DataSetStayed[DataSetStayed$prediction >= 0.5,])
View(DataSetStayed)

table(DataSet$salary)
table(DataSet$department)
