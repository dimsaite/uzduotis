#############################
# 1) DATA MANIPULATION TASK #
#############################

### select random subsample of data set

bank<-read.csv2("bank.csv")
bank_full<-read.csv2("bank-full.csv")

subsample<-bank[sample(nrow(bank),452), ]


### filter desired rows using simple and more complex conditions

library(dplyr)

# For simple filtering I will choose clients who are 25 or under and are students. 
simple_filtering<-bank%>%filter(age<=25 & job=="student")

# For complex filtering I will select 10 columns (age, job, education, balance, housing,
# loan, contact, day, month and duration) and filter them by choosing may 5 or jun 9
# and that client have personal loan.
complex_filtering<-bank%>%select(age,job,education,balance,housing,loan,contact,day,month,duration)%>%
  filter(((month=="may" & day==5) | (month=="jun" & day==9)) & loan=="yes")


### drop unnecessary variables, rename some variables

library(tidyverse)

# I will rename housing and loan variables and drop unnecessary variables by using select().
droped_renamed_bank<-bank%>%rename(housing_loan=housing,
                                   personal_loan=loan)%>%
  select(age,job,marital,education,default,balance,housing_loan,personal_loan)
  

### calculate summarizing statistics (for full sample and by categorical variables as well)

library(vtable)

# summarizing statistics for full sample
st(bank_full)

# summarizing statistics by education
st(bank_full, group = 'education',group.long = TRUE)


### create new variables using simple transformation and custom functions

# Creating new duration variable in minutes
duration_min<-bank$duration/60

# Creating new variable by combining two variables
month_day<-paste(bank$month,' ',as.character(bank$day))

# Inserting columns into data
col_bank<-add_column(bank,duration_min,.after="duration")
col_bank<-add_column(col_bank,month_day,.after="month")


### order data set by several variables

# basic data sorting by average yearly balance (high to low) and marital status (married, divorced, single)
bank[with(bank,order(-balance,marital)), ]

# data sorting using dplyr package by average yearly balance (high to low) and marital status (married, divorced, single)
arrange(bank,desc(balance),marital)

##############################
# 2) DATA VISUALISATION TASK #
##############################

### In order to understand the data please visualize it.
### You are free to select the scope, types of plots, etc.

library(ggplot2)

g<-ggplot(bank_full, aes(x=job)) + 
  geom_bar(aes(fill=factor(housing)),position=position_dodge()) +
  geom_text(aes(label=..count.., group=factor(housing)),stat = "count", vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5) +
  ggtitle("Housing loan")
g  
# From this graph, we can see that customers with blue-collar jobs have the highest number
# of home loans. The majority of retired clients do not have a mortgage. 

gg<- ggplot(bank_full, aes(x=job)) + 
  geom_bar(aes(fill=factor(loan)),position=position_dodge()) +
  geom_text(aes(label=..count.., group=factor(loan)),stat = "count", vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5) + 
  ggtitle("Personal loan")
gg
# This graph shows that a very small proportion of clients in all jobs have a personal loan. 

ggplot(bank,aes(x=month)) + geom_bar() + ggtitle("Last contact month of year")
# As we can see from the graph, May was the month with the highest number of last contact.

ggplot(bank,aes(x=campaign)) + geom_bar() + ggtitle("Number of contacts performed on clients")
# This graph shows that the most frequent contact with clients was one or two times

ggplot(bank, aes(x="", y=age, fill=education)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Clients education")+
  theme_void()
# As we can see from the pie chart, half of all customers have secondary education.

#####################
# 3) MODELLING TASK #
#####################

### Perform a logistic regression to obtain the predicted probability
### that a customer has subscribed for a term deposit.

### Use continuous variables and dummy variables created for categorical columns.
### Not necessarily all variables provided in data sample should be used.

### Evaluate model goodness of fit and predictive ability.
### If needed, data set could be split into training and test sets.

# For logistic regression model I will use 6 variables: age, balance, default, housing, loan and duration

# First of all I split data into training and test sets.
set.seed(105)
sample <- sample(c(TRUE, FALSE), nrow(bank_full), replace=TRUE, prob=c(0.7,0.3))
train <- bank_full[sample, ]
test <- bank_full[!sample, ]  

# Now I will create dummy variables for categorical columns.
train$y<-ifelse(train$y=='yes',1,0)
test$y<-ifelse(test$y=='yes',1,0)

train$housing<-ifelse(train$housing=='yes',1,0)
test$housing<-ifelse(test$housing=='yes',1,0)
train$loan<-ifelse(train$loan=='yes',1,0)
test$loan<-ifelse(test$loan=='yes',1,0)

train$default<-ifelse(train$default=='yes',1,0)
test$default<-ifelse(test$default=='yes',1,0)

# Model
model1<-glm(y~age+balance+default+housing+loan+duration,data=train,family="binomial")
summary(model1)
# As we can see all p-values for all variables except age(0.6957) are less than 0.05, 
# which means that balance, default, housing, loan and duration are statistically significant variables.
# Since age is not statistically significant I will remove it and build new model.

model2<-glm(y~balance+default+housing+loan+duration,data=train,family="binomial")
summary(model2)
# As we can see in the second model, all p-values are less than 0.05

# Model goodness of fit and predictive ability
AIC(model1,model2)
# Comparing the AIC of the models,
# we can see that the second model is better than the first, although the difference is very small.

# Probability that the customer has subscribed to the term deposit in test data set
probability <- model2 %>% predict(test, type = "response")
head(probability)

library(pROC)

plot.roc(test$y, probability,print.auc=TRUE, main="ROC curve")
# We can see that the AUC is 0.840, which is quite high.
# This indicates that our model does a good job of predicting whether or not an client will subscribe a term deposit.
