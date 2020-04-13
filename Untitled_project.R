library(tidyverse)
library(car)
library(MASS)
library(broom)
library(plyr)
library(dplyr)
library(ggplot2)
library(DT)
library(psych)
library(purrr)
library(pscl)
library(ROCR)
dataset <-read.csv("/Users/daisyshi/Downloads/train_2v - train_2v (1).csv",header=TRUE,sep = ",")
hist(dataset$bmi)#this is new added hist chart
################################################################################
# Data processing
################################################################################

#43400 rows and 12 columns in the dataset, 1462 NA in the columns "bmi"
names(dataset)
summary(dataset)
dim(dataset)
str(dataset)


#Removing "other" in gender and "NA" in bmi columns
dataset<-dataset %>%
  filter(gender!="Other")%>%
  filter(!is.na(bmi))


#converting categorical variables to dummy variables
dataset1<- dataset %>%
  mutate(gender=ifelse(gender=="Male",0,1)) %>%
  mutate(ever_married=ifelse(ever_married=="Yes",0,1))%>%
  mutate(Residence_type=ifelse(Residence_type=="Rural",1,0))

age_period1<- dataset1 %>%
  filter(gender!="Other")%>%
  filter(!is.na(bmi))%>%
  mutate(age_period=case_when(age<=12~"children",
                              age<=18 & age > 12~"teenagers",
                              age<= 36 & age > 18~"adult",
                              age>36 & age<69 ~ "middle-aged",
                              age>=69~"older"))  
 

  
age_period1 %>%
  select("age","age_period") %>%
  filter(is.na(age_period))

ggplot(age_period,aes(stroke,ever_married))
+geom_point()+facet_wrap(~age_period)

age_period %>%
group_by(age,heart_disease,smoking_status) %>%
summarise(heart_disease = count(heart_disease)) 



split_stroke <- age_period %>% 
  split(.$stroke) %>% map(describe)

print(split_stroke)

################################################################################
#linear regression
################################################################################
skewness(log.glu)
kurtosis(log.glu)
hist(dataset1$avg_glucose_level)



lm.model<- lm(avg_glucose_level ~ ., data=dataset1)
summary(lm.model)

outlierTest(lm.model)
qqPlot(lm.model, main="QQ Plot")

stepAIC(lm.model)

lm.model1<- lm(formula = avg_glucose_level ~  age + hypertension + 
                 heart_disease + work_type + bmi + smoking_status + 
                 stroke, data = dataset[-c(35659,38150),])

summary(lm.model1)

lm.model2<- lm(formula = avg_glucose_level ~  age + hypertension + 
                 heart_disease + work_type + bmi  + 
                 stroke, data = dataset[-c(35659,38150),])
summary(lm.model2)


anova(lm.model1,lm.model2)

################################################################################
#logistic regression
################################################################################

glm.model<- glm(stroke ~ ., data = dataset[-c(36470,39299),],family=binomial)
summary(glm.model)
stepAIC(glm.model)

glm.model1<- glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level + 
                   smoking_status, data = dataset[-c(36470,39299),],family=binomial)
summary(glm.model1)

#optimized
glm.model2<- glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level, 
                 data = dataset[-c(36470,39299),],family=binomial)
summary(glm.model2)

################################################################################
#plots and visualizations
################################################################################
dataset %>%
  ggplot(aes(avg_glucose_level,work_type,color=work_type))+geom_line(lwd=2)+ coord_flip()+
  labs(title = "Average glucose levels for different types of work")

dataset %>%
  ggplot(aes(age,avg_glucose_level,color=as.factor(gender)))+ geom_point() +
  geom_smooth(method = "lm",se = FALSE,lwd=1,col="blue") +
  labs(title="Average glucose levels for ages")

ggplot(dataset,aes(x=stroke,y=avg_glucose_level,color=gender)) + geom_boxplot() +
  labs(title="Average glucose levels vs. stroke")

ggplot(dataset,aes(x=heart_disease,y=avg_glucose_level,color=work_type)) + geom_boxplot()  +
  labs(title="Average glucose levels vs. heart disease") 

ggplot(dataset,aes(x=hypertension,y=avg_glucose_level,color=gender)) + geom_boxplot()  +
  labs(title="Average glucose levels vs. hypertension") 

ggplot(dataset,aes(x=heart_disease,y=avg_glucose_level,color=ever_married)) + geom_boxplot()  +
  labs(title="Average glucose levels vs. heart disease") 


ggplot(dataset, aes(x=stroke,y=avg_glucose_level,color=stroke)) + geom_point()+ facet_wrap(~work_type)+
  labs(title="Average glucose levels vs. stroke by work bype") 

ggplot(age_period1, aes(x=stroke,y=avg_glucose_level,color=stroke)) + geom_point()+ facet_wrap(~age_period)+
  labs(title="Average glucose levels vs. stroke by age group") 
 


  
