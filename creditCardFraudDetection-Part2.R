#Importing the dataset from the respective folder 
credit_card <- read.csv("D:\\bvm sem\\sem5\\AdvanceProgrammingLab\\project\\creditcard.csv")


#Glance at the structure of the dataset 
str(credit_card)

#Convert class to a factor variable

credit_card$Class <- factor(credit_card$Class, levels = c(0, 1))

#Get the summary of the data

summary(credit_card)

#Count the missing values

sum(is.na(credit_card))

# get the distribution of fraud and legit transactions in the dataset
table(credit_card$Class)

# get the percentage of fraud and legit transactions in the dataset
prop.table(table(credit_card$Class))

#Pie chart of credit card transactions

labels <- c("legit", "fraud")
labels <- paste(labels, round (100*prop.table(table(credit_card$Class)), 2)) 
labels <- paste0(labels, "%")

pie(table(credit_card$Class), labels, col = c("orange", "red"), 
	main="Pie chart of credit card transactions")
#32
#36
#No model Predictions

predictions <- rep.int(0, nrow(credit_card))
predictions <- factor (predictions, levels = c(0, 1))

#install.packages ('caret')

library(caret)
confusionMatrix(data = predictions, reference = credit_card$Class)
#46
library(dplyr)

set.seed(1)
credit_card <- credit_card %>% sample_frac(0.1)

table(credit_card$class)



library(ggplot2)
ggplot(data = credit_card, aes(x = V1, y = V2, col = Class)) +
  geom_point () + 
  theme_bw()+  
  scale_color_manual(values = c('dodgerblue2', 'red'))
#60
#Creating training and Test sets for Fraud Detection Model

#63
#install.packages ('caTools') 
library(caTools)

set.seed(123)

data_sample = sample.split(credit_card$Class, SplitRatio=0.80)

train_data = subset(credit_card, data_sample==TRUE)

test_data = subset (credit_card,data_sample==FALSE)

dim(train_data)

dim(test_data)


#78
#Random Over-Sampling (ROS)


table(train_data$Class)

n_legit <- 22750
new_frac_legit <- 0.50
new_n_total <- n_legit/new_frac_legit # = 22750/0.50


#install.packages('ROSE') 
library(ROSE)

oversampling_result <- ovun.sample(Class ~ .,
                                   data=train_data,
                                   method="over",
                                   N= new_n_total,
                                   seed = 2019)
#93

oversampled_credit <- oversampling_result$data
table (oversampled_credit$class)

ggplot(data = oversampled_credit,aes(x=V1, y = V2, col = Class)) +
  geom_point(position=position_jitter (width = 0.2))+
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2', 'red'))

#104
#106 Undersampling

table(train_data$Class)

n_fraud <- 35

new_frac_fraud <- 0.50

new_n_total <- n_fraud/new_frac_fraud # = 35/0.50

#library (ROSE)
undersampling_result <- ovun.sample(Class ~ .,
                                    data = train_data,
                                    method="under",
                                    N = new_n_total,
                                    seed = 2019)

undersampled_credit <- undersampling_result$data

table(undersampled_credit$Class)
#123

#126
ggplot(data = undersampled_credit, aes(x = V1, y = V2, col=Class)) +
  geom_point() +
  theme_bw()+ 
  scale_color_manual(values= c('dodgerblue2', 'red'))
#130
#ROS and RUS

n_new <- nrow(train_data) # = 22785 
fraction_fraud_new <- 0.50

sampling_result <-ovun.sample(Class ~ .,
                              data = train_data,
                              method="both",N=n_new,
                              p=fraction_fraud_new,
                              seed=2019)
#143

sampled_credit <- sampling_result$data

table(sampled_credit$Class)

ggplot(data=sampled_credit, aes(x = V1, y = V2, col=Class))+
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+ 
  scale_color_manual(values = c('dodgerblue2', 'red')) 
#155
#Using SMOTE to balance the dataset

#install.packages ("smotefamily")

library(smotefamily)

table(train_data$Class)

#set the number of fraud and legitimate cases, and the desired percentage of legitimate cases

n0 <- 22750
n1 <- 35
r0<- 0.6

#166

#168
# calculate the value for the dup_size parameter of SMOTE 
ntimes <- ((1-r0)/r0)*((n0/n1)-1)

smote_output = SMOTE(X = train_data[ , -c(1, 31)], 
                     target= train_data$Class,
                     K = 5,
                     dup_size = ntimes)

credit_smote <- smote_output$data

colnames (credit_smote) [30] <- "Class"

prop.table(table(credit_smote$Class))
#181

#183
#class distribution for original dataset 
ggplot (train_data, aes (x = V1, y = V2, color =Class)) +
  geom_point () +
  scale_color_manual (values = c('dodgerblue2', 'red'))

#class distribution for over-sampled dataset using SMOTE

ggplot (credit_smote, aes (x = V1, y = V2, color = Class)) +
  geom_point () + 
  scale_color_manual (values= c('dodgerblue2', 'red'))
#194
#install.packages ('rpart') 
#install.packages ('rpart.plot')
#196

library(rpart)
library(rpart.plot)

#201
CART_model <- rpart(Class ~ . ,credit_smote)

rpart.plot(CART_model, extra = 0, type = 5, tweak =1.2)
#predict fraud
predicted_val <- predict(CART_model, test_data, type='class')
#build confusion matrix
library(caret)
confusionMatrix(predicted_val, test_data$Class)

#215
predicted_val <- predict(CART_model, credit_card[-1], type ="class")
confusionMatrix(predicted_val, credit_card$Class)

#222
CART_model <- rpart(Class ~ . , train_data[,-1])
rpart.plot(CART_model, extra =0, type = 5, tweak = 1.2)

#228
predicted_val <- predict(CART_model, test_data[,-1], type ="class")

library(caret)
confusionMatrix(predicted_val, test_data$Class)

predicted_val <- predict(CART_model, credit_card[,-1], type = 'class')
confusionMatrix(predicted_val, credit_card$Class)
