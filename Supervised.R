setwd("C:/Users/ivobo/Desktop/stat.proj")

library(readr)
library(magrittr)
library(dplyr)
library(recipes)
library(rsample)
library(workflows)
library(parsnip)
library(yardstick)
library(themis)
library(VIM)
library(tidyr)
library(broom)
library(tidyverse)
library(gridExtra)
library(tune)

#1-FEATURES ENGINEERING 
stroke_data<-read.csv("Supervised/stroke-data.csv") #import dataset
stroke_data$bmi<-as.numeric(stroke_data$bmi) #change type
stroke_data$smoking_status<-replace(stroke_data$smoking_status,
                                    stroke_data$smoking_status=="Unknown",NA) 

#K-NN to imput missing data
set.seed(123)
stroke_data1<-kNN(stroke_data, variable=c("bmi", "smoking_status"), k=5) 
stroke_data1<-stroke_data1[,-c(13,14)] #delete unnecessary col. from K-NN

#levels for each variable
i<-1
for (i in 1:12) {
  if (is.numeric(stroke_data[,i])==FALSE) {
    print(unique(stroke_data[,i]))
    i<-i+1
  }
  else {
    i<-i+1
  }
} 

stroke_data1 %>% glimpse() #5110 obs. 12 variables

stroke_data2<-stroke_data1 %>% #transform characters into factors
              mutate(across(c(gender, ever_married, work_type, Residence_type, 
                              smoking_status, stroke), as.factor)) %>%
              select(-id) %>% #remove id 
              filter(age>17) %>% 
              filter(work_type!="Never_worked") %>% 
              filter(gender!="Other") %>% #remove row where gender is unknown
              filter(bmi<=55) %>% 
              filter(avg_glucose_level<=220) %>% 
              droplevels #drop empty levels

stroke_data2 %>% glimpse()

#2-EXPLORATIVE DATA ANALYSIS
p <- ggplot(stroke_data2, aes(x = stroke)) +
  geom_bar(aes(fill = stroke)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p1 <- ggplot(stroke_data2, aes(x = gender)) +
  geom_bar(aes(fill = stroke)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p2 <- ggplot(stroke_data2, aes(x = hypertension)) +
  geom_bar(aes(fill = stroke)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p3 <- ggplot(stroke_data2, aes(x = heart_disease)) +
  geom_bar(aes(fill = stroke)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p4 <- ggplot(stroke_data2, aes(x = ever_married)) +
  geom_bar(aes(fill = stroke)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p5 <- ggplot(stroke_data2, aes(x = work_type)) +
  geom_bar(aes(fill = stroke)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p6 <- ggplot(stroke_data2, aes(x = Residence_type)) +
  geom_bar(aes(fill = stroke)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p7 <- ggplot(stroke_data2, aes(x = smoking_status)) +
  geom_bar(aes(fill = stroke)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#categorical variables
grid.arrange(p, p1, p2, p3, p4, p5, p6, p7, ncol=3)

p8 <- ggplot(data = stroke_data2, aes(age, color = stroke))+
  geom_freqpoly(binwidth = 5, size = 1)

p9 <- ggplot(data = stroke_data2, aes(avg_glucose_level, color = stroke))+
  geom_freqpoly(binwidth = 5, size = 1)

p10 <- ggplot(data = stroke_data2, aes(bmi, color = stroke))+
  geom_freqpoly(binwidth = 5, size = 1)

#numerical variables
grid.arrange(grobs=list(p8,p9,p10), col=1) 

p11<- ggplot(stroke_data2, aes(x=bmi))+
  geom_boxplot()

p12<- ggplot(stroke_data2, aes(x=avg_glucose_level))+
  geom_boxplot()

p13<- ggplot(stroke_data2, aes(x=age))+
  geom_boxplot()

#box plots
grid.arrange(p13, p12, p11, nrow=3) 

p14<-ggplot(stroke_data2, aes(x=age))+geom_histogram(aes(y=..density..),
                                      binwidth=5) + geom_density(col="red")

p15<-ggplot(stroke_data2, aes(x=bmi))+
  geom_histogram(aes(y=..density..))+
  geom_density(col="red")

p16<-ggplot(stroke_data2, aes(x=avg_glucose_level))+
  geom_histogram(aes(y=..density..)) +
  geom_density(col="red")

grid.arrange(p14, p15, p16, nrow=1) #probability densities

#partial correlation
pc<-cor(stroke_data2[,c(2,8,9)]) 
pc

#3-DATA SPLITTING
set.seed(123) #for replicability
# Put 3/4 of the data into the training set
data_split <- initial_split(stroke_data2, prop = 0.75)  
data_split

train_data <- training(data_split)

test_data <- testing(data_split)

#4-LOGISTIC MODEL WITH SMOTE
#Recipe's steps (except for SMOTE) are applied also on the test set 
#automatically before fitting the model. In order to standardize, 
#the mean and the s.d. of the training are used to avoid to use information 
#from the test set when training the algorithm.

stroke_rec <- recipe(stroke ~., data = train_data) %>% 
              step_dummy(all_nominal_predictors()) %>% #transform into dummies 
              step_zv(all_predictors()) %>% #remove variables with zero var
              step_smote(stroke, seed = sample.int(10^5, 1)) #oversampling 
              
stroke_rec %>% prep() %>% juice() %>% glimpse() #evaluate recipe

stroke_prep<-prep(stroke_rec) #apply the recipe and estimate parameters

#defining the logistic model
logit_mod <- logistic_reg() %>% 
              set_engine("glm")

#defining the workflow
stroke_wflow <- workflow() %>% 
                add_model(logit_mod) %>% 
                add_recipe(stroke_rec)

#train the algorithm
stroke_fit <- stroke_wflow %>% 
              fit(data = train_data)
stroke_fit

stroke_fit %>% extract_fit_parsnip() %>% tidy()

#testing the accuracy
pred<-predict(stroke_fit, test_data) #predicted class
pred_class<-pred %>% pull(,1)
table(pred_class, actual=test_data$stroke) #confusion matrix 

stroke_aug <- augment(stroke_fit, test_data)
stroke_aug %>% select(stroke, .pred_class, .pred_0, .pred_1) 

#roc curve
stroke_aug %>% 
  roc_curve(truth = stroke, .pred_0) %>% 
  autoplot()

stroke_aug %>% 
  roc_auc(truth = stroke, .pred_0) 

#5-LOGISTIC WITH NO SMOTE
stroke_norecipe_fit <- workflow() %>% 
  add_formula(stroke ~ .) %>% 
  add_model(logit_mod) %>% 
  fit(data = train_data)

pred_norecipe<-predict(stroke_norecipe_fit, test_data) #predicted class
pred_norecipe_class<-pred_norecipe %>% pull(,1)
table(pred_norecipe_class, actual=test_data$stroke) 

stroke_norecipe_aug <- augment(stroke_norecipe_fit, test_data)

stroke_norecipe_aug %>% 
  roc_curve(truth = stroke, .pred_0) %>% 
  autoplot()

stroke_norecipe_aug %>% 
  roc_auc(truth = stroke, .pred_0) 

#6-LOGISTIC WITH LASSO
set.seed(234)
val_set <- validation_split(train_data, 
                            strata = stroke, #same prop. of strokes in both sets
                            prop = 0.80)    #only the training is used for tuning

lr_recipe <- recipe(stroke ~., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>% #transform into dummies 
  step_normalize(all_numeric_predictors()) %>% #centering and scaling 
  step_zv(all_predictors()) %>% #remove variables with zero var
  step_smote(stroke, seed = sample.int(10^5, 1)) #oversampling  

#set the engine, mixture=1 stands for LASSO
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)

lr_reg_grid <- tibble(penalty = 10^seq(-3.5, -0.6, length.out = 30))

lr_reg_grid %>% top_n(-5) #lowest parameters

lr_reg_grid %>% top_n(5) #highest parameters

lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot #shows the AUC for each of the 30 values assigned to the penalty

top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 10) %>% 
  arrange(penalty)

top_models #pick the penalties leading to the best AUC.

lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(17) #best has a penalty of 0.0126

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(stroke, .pred_0) %>% 
  mutate(model = "Logistic Regression")

#ROC
autoplot(lr_auc) 

stroke_prep<-prep(lr_recipe) #apply the recipe and estimate parameters 

lr_final<-logistic_reg(penalty=0.0126, mixture=1) %>% 
  set_engine("glmnet") 

lr_wflow <- workflow() %>% 
  add_model(lr_final) %>% 
  add_recipe(lr_recipe)

lr_fit <- lr_wflow %>% 
  fit(data = train_data)

lr_fit

lr_fit %>% extract_fit_parsnip() %>% tidy()
