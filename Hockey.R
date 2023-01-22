

#Author: Ender Erimhan
#Title: What variables predict RIT Hockey team winning?

### Load required packages for analysis ###

require(ggplot2)
require(dplyr)
require(corrplot)
require(MASS)
require(class)


### Reading in the  training and test csv files ###

dirdata <- "/Users/endererimhan/Documents/Capstone/"
hockey_raw <- read.table(paste0(dirdata,"HData2.CSV"), header = TRUE, sep = ",")
hockey_raw_test <- read.table(paste0(dirdata,"TestHData2.CSV"), header = TRUE, sep = ",")

### Cleaning up the train data ###

hockey_train <- hockey_raw 

hockey_train <- hockey_train %>% mutate(Location = as.character(Location)) 

hockey_train <- hockey_train %>% 
  mutate(scorediffp1 = p1T - p1G, 
         scorediffp2 = scorediffp1 + p2T  - p2G, 
         scoreddiffFin = scorediffp2 + p3T - p3G,
         ritp1pwr = POW1T - POW1G, 
         ritp2pwr = ritp1pwr + POW2T - POW2G,
         rit3pwr = ritp2pwr + POW3T - POW3G,
         rit_final = p1T + p2T + p3T,
         op_final = p1G + p2G + p3G,
         Location = ifelse(Location == "H", "Home", "Away"),
         outcome = ifelse(scoreddiffFin > 0, "Win", "Loss")
         ) 

hockey_train_full <- hockey_train 

hockey_train <- hockey_train[,-c(1,3:14)]

### Cleaning up the test data ###

hockey_test <- hockey_raw_test 

hockey_test <- hockey_test %>% 
  mutate(Location = as.character(Location),
         scorediffp1 = p1T - p1G, 
         scorediffp2 = p1T + p2T - p1G - p2G, 
         scoreddiffFin = p1T + p2T + p3T - p1G - p2G -p3G,
         ritp1pwr = POW1T - POW1G, 
         ritp2pwr = ritp1pwr + POW2T - POW2G,
         rit_final = p1T + p2T + p3T,
         op_final = p1G + p2G + p3G,
         outcome = ifelse(scoreddiffFin > 0, "Win", "Loss"),
         Location = ifelse(Location == "H", "Home", "Away"))

hockey_test_full <- hockey_test 
hockey_test <- hockey_test[,-c(1,3:12)]

### Data Exploration ###

#Can score difference predict game outcome?
ggplot(hockey_train, aes(x = scorediffp1, y = scorediffp2, color = outcome)) + 
    geom_jitter(size = 3, width = 0.1 , height = 0.2) + 
    xlab("Score Difference at Period 1") + 
    ylab("Score Difference at Period 2") + 
    ggtitle("Game Outcome")

win_location_counts <- hockey_train %>% 
  group_by(Location, outcome) %>% 
  summarise(count = n())

#Can game location predict game outcome?
ggplot(win_location_counts, aes(Location, count, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Outcome of Game by Location")

#How good is the rit mens hockey team?
scoreddiffFin.counts <- as.data.frame(table(hockey_train$scoreddiffFin))
ggplot(data = scoreddiffFin.counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = Freq), vjust=1.6, color = "white", size = 3.5)+
  theme_minimal() +xlab("Difference") + ylab("Frequency") +
  ggtitle("Final Score Difference")

#Can powerplay difference predict game outcome?
ggplot(data=hockey_train, aes(x = ritp1pwr, group = outcome, fill = outcome)) +
  geom_density(adjust = 1.3, alpha = 0.4)

ggplot(data=hockey_train, aes(x=ritp2pwr, group=outcome, fill = outcome)) +
  geom_density(adjust = 1.3, alpha = 0.4)

#Prepare dataset for correlation analysis and machine learning

hockey_final <- hockey_train %>% 
  mutate(Location = ifelse(Location == "Home", 1, 0) , 
                        outcome = ifelse(outcome == "Win", 1 ,0))

#What variables are correlated?
corrplot(cor(hockey_final[ ,c(1:3,5:6,10)]), method = "number" , type = "lower")

### Statistical Machine Learning ### 

#Training a logistic regression model 
glm.fit.all <- glm(outcome ~ scorediffp1 + scorediffp2 + Location, data = hockey_final, family = binomial)
step.model <- glm.fit.all %>% stepAIC(trace = FALSE)
step.model

summary(glm(outcome ~ scorediffp2, data = hockey_final, family = binomial))

ggplot(hockey_final, aes(x = scorediffp2, y = outcome)) + geom_point() + 
  stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE) +
  ylab("Probability of Win") + xlab("Period 2 difference")

set.seed(123)
fold <- sample(rep(1:6, length = 114)) 
cutoff <- seq(0, 1, length.out = 51) 
glm.accur <- matrix(0, 6, 51)

#6-fold cross validation for probability cutoff
for (i in 1:6) {
  glm <- glm(outcome ~ scorediffp2, data = hockey_final[fold!=i,], family = binomial)
  test <- hockey_final[fold == i, 3]
  test <- as.data.frame(test)
  colnames(test) <- c("scorediffp2")
  glm.probs <- predict(glm ,test , type = 'response' ) 
  for (j in 1:51) {
    glm.pred <- ifelse(glm.probs > cutoff[j] ,1 ,0)
    glm.accur[i,j] <- 1 - mean(glm.pred == hockey_final[fold == i,10]) 
    }
}

error.log <- rep(0, 51) 

for (k in 1:51) {
  error.log[k] <- mean(glm.accur[ , k]) 
  }

min.log <- round(min(error.log), 3)
title.log <- paste0("Minimum error: " , min.log)
df.log <- data.frame(cutoff, error.log) 

ggplot(data = df.log, aes(x = cutoff, y = error.log, group = 1)) +
  geom_line(color = "red") + xlab("Cut-off Probabilty") + ylab("Mean Error rate") +
  ggtitle("6-fold cross validation: Logistic regression" , title.log) #Choose 0.5 as optimal probability cutoff

### K-Nearest Neighbors

#Training KNN model

set.seed(105)
fold <- sample(rep(1:6, length = 114)) 
K_choice <- seq(1, 95, length.out = 95) 
knn_accur <- matrix(0, 6, 95)

for (i in 1:6) {
  train2 <- data.frame(hockey_final[fold != i , c(1,2,3)]) 
  test2 <- data.frame(hockey_final[fold == i , c(1,2,3)]) 
  test2real <- data.frame(hockey_final[fold == i, 10]) 
  train2real <- as.vector(hockey_final[fold != i, 10])
  for (j in 1:95) {
    knn_model <- knn(train = train2, test = test2, cl = train2real, k = K_choice[j])
    knn_accur[i,j] <- 1 - mean(as.numeric(levels(knn_model)[as.integer(knn_model)]) == test2real)
  }
}

error_knn = rep(0, 95) 
for (i in 1:95) {
  error_knn[i] = mean(knn_accur[, i]) 
  }

min_knn <- round(min(error_knn), 3)
title_knn <- paste0("Minimum error: ", min_knn)
df_knn <- data.frame(K_choice, error_knn) 

ggplot(data=df_knn, aes(x = K_choice, y = error_knn, group = 1)) +
  geom_line(color = "red") + 
  xlab("Choice of K") + ylab("Mean Error rate") + 
  ggtitle("6-fold cross validation: KNN", title_knn)


#Zooming in for optimal K; between 1 and 40 
set.seed(105)
fold <- sample(rep(1:6,length = 114)) 
K_choice <- seq(1 , 40 , length.out = 40) 
knn_accur <- matrix(0 , 6 , 40)

for (i in 1:6) {
  train2 <- data.frame(hockey_final[fold != i, c(1,2,3)]) 
  test2 <- data.frame(hockey_final[fold == i, c(1,2,3)]) 
  test2real <- data.frame(hockey_final[fold == i, 10]) 
  train2real <- as.vector(hockey_final[fold != i, 10])
  for (j in 1:40) {
    knn_model <- knn(train = train2, test = test2, cl = train2real, k = K_choice[j])
    knn_accur[i,j] <- 1 - mean(as.numeric(levels(knn_model)[as.integer(knn_model)]) == test2real)
  }
}

error_knn <- rep(0, 40) 
for (i in 1:40) {
  error_knn[i] = mean(knn_accur[, i]) 
}

sd_knn <- rep(0,40) 
for (i in 1:40) {
  sd_knn[i] = sd(knn_accur[, i]) 
  }

min_knn <- round(min(error_knn), 3)
title_knn <- paste0("Minimum error: ", min_knn)
df_knn <- data.frame(K_choice, error_knn) 

ggplot(data = df_knn, aes(x = K_choice, y = error_knn, group = 1)) +
  geom_line(color = "red") + 
  xlab("Choice of K") + ylab("Mean Error rate") + 
  ggtitle("6-fold cross validation: KNN", title_knn)

df_knn2 <- data.frame(K_choice, sd_knn)

ggplot(data = df_knn2, aes(x = K_choice, y = sd_knn, group = 1)) + 
  geom_line(color = "red") +
  xlab("Choice of K") + ylab("Standard deviation of Error rate") +
  ggtitle("6-fold cross validation: KNN", title_knn) #Choose K=10

### Discriminant Analysis ###

set.seed(121)
fold <- sample(rep(1:6, length = 114)) 
Priors <- seq(0.01, 0.99, length.out = 99) 
lda_accur <- matrix(0, 6, 99)

for (i in 1:6) {
  test <- data.frame(hockey_final[fold == i, ]) 
  train <- data.frame(hockey_final[fold != i, ]) 
  for (j in 1:99) {
    prior1 <- Priors[j]
    lda_model <- lda(outcome ~ scorediffp1 + scorediffp2, data = test, prior = c(prior1, 1 - prior1))
    predictions <- lda_model %>% predict(test)
    lda_accur[i,j] <- 1 - mean(predictions$class == test$outcome) 
    }
}

error_lda <- rep(0, 99) 
for (i in 1:99) {
  error_lda[i] = mean(lda_accur[, i])
}

min_lda <- round(min(error_lda), 3)
title_lda <- paste0("Minimum error: ", min_lda)

df_lda <- data.frame(Priors , error_lda) 

ggplot(data = df_lda, aes(x = Priors, y = error_lda, group = 1)) +
  geom_line(color = "red") + 
  xlab("Choice of Prior") + ylab("Mean Error rate") +
  ggtitle("6-fold cross validation: LDA", title_lda) #Choose 0.57 as optimal probability cut-off

#QDA 

set.seed(121)
fold <- sample(rep(1:6,length = 114)) 
Priors <- seq(0.01, 0.99, length.out = 99) 
qda_accur <- matrix(0, 6, 99)

for (i in 1:6) {
  test <- data.frame(hockey_final[fold == i, ]) 
  train <- data.frame(hockey_final[fold != i, ])
  for (j in 1:99) {
    prior1 <- Priors[j]
    qda_model <- qda(outcome ~ scorediffp1 + scorediffp2, data = test, prior = c(prior1 , 1 - prior1))
    predictions <- qda_model %>% predict(test)
    qda_accur[i,j] <- 1 - mean(predictions$class == test$outcome)
    } 
  }


error_qda <- rep( 0 , 99) 
for (i in 1:99) {
  error_qda[i] = mean(qda_accur[, i]) }

min_qda <- round(min(error_qda),3)
title_qda <- paste0("Minimum error: ", min_qda)
df_qda <- data.frame(Priors, error_qda) 

ggplot(data = df_qda, aes(x = Priors, y = error_qda, group = 1)) +
  geom_line(color = "red") + 
  xlab("Choice of Prior") + ylab("Mean Error rate") +
  ggtitle("6-fold cross validation: QDA" , title_qda) #Choose 0.67 as optimal probabilty cut-off


### Comparing Models using ROC Analysis ### 

hockey_test2 <- hockey_test %>% 
  mutate(Location = ifelse(Location == "Home" , 1, 0) , 
         outcome = ifelse(outcome == "Win" , 1 ,0))

glm_final <- glm(outcome ~ scorediffp2, data = hockey_final, family = binomial) 
test <- data.frame(hockey_test2[ ,3])
test <- as.data.frame(test)
colnames(test) <- c("scorediffp2")
glm_final_probs <- predict(glm_final, test, type = 'response') 
glm_final_pred <- ifelse(glm_final_probs > 0.5, 1, 0)
glm_table <- table(predicted = as.factor(glm_final_pred), actual = as.factor(hockey_test2$outcome))


set.seed(100)
train_knn <- hockey_final[c(1, 2, 3)]
test_knn <- hockey_test2[c(1, 2, 3)]
cl_knn <- as.factor(hockey_final$outcome)
knn_final <- knn(train = train_knn, test = test_knn, cl = cl_knn, k = 10)
knn_table <- table(predicted = as.factor(knn_final), actual = as.factor(hockey_test2$outcome) )


lda_final <- lda(outcome ~ scorediffp1 + scorediffp2, data = hockey_final, prior = c(0.57, 0.43))
predictions_lda <- lda_final %>% predict(hockey_test2)
lda_table <- table(predicted = predictions_lda$class, actual = hockey_test2$outcome )
#error_lda = 1 - mean(predictions.lda$class == testdata$outcome) 


qda_final <- qda(outcome ~ scorediffp1 + scorediffp2, data = hockey_final , prior = c(0.67 ,0.33))
predictions_qda <- qda_final %>% predict(hockey_test2)
qda_table <- table(predicted = predictions_qda$class, actual = hockey_test2$outcome)

log_sen <- glm_table[2,2] / (glm_table[2,2] + glm_table[1,2])
log_fp <- glm_table[2,1] / (glm_table[2,1] + glm_table[1,1]) 
log_act <- (glm_table[1,1] + glm_table[2,2]) / sum(glm_table)

knn_sen <- knn_table[2,2] / (knn_table[2,2] + knn_table[1,2]) 
knn_fp <- knn_table[2,1] / (knn_table[2,1] + knn_table[1,1]) 
knn_act <- (knn_table[1,1] + knn_table[2,2]) / sum(knn_table) 

lda_sen <- lda_table[2,2] / (lda_table[2,2] + lda_table[1,2]) 
lda_fp <- lda_table[2,1] / (lda_table[2,1] + lda_table[1,1]) 
lda_act <- (lda_table[1,1] + lda_table[2,2]) / sum(lda_table) 

qda_sen <- qda_table[2,2] / (qda_table[2,2] + qda_table[1,2]) 
qda_fp <- qda_table[2,1] / (qda_table[2,1] + qda_table[1,1]) 
qda_act <- (qda_table[1,1] + qda_table[2,2]) / sum(qda_table) 

Methods <- c("Logistic", "KNN" , "LDA" , "QDA")
Methods_roc <- c("Logistic/LDA" , "KNN" , "QDA")
Accuracy <- c(log_act, knn_act, lda_act, qda_act) 
Sensitivity <- c(log_sen , knn_sen , lda_sen , qda_sen) 
Specificity = c(1-log_fp , 1-knn_fp , 1-lda_fp , 1-qda_fp)
compare_roc <- data.frame(Methods_roc, Sensitivity[c(1,2,4)], 1-Specificity[c(1,2,4)])
colnames(compare_roc) <- c("Methods" , "tp_rate" , "fp_rate")

comparisions <- data.frame(Methods, Accuracy, Sensitivity, Specificity) 
comparisions

ggplot(compare_roc, aes(x = fp_rate, y = tp_rate , col = Methods)) + 
geom_point() + xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1) + 
ggtitle("ROC graph") +xlab("1 - Specificity") + ylab("Sensitivity")

#Choose QDA for best accuracy. 

#Choose Logistic Regression for best interpretability. 
