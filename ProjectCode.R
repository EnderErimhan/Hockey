
require(ggplot2)
require(plyr)
require(dplyr)
require(caret)
require(car)
require(Metrics)
require(class)
require(MASS)
require(rmarkdown)
require(corrplot)

dirdata <- "/Users/endererimhan/Documents/Capstone/"
myData = read.table(paste0(dirdata,"HData2.CSV"), header =T , sep = ",")
locations = as.character(myData$Location)

myTestData = read.table(paste0(dirdata,"TestHData2.CSV"), header =T , sep = ",")
locationsTest = as.character(myTestData$Location)

# WE BEGIN BY EXPLORING THE DATA  

RitPeriodScore = c(myData$p1T,myData$p2T , myData$p3T)
opPeriodScore = c(myData$p1G,myData$p2G , myData$p3G)

RitPeriodScore.df = as.data.frame(table(RitPeriodScore))

ggplot(data=RitPeriodScore.df, aes(x=RitPeriodScore, y=Freq)) +
  geom_bar(stat="identity", fill="blue")+
  geom_text(aes(label=Freq), vjust=-0.6, color="black", size=3.5)+
  theme_minimal() +xlab("Number of Goals") + ylab("Frequency") +
  ggtitle("Number of goals scored by RIT per period")

opPeriodScore.df = as.data.frame(table(opPeriodScore))

ggplot(data=opPeriodScore.df, aes(x=opPeriodScore, y=Freq)) +
  geom_bar(stat="identity", fill="red")+
  geom_text(aes(label=Freq), vjust=-0.6, color="black", size=3.5)+
  theme_minimal() +xlab("Number of Goals") + ylab("Frequency") +
  ggtitle("Number of goals scored by opponent per period")




mean(RitPeriodScore) #0.997
var(RitPeriodScore) #1.035

mean(opPeriodScore) #1.029
var(opPeriodScore) #0.984

#Chi-square goodness of fit tests for poisson distribution

probs = dpois(0:5, lambda = mean(RitPeriodScore))
comp = 1-sum(probs)
chisq.test(x=c(histo1$counts,0), p = c(probs,comp), simulate.p.value = TRUE)

probs2 = dpois(0:4, lambda = mean(opPeriodScore))
comp2 = 1-sum(probs2)
chisq.test(x=c(histo2$counts,0), p = c(probs2,comp2), simulate.p.value = TRUE)

#Creating new variables

ritp1score = myData$p1T
opp1score = myData$p1G
scorediffp1 = ritp1score - opp1score

ritp1score = myData$p1T
opp1score = myData$p1G
scorediffp1 = ritp1score - opp1score

ritp2score = myData$p1T + myData$p2T
opp2score = myData$p1G + myData$p2G
scorediffp2 = ritp2score - opp2score

ritFinscore = myData$p1T + myData$p2T + myData$p3T
opFinscore = myData$p1G + myData$p2G + myData$p3G
scorediffFin = ritFinscore - opFinscore

ritp1pwr = myData$POW1T - myData$POW1G
ritp2pwr = ritp1pwr + (myData$POW2T - myData$POW2G)
ritp3pwr = ritp2pwr + (myData$POW3T - myData$POW3G)




boxplot(ritp1pwr)
boxplot(ritp2pwr)
boxplot(ritp3pwr)

#Normality test on powerplay differences 

qqnorm(ritp1pwr)
qqline(ritp1pwr, col = "red")
shapiro.test(ritp1pwr) #Failed the normality assumption 


scoreddiffFin.counts <- as.data.frame(table(scorediffFin))

ggplot(data=scoreddiffFin.counts, aes(x=scorediffFin, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+
  theme_minimal() +xlab("Difference") + ylab("Frequency") +
  ggtitle("Final Score Difference")

cor(scorediffp1,scorediffFin)
cor(scorediffp2,scorediffFin)

scorediff.df = data.frame(scorediffp1,scorediffp2,scorediffFin)
cor(scorediff.df)


outcome = ifelse(scorediffFin>0, 1 , ifelse(scorediffFin<0,-1,0))
hist(outcome)



home = ifelse(locations == "H" , 1 , 0)

myData2 = data.frame(home,scorediffp1,scorediffp2,scorediffFin,ritp1pwr,ritp2pwr,ritp3pwr)
myData2$outcome = ifelse(scorediffFin > 0 , 1, 0)
cor(myData2)
cor(myData2$home , myData2$outcome)
myData2$P1outcome = ifelse(scorediffp1 > 0 , 1, 0)
myData2$P2outcome = ifelse(scorediffp2 > 0,1,0)

corrplot(cor(myData2[,c(1,2,3,8)]), method="number" , type = "lower")


#Test data set 

home = ifelse(locationsTest == "H" , 1 , 0)
testdata = data.frame(home)
testdata$scorediffp1 = myTestData$p1T - myTestData$p1G
testdata$scorediffp2 = testdata$scorediffp1 + (myTestData$p2T - myTestData$p2G)
testdata$scorediffFin = testdata$scorediffp2 + (myTestData$p3T - myTestData$p3G)
testdata$ritp1pwr = myTestData$POW1T - myTestData$POW1G
testdata$ritp2pwr = testdata$ritp1pwr + (myTestData$POW2T - myTestData$POW2G)
testdata$outcome = ifelse(testdata$scorediffFin > 0 ,1,0)
print(testdata)

#Confusion matrices from period outcomes 
confusionMatrix(as.factor(myData2$P1outcome), as.factor(myData2$outcome))

confusionMatrix(as.factor(myData2$P2outcome), as.factor(myData2$outcome))


# Plots for classification algorithms #  


ggplot(myData2, aes(x=scorediffp1, y=scorediffp2, color=outcome)) + 
  geom_point(size=6) + xlab("Period 1 difference") + ylab("Period 2 difference") #Overlap

                      ### LOGISTIC REGRESSION MODELS ###


#Logistic regression on scoreddiffp1
glm.fit <- glm(outcome ~ scorediffp1, data = myData2, family = binomial)
summary(glm.fit)
new <- data.frame(scorediffp1 = myData2$scorediffp1)
glm1.probs = predict(glm.fit,newdata = new, type = 'response')
glm1.pred = ifelse(glm1.probs >0.5 , 1 ,0)
confusionMatrix(as.factor(glm1.pred),as.factor(myData2$outcome)) #0.7632 
ggplot(myData2, aes(x=scorediffp1, y=outcome)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

test <- data.frame(scorediffp1 =testdata$scorediffp1)
glm1.probsT = predict(glm.fit, test, type = 'response')
glm1.predT = ifelse(glm1.probsT > 0.5 ,1,0)
accuracy(as.factor(glm1.predT) , as.factor(testdata$outcome)) #0.679


#Logistic regression on scoreddiffp2 

glm.fit2 <- glm(outcome ~ scorediffp2, data = myData2, family = binomial)
summary(glm.fit2)
new <- data.frame(scorediffp2 = myData2$scorediffp2)
glm2.probs = predict(glm.fit2,newdata = new, type = 'response')
glm2.pred = ifelse(glm2.probs >0.5 , 1 ,0)
confusionMatrix(as.factor(glm2.pred),as.factor(myData2$outcome)) #0.8509 
ggplot(myData2, aes(x=scorediffp2, y=outcome)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

test <- data.frame(scorediffp2 =testdata$scorediffp2)
glm2.probsT = predict(glm.fit2, test, type = 'response')
glm2.predT = ifelse(glm2.probsT > 0.5 ,1,0)
accuracy(as.factor(glm2.predT) , as.factor(testdata$outcome)) #0.75





summary(glm(outcome ~ ritp2pwr + scorediffp2, data = myData2, family = binomial))

# Logistic regression fails to converge with the power play variables

# Cross - Validation of Logistic Regression with 2 features

fold = sample(rep(1:6,length=114)) 
cutoff = seq(0 , 1 , length.out = 51)
glm4.accur = matrix(0 , 6 , 51)
for (i in  1:6) {
  glm4 <- glm(outcome ~ scorediffp1 + scorediffp2, data = myData2[fold!=i,], family = binomial)
  test <-data.frame(myData2[fold==i , c(2,3)])
  glm4.probsT = predict(glm4 ,test , type = 'response' )
  for (j in 1:51) {
    glm4.predT = ifelse(glm4.probsT > cutoff[j] ,1 ,0)
    glm4.accur[i,j] = 1 - mean(glm4.predT ==myData2[fold ==i,8])
  }
}

error.log = rep( 0 , 51)
for (i in 1:51) {
  error.log[i] = mean(glm4.accur[ , i])
}

min.log = round(min(error.log),3)
title.log = paste0("Minimum error: " , min.log)

df.log = data.frame(cutoff , error.log)
ggplot(data=df.log, aes(x=cutoff, y=error.log, group=1)) +
  geom_line(color = "red") +xlab("Cut-off Probabilty") + ylab("Mean Error rate") +
  ggtitle("6-fold cross validation: Logistic regression" , title.log)

# Use 0.5 as a cutoff value

# Cross validation with home added as a feature

fold = sample(rep(1:6,length=114)) 
cutoff = seq(0 , 1 , length.out = 51)
glm5.accur = matrix(0 , 6 , 51)
for (i in  1:6) {
  glm5 <- glm(outcome ~ scorediffp1 + scorediffp2 + home, data = myData2[fold!=i,], family = binomial)
  test <-data.frame(myData2[fold==i , c(1,2,3)])
  glm5.probsT = predict(glm5 ,test , type = 'response' )
  for (j in 1:51) {
    glm5.predT = ifelse(glm5.probsT > cutoff[j] ,1 ,0)
    glm5.accur[i,j] = 1 - mean(glm5.predT ==myData2[fold ==i,8])
  }
}

error.log2 = rep( 0 , 51)
for (i in 1:51) {
  error.log2[i] = mean(glm5.accur[ , i])
}

df.log2 = data.frame(cutoff , error.log2)
ggplot(data=df.log2, aes(x=cutoff, y=error.log2, group=1)) +
  geom_line(color = "red") +xlab("Cut-off Probabilty") + ylab("Mean Error rate") +
  ggtitle("6-fold cross validation: Logistic regression" , title.log)


# Diagnostics of 2-feature logistic regression: Leave 3 out cross-validation box-plots

fold = sample(rep(1:6,length=114)) 

glm6.accur = rep(0,6)
for (i in  1:6) {
  glm6 <- glm(outcome ~ scorediffp1 + scorediffp2, data = myData2[fold!=i,], family = binomial)
  test <-data.frame(myData2[fold==i , c(2,3)])
  glm6.probsT = predict(glm4 ,test , type = 'response' )
  glm6.predT = ifelse(glm6.probsT > 0.5 ,1 ,0)
  glm6.accur[i] = 1 - mean(glm6.predT ==myData2[fold ==i,8])
  }

boxplot(glm6.accur)


              ### KNN Classification ### 

ggplot(myData2, aes(x=scorediffp1, y=scorediffp2, color=outcome)) + 
  geom_point(size=4) #Find the best decision boundary 

knn.accur = rep(0,27)
knn.spec = rep(0,27)
knn.sens = rep(0.27)
for (i in 1:27) {
  knn1 = knn(train = myData2[c(2,3)], test = testdata[c(2,3)], cl = myData2$outcome, k=i)
  knn.accur[i] = accuracy(testdata$outcome,knn1)
  knn.spec[i] = specificity(as.factor(testdata$outcome), knn1)
  knn.sens[i] = sensitivity(as.factor(testdata$outcome),knn1)
}
plot(1:27,knn.accur , xlab = "K nearest neigbor", ylab = "Accuracy" )

knn.accur = rep(0,27)
knn.spec = rep(0,27)
knn.sens = rep(0.27)
for (i in 1:27) {
  knn1 = knn(train = myData2[c(2,3)], test = testdata[c(2,3)], cl = as.factor(myData2$outcome), k=i)
  knn.accur[i] = accuracy(testdata$outcome,knn1)
  knn.spec[i] = specificity(as.factor(testdata$outcome), knn1)
  knn.sens[i] = sensitivity(as.factor(testdata$outcome),knn1)
}
plot(1:27,knn.accur , xlab = "K nearest neigbor", ylab = "Accuracy" )

knn.accur = matrix(0,114,5)
knn.spec = matrix(0,114,5)
knn.sens = matrix(0,114,5)
for (i in 1:114) {
  for (j in 1:5) {
  knn1 = knn(train = scale(myData2[c(2,3)]), test = scale(testdata[c(2,3)]), cl = myData2$outcome, k=i)
  knn.accur[i,j] = accuracy(testdata$outcome,knn1)
  knn.spec[i,j] = specificity(as.factor(testdata$outcome), knn1)
  knn.sens[i,j] = sensitivity(as.factor(testdata$outcome),knn1)
  }
}


knn.accurate = rep(0,114)
for (i in 1:114) {
  knn.accurate[i] = mean(knn.accur[i,])
}
knn.error = 1 - knn.accurate
plot(1:114,knn.accurate , xlab = "K nearest neigbor", ylab = "Accuracy" )
plot(1:114,knn.error, xlab = "K" , ylab = "Error rate" , main = " Choice of K vs Misclassification rate")

# Compare 1KNN and 2KNN 

knn.accur2 = matrix(0,2,20)
for (i in 1:2) {
  for (j in 1:20) {
    knn2 = knn(train = scale(myData2[c(2,3)]), test = scale(testdata[c(2,3)]), cl = myData2$outcome, k=i)
    knn.accur2[i,j] = accuracy(testdata$outcome,knn2)
    
  }
}
knn.accur2 = t(knn.accur2)
knn.error2 = 1 - knn.accur2
knn.error2.df = data.frame(knn.error2)
boxplot(knn.error2.df)
mean(knn.error2.df$X1)
mean(knn.error2.df$X2)
sd(knn.error2.df$X1)
sd(knn.error2.df$X2)  # We will choose 2NN based on mean/sd  


fold = sample(rep(1:6,length=114)) 
K.choice = seq(1 , 95 , length.out = 95)
knn4.accur = matrix(0 , 6 , 95)
for (i in  1:6) {
  train2 <- data.frame(myData2[fold!=i , c(2,3)])
  test2 <- data.frame(myData2[fold==i , c(2,3)])
  test2real <- data.frame(myData2[fold==i , 8])
  train2real <- as.vector(myData2[fold!=i , 8])
  for (j in 1:95) {
    knn4 <- knn(train = train2, test = test2, cl = train2real, k=K.choice[j])
    knn4.accur[i,j] = 1 - mean(as.numeric(levels(knn4)[as.integer(knn4)])==test2real)
  }
}


error.knn = rep( 0 , 95)
for (i in 1:95) {
  error.knn[i] = mean(knn4.accur[ , i])
}

min.knn = round(min(error.knn),3)
title.knn = paste0("Minimum error: " , min.knn)

df.knn = data.frame(K.choice , error.knn)
ggplot(data=df.knn, aes(x=K.choice, y=error.knn, group=1)) +
  geom_line(color = "red") +xlab("K") + ylab("Mean Error rate") +
  ggtitle("6-fold cross validation: KNN" , title.knn)




### Linear discriminant analysis and Quadratic discriminant analysis ### 

# LDA with cross-validation to find optimal priors 
#LDA assumes normality. Normality test

norms = (myData2$scorediffp1 - mean(myData2$scorediffp1)) / sd(myData2$scorediffp1)

shapiro.test(myData2$scorediffp1) #failed 
shapiro.test(scale(myData2$scorediffp1))



Priors = seq(0.01,0.99,length.out=200)
k = length(Priors)
errors = rep(0,k)
for (i in 1:k) {
  prior1 = Priors[i]
  prior2 = 1 - prior1
  lda1 = lda(outcome ~ scorediffp1 + scorediffp2 , data = myData2 , prior = c(prior1 , prior2))
  predictions = lda1 %>% predict(testdata)
  errors[i] = 1 - mean(predictions$class==testdata$outcome)
}

fold = sample(rep(1:6,length=114)) 
Priors = seq(0.01,0.99,length.out=99)
lda.accur = matrix(0 , 6 , 99)
for (i in  1:6) {
  test <-data.frame(myData2[fold==i,])
  train <- data.frame(myData2[fold!=i,])
  for (j in 1:99) {
    prior1 =Priors[j]
    lda2 = lda(outcome ~ scorediffp1 + scorediffp2 , data = test , prior = c(prior1 , 1 - prior1))
    predictions = lda2 %>% predict(test)
    lda.accur[i,j] = 1 - mean(predictions$class==test$outcome)
  }
}

error.lda = rep( 0 , 99)
for (i in 1:99) {
  error.lda[i] = mean(lda.accur[ , i])
}

min.lda = round(min(error.lda),3)
title.lda = paste0("Minimum error: " , min.lda)

df.lda = data.frame(Priors , error.lda)
ggplot(data=df.lda, aes(x=Priors, y=error.lda, group=1)) +
  geom_line(color = "red") +xlab("Choice of Prior") + ylab("Mean Error rate") +
  ggtitle("6-fold cross validation: LDA" , title.lda)




plot(x=Priors , y=errors)

lda2 = lda(outcome ~ scorediffp1 + scorediffp2, data = myData2)
predictions.lda = lda2 %>% predict(testdata)
table(predicted = predictions.lda$class , actual = testdata$outcome )
error.lda = 1 - mean(predictions.lda$class == testdata$outcome) #0.25
plot(predictions.lda$x[,1], predictions.lda$class, col=testdata$outcome+10)

qda1 = qda(outcome ~ scorediffp1 + scorediffp2 , data = myData2)
predictions.qda = qda1 %>% predict(testdata)
error.qda = 1 - mean(predictions.qda$class == testdata$outcome) #0.25 

#Cross-validation of Quadratic discriminant analysis with 2 features

fold = sample(rep(1:6,length=114)) 
Priors = seq(0.01,0.99,length.out=99)
qda.accur = matrix(0 , 6 , 99)
for (i in  1:6) {
  test <-data.frame(myData2[fold==i,])
  train <- data.frame(myData2[fold!=i,])
  for (j in 1:99) {
    prior1 =Priors[j]
    qda2 = qda(outcome ~ scorediffp1 + scorediffp2 , data = test , prior = c(prior1 , 1 - prior1))
    predictions = qda2 %>% predict(test)
    qda.accur[i,j] = 1 - mean(predictions$class==test$outcome)
  }
}

error.qda = rep( 0 , 99)
for (i in 1:99) {
  error.qda[i] = mean(qda.accur[ , i])
}

min.qda = round(min(error.qda),3)
title.qda = paste0("Minimum error: " , min.qda)

df.qda = data.frame(Priors , error.qda)
ggplot(data=df.qda, aes(x=Priors, y=error.qda, group=1)) +
  geom_line(color = "red") +xlab("Choice of Prior") + ylab("Mean Error rate") +
  ggtitle("6-fold cross validation: QDA" , title.qda)


## Compare and contrast Final learning machines on the next season ### 

#Logistic regression with 2 features

glm.final <- glm(outcome ~ scorediffp2 + scorediffp2, data = myData2, family = binomial)
test <- data.frame(testdata[,c(2,3)])
glmfinal.probsT = predict(glm.final, test, type = 'response')
glmfinal.predT = ifelse(glmfinal.probsT > 0.5 ,1,0)
table(as.factor(glmfinal.predT) , as.factor(testdata$outcome))
accuracy(as.factor(glmfinal.predT) , as.factor(testdata$outcome)) #0.75

#Logistic regression with home added as feature

glm.final2 <- glm(outcome ~ scorediffp1 + scorediffp2 + home, data = myData2, family = binomial)
test <- data.frame(testdata[,c(1,2,3)])
glmfinal2.probsT = predict(glm.final2, test, type = 'response')
glmfinal2.predT = ifelse(glmfinal2.probsT > 0.5 ,1,0)
table(as.factor(glmfinal2.predT) , as.factor(testdata$outcome))
accuracy(as.factor(glmfinal2.predT) , as.factor(testdata$outcome)) #0.75 Makes no difference 

