

# Set working directory of your choice
#setwd('C://Users//PARTHI vs BHARATHI//Downloads//PRAXIS//A Term 2//ML2//ML 2 Assign 1')


#Open the MASS Library for the dataset

library(MASS)

# Data description below:
?GAGurine

# Creating data frame
df <- GAGurine
View(df)
dim(df)

# Checking for any missing values:
sum(is.na.data.frame(df))


# structure and summary : 
str(df)
summary(df)
View(cor(df))


314*0.7

# data splitting test 30% and train 70%: 
set.seed(0)
rand = sample(1:nrow(df),220)
train = df[rand, ]
test = df[-rand, ]



#  Fitting a Polynomial Regression of Order 1


m1 <- lm(GAG ~ Age, train)
m1
m1$coefficients
summary(m1)



#PLOTTING THE MODEL OVER THE DATA
plot(train$Age,train$GAG, pch=19, cex=0.5, xlab = 'Age of child in Years', 
     ylab = 'GAG concentration', main = 'Fitted lines of model (Train dataset)')
lines(sort(train$Age), fitted(m1)[order(train$Age)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
sum((pred-test$GAG)^2)

# Creating a table/data frame "t" for storing the Polynomial Order size,
# Test and Train SSEs
t = array(c(length(m1$coefficients)-1, sum(m1$residuals^2), 
            sum((pred-test$GAG)^2)), dim = c(1,3))
t = as.data.frame(t)
colnames(t) = list('Order', 'Train SSE', 'Test SSE')[1:3]

View(t)



#  Fitting a Polynomial Regression of Order 2


m2 <- lm(GAG ~ Age + I(Age^2), train)
m2

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m2)[order(train$Age)], col='blue', type='l', pch=19) 

#TRAIN AND TEST ACCURACY
sum(m2$residuals^2)
pred = predict(m2, newdata=test)
sum((pred-test$GAG)^2)


t <- rbind(t, c(length(m2$coefficients)-1, sum(m2$residuals^2), 
                sum((pred-test$GAG)^2)))

View(t)



#  Fitting a Polynomial Regression of Order 3


m3 <- lm(GAG ~ Age + I(Age^2) + I(Age^3), train)
m3

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m3)[order(train$Age)], col='darkgreen', type='l', pch=19) 

#TRAIN AND TEST ACCURACY
sum(m3$residuals^2)
pred = predict(m3, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m3$coefficients)-1, sum(m3$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)


#  Fitting a Polynomial Regression of Order 4


m4 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4), train)
m4

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m4)[order(train$Age)], col='magenta', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m4$residuals^2)
pred = predict(m4, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m4$coefficients)-1, sum(m4$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)


#  Fitting a Polynomial Regression of Order 5


m5 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) + I(Age^5), train)
m5

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m5)[order(train$Age)], col='green', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m5$residuals^2)                  #Train
pred = predict(m5, newdata=test)
sum((pred-test$GAG)^2)               #Test

t <- rbind(t, c(length(m5$coefficients)-1, sum(m5$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)



#  Fitting a Polynomial Regression of Order 6


m6 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) + I(Age^5)+ I(Age^5) + I(Age^6), train)
m6

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m6)[order(train$Age)], col='mGAGnta', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m6$residuals^2)
pred = predict(m6, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m6$coefficients)-1, sum(m6$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)



#  Fitting a Polynomial Regression of Order 7



m7 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) + I(Age^5)+ I(Age^5) + I(Age^6) 
         + I(Age^6) + I(Age^7), train)
m7

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m7)[order(train$Age)], col='brown', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m7$residuals^2)
pred = predict(m7, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m7$coefficients)-1, sum(m7$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)


#  Fitting a Polynomial Regression of Order 8



m8 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) + I(Age^5)+ I(Age^5) + I(Age^6) 
         + I(Age^6) + I(Age^7) + I(Age^8), train)
m8

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m8)[order(train$Age)], col='violet', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m8$residuals^2)
pred = predict(m8, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m8$coefficients)-1, sum(m8$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)


#  Fitting a Polynomial Regression of Order 9


m9 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
           I(Age^5) + I(Age^6) + I(Age^7) +
           I(Age^8) + I(Age^9) , train)

m9


#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m9)[order(train$Age)], col='orange', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m9$residuals^2)
pred = predict(m9, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m9$coefficients)-1, sum(m9$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)




#  Fitting a Polynomial Regression of Order 10


m10 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
            I(Age^5) + I(Age^6) + I(Age^7) +
            I(Age^8) + I(Age^9) + I(Age^10) , train)

m10


#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m10)[order(train$Age)], col='yellow', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m10$residuals^2)
pred = predict(m10, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m10$coefficients)-1, sum(m10$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)


#  Fitting a Polynomial Regression of Order 11


m11 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
            I(Age^5) + I(Age^6) + I(Age^7) +
            I(Age^8) + I(Age^9) + I(Age^10) + I(Age^11), train)

m11


#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m11)[order(train$Age)], col='134', type='l',pch=20) 


#TRAIN AND TEST ACCURACY
sum(m11$residuals^2)
pred = predict(m11, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m11$coefficients)-1, sum(m11$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)



#  Fitting a Polynomial Regression of Order 12


m12 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
            I(Age^5) + I(Age^6) + I(Age^7) +
            I(Age^8) + I(Age^9) + I(Age^10) + I(Age^11) + I(Age^12), train)

m12


#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m12)[order(train$Age)], col='441', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m12$residuals^2)
pred = predict(m12, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m12$coefficients)-1, sum(m12$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)


#  Fitting a Polynomial Regression of Order 13


m13 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
            I(Age^5) + I(Age^6) + I(Age^7) +
            I(Age^8) + I(Age^9) + I(Age^10) + I(Age^11) + I(Age^12) + I(Age^13), train)

m13


#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m13)[order(train$Age)], col='254', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m13$residuals^2)
pred = predict(m13, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m13$coefficients)-1, sum(m13$residuals^2), 
                sum((pred-test$GAG)^2)))

#View(t)


#  Fitting a Polynomial Regression of Order 14


m14 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
            I(Age^5) + I(Age^6) + I(Age^7) +
            I(Age^8) + I(Age^9) + I(Age^10) + I(Age^11) + I(Age^12) + I(Age^13) + I(Age^14), train)

m14


#PLOTTING THE MODEL OVER THE DATA
#plot(train$Age,train$GAG, pch=19, cex=0.5)
lines(sort(train$Age), fitted(m14)[order(train$Age)], col='84', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m14$residuals^2)
pred = predict(m14, newdata=test)
sum((pred-test$GAG)^2)

t <- rbind(t, c(length(m14$coefficients)-1, sum(m14$residuals^2), 
                sum((pred-test$GAG)^2)))


View(t)

# ----------------------------------------

# Graph 1:
# Here, you can save the graph for the fitted lines of train data points of Telco
# Here the train data is modeled for upto complexity levels of Order 16

# This graph will show the different shapes of fitted model lines for
# different ploynomial orders of size.

# ----------------------------------------



# Creating new table "t1" with RMSE values of test and train
# along with the existing SSE values :
t1<- t
t1['Train RMSE'] <- sqrt(t1$`Train SSE`/(220-t1$Order-1))
t1['Test RMSE'] <- sqrt(t1$`Test SSE`/(94-t1$Order-1))
View(t1)


# ----------------------------------------

# Graph 2.1 :  RSS vs Complexity of fixed Train and Sample size in the ratio 0.7:0.3
# saving file as jpg:

jpeg('RSS vs Complexity of GAGurine.jpg')

plot(t1$Order, t1$`Test SSE`, type = 'l', 
     main = 'RSS vs Complexity\n(fixed Train and Sample size in the ratio 0.7:0.3)', 
     xlab="Complexity (Orders of Regression)",
     ylab='RSS of Train and Test', col='red', ylim=c(1000,9000))

lines((t1$`Train SSE`),col='blue')
legend(11,8800,legend=c("Train","Test"), col=c("blue","red"),
       lty=c(1,1,1), ncol=1)

dev.off()
dev.off()

# ----------------------------------------

# Graph 2.2 :  RMSE vs Complexity of fixed Train and Sample size in the ratio 0.7:0.3
#saving file as jpg:
jpeg('RMSE vs Complexity of GAGurine.jpg')

plot(t1$Order, t1$`Test RMSE`, type = 'l', 
     main = 'RMSE vs Complexity\n(fixed Train and Sample size in the ratio 0.7:0.3)', 
     xlab="Complexity (Orders of Regression)",
     ylab='RMSE of Train and Test', col='red', ylim=c(4,7))

lines((t1$`Train RMSE`),col='blue')
legend(11,7,legend=c("Train","Test"), col=c("blue","red"),
       lty=c(1,1,1), ncol=1)

dev.off()
dev.off()

# ----------------------------------------


# Graph 3.1:

# Here this will give us a graph on Test Errors Vs Model with 
# increasing Train size yet with Fixed Test size of small 
# using the Polynomial Regression of Order 1


table1 <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(table1) <- c('Train Sample Size', 'Test SSE', 'Test RMSE')

View(table1)

dim(df)
n = dim(df)[1]
count = 1
set.seed(0)
rand1 = sample(1:nrow(df), as.integer(0.1*n))
n1 = n -length(rand1)
test1 = df[rand1,]
train_remain = df[-rand1,]
dim(train_remain)

for (i in seq(0.1, 0.89, by=0.05)) {
  #set.seed(0)
  #rand = sample(1:nrow(df),as.integer(n*i))
  train1 = df[sample(nrow(train_remain), as.integer(n1*i)),]
  #test1 = df[-rand, ]
  model1 <- lm(GAG ~ Age, train1)
  model1
  
  sum(model1$residuals^2)
  pred1 = predict(model1, newdata=test1)
  sum((pred1-test1$GAG)^2)
  
  table1[count,] <- c(dim(train1)[1], sum((pred1-test1$GAG)^2), 
                      sqrt(sum((pred1-test1$GAG)^2)/(as.integer(n1*i) - 1 -1)))
  count = count + 1
}

View(table1)

# Graph of Train data sample size vs Test SSE
# For saving use below lines:
jpeg('SSE vs Train Sample Sizes for Order 1.jpg')
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 1\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 1\n(Fixed sample size of 31)')


# Graph of Train data sample size vs Test RMSE
# For saving use below lines:
jpeg('RMSE vs Train Sample Sizes for Order 1.jpg')
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 1\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 1\n(Fixed sample size of 31)')


# ----------------------------------------


# Graph 3.2:

# Here this will give us a graph on Test Errors Vs Model with 
# increasing Train size yet with Fixed Test size of small 
# using the Polynomial Regression of Order 2


table1 <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(table1) <- c('Train Sample Size', 'Test SSE', 'Test RMSE')

View(table1)

dim(df)
n = dim(df)[1]
count = 1
set.seed(0)
rand1 = sample(1:nrow(df), as.integer(0.1*n))
n1 = n -length(rand1)
test1 = df[rand1,]
train_remain = df[-rand1,]
dim(train_remain)

for (i in seq(0.1, 0.89, by=0.05)) {
  #set.seed(0)
  #rand = sample(1:nrow(df),as.integer(n*i))
  train1 = df[sample(nrow(train_remain), as.integer(n1*i)),]
  #test1 = df[-rand, ]
  model1 <- lm(GAG ~ Age + I(Age^2), train1)
  model1
  
  sum(model1$residuals^2)
  pred1 = predict(model1, newdata=test1)
  sum((pred1-test1$GAG)^2)
  
  table1[count,] <- c(dim(train1)[1], sum((pred1-test1$GAG)^2), 
                      sqrt(sum((pred1-test1$GAG)^2)/(as.integer(n1*i) - 8 -1)))
  count = count + 1
}

View(table1)

# Graph of Train data sample size vs Test SSE
# For saving use below lines:
jpeg('SSE vs Train Sample Sizes for Order 2.jpg')
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 2\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 2\n(Fixed sample size of 31)')


# Graph of Train data sample size vs Test RMSE
# For saving use below lines:
jpeg('RMSE vs Train Sample Sizes for Order 2.jpg')
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 2\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 2\n(Fixed sample size of 31)')


# ----------------------------------------



# Graph 3.3:

# Here this will give us a graph on Test Errors Vs Model with 
# increasing Train size yet with Fixed Test size of small 
# using the Polynomial Regression of Order 3


table1 <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(table1) <- c('Train Sample Size', 'Test SSE', 'Test RMSE')

View(table1)

dim(df)
n = dim(df)[1]
count = 1
set.seed(0)
rand1 = sample(1:nrow(df), as.integer(0.1*n))
n1 = n -length(rand1)
test1 = df[rand1,]
train_remain = df[-rand1,]
dim(train_remain)

for (i in seq(0.1, 0.89, by=0.05)) {
  #set.seed(0)
  #rand = sample(1:nrow(df),as.integer(n*i))
  train1 = df[sample(nrow(train_remain), as.integer(n1*i)),]
  #test1 = df[-rand, ]
  model1 <- lm(GAG ~ Age + I(Age^2) + I(Age^3), train1)
  model1
  
  sum(model1$residuals^2)
  pred1 = predict(model1, newdata=test1)
  sum((pred1-test1$GAG)^2)
  
  table1[count,] <- c(dim(train1)[1], sum((pred1-test1$GAG)^2), 
                      sqrt(sum((pred1-test1$GAG)^2)/(as.integer(n*i) - 8 -1)))
  count = count + 1
}

View(table1)

# Graph of Train data sample size vs Test SSE
# For saving use below lines:
jpeg('SSE vs Train Sample Sizes for Order 3.jpg')
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 3\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 3\n(Fixed sample size of 31)')


# Graph of Train data sample size vs Test RMSE
# For saving use below lines:
jpeg('RMSE vs Train Sample Sizes for Order 3.jpg')
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 3\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 3\n(Fixed sample size of 31)')


# ----------------------------------------


# Graph 3.4:

# Here this will give us a graph on Test Errors Vs Model with 
# increasing Train size yet with Fixed Test size of small 
# using the Polynomial Regression of Order 7


table1 <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(table1) <- c('Train Sample Size', 'Test SSE', 'Test RMSE')

View(table1)

dim(df)
n = dim(df)[1]
count = 1
set.seed(0)
rand1 = sample(1:nrow(df), as.integer(0.1*n))
n1 = n -length(rand1)
test1 = df[rand1,]
train_remain = df[-rand1,]
dim(train_remain)

for (i in seq(0.1, 0.89, by=0.05)) {
  #set.seed(0)
  #rand = sample(1:nrow(df),as.integer(n*i))
  train1 = df[sample(nrow(train_remain), as.integer(n1*i)),]
  #test1 = df[-rand, ]
  model1 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
                 I(Age^5) + I(Age^6) + I(Age^7), train1)
  model1
  
  sum(model1$residuals^2)
  pred1 = predict(model1, newdata=test1)
  sum((pred1-test1$GAG)^2)
  
  table1[count,] <- c(dim(train1)[1], sum((pred1-test1$GAG)^2), 
                      sqrt(sum((pred1-test1$GAG)^2)/(as.integer(n*i) - 8 -1)))
  count = count + 1
}

View(table1)

# Graph of Train data sample size vs Test SSE
# For saving use below lines:
jpeg('SSE vs Train Sample Sizes for Order 7.jpg')
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 7\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 7\n(Fixed sample size of 31)')


# Graph of Train data sample size vs Test RMSE
# For saving use below lines:
jpeg('RMSE vs Train Sample Sizes for Order 7.jpg')
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 7\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 7\n(Fixed sample size of 31)')


# ----------------------------------------



# Graph 3.5:

# Here this will give us a graph on Test Errors Vs Model with 
# increasing Train size yet with Fixed Test size of small 
# using the Polynomial Regression of Order 8


table1 <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(table1) <- c('Train Sample Size', 'Test SSE', 'Test RMSE')

View(table1)

dim(df)
n = dim(df)[1]
count = 1
set.seed(0)
rand1 = sample(1:nrow(df), as.integer(0.1*n))
n1 = n -length(rand1)
test1 = df[rand1,]
train_remain = df[-rand1,]
dim(train_remain)

for (i in seq(0.1, 0.89, by=0.05)) {
  #set.seed(0)
  #rand = sample(1:nrow(df),as.integer(n*i))
  train1 = df[sample(nrow(train_remain), as.integer(n1*i)),]
  #test1 = df[-rand, ]
  model1 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
                 I(Age^5) + I(Age^6) + I(Age^7) + I(Age^8), train1)
  model1
  
  sum(model1$residuals^2)
  pred1 = predict(model1, newdata=test1)
  sum((pred1-test1$GAG)^2)
  
  table1[count,] <- c(dim(train1)[1], sum((pred1-test1$GAG)^2), 
                      sqrt(sum((pred1-test1$GAG)^2)/(as.integer(n*i) - 8 -1)))
  count = count + 1
}

View(table1)

# Graph of Train data sample size vs Test SSE
# For saving use below lines:
jpeg('SSE vs Train Sample Sizes for Order 8.jpg')
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 8\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 8\n(Fixed sample size of 31)')


# Graph of Train data sample size vs Test RMSE
# For saving use below lines:
jpeg('RMSE vs Train Sample Sizes for Order 8.jpg')
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 8\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 8\n(Fixed sample size of 31)')


# ----------------------------------------


# Graph 3.6:

# Here this will give us a graph on Test Errors Vs Model with 
# increasing Train size yet with Fixed Test size of small 
# using the Polynomial Regression of Order 9


table1 <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(table1) <- c('Train Sample Size', 'Test SSE', 'Test RMSE')

View(table1)

dim(df)
n = dim(df)[1]
count = 1
set.seed(0)
rand1 = sample(1:nrow(df), as.integer(0.1*n))
n1 = n -length(rand1)
test1 = df[rand1,]
train_remain = df[-rand1,]
dim(train_remain)

for (i in seq(0.1, 0.89, by=0.05)) {
  #set.seed(0)
  #rand = sample(1:nrow(df),as.integer(n*i))
  train1 = df[sample(nrow(train_remain), as.integer(n1*i)),]
  #test1 = df[-rand, ]
  model1 <- lm(GAG ~ Age + I(Age^2) + I(Age^3) + I(Age^4) +
                 I(Age^5) + I(Age^6) + I(Age^7) + I(Age^8) + I(Age^9), train1)
  model1
  
  sum(model1$residuals^2)
  pred1 = predict(model1, newdata=test1)
  sum((pred1-test1$GAG)^2)
  
  table1[count,] <- c(dim(train1)[1], sum((pred1-test1$GAG)^2), 
                      sqrt(sum((pred1-test1$GAG)^2)/(as.integer(n*i) - 8 -1)))
  count = count + 1
}

View(table1)

# Graph of Train data sample size vs Test SSE
# For saving use below lines:
jpeg('SSE vs Train Sample Sizes for Order 9.jpg')
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 9\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test SSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test SSE', 
     main = 'Train data sample size vs Test SSE for\nfixed model complexity of Order 9\n(Fixed sample size of 31)')


# Graph of Train data sample size vs Test RMSE
# For saving use below lines:
jpeg('RMSE vs Train Sample Sizes for Order 9.jpg')
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 9\n(Fixed sample size of 31)')
dev.off()
dev.off()
# For display purpose below:
plot(table1$`Train Sample Size`, table1$`Test RMSE`, type = 'l',
     xlab = 'Train Sample Size (n Train)', ylab = 'Test RMSE', 
     main = 'Train data sample size vs Test RMSE for\nfixed model complexity of Order 9\n(Fixed sample size of 31)')


# ----------------------------------------











