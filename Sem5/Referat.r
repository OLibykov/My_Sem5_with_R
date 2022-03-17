#тестовый пример
library(MASS)
mydat <- iris[iris$Species  != "virginica", c(1, 2, 5)]
mydat$Species <- droplevels(mydat$Species)
str(mydat)
mydat1 <- mydat[mydat$Species  == "setosa",]
mydat2 <- mydat[mydat$Species  == "versicolor",]
plot(mydat1$Sepal.Length~mydat1$Sepal.Width, col = "blue", pch = 20,
     xlim = c(2, 5), ylim = c(4, 7))
points(mydat2$Sepal.Length~mydat2$Sepal.Width,col = "red", pch = 20)
#проверка условий метода
library(mvnormtest) 
library(biotools)
mshapiro.test(t(mydat[,c(1, 2)]))
boxM(as.matrix(mydat[, 1:2]), mydat$Species) 
#бьем на train-test и настраиваем параметры
index <- sample(1:nrow(mydat), round(0.70*nrow(mydat)))
train <- mydat[index,]
test <- mydat[-index,]

z <- lda(Species~., data = train)
z
#построение разделяющей кривой
pr1 <- length(mydat$Species[mydat$Species!="setoca"])/length(mydat$Species)
pr2 <- 1 - pr1

mean1 <- apply(mydat1[,c(1,2)], 2, mean)
mean2 <- apply(mydat2[,c(1,2)], 2, mean)

sigma1 <- cor(mydat1[,c(1, 2)])
sigma2 <- cor(mydat2[,c(1, 2)])
sigma3 <- (sigma1+sigma2)/2
revsigm <- ginv(sigma3)

w <- (mean1 - mean2)%*%revsigm
c1 <- mean1 %*% revsigm
c2 <- mean2 %*% revsigm
c <- 0.5*(c1 %*% mean1) - 0.5*(c2 %*% mean2)
x <- seq(2, 5, 0.1)
y = (-x*w[2] + as.vector(c))/w[1]

plot(mydat1$Sepal.Length~mydat1$Sepal.Width, col = "blue", pch = 20, 
     xlim = c(2, 5), ylim = c(4, 7))
points(mydat2$Sepal.Length~mydat2$Sepal.Width,col = "red", pch = 20)
lines(y ~ x)

#функция оценки качества
Accur <- function(model, group1, group2) {

  classified <- predict(model, test)$class 
  t1 <- table(group1, classified)  
  Err_S <- mean(group1 != classified)

  t2 <-  table(group2, update(model, CV = T)$class -> LDA.cv) 
  Err_CV <- mean(group2 != LDA.cv) 
  Err_CV.N <- c(Err_CV, length(group2)) 
  cbind(t1, t2, Err_CV.N)
}

# --- Выполнение расчетов

Accur(z, test$Species, train$Species) 
#///////////////////////////////////////////////////////////////////
#Реальные данные
library(rfUtilities)
phone_data <-data.frame(read.csv(file.choose()))
str(phone_data)

#вывод зависимости парамаетров
Colfill = topo.colors(4)
upg_phone_data = head(phone_data[c(1, 3, 7, 9, 14, 21)], 250)
#par(mfrow = c(2, 3))
for (i in 1:13){
    for (j in (i + 1):14){
        
        plot(0, xlim = c(min(upd_phone_data[,i]), max(upd_phone_data[,i])),
                ylim = c(min(upd_phone_data[,j]), max(upd_phone_data[,j])),
             xlab = colnames(upd_phone_data)[i], 
             ylab = colnames(upd_phone_data)[j])
        for (k in levels(as.factor(upd_phone_data$price_range))){
           group <- upd_phone_data[as.factor(upd_phone_data$price_range) == k,]
           points(group[,j]~group[,i], 
                  col = Colfill[as.numeric(k) + 1], pch = 20)
        }
    }
}
par(mfrow = c(1, 1))
#1-ый тест на реальных данных
upg_phone_data = phone_data[c(1, 3, 7, 9, 14, 21)]

index <- sample(1:nrow(upg_phone_data), round(0.70*nrow(upg_phone_data)))
train <- upg_phone_data[index,]
test <- upg_phone_data[-index,]
#lda
z <- lda(price_range~., data = train)
p <- predict(z, test)

#точность модели lda

a <- accuracy(p$class, test$price_range)
barplot(t(a$confusion),col = topo.colors(4),
        beside = TRUE, xpd = FALSE,
        ylab = "Количество телефонов", xlab = "Группы")
legend(locator(1), levels(as.factor(test$price_range)), fill = Colfill)
a
#Rf
library(randomForest)
set.seed(12)
phone_rf <- randomForest(as.factor(train$price_range) ~., data = train)
phone_rfp <- predict(phone_rf, test)
#точность модели Rf
a <- accuracy(phone_rfp, test$price_range) 
barplot(t(a$confusion),col = topo.colors(4),
        beside = TRUE, xpd = FALSE,
        ylab = "Количество телефонов", xlab = "Группы")
legend(locator(1), levels(as.factor(test$price_range)), fill = Colfill)
a
#2-ой тест на реальных данных
upd_phone_data <- phone_data[-c(2, 4, 6, 18, 19, 20)]

index <- sample(1:nrow(upd_phone_data), round(0.70*nrow(upd_phone_data)))
train <- upd_phone_data[index,]
test <- upd_phone_data[-index,]
#lda
z <- lda(price_range~., data = train)
p <- predict(z, test)

#точность модели lda
a <- accuracy(p$class, test$price_range) 
barplot(t(a$confusion),col = topo.colors(4),
        beside = TRUE, xpd = FALSE,
        ylab = "Количество телефонов", xlab = "Группы")
legend(locator(1), levels(as.factor(test$price_range)), fill = Colfill)
a

library(randomForest)
#Rf
set.seed(12)
phone_rf <- randomForest(as.factor(train$price_range) ~., data = train)
phone_rfp <- predict(phone_rf, test)

#точность модели Rf

a <- accuracy(phone_rfp, test$price_range) 
barplot(t(a$confusion),col = topo.colors(4),
        beside = TRUE, xpd = FALSE,
        ylab = "Количество телефонов", xlab = "Группы")
legend(locator(1), levels(as.factor(test$price_range)), fill = Colfill)
a
