#Part1.Модификация функции семинарской функции regr()
regr <- function(data, indices) {
  dat <- data[indices, ] 
  fit <- lm(y ~ -1 + x, data = dat)
  return(3.09e19/summary(fit)$coefficients[1]/(60^2*24*365))
} #Модифицировать для определения сразу возраста Вселенной

library(boot)
(results <- boot(data = hubble, statistic = regr, R = 1000))

#plot(results)

quantile(results$t, c(0.025, 0.975))
#Part2. Построение CI(с доверительным интервалом) и PI линейной модели.
par(mfrow = c(1, 1))
work_data <-data.frame(read.csv(file.choose()))
norm_data <- head(work_data, 100)
norm_data <- norm_data[norm_data$Number.of.trades < 1000,]

norm_data$delta <- norm_data$High - norm_data$Low
M <- lm(delta ~ Number.of.trades, data = norm_data)
summary(M)
#Посмотрим, как это будет выглядеть на графике
CPI.df <- cbind(predict(M,interval ="conf"), predict(M,interval ="pred"))  
CPI.df <- CPI.df[,-4] 
colnames(CPI.df) <- c("Y_fit","CI_l","CI_u","PI_l","PI_u")
head(CPI.df)
matplot(norm_data$Number.of.trades, CPI.df, type = "l", 
        lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4),
        ylab = "Изменение цены",xlab="Кол-во сделок")
matpoints(norm_data$Number.of.trades,norm_data$delta , pch = 20)
#Построение доверительных интервалов CI.

#Параметрический метод
par <- summary(M)$coefficients
beta1 <- par[1, 1]
SE1 <- par[1, 2]
beta2 <- par[2, 1]
SE2 <- par[2, 2]
ci.lower1 <- beta1 - qt(0.975, df = 44)*SE1#df = n - p = 46 - 2 = 44
ci.upper1 <- beta1 + qt(0.975, df = 44)*SE1#p - число параметров
c(ci.lower1, ci.upper1)
ci.lower2 <- beta2 - qt(0.975, df = 44)*SE2#df = n - p = 46 - 2 = 44
ci.upper2 <- beta2 + qt(0.975, df = 44)*SE2#p - число параметров
c(ci.lower2, ci.upper2)
# Метод имитации
library(arm)
simulations <- sim(M, 1000)
(quantile(simulations@coef[,1],c(0.025, 0.975)))
(quantile(simulations@coef[,2], c(0.025, 0.975)))

#Part3.Оценка качества модели


summary(M)
norm_data$fit = fitted(M)

library(ggplot2)

p1 = ggplot(norm_data, aes(Number.of.trades, delta)) + geom_point() +
  geom_hline(aes(yintercept=mean(norm_data$delta)), color = "blue") +
  geom_segment(aes(x = Number.of.trades, y = delta, xend = Number.of.trades, yend = mean(norm_data$delta))) +
  ggtitle("Общая сумма квадратов TSS")

p2 = ggplot(norm_data, aes(Number.of.trades, delta)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_segment(aes(x = Number.of.trades, y = delta, xend = Number.of.trades, yend = fit)) +
  ggtitle("Сумма квадратов остатков RSS")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

#Part4. Полиномиальная и логистическая регрессия
M2 <- lm(norm_data$delta ~ norm_data$Number.of.trades + I(norm_data$Number.of.trades^2))
summary(M2)
log.ss1 <- nls(norm_data$delta ~ SSlogis(norm_data$Number.of.trades, phi1, phi2, phi3))
summary(log.ss1)
Rsquared <- 1 - var(residuals(log.ss1))/var(norm_data$Number.of.trades)