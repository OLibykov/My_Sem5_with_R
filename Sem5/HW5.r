#HW5
#Part1
# Получение ОМП для нормального распределения
#Генерируем 10000 экспериментов(считаем,  что они независимы) 
#из нормального распределения 
n <- 10000
p1 <- c(rnorm(n, mean = 2, sd = 1))
hist(p1, breaks = 20, freq = FALSE, col = "lightblue")
lines(density(p1, bw = 0.8), col = "red", lwd = 2)
#Забываем, что mean = 2 sd = 1
#Воспользуемся библиотекой Rstat4 (функцией mle)
#Создаем функцию оценки минимума -логарифма для нормального распределения
nll <- function(mymean, mysd) {
  y <- p1
  n/2 * log(2 * pi * mysd ^ 2) + 1/(2 * mysd^2) * sum ((y - mymean) ^ 2)
}
#Применяем функцию   mle - итерационный метод построения параметров
est <- mle(minuslogl =  nll, start = c(0, 5), method = "BFGS")
est

#Part2
#Генерируем небольшие выборки из нормального распределения
c1 <- c(rnorm(100, 0, 1))
c2 <- c(rnorm(50, 3, 5))
#Сортируем выьорку
c1 <- sort(c1)
c2 <- sort(c2)
#Пользуемся семинарской функцией приближения плотности и функции распределения
graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("Эмпирическая ФР и ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("Эмпирическая плотность и ", main_name))
  plot(density(x), lwd = 2, col = "blue", main = mn) 
  lines(x, pd, col = "red", lwd = 2)
  par(op)
}
#Оцениваем параметры ep1, ep2 с помощью fitdistr
(dof <- fitdistr(c1,"normal"))
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
graph_distr(c1, pnorm(c1, mean = ep1, sd = ep2),
            dnorm(c1, mean = ep1, sd = ep2),
            "нормальное распределение")
(dof <- fitdistr(c2,"normal"))
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]=
graph_distr(c2, pnorm(c2, mean = ep1, sd = ep2),
            dnorm(c2, mean = ep1, sd = ep2),
            "нормальное распределение")
#Оцениваем выборку по QQ графикам
par(mfrow = c(1, 1))
qqnorm(c1)
qqline(c1)
qqnorm(c2)
qqline(c2)
#Оцениваем выборку по аналогу метода огибающих(с доверительным интервалом)
library(car)
qqPlot(c1, dist = "norm", col = palette()[1], pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР")
qqPlot(c2, dist = "norm", col = palette()[1], pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР")
#Оцениваем выборку по 6 тестам
library(nortest)
dof <- fitdistr(c1, "normal")
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(c1, pnorm, ep1, ep2)
shapiro.test(c1)
ad.test(c1)
cvm.test(c1)
lillie.test(c1)
sf.test(c1)

library(nortest)
dof <- fitdistr(c2, "normal")
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(c2, pnorm, ep1, ep2)
shapiro.test(c2)
ad.test(c2)
cvm.test(c2)
lillie.test(c2)
sf.test(c2)


