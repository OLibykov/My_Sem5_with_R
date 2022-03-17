#Part2
#Нагенерирум данные из нормального распределения
c1 <- c(5260, 5470, 5640, 6180, 6390, 6515,
        6805, 7515, 7515, 8230, 8770, 7000, 8000)
sd(c1)
#Проверка данных на нормальность их распределения
shapiro.test(c1)
c2 <- rnorm(n = 13, mean = mean(c1), sd = sd(c1))
c3 <- rnorm(n = 5000, mean = 0, sd = 1)
#Используем t - критерий Стьюдента и оценку его мощности
#Для маленьких данных(two-sided)
t.test(c1, mu = 6800, alternative = "two.sided")
t.test(c1, mu = 5000, alternative = "two.sided")
#greater
t.test(c1, mu = 6800, alternative = "greater")
t.test(c1, mu = 5000, alternative = "greater")
#less
t.test(c1, mu = 6800, alternative = "less")
t.test(c1, mu = 5000, alternative = "less")
# Поменяем параметр conf.level
t.test(c1, mu = 6800, conf.level = 0.99)
t.test(c1, mu = 5000, conf.level = 0.90)
#Оценка мощности критериев
#Интересно посмотреть, как в зависимости от delta будем меняться мощность
power.t.test(n = 13, delta = 1000, sd = sd(c1), sig.level = 0.05, 
             type  = "one.sample")
power.t.test(n = 13, delta = 700, sd = sd(c1), sig.level = 0.05, 
             type  = "one.sample")
power.t.test(n = 13, delta = 1300, sd = sd(c1), sig.level = 0.05, 
             type  = "one.sample")
power.t.test(n = 5000, delta = 5.0, sd = 1, sig.level = 0.05, 
             type  = "one.sample")
power.t.test(n = 5000, delta = 0.01, sd = 1, sig.level = 0.05, 
             type  = "one.sample")
power.t.test(n = 5000, delta = 0.5, sd = 1, sig.level = 0.05, 
             type  = "one.sample")
power.t.test(n = 5000, delta = 2, sd = 1, sig.level = 0.05, 
             type  = "one.sample")
#Посмотрим на работу теста Уилкоксона-Манна-Уитни
wilcox.test(c1, mu = 5000)
wilcox.test(c1, mu = 6800)

wilcox.test(c1, c2, paired = TRUE)
wilcox.test(c1, c2, paired = FALSE)

#Посмотрим на работу тестов-проверки гипотезы об однородности
#Для этого подключим библиотеку MASS
library(MASS)
data(genotype)
attach(genotype)
var.test(c1, c2)
leveneTest(Wt~Litter)
bartlett.test(Wt~Litter)
fligner.test(Wt~Litter)
#Part3. Свои данные
shapiro.test(Wt)
#Шапиро тест дает p-value = 0.4432, что вполне допускает гипотезу о 
#нормальной распределенности генеральной выборки.
#t Критерий Стьюдента
t.test(Wt, mu = 55, alternative = "two.sided", conf.level = 0.95)
t.test(Wt, mu = 55, alternative = "less", conf.level = 0.99)
t.test(Wt, mu = 55, alternative = "greater", conf.level = 0.90)
#power-test(размер выборки 61)
power.t.test(n = 61, delta = 1, sd = sd(Wt), sig.level = 0.05, 
             type  = "one.sample")
power.t.test(n = 61, delta = 2, sd = sd(Wt), sig.level = 0.05, 
             type  = "one.sample")
power.t.test(n = 61, delta = 4, sd = sd(Wt), sig.level = 0.05, 
             type  = "one.sample")

#тест Уилкоксона-Манна-Уитни
wilcox.test(Wt, mu = 55)
#Тесты Фишера, Левене, Бартлетта, Флигнера-Килина
var.test(c1, Wt)
leveneTest(Wt~Mother)
bartlett.test(Wt~Mother)
fligner.test(Wt~Mother)

