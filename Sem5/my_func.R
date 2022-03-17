# Примеры  создания собственных функций
stat_param <- function(x){
  aver <- mean(x);
  stdev <- sd(x);
  c(MEAN = aver, SD = stdev)}
# Использовать return() не обязательно

stat_param(1:10)

# Значения по умолчанию
power <- function(x, n = 3){ x^n }
power(2)
power(2,2)

my_exampl <- function(n, func_trans){
  # Генерация равномерно распределенных значений
  x <- runif(n);
  abs(func_trans(x))
}

# Имя функции передается в качестве параметра
my_exampl(5, log)

# Функция для сравнения длин двух векторов
compare <- function(x, y){ 
  nl <- length(x); n2 <- length(y)
  if(nl != n2) {
    if(nl  > n2){
      z = (nl - n2)
      cat("Первый вектор имеет на ", z, " элементов 6ольше \n") }
    else {
        z = (n2 - nl)
        cat("Второй вектор имеет на ", z, " элементов 6ольше \n") } }
  else {
    cat("Количество элементов одинаково ", nl, "\n") } 
}

x <- c(1:4)
y <- c(1:9)
compare(x, y)