# Part1
my_matr <- matrix(seq(1, 7, 2), nrow = 3, ncol = 3, byrow = TRUE)
my_matr

rownames(my_matr) <- seq(1,3)
colnames(my_matr) <- c("A", "B", "C")

my_matr

my_matr <- t(my_matr)
my_matr

#Part2
a1 <- seq(1,20,5)
a2 <- c(1, 2, 3, 4)
a3 <- rep(1, 10)

my_mat <- rbind(a1, a2, a3)
my_mat

dim(my_mat)
#Part3
class(my_mat[2, 2])
is.numeric(my_mat[2, 2])
is.logical(my_mat[2, 2])
is.integer(my_mat[2, 2])
as.logical(my_mat[2, 2] > 0)
as.logical(my_mat[2, 2] == " ")

#Part4

frame_of_data <- data.frame(head(read.csv(file.choose()), 10))
frame_of_data <- frame_of_data[ , -c(1)]
frame_of_data

list_of_data <- list(frame_of_data)
list_of_data
str(list_of_data)

#Part5
#i will use only 1 columns


my_func1 <- frame_of_data[, 9]
my_func1


my_time <- seq(1, 10)
plot(my_time, my_func1, xlab = "Минуты", ylab = "Кол-во сделок", type = "l")
plot(my_time, my_func1, xlab = "Минуты", ylab = "Кол-во сделок", type = "h")

#part6
#Проверка будет происходить по таблице, по списку аналогично
ok = TRUE
for (i in 1:dim(frame_of_data)[1]){
  for (j in 1:dim(frame_of_data)[2]){
    if (frame_of_data[i, j] < 0) {
      print("Найден не подходящий элемент")
      ok = FALSE
      break
    }
  }
  if (!ok) break
}
ok = TRUE
for (i in 1:dim(frame_of_data)[1]){
  for (j in 1:dim(frame_of_data)[2]){
    if (frame_of_data[i, j] < 30000 && frame_of_data[i, j] > 1000) {
      print("Найден не подходящий элемент")
      ok = FALSE
      break
    }
  }
  if (!ok) break
}
