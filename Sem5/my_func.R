# �������  �������� ����������� �������
stat_param <- function(x){
  aver <- mean(x);
  stdev <- sd(x);
  c(MEAN = aver, SD = stdev)}
# ������������ return() �� �����������

stat_param(1:10)

# �������� �� ���������
power <- function(x, n = 3){ x^n }
power(2)
power(2,2)

my_exampl <- function(n, func_trans){
  # ��������� ���������� �������������� ��������
  x <- runif(n);
  abs(func_trans(x))
}

# ��� ������� ���������� � �������� ���������
my_exampl(5, log)

# ������� ��� ��������� ���� ���� ��������
compare <- function(x, y){ 
  nl <- length(x); n2 <- length(y)
  if(nl != n2) {
    if(nl  > n2){
      z = (nl - n2)
      cat("������ ������ ����� �� ", z, " ��������� 6����� \n") }
    else {
        z = (n2 - nl)
        cat("������ ������ ����� �� ", z, " ��������� 6����� \n") } }
  else {
    cat("���������� ��������� ��������� ", nl, "\n") } 
}

x <- c(1:4)
y <- c(1:9)
compare(x, y)