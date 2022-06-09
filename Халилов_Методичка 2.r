#install.packages("plot3D",dependencies=TRUE)
library(readxl)
library(openxlsx)
library(zeallot)

cat("Задача о максимизации прибыли монополиста")
cat()

#экспорт данных варианта из csv файла
x = read.csv("C:/Users/Artyom/Desktop/Финансовый университет/Третий курс/Мат Методы/data2.csv", sep = ";")
cat("Проверка записи данных")
cat()
x
x_1 = list(x[1,])
c(a,b,c,d,k,l,m,n) %<-% x[1,]

#Функция для минимизации
F <-function(x) {
  q1 <-x[1]
  q2 <-x[2]
  -((1/b - k)*q1*q1 + (1/d - m)*q2*q2-a/b*q1 - c/d*q2 - l*q1*q2 - n)
}

#Функция, вычисляющя частные производные (градиент) функции
gr <- function(x) {
  q1 <- x[1]
  q2 <- x[2]
  c(-(2*(1/b-k)*q1-l*q2-a/b),
    -(2*(1/d-m)*q2-l*q1-c/d))
}

res <- optim(c(0.5,0.5), F, gr, method = "BFGS")
cat("Безусловная минимизация")
cat()
res

# Вычисление градиента и гессиана исследуемой функции в точке эсктремума
x_val = res$par[1]
y_val = res$par[2]
Deriv2Dn <- function (expr,x=0, y=0){ 
  M <- c(x=x,y=y)
  value <- eval(expr)
  grad <- c(x=0,y=0)
  grad["x"] <- eval(D(expr, "x"))
  grad["y"] <- eval(D(expr, "y"))
  hess <- array(0, c(2L, 2L), list(c("x", "y"), c("x", "y")))
  hess["x","x"] <- eval(D(D(expr, "x"), "x"))
  hess["y","y"] <- eval(D(D(expr, "y"), "y"))
  hess["x","y"] <- hess["y","x"] <- eval(D(D(expr, "x"), "y"))
  Delta1<-hess["x","x"]
  Delta2<- hess["x","x"]*hess["y","y"]-hess["x","y"]^2
  Rez <- list(Function = expr, Point = M, Value_Function= value,
  Gradient= grad, Hessian = hess,
  Delta1=Delta1,Delta2=Delta2)
  return(Rez)
}
cat("Результат функции Deriv2Dn")
cat()
Funct = Deriv2Dn(substitute((1/b - k)*x**2 + ((1/d - m)*y**2) - ((a/b)*x) - ((c/d)*y) - ((l)*x*y) - ((n))),x=x_val,y=y_val)
Funct
cat("Оптимальный план (q1*,q2*): ")
Funct$Point
cat("Максимальная прибыль: ")
Funct$Value_Function


#Геометрическая интерпритация модели
require(plot3D)
#Функция прибыли
M <- mesh(seq(0, 8, length.out = 100), seq(0,8, length.out = 100))
u <- M$x
v <- M$y
x <- u
y <- v
z <- (1/b - k)*x**2 + ((1/d - m)*y**2) - ((a/b)*x) - ((c/d)*y) - ((l)*x*y) - ((n))

# Функция прибыли - поверхности
surf3D(x, y, z, colvar = z, phi = 5, bty = "b2",theta = 200,
lighting = TRUE, ltheta = 800, colkey = TRUE, box = TRUE)

# Линии уровня
x <- seq(0, 8, by=0.01)
y <- seq(0, 8, by=0.01)
f <- function(x,y) {(1/b - k)*x**2 + ((1/d - m)*y**2) - ((a/b)*x) - ((c/d)*y) - ((l)*x*y) - ((n))}
z <- outer(x, y, f)
image(x, y, z, col= heat.colors(20))

# Линии уровня - разные
contour(x,y,z, col="white",add=TRUE, nlevels = 9,method = "edge", vfont = c("sans serif", "plain"))

# Точка максимума к прибыли
points(x_val,y_val,pch=16,cex = 1, col = "black")
text(x_val,y_val, paste("M*",round(x_val),round(y_val)), adj=c(-0.25,-0.25))

