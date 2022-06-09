#install.packages("plot3D",dependencies=TRUE)
library(readxl)
library(openxlsx)
library(zeallot)

cat("Задача оптимального выбора потребителя")
cat("")

# Вычисление производной
trig = expression(log(x-7)+8*log(y-1))
D.x = D(trig, "x")
D.y = D(trig, "y")
cat("Проверка")
cat("")

D.x
D.y

x = read.csv("C:/Users/Artyom/Desktop/Финансовый университет/Третий курс/Мат Методы/data3.csv", sep = ";")
c(p,q,i) %<-% x[1,]
cat("Проверка")
cat("")
p
q
i

F <- function(x) {
x1 <-x[1]
x2 <-x[2]
  log(x1-7)+8*log(x2-1)
}

# Функция, вычисляющая частные производные (градиент):
gr <- function(x) {
x1 <- x[1]
x2 <- x[2]
  c(-1/(x1-7), -8/(x2-1))
}

# Оптимизация
res = constrOptim(c(10,50), F, gr,ui=rbind(c(-p,-q),c(1,0),c(0,1)), ci=c(-i,7,1))
res
x_val = res$par[1]
y_val = res$par[2]
y_val
res_val = res$value
cat("Оптимизация")
res_val
cat("")


#Геометрическая интерпритация задачи
require(plot3D)
# Функция полезности
M <- mesh(seq(20, 100, length.out = 200),
seq(10, 40,length.out = 100))
u <- M$x
v <- M$y
x <- u
y <- v
z <- log(x-7)+8*log(y-1)
surf3D(x, y, z, colvar = z, phi = 5, bty = "b2",theta = 400,
lighting = TRUE, ltheta = 60, colkey = TRUE, box = TRUE)

# Линии уровня и допустимое множество
x <- seq(-5, 200, by=1)
x_min = min(x)
y <- seq(1, 800, by=1)
y_min = min(y)
f <- function(x,y) {log(x-7)+8*log(y-1)}
z <- outer(x, y, f)
image(x, y, z, col= heat.colors(20))

polygon(c(i/p-q*y_min/p, y_min, y_min),c(x_min, i/q-p*x_min/q, x_min),  density = NA, angle = 60, 
        border = c("darkgreen", "yellow"), col ="green", lwd=3, lty="solid")

contour(x,y,z, col="white",add=TRUE, levels=c(25,28,30,32,36,38),
method = "edge", vfont = c("sans serif", "plain"))

contour(x,y,z, col="blue", add=TRUE, levels= -res_val,
        method = "edge", vfont = c("sans serif", "plain"))
points(x_val,y_val ,pch=15,cex = 1, col = "black")

# Функция спроса
# X=p(y-1)+56q/8q
X <- function(x) {
p <-x
  ((8*10-56)*q+p)/p}
x <- seq(-5, 100, by = 1)
p <- x
X(p)
y <- X(p)
plot(x,y,type="l",lty=1, fg="pink4",
xlim=c(0,20),ylim=c(0,150),ylab="X(p)",xlab="p",
lwd = 3,col="violetred3",
col.axis="palevioletred4",
main = "Функция спроса",col.main = "red")
abline(h = 0, v = 0, col = "gray40")

