library(readxl)
library(openxlsx)
library(zeallot)

# Задача максимизации прибыли с ограничениями
# Функция прибыли с ПФ Кобба-Дугласа


x = read.csv("C:/Users/Artyom/Desktop/Финансовый университет/Третий курс/Мат Методы/data4.csv", sep = ";")
c(A,a1,a2,w1,w2,I) %<-% x[1,]
x

trig = expression(-((32) * x1^((0.5)) * x2^((0.25))-(2)*x1-(5)*x2))
D.x1 = D(trig, "x1")
D.x2 = D(trig, "x2")
D.x1
D.x2
F <-function(x) {
x1 <-x[1]
x2 <-x[2]
-(32*x1^(0.5)*x2^(0.25)-2*x1-5*x2)}
# Функция, вычисляющая частные производные:
gr <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  c(-16*x1^(-0.5)*x2^(0.25)+2,-8*x1^(0.5)*x2^(-0.75)+5)}

# Безусловная оптимизация
res <- optim(c(1,50), F, gr, method = "BFGS")
res

x = constrOptim(c(1,1), F, gr, ui=rbind(c(-w1,-w2),c(1,0),c(0,1)), ci=c(-I,0,0))
x_val = x$par[1]
y_val = x$par[2]
res_val = x$value
x
val_nedeed = -res_val;val_nedeed
require(plot3D) 
# Функция прибыли
M <- mesh(seq(20, 100, length.out = 200), seq(10, 40, length.out = 100))
u <- M$x
v <- M$y
x <- u
y <- v
z <- 32 * x^(0.5) * y^(0.25)-2*x-5*y
surf3D(x, y, z, colvar = z, phi = 5, bty = "b2",theta = 270,
lighting = TRUE, ltheta = 60, colkey = TRUE, box = TRUE)
# Линии уровня и допустимое множество
x <- seq(0, 400, by=1)
y <- seq(0, 300, by=1)
f <- function(x,y) { 32 * x^(0.5) * y^(0.25)-2*x-5*y }
# Задаем функцию f
z <- outer(x, y, f)
image(x, y, z, col= heat.colors(12))

polygon(c(0, I/w1-w2*0/w1, 0),c(I/w2-w1*0/w2, 0, 0), density = NA, angle = 60,
border = c("darkblue", "yellow"),
col ="skyblue", lwd=3, lty="solid")
contour(x,y,z, col="green",add=TRUE,
levels=c(150,250,350,450),
method = "edge", vfont = c("sans serif", "plain"))

contour(x,y,z, levels=c(-res_val),col="red", lwd=5,add=TRUE,
method = "edge", vfont = c("sans serif", "plain"))

points(x_val,y_val,pch=16,cex = 1, col = "black")

text(x_val,y_val,col = "white", paste("X*(",x_val,y_val,")"), cex = .75, adj = c(1,1))
text(0,I/w2-w1*0/w2, col = "white", "A", cex = .75, adj = c(-0.7,-0.7))
text(I/w1-w2*0/w1,0,col = "white", "B", cex = .75, adj = c(0,-1))
text(0,0, "C", col = "white", cex = .75, adj = c(-0.7,-0.7))