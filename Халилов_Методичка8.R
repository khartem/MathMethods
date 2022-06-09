#install.packages("lpSolveAPI")
library(lpSolveAPI)

#Ввод строк матрицы
row1 <- as.numeric(unlist(strsplit(readline(prompt = "Введите первую строку матрицы (числа через пробел): "), split = " ")))
row2 <- as.numeric(unlist(strsplit(readline(prompt = "Введите вторую строку матрицы (числа через пробел): "), split = " ")))
row3 <- as.numeric(unlist(strsplit(readline(prompt = "Введите третью строку матрицы (числа через пробел): "), split = " ")))
row1;row2;row3

#Построение модели с задачей максимизации и указанием ограничений
M <- make.lp(ncol=3)
name.lp(M,"Matr-Game") 
colnames(M)<-c("w1","w2","w3") 
lp.control(M,sense="max")$sense 
set.objfn(M,c(1,1,1)) 
add.constraint(M,row1,"<=", 1) 
add.constraint(M,row2,"<=", 1) 
add.constraint(M,row3,"<=", 1) 
rownames(M)<-c("A","B","C") 
M

solve.lpExtPtr(M)
get.variables(M) #Оптимальный план
get.objective(M) #Оптимальное значение целевой функции

w1_opt<-get.variables(M)[1]
w2_opt<-get.variables(M)[2]
w3_opt<-get.variables(M)[3]
F_max<-get.objective(M)
w1_opt;w2_opt;w3_opt
F_max

#Анализ оптимального решения и проверка выполнения ограничений
b<-get.constr.value(M);b #Заданные ограничения
b_opt<-get.constraints(M);b_opt #Реальный расход
round(abs(b-b_opt),10) #Дефицит

#Оценка устойчивости коэффициентов целевой функции
min<-get.sensitivity.obj(M)$objfrom;min
max<-get.sensitivity.obj(M)$objtill;max

#Диапазон коэффициентов целевой функции, сохраняющие оптимальность
cbind(min,max)

#Оптимальные стратегии второго игрока и цена игры
Sumw<-w1_opt+w2_opt+w3_opt
y1<-w1_opt/Sumw
y2<-w2_opt/Sumw
y3<-w3_opt/Sumw
Nu_A<-1/Sumw
y1;y2;y3
Nu_A

#Решение двойственной задачи
u1_opt<-get.sensitivity.rhs(M)$duals[1]
u2_opt<-get.sensitivity.rhs(M)$duals[2]
u3_opt<-get.sensitivity.rhs(M)$duals[3]
u1_opt;u2_opt;u3_opt

#Оптимальные стратегии первого игрока
Sumu<-u1_opt+u2_opt+u3_opt
x1<-u1_opt/Sumu
x2<-u2_opt/Sumu
x3<-u3_opt/Sumu
x1;x2;x3

cat("Выводы")
cat()
cat(x1,x2,x3, "- оптимальная стратегия первого игрока")
cat()
cat(y1, y2, y3, "- оптимальная стратегия второго игрока")
cat()
cat(Nu_A, "- цена игры")
