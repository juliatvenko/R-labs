#рівняння
f <- function(x, y) return(eval(x+y))

#межі інтервалу
a0 = 0
b0 = 1
y0 = 0
#крок
h = 0.1


#таблиця
Df = data.frame(
  x_i = a0,
  y_i= y0,
  "f(x_i, y_i)" = f(a0, y0)
)

#заповнення таблиці
for (i in 2:((b0-a0)/h+1)){
  
  Df[i,1] = Df[i-1,1] + h
  Df[i,2] = Df[1,2] + h*Df[1,3]
  if (i > 2)
    Df[i,2] = Df[i-1,2] + h*(3*Df[i-1,3]-Df[i-2,3])/2
  Df[i,3] = f(Df[i,1], Df[i,2])
  
}

#вивід таблиці
print(Df)
