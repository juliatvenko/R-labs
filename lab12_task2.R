#рівняння
f = function(x){
  return(x^2-sin(pi*x))
}

y <- expression(x^2-sin(pi*x))

#похідна другого порядку
f_der2 <- function(x) eval(D(D(y, 'x'), 'x'))

#межі інтервалу
a0 = 0.5
b0 = 1
#шукана точність кореня
eps = 0.001

x0 = a0 - f(a0)*(b0 - a0)/(f(b0) - f(a0))

#таблиця
Df = data.frame(
  x = x0, #1
  "f(x)"= f(x0), #2
  a = a0, #3
  b = b0, #4
  'f"(x)' = f_der2(x0), #5
  "f(a)" = f(a0), #6
  "f(b)" = f(b0), #7
  delta = abs(x0-a0) #8
)                                 

i=2
while (Df[i - 1, 8] > eps){
  if (Df[i - 1, 5]*Df[i - 1, 6] > 0){
    Df[i, 1] = Df[i-1, 1]- f(Df[i-1, 1])*(Df[i-1, 1] - Df[i-1, 3])/(f(Df[i-1, 1]) - f(Df[i-1, 3]))
    Df[i, 3] = Df[i-1, 3]
    Df[i, 4] = Df[i, 1]
  } else{
    Df[i, 1] = Df[i-1, 1]- f(Df[i-1, 1])*(Df[i-1, 4] - Df[i-1, 1])/(f(Df[i-1, 4]) - f(Df[i-1, 1]))
    Df[i, 3] = Df[i, 1]
    Df[i, 4] = Df[i-1, 4]
  }
  
  Df[i, 2]= f(Df[i, 1]) 
  Df[i, 5] = f_der2(Df[i, 1]) 
  Df[i, 6] = f(Df[i, 3]) 
  Df[i, 7] = f(Df[i, 4]) 
  Df[i, 8] = abs(Df[i, 1]-Df[i-1, 1]) 
  
  i = i + 1
  
}

#вивід таблиці
print(Df)



