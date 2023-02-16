#рівняння
f = function(x, y) sqrt((sin(x^2+y^3))^2+4)

Euler_method <- function(a0, b0, y0, h)
{
  #таблиця
  Df = data.frame(
    x_i = a0,
    y_i= y0,
    "f(x_i, y_i)" = f(a0, y0)
  )
  
  #заповнення таблиці
  for (i in 2:((b0-a0)/h+1)){
    Df[i,1] = Df[i-1,1] + h
    Df[i,2] = Df[i-1,2] + h*Df[i-1,3]
    Df[i,3] = f(Df[i,1], Df[i,2])
    
  }
  
  plot(Df[,1], Df[,2], type = 'l')
}


Runge_Kutt_method <- function(a0, b0, y0, h)
{
  #таблиця
  Df = data.frame(
    i = 0,
    x_i = a0,
    y_i= y0,
    "f(x_i, y_i)" = f(a0, y0),
    k = h*f(a0, y0)
  )
  
  x <- c(a0)
  y <- c(y0)
  
  #заповнення таблиці
  for (i in 2:(4*(((b0-a0)/h)+1))){
    #i
    Df[i, 1] = floor((i-1)/4)
    
    #x, y
    j = i %% 4
    
    if (j == 1) {
      Df[i,2] = Df[i-4,2] + h
      Df[i,3] = Df[i-4,3] + (Df[i-4,5]+2*(Df[i-3,5]+Df[i-2,5])+Df[i-1,5])/6
      x <- c(x, Df[i,2])
      y <- c(y, Df[i,3])
    } else if (j == 2) {
      Df[i, 2] = Df[i-1, 2]+ h/2
      Df[i, 3] = Df[i-1, 3] + Df[i-1, 5]/2
    } else if (j == 3) {
      Df[i, 2] = Df[i-2, 2]+ h/2
      Df[i, 3] = Df[i-2, 3] + Df[i-1, 5]/2
      x <- c(x, Df[i,2])
      y <- c(y, Df[i,3])
    } else if (j == 0) {
      Df[i, 2] = Df[i-3, 2]+ h
      Df[i, 3] = Df[i-3, 3] + Df[i-1, 5]
    }
    
    #f(x,y)
    Df[i, 4] = f(Df[i, 2], Df[i, 3])
    
    #k
    Df[i, 5] = h*Df[i, 4]
    
  }
  
  plot(x, y, type = 'l')
  
}

#межі інтервалу
a0 = 0
b0 = 2
y0 = 1
#крок
h = 0.01

Euler_method(a0, b0, y0, h)


Runge_Kutt_method(a0, b0, y0, h)


