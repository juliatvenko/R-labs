simple_iteration <- function(x0, eps, f, x){
  Df = data.frame (
    x = x0,
    "f(x)" = f(x0),
    delta = 1
  )
  
  i = 2
  
  while(Df[i - 1, 3] > eps){
    Df[i, 1] = x(Df[i - 1, 1])
    Df[i, 2] = f(Df[i, 1])
    Df[i, 3] = abs(Df[i, 1] - Df[i-1, 1])
    
    i = i + 1
  }
  
  print(Df)
  
}

eps = 0.001
x0_1 = -0.9
f_1 <- function(x) x^3-0.1*x^2+0.4*x+1.2
x_1 <- function(x) (-5/18)*(x^3-0.1*x^2-3.2*x+1.2)


x0_2 = 0
f_2 <- function(x) x^2+4*sin(x)
x_2 <- function(x) x-(3*f_2(x)/(6*sqrt(3)+pi)) 

simple_iteration(x0=x0_1 , eps=eps, f=f_1, x=x_1)

simple_iteration(x0=x0_2 , eps=eps, f=f_2, x=x_2)
