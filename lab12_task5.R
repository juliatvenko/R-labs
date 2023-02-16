simple_iteration <- function(a0,  b0, M, eps, f){
  
  x <- function(x) { x - f(x)/M }
  
  Df = data.frame (
    x = x(a0),
    "f(x)" = f(x(a0)),
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

f <- function(x) x - sin(x)

simple_iteration(a0=0,  b0=1, M=0.99, eps=0.001, f=f)