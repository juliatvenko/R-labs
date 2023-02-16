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
  
  return(Df)
  
}


dyhotonomy <- function(a0, b0, f, eps){
  #таблиця
  Df = data.frame(
    x = (b0-a0)/2,
    "f(x)"= f((b0-a0)/2),
    a = a0,
    b = b0,
    delta = abs(b0-a0)
  )
  
  #заповнення таблиці
  i = 1
  while(abs(Df[i,4]-Df[i,3])>= eps){
    Df[i,1] = (Df[i,4]+Df[i,3])/2
    Df[i,2] = f(Df[i,1])
    if(Df[i,2]<0){
      Df[i+1,3]=Df[i,3]
      Df[i+1,4]=Df[i,1]
    }
    else{
      Df[i+1,3]=Df[i,1]
      Df[i+1,4]=Df[i,4]
    }
    Df[i,5]= abs(Df[i,4]-Df[i,3])
    i= i+1
    
  }
  
  Df[i,1] = (Df[i,4]+Df[i,3])/2
  Df[i,2] = f(Df[i,1])
  Df[i,5] = abs(Df[i,4]-Df[i,3])
  
  return(Df)
  
}

khord <- function(a0, b0, f, y, eps) {
  
  f_der2 <- function(x) eval(D(D(y, 'x'), 'x'))
  
  if (f(a0)*f_der2(a0) > 0 ){
    x0 = b0
    x_func <- function(x) {x - (x-a0)*f(x)/(f(x)-f(a0))}
    
  } else{
    x0 = a0
    x_func <- function(x) {x - (b0-x)*f(x)/(f(b0)-f(x))}
  }
  
  #таблиця
  Df = data.frame(
    x = x0, 
    "f(x)"= f(x0), 
    delta = 1 
  )                                 
  
  i=2
  while (Df[i - 1, 3] > eps){
    
    Df[i, 1] = x_func(Df[i-1, 1])
    Df[i, 2]= f(Df[i, 1]) 
    Df[i, 3] = abs(Df[i, 1]-Df[i-1, 1]) 
    i = i + 1
    
  }
  
  return(Df)
  
}

newton <- function(a0, b0, f, y, eps) {
  
  f_der1 <- function(x) eval(D(y, 'x'))
  
  f_der2 <- function(x) eval(D(D(y, 'x'), 'x'))
  
  if (f(a0)*f_der2(a0) > 0 ){
    x0 = a0
  } else{
    x0 = b0
  }
  
  Df = data.frame (
    x = x0,
    "f(x)" = f(x0),
    "f'(x)" = f_der1(x0),
    h = f(x0)/f_der1(x0),
    delta = abs(b0-a0)
  )
  
  i = 2
  while(Df[i-1, 5] > eps){
    Df[i, 1] = Df[i-1, 1] - Df[i-1, 4]
    Df[i, 2] = f(Df[i, 1])
    Df[i, 3] = f_der1(Df[i, 1])
    Df[i, 4] =  Df[i, 2]/Df[i, 3]
    Df[i, 5] = abs(Df[i, 1] - Df[i-1, 1])
    i = i + 1
  }
  
  return(Df)
}

y <- expression((10*x-2)/(3+x^2)-2*cos(2*x)-x^(1/4))

f <- function(x) (10*x-2)/(3+x^2)-2*cos(2*x)-x^(1/4)

x <- function(x) x - (4*f(x)/(5+16*sin(2)))

eps = 0.0000001


simple_iteration(x0=0.6, f=f, x=x, eps=eps)
khord(a0=2.5, b0=3, f=f, y=y, eps=eps)
newton(a0=3.5, b0=4, f=f, y=y, eps=eps)
dyhotonomy(a0=5.4, b0=5.6, f=f, eps=eps)
















