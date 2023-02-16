
rectangle_method <- function(f, a, b, n){
  h = (b-a)/n
  
  #таблиця
  Df = data.frame(
    x_i = a,
    y_i= f(a),
    delta_y_i = 0,
    delta2_y_i = 0
  )
  
  for (i in 2:(n+1)){
    Df[i, 1] = Df[i-1, 1]+h
    Df[i, 2] = f(Df[i, 1])
    Df[i-1, 3] = Df[i, 2] - Df[i-1,2]
    if (i > 2)
      Df[i-2, 4] = Df[i-1, 3] - Df[i-2,3]
  }
  
  
  #повертає значення інтегралу та середнє арифметичне різниць другого порядку
  return(c(h*((Df[1,2]+Df[n+1,2])/2+sum(Df[2:n,2])), mean(Df[1:(n-1),4])))
}



f1 <-function(x) 1/(4-(sin(x))^2 +0.6*(cos(x))^2)

f2 <- function(x) sqrt(x)


Q1 <- rectangle_method(f1, a=0, b=pi/2, n=20)[[1]]

Q2 <- rectangle_method(f2, a=35, b=55, n=20)[[1]]

Z <- function(Q1, Q2) sin(Q1+Q2)/cos(Q1-Q2)+3*Q2-4*Q1

Z(Q1, Q2)




