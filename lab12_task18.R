 y <- function(x){
   return((1+x)/(1+sqrt(abs(x)*exp(-x)+abs(sin(x)))))
}

g <- function(x){
  if (x > 0) {
    return((sin(x))^2+(1+x)/(1+(cos(x))^2))
  } else {
    return((1+x^2)^(1/3))
  }
}

z <- function(x){
  if (x < 0){
    return(abs(x)*exp(-2*x)/(1+x^2))
  } else if (x >= 1){
    return ((1+sin(x))/(1+x) + 3*x)
  } else {
    return(sqrt(1+x^2))
  }
}

x = seq(-1.4, 1.4, 0.2)


plot(x, y(x), type = 'l')
plot(x, lapply(x, g), type = 'l')
plot(x, lapply(x, z), type = 'l')
 
 