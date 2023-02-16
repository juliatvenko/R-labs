n = 10
a0 = 0
b0 = 1.5

h = (b0-a0)/n


f = function(x){
  return(eval(2*(x^18)*exp(sqrt(x^4+3))*(cos(2*x^4))^3*log2(x^6+0.15)))
}

#таблиця
Df = data.frame(
  x_i = a0,
  y_i= f(a0),
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

print("n =")
print(n)
print("Result delta")
print(mean(Df[1:(n-1),"delta2_y_i"]))
print("Result integral")
print(h*((Df[1,2]+Df[n+1,2])/2+sum(Df[2:n,2])))
print("Built-in integral")
print(integrate(f, a0, b0)[[1]])

#print(Df)

