# золоте число
goldenRatio = (1 + sqrt(5)) / 2

# функція
f <- function(x) return (-exp(2+0.1*x))

# інтервал
a0 = -1
b0 = 1
# точність
eps = 0.01

#таблиця
Df = data.frame(
  k = 0, 
  a = a0, 
  b = b0, 
  L = b0 - a0,
  x1 = b0 - (b0 - a0) / goldenRatio, 
  x2 = a0 + (b0 - a0) / goldenRatio, 
  "f(x1)"= f(b0 - (b0 - a0) / goldenRatio), 
  "f(x2)"= f(a0 + (b0 - a0) / goldenRatio) 
)

k = 2
while (Df[k-1, 4] > eps) {
  
  Df[k, 1] = k - 1
  
  if (Df[k-1, 7] <= Df[k-1, 8]) {
    Df[k, 2] = Df[k-1, 5]
    Df[k, 3] = Df[k-1, 3]
  } else {
    Df[k, 2] = Df[k-1, 2]
    Df[k, 3] = Df[k-1, 6]
  }
  
  Df[k, 4] = Df[k, 3] - Df[k, 2]
  
  Df[k, 5] = Df[k, 3] - Df[k, 4]/ goldenRatio
  Df[k, 6] = Df[k, 2] + Df[k, 4]/ goldenRatio
  
  Df[k, 7] = f(Df[k, 5])
  Df[k, 8] = f(Df[k, 6])
  
  k = k + 1
} 


golden_search <- function(a, b, eps){
  while (abs(b - a) > eps) {
    x1 = b - (b - a) / goldenRatio;
    x2 = a + (b - a) / goldenRatio;
    if (f(x1) <= f(x2)) {
      a = x1;
    } else {
      b = x2;
    }
    
  } 
  return ((a + b) / 2)
}



x_max = (Df[k-1,2] + Df[k-1,3])/2
y_max = f(x_max)


print(Df)
print(sprintf("x = %f  y = %f", x_max, y_max))

