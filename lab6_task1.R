x0 <- c(25.2, 35.8, 34.1, 29.8, 32, 32.2, 23.6, 34.6, 33.9, 27, 37.6, 28.5)
x1 <- c(73, 79, 70, 76, 73, 73, 59, 77, 74, 82, 80, 87)
x2 <- c(263.3, 273.5, 305.6, 295.3, 316.7, 224.3, 277.7, 277, 194.8, 230.9, 276.8, 267.1)

Df = data.frame(
  "x0" = x0,
  "x1" = x1,
  "x2" = x2,
  "x1**2" = x1^2,
  "x2**2" = x2^2,
  "x0*x1" = x0*x1,
  "x0*x2"=x0*x2,
  "x1*x2"=x1*x2)

regr_A <- c (length(x0), colSums(Df)[[2]], colSums(Df)[[3]], 
             colSums(Df)[[2]], colSums(Df)[[4]], colSums(Df)[[8]], 
             colSums(Df)[[3]], colSums(Df)[[8]], colSums(Df)[[5]])

regr_b <- c(colSums(Df)[[1]], colSums(Df)[[6]], colSums(Df)[[7]])
regr_coeff <- solve(matrix(regr_A, byrow = TRUE, nrow = 3, ncol = 3), regr_b)


print("Коефіцієнти функції регресії")
paste(sprintf("b0 = %f", regr_coeff[1]))
paste(sprintf("b1 = %f", regr_coeff[2]))
paste(sprintf("b2 = %f", regr_coeff[3]))

