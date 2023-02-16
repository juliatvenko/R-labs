#19.1
a1 = c(1, 4,
      -2, 3,
      7, 8)

b1 = c(1.1,4.2,
      -3.1, 4/3,
      2/7, -1)

A1 = matrix(a1, nrow=3, ncol = 2, byrow = TRUE) 
B1 = matrix(b1, nrow=3, ncol = 2, byrow = TRUE)

lambda = 0.25
mu = -2/3

lambda*A1 + mu*B1
lambda*A1 - mu*B1

(lambda*A1)%*%(mu*t(B1))

#19.2
a2 = c(1, 0, -2, 4,
       5, -1, 3, 4)

b2 = c(1, 0,
       3, -2, 
       7, 8,
       9, 0)

A2 = matrix(a2, nrow=2, ncol = 4, byrow = TRUE) 
B2 = matrix(b2, nrow=4, ncol = 2, byrow = TRUE)


A2%*%B2
B2%*%A2

#19.3
a3 = c(1, -2, 
       -1, 3)

b3 = c(-1, 4)

d3 = c(2, -1)

A3 = matrix(a3, nrow=2, ncol = 2, byrow = TRUE) 
B3 = matrix(b3, nrow=2, ncol = 1, byrow = TRUE)
D3 = matrix(d3, nrow=1, ncol = 2, byrow = TRUE)


C3 = 2*(B3%*%D3) - 4*(A3%*%A3)
C3

#19.4
a4 = c(2, 1, 0, 4,
       6, 7, -1, 3,
       2, 2, -1, 0)

A4 = matrix(a4, nrow=3, ncol=4, byrow = TRUE) 

t(A4)

#19.5
q = c(1, 1, 0, 5,
     1, 4, 2, 1,
     3, -1, 7, -1,
     5, 2, 6, -1)

Q = matrix(q, nrow=4, ncol=4, byrow = TRUE) 

det(Q)

#19.6

z = c(10, 20, -30,
      0, 10, 20, 
      0, 0, 10)
  
Z = matrix(z, nrow=3, ncol=3, byrow = TRUE) 

solve(Z)



