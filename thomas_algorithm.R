#checking the correctness of the dimensions of the entered vectors
find_size <- function(vector_A, b)
{
  if (length(vector_A) == (length(b))^2)
    return (length(b))
  else
    print("Wrong format!")
}

#checking the matrix for tridiagonality
is_tridiagonal_matrix <- function(matrix_A, n_size)
{
  n_size <- find_size(vector_A, b)

  for (i in 1:n_size)
    for (j in 1:n_size)
      if ( i != j && i != j+1 && i+1 != j)
        if (matrix_A[i, j] != 0)
          return(FALSE)
  return(TRUE)
}

# transformation of a vector into a tridiagonal matrix
find_matrix <- function(vector_A, n_size)
{
  matrix_A <- matrix(vector_A, byrow = TRUE, nrow = n_size, ncol = n_size)
  if (is_tridiagonal_matrix(matrix_A, n_size))
    return(matrix_A)
  else
    print("Matrix is not tridiagonal")
}

# thomas algorithm
thomas_algorithm <- function(vector_A, b)
{
  n_size <- find_size(vector_A, b)

  matrix_A <- find_matrix(vector_A, n_size)

  # vectors with matrix elements
  s <- matrix_A[row(matrix_A)==col(matrix_A)]
  u <- matrix_A[row(matrix_A)+1==col(matrix_A)]
  l <- matrix_A[row(matrix_A)==col(matrix_A)+1]

  # vectors with alpha and beta coefficients
  alpha <- c(-u[1]/s[1])
  beta <- c(b[1]/s[1])

  # calculation of alpha and beta coefficients
  for (i in 2:n_size)
  {
    alpha[i] = -u[i]/(l[i-1]*alpha[i-1]+s[i])
    beta[i] = (b[i]-l[i-1]*beta[i-1])/(l[i-1]*alpha[i-1]+s[i])
  }

  # vector with solutions
  x <- c(beta[n_size])

  # calculation of solutions in reverse
  for (i in 1:(n_size-1))
  {
    x[i+1] = alpha[n_size-i]*x[i]+beta[n_size-i]
  }

  # reverse the solutions in the reverse order
  x <- rev(x)

  #printing solutions of the system
  for (i in 1:n_size)
    print(sprintf("x%d = %f", i-1, x[i]))

  return(x)
}
