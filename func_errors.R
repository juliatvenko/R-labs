delta <- function(x) {
  if ((x %% 1) != 0) {
    decimal_places = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  }
  else {
    decimal_places = 0
  }

  return (5/10^(decimal_places+1))
}

func_errors <- function(f, x, delta_x=0, y, delta_y=0, z, delta_z=0){

  if (missing(delta_x)&&(!missing(x))){
    delta_x = delta(x)
    cat(sprintf("delta_x = %f", delta_x), '\n')
  }

  if (missing(delta_y)&&(!missing(y))){
    delta_y = delta(y)
    cat(sprintf("delta_y = %f", delta_y), '\n')
  }

  if (missing(delta_z)&&(!missing(z))){
    delta_z = delta(z)
    cat(sprintf("delta_z = %f", delta_z), '\n')
  }

  abs_delta_f <- abs(eval(D(f,'x')))*delta_x + abs(eval(D(f,'y')))*delta_y + abs(eval(D(f,'z')))*delta_z
  rel_delta_f <- abs(abs_delta_f / eval(f))

  cat(sprintf("Value of function f = %f", eval(f)), '\n')
  cat(sprintf("Absolute error ∆(f*) = %f", abs_delta_f), '\n')
  cat(sprintf("Relative error δ(f*) = %f ", rel_delta_f), '\n')
}






