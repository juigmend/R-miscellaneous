

# Maclaurin-series approximation to the sine function centered at zero
# Comment/uncomment or edit the code to try alternatives 

n <- 2 # number of iterations minus one
int_min <- -pi # interval minimum
int_max <- pi # interval maximum

X <- seq(int_min,int_max,0.01)
f_x <- X
g_x <- X
for (i in seq(1,length(X))) {
  x <- X[i]
  summa <- 0
  for (m in seq(0,n)) {
    k <- 2*m + 1
    # summa <- summa + cos(m*pi)*x^k / factorial(k)
    summa <- summa + ((-1)^m)*x^k / factorial(k)
  }
  f_x[i] <- summa
  g_x[i] <- sin(x)
}

error <- f_x - g_x
mse <- mean(error^2)
cat('RMSE =',sqrt(mse))

plot(X,g_x,col='blue','type' = 'l',xlab = "x", ylab = "y")
lines(X,f_x,col='red')
legend('topleft', legend = c('f(x) = Maclaurin approx.','g(x) = sin(x)'),
       col = c('red', 'blue'), lty = 1, lwd = 2)
