# Riemann sum with partitions of the same length.
# Comment/uncomment or edit the code to try alternatives.
# If NaNs are produced and no result, try changing bounds or plot interval.
# ------------------------------------------------------------------------------

n <- 10 # number of partitions
tagfrac <- 0.5 # fraction of a partition for a block's height, from positive side

f <- function(x){sqrt(1-x^2)}
# f <- function(x){1+x}
# f <- function(x){2*x}
# f <- function(x){x^2}

a <- 0 # lower bound
b <- 1 # upper bound

delta_x <- (b-a)/n
summa <- 0
for (k in c(1:n)){
  t_k <- a + (k-tagfrac)*delta_x
  summa <- summa + f(t_k)*delta_x
}
cat('Riemann sum =',summa,'\n')

# ------------------------------------------------------------------------------
# VISUALISATION:

plot_min <- a-2 # plot interval minimum
plot_max <- b+2 # plot interval maximum

X <- seq(plot_min, plot_max, delta_x/10)
plot(X, f(X), type='l', xlab ='x', ylab ='f(x)')
ybottom <- 0
for (k in c(1:n)){
  t_k <- a + (k-tagfrac)*delta_x
  xleft <- a + (k-1)*delta_x
  xright <- a + k*delta_x
  ytop <- f(t_k)
  rect(xleft, ybottom, xright, ytop, col=rgb(0, 1, 0, 0.4))
}