`Part A`
# seed = 10
lcm <- function(N, x0, a, c, m) {
  x <- rep(0, N) # generate numbers between 0 and N
  x[1] <- x0
  for (i in 2:N) 
    x[i] <- (a * x[i - 1] + c) %% m #ith number is the LCM
  u <- x / m # generating numbers between 0 and 1
  return(u)
}

N <- 10  # Number of random numbers to generate
x0 <- 10 # Initial value of x (seed)
a <- 13  # Multiplier
c <- 0  # Increment
m <- 64  # Modulus

u <- lcm(N, x0, a, c, m) 
print(u)

--------------------------------------------------------------------------------

# seed = 4
lcm <- function(N, x0, a, c, m) {
  x <- rep(0,N)
  x[1] <- x0
  for (i in 2:N) x[i] <- (a*x[i-1]+c)%%m
  u <- x/m
  return(u)
}

N <- 10  # Number of random numbers to generate
x0 <- 4 # Initial value of x (seed)
a <- 13  # Multiplier
c <- 0  # Increment
m <- 64  # Modulus

u <- lcm(N, x0, a, c, m)
print(u)

--------------------------------------------------------------------------------
# histogram of 1000 numbers
# N = 1000
# seed = 4

lcm <- function(N, x0, a, c, m) {
    x <- rep(0,N)
    x[1] <- x0
    for (i in 2:N) x[i] <- (a*x[i-1]+c)%%m
    u <- x/m
    return(u)
}

N <- 1000 #Number of random numbers to generate
x0 <- 10 # Initial value of x (seed)
a <- 13  # Multiplier
c <- 0  # Increment
m <- 64  # Modulus

u <- lcm(N, x0, a, c, m)

hist(u)

--------------------------------------------------------------------------------
`Part B`

set.seed(31023)
a=runif(100000)
hist(a)
plot(density(a))
plot(ecdf(a))
acf(a)

------------------------------------------------------------------
`Part C`

A <- 4.61 # mean
B <- 0.32 # sd
X <- 1000 # variates
q <- qnorm(runif(X),A,B)
hist(q)
summary(q)

r <- rnorm(X,A,B)
hist(r)
summary(r)
shapiro.test(r)

acf(q)
qqnorm(q)
qqline(q, col = "red")
shapiro.test(q)