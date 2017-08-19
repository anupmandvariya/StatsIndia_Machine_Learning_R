# Read Data
heights.weights <- read.csv('./data/01_heights_weights_genders.csv')

# Linear Model
coef(lm(Weight ~ Height, data = heights.weights))
#(Intercept) Height
#-350.737192 7.717288

# Optimization function
height.to.weight <- function(height, a, b)
{
  return(a + b * height)
}

# Check Error
squared.error <- function(heights.weights, a, b)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(errors ^ 2))
}

# Error calculations for a, b
for (a in seq(-1, 1, by = 1))
{
  for (b in seq(-1, 1, by = 1))
  {
    print(squared.error(heights.weights, a, b))
  }
}

# General-purpose optimization based on 
# Nelder–Mead, quasi-Newton and conjugate-gradient algorithms.
optim(c(0, 0),
      function (x)
      {
        squared.error(heights.weights, x[1], x[2])
      })
#$par
#[1] -350.786736    7.718158
#
#$value
#[1] 1492936
#
#$counts
#function gradient 
#     111       NA 
#
#$convergence
#[1] 0
#
#$message
#NULL

# Error respective to coefficient 'a'
a.error <- function(a)
{
  return(squared.error(heights.weights, a, 0))
}

# Error plot 
curve(sapply(x, function (a) {a.error(a)}), from = -1000, to = 1000)

# Error respective to coefficient 'b'
b.error <- function(b)
{
  return(squared.error(heights.weights, 0, b))
}

curve(sapply(x, function (b) {b.error(b)}), from = -1000, to = 1000)

# Ridge Regularization
ridge.error <- function(heights.weights, a, b, lambda)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(errors ^ 2) + lambda * (a ^ 2 + b ^ 2))
}

lambda <- 1

optim(c(0, 0),
      function (x)
      {
        ridge.error(heights.weights, x[1], x[2], lambda)
      })

#$par
#[1] -340.434108    7.562524
#
#$value
#[1] 1612443
#
#$counts
#function gradient 
#     115       NA 
#
#$convergence
#[1] 0
#
#$message
#NULL

# Ridge Error respective to coefficient 'a' and lambda
a.ridge.error <- function(a, lambda)
{
  return(ridge.error(heights.weights, a, 0, lambda))
}
curve(sapply(x, function (a) {a.ridge.error(a, lambda)}), from = -1000, to = 1000)

# Ridge Error respective to coefficient 'b' and lambda
b.ridge.error <- function(b, lambda)
{
  return(ridge.error(heights.weights, 0, b, lambda))
}
curve(sapply(x, function (b) {b.ridge.error(b, lambda)}), from = -1000, to = 1000)

# Absolute Error
absolute.error <- function(heights.weights, a, b)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(abs(errors)))
}

# Absolute Error respective to coefficient 'a'
a.absolute.error <- function(a)
{
  return(absolute.error(heights.weights, a, 0))
}

curve(sapply(x, function (a) {a.absolute.error(a)}), from = -1000, to = 1000)

# Absolute Error respective to coefficient 'b'
b.absolute.error <- function(b)
{
  return(absolute.error(heights.weights, 0, b))
}

curve(sapply(x, function (b) {b.absolute.error(b)}), from = -1000, to = 1000)