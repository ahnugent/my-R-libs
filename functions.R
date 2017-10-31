# File:         functions.R
# Authors:      Allen H Nugent, Apr'16
# Authors:      Allen H. Nugent, 2015+
# Last edit:    2017-10-31
# Last test:    ?
# Purpose:      Math and stats functions.
#


# Poisson distribution ...

pois <- function (x, lamda) {
    y <- (lamda ^ x * exp(-x)) / factorial(x)
    return(y)
}

x <- seq(from = 0, to = 10, by = 0.1)
y <- pois(x, 1)

plot(x, y, type = "l")

