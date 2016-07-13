## 05/07/16


# Poisson distribution ...

pois <- function (x, lamda) {
    y <- (lamda ^ x * exp(-x)) / factorial(x)
    return(y)
}

x <- seq(from = 0, to = 10, by = 0.1)
y <- pois(x, 1)

plot(x, y, type = "l")

