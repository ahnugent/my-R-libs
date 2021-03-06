# Script:       numfns.R
# Authors:      Allen H Nugent
# Created:      2016-10-21
# Last edit:    2016-10-21
# Last test:    2016-12-21
#
# Purpose:      Library of general functions, utilities, and data structures for basic analytics and numerical analysis.
#
# Contents:
#
#	cscale 				Applies scaling to specified columns.
#
#   get.ndim            Gets number of columns in vector or data frame (compensates for R's ludicrous omission).
#
#   get.nrow            Gets number of rows in vector or data frame (compensates for R's ludicrous omission).
#
#   is.all.integer      Returns TRUE if all members of the input vector are integral.
#
#   is.wholenumber      Returns TRUE if argument is a whole number.
#
#   numdec              Returns the number of decimal places in a number.
#
#   numsig              Returns the number of significant figures in a number.
#
#	percentdiff			Returns percentage difference between the arguments.
#
#--------------------------------------------------------------------------------------------


percentdiff <- function(x1, x2, absolute = FALSE, percent = TRUE, ndec = 2) {
    
    #Returns percentage difference between the arguments.
    
    result <- x2 - x1
    if (absolute) {
        result <- abs(result)
    }
    
    result <- result / abs(x1)
    
    if (percent) {
        result <- result * 100
    }
    
    return(cround(result, 2))
}


get.nrow <- function(data) {
    
    n <- nrow(data)
    if (is.null(n)) { 
        n <- NROW(data)
    }
    return(n)
}


get.ndim <- function(data) {
    
    if (is.null(nrow(data))) {
        ndims <- 1
    } else {
        ndims <- length(data)
    }
    return(ndims)
}


numdec <- function(x) 
{
    # Returns the number of decimal places in a number.
    
    if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
        return(0)
    }
}


numsig <- function(x)
{
    # Returns the number of significant figures in a number.
    
    # Nb. Argument can be a string (to preserve trailing zeros).
    
    #   untested !!!!!!!!!!!!!!!!
    
    s1 <- trim(as.character(x))
    s1 <- sub('^[+-$', '', s1)
    s1 <- sub('[.,]', '', s1)
    
    return(nchar(s1))
}


is.all.integer <- function(x)
{   
    # Returns TRUE if all members of the input vector are integral.
    
    if ((sum(sapply(x, is.numeric)) != NROW(x))) {
        out <- FALSE
    } else {
        out <- (sum(sapply(x, is.wholenumber)) == NROW(x))
    }
    
    return(out)
}


is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) 
{
    # Returns TRUE if argument is integral (i.e. a whole number).
    
    return(abs(x - round(x)) < tol)
}


cscale <- function (d, col.names, scales) {
    
    # Applies scaling to specified columns.
    
    ncols <- length(col.names)
    if (length(scales) < ncols) {
        scales <- rep(scales, ncols)
    }
    
    for (i in 1:ncols) {
        icol <- which(names(d) == col.names[i])
        d[, icol] <- d[, icol] * scales[i]
    }
    
    return(d)
}


