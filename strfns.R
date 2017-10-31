# Script:       strfns.R
# Authors:      Allen H. Nugent, 2015+
# Last edit:    2017-03-22
# Last test:    2017-03-22
#
# Purpose:      Library of general utility functions, many of which address 
#               surprising omissions from the R {base} system.
#
# Contents:
#
#   crange                  Formats a range for printing (e.g. as r[1] + " to " + r[2]).
#
#   cround                  Rounds a number to the specified number of decimal places and formats accordingly.
#
#   get.regex.pattern       Gets the appropriate regex argument for the desired character string.
#
#   left                    Returns the left N characters of the argument.
#
#   replaceforeignchars     Replaces foreign characters (UNDER EVALUATION !!!)
#
#   right                   Returns the right N characters of the argument.
#
#   strrev                  Reverses a string (or a vector of strings).
#
#   trim                    Returns string without leading or trailing whitespace (or user-specified character).
#
#   trim.leading            Returns string without leading whitespace (or user-specified character).
#
#   trim.trailing           Returns string without trailing whitespace (or user-specified character).
#


# From: f3lix (http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r) ...


get.regex.pattern <- function(char)
{
    # Gets the appropriate regex argument for the desired character string.
    #
    # Nb. assumes regex will be invoked with FIXED=FALSE.
    
    if (char == ' ') {
        return("\\s")
    }
    
    # TODO: handle other special characters !!!
    
    return(char)
    
}


trim.leading <- function (x, pattern = " ")  
{
    # Returns string without leading whitespace (or user-specified character).
    
    p <- ifelse(is.null(pattern), get.regex.pattern(" "), get.regex.pattern(pattern))
    y <- (sub(paste0("^", p, "+"), "", x))
    return(y)
}


trim.trailing <- function (x, pattern = ' ') 
{
    # Returns string without trailing whitespace (or user-specified character).
    
    p <- ifelse(is.null(pattern), get.regex.pattern(" "), get.regex.pattern(pattern))
    y <- (sub(paste0(p, "+$"), "", x))
    return(y)
}


trim <- function (x, pattern = ' ') 
{
    # Returns string without leading or trailing whitespace (or user-specified character).
    
    p <- ifelse(is.null(pattern), get.regex.pattern(" "), get.regex.pattern(pattern))
    y <- gsub(paste0("^", p, "+|", p, "+$"), "", x)
    return(y)
}


left <- function(s, N = 1) 
{
  # Returns the left N characters of s.
  
  L <- nchar(s)
  t <- substr(s, 1, N)
  return(t)
}


right <- function(s, N = 1) 
{
  # Returns the right N characters of s.
  
  L <- nchar(s)
  t <- substr(s, (L - N + 1), L)
  return(t)
}


cround <- function(x, ndec)
{
    if (ndec == 0)
    {
        fmt1 <- "%d"
    } else 
    {
        fmt1 <- paste0("%.", ndec, "f")
    }
    result <- sprintf(fmt = fmt1, round(x, ndec))
    return(result)
}


replaceforeignchars <- function(x)
{
    # a function to replace foreign characters
    # author: krishnamn (http://stackoverflow.com/questions/17517319/r-replacing-foreign-characters-in-a-string)
    
    require(gsubfn)
    
    x <- gsub("s","s",x)
    x <- gsub("o","oe",x)
    x <- gsub("z","z",x)
    x <- gsub("?","ss",x)
    x <- gsub("?","y",x)
    x <- gsub("?","a",x)
    x <- gsub("?","a",x)
    x <- gsub("?","a",x)
    x <- gsub("?","a",x)
    x <- gsub("?","a",x)
    x <- gsub("?","a",x)
    x <- gsub("?","ae",x)
    x <- gsub("?","c",x)
    x <- gsub("?","e",x)
    x <- gsub("?","e",x)
    x <- gsub("?","e",x)
    x <- gsub("?","e",x)
    x <- gsub("?","i",x)
    x <- gsub("?","i",x)
    x <- gsub("?","i",x)
    x <- gsub("?","i",x)
    x <- gsub("?","d",x)
    x <- gsub("?","n",x)
    x <- gsub("?","o",x)
    x <- gsub("?","o",x)
    x <- gsub("?","o",x)
    x <- gsub("?","o",x)
    x <- gsub("?","o",x)
    x <- gsub("?","oe",x)
    x <- gsub("?","u",x)
    x <- gsub("?","u",x)
    x <- gsub("?","u",x)
    x <- gsub("?","u",x)
    x <- gsub("?","y",x)
    x <- gsub("?","y",x)
    x <- gsub("g","g",x)
    
    return(x)
}

to.plain <- function(s) {
    # improvement (author: G. Grothendieck)
    
    # 1 character substitutions
    old1 <- "sz????????????????????????????"
    new1 <- "szyaaaaaaceeeeiiiidnooooouuuuy"
    s1 <- chartr(old1, new1, s)
    
    # 2 character substitutions
    old2 <- c("o", "?", "?", "?")
    new2 <- c("oe", "ss", "ae", "oe")
    s2 <- s1
    for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
    
    s2
}


strrev <- function(x)
{
    # Reverses a string (or a vector of strings)
    
    xrev <- sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
    return(xrev)
}


crange <- function(x, sep = "to", space = TRUE)
{
    # Formats a range for printing (e.g. as r[1] + " to " + r[2]).
    #
    # Either a range or the original vector can be passed.
    #
    # Anything can be used for sep; "-" and "~" would be the most likely single-character choices.

    # c1 <- x[1]
    # if (NROW(x) != 2) 
    # {
    #     print("WARNING: crange(): Input is not a range - output may be erroneous!")
    #     c2 <- x[NROW(x)]
    # }
    # else
    # {
    #     c2 <- x[2]
    # }
    
    c12 <- range(x)
    c1 <- c12[1]
    c2 <- c12[2]
    
    if (space) {
        spc <- " "
    } else {
        spc <- ""
    }
    
    outstr <- paste0(c1, spc, sep, spc, c2)
}


