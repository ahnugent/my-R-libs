# Script:       strfns.R
# Authors:      Allen H. Nugent, 2015+
# Last edit:    2015-09-03
# Last test:    2015-09-03
#
# Purpose:      Library of general utility functions, many of which address 
#               surprising omissions from the R {base} system.
#
# Contents:
#
#   cround      Rounds a number to the specified number of decimal places and 
#               formats accordingly.
#
#   left        Returns the left N characters of the argument.
#
#   right       Returns the right N characters of the argument.
#


# From: f3lix (http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r) ...

trim.leading <- function (x)  {
    # returns string w/o leading whitespace:
    return (sub("^\\s+", "", x))
    }

trim.trailing <- function (x) {
    # returns string w/o trailing whitespace:
    return (sub("\\s+$", "", x))
    }

trim <- function (x) {
    # returns string w/o leading or trailing whitespace:
    return (gsub("^\\s+|\\s+$", "", x))
}


left <- function(s, N = 1) {
  
  # Allen H. Nugent, Aug'2015.
  # Returns the left N characters of s.
  
  L <- nchar(s)
  t <- substr(s, 1, N)
  return(t)
}


right <- function(s, N = 1) {
  
  # Allen H. Nugent, Aug'2015.
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
    x <- gsub("�","ss",x)
    x <- gsub("�","y",x)
    x <- gsub("�","a",x)
    x <- gsub("�","a",x)
    x <- gsub("�","a",x)
    x <- gsub("�","a",x)
    x <- gsub("�","a",x)
    x <- gsub("�","a",x)
    x <- gsub("�","ae",x)
    x <- gsub("�","c",x)
    x <- gsub("�","e",x)
    x <- gsub("�","e",x)
    x <- gsub("�","e",x)
    x <- gsub("�","e",x)
    x <- gsub("�","i",x)
    x <- gsub("�","i",x)
    x <- gsub("�","i",x)
    x <- gsub("�","i",x)
    x <- gsub("�","d",x)
    x <- gsub("�","n",x)
    x <- gsub("�","o",x)
    x <- gsub("�","o",x)
    x <- gsub("�","o",x)
    x <- gsub("�","o",x)
    x <- gsub("�","o",x)
    x <- gsub("�","oe",x)
    x <- gsub("�","u",x)
    x <- gsub("�","u",x)
    x <- gsub("�","u",x)
    x <- gsub("�","u",x)
    x <- gsub("�","y",x)
    x <- gsub("�","y",x)
    x <- gsub("g","g",x)
    
    return(x)
}

to.plain <- function(s) {
    # improvedment (author: G. Grothendieck)
    
    # 1 character substitutions
    old1 <- "sz����������������������������"
    new1 <- "szyaaaaaaceeeeiiiidnooooouuuuy"
    s1 <- chartr(old1, new1, s)
    
    # 2 character substitutions
    old2 <- c("o", "�", "�", "�")
    new2 <- c("oe", "ss", "ae", "oe")
    s2 <- s1
    for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
    
    s2
}
