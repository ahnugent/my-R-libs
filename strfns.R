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
    x <- gsub("ß","ss",x)
    x <- gsub("þ","y",x)
    x <- gsub("à","a",x)
    x <- gsub("á","a",x)
    x <- gsub("â","a",x)
    x <- gsub("ã","a",x)
    x <- gsub("ä","a",x)
    x <- gsub("å","a",x)
    x <- gsub("æ","ae",x)
    x <- gsub("ç","c",x)
    x <- gsub("è","e",x)
    x <- gsub("é","e",x)
    x <- gsub("ê","e",x)
    x <- gsub("ë","e",x)
    x <- gsub("ì","i",x)
    x <- gsub("í","i",x)
    x <- gsub("î","i",x)
    x <- gsub("ï","i",x)
    x <- gsub("ð","d",x)
    x <- gsub("ñ","n",x)
    x <- gsub("ò","o",x)
    x <- gsub("ó","o",x)
    x <- gsub("ô","o",x)
    x <- gsub("õ","o",x)
    x <- gsub("ö","o",x)
    x <- gsub("ø","oe",x)
    x <- gsub("ù","u",x)
    x <- gsub("ú","u",x)
    x <- gsub("û","u",x)
    x <- gsub("ü","u",x)
    x <- gsub("ý","y",x)
    x <- gsub("ÿ","y",x)
    x <- gsub("g","g",x)
    
    return(x)
}

to.plain <- function(s) {
    # improvedment (author: G. Grothendieck)
    
    # 1 character substitutions
    old1 <- "szþàáâãäåçèéêëìíîïðñòóôõöùúûüý"
    new1 <- "szyaaaaaaceeeeiiiidnooooouuuuy"
    s1 <- chartr(old1, new1, s)
    
    # 2 character substitutions
    old2 <- c("o", "ß", "æ", "ø")
    new2 <- c("oe", "ss", "ae", "oe")
    s2 <- s1
    for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
    
    s2
}

