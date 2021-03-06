# File:         cleanly.R
# Authors:      Allen H Nugent, Dec'15+
# Last edit:    2017-10-31
# Last test:    2017-10-26
# Prev edit:    2017-06-29
# Purpose:      Data cleaning and profiling functions. (Nb. SparkR data cleaning functions are defined in another library.)
#
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
# Contents:
#
#   column.counts       Returns count of cells in each column meeting specified criteria.
#
#   count.column        Used by column.counts(): Returns count of elements in the input vector containing the value 
#                       specified.
#
#   cscale              Applies rescaling to specified columns.
#
#   dirty.columns       Returns a list or a vector containing indices of all columns meeting specified criteria 
#                       (e.g. any/all rows contain NA/null, etc.).
#
#   filldates           Returns a data.frame with missing dates filled in.
#
#   format.path         Converts a Windows path to the format used in R.
#
#   get.col.num         Returns column number pertaining to the name provided.
#
#   get.col.nums        Returns a vector of column number pertaining to the names provided.
#  
#   lookup              Returns a vector of lookup values from lookupTable using keyVector as indices.
#
#   na.replace          Replaces NA within a vector with specified value. 
#
#   set.factorsp        Defines a factor based on a set of levels and an arbitrary sequence,
#                       and applies it to the input vector.
#
#   set.levels			Replaces a factor-like variable based on a set of levels and an arbitrary sequence,
#						and applies it to the input vector.
#
#   test.column         Used by dirty.columns(): Returns TRUE if the input vector contains any/all of the value 
#                       specified.
#
#
# See also:
#
#	strfns.R 
# 	numfns.R 
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# packages required:
#install.packages("xts") # Nb: not yet used -- zoo does not support data.frames
#install.packages("lubridate")
#install.packages("data.table")
#install.packages("ggplot2")



# TODO:
#
#   write test.row (to return a vector of all rows in the current column that meet the criteria)
#   write dirty.rows (to return a vector of all rows in the data frame that meet the criteria)

test.column <- function(v, find.what = "NA", find.how = "any")
{
    # (c) Allen H Nugent, 27/12/2015.
    #
    # Used by dirty.columns().
    # Returns TRUE if the input vector contains any/all of the value specified in find.what:
    #
    #   "NA"        test for NA
    #   "NULL"      test for nulls
    #   "ZERO"      test for 0
    #   "NEGATIVE"  test for < 0
    #   "#DIV/0!"   test for string "#DIV/0!" (as exported by Excel)
    # TODO:
    #   "NUMERIC"   test for numeric values
    #   "NON-NUMERIC"   test for non-numeric values
    #
    
    if(find.how == "any") { fun <- any }
    if(find.how == "all") { fun <- all }
    
    if(find.what == "NA")
    {
        found <- (fun(is.na(v)))
    }
    if(find.what == "NULL")
    {
        found <- (fun(is.null(v)))
    }  
    if(find.what == "ZERO")
    {
        found <- (fun(v == 0))
    }
    if(find.what == "NEGATIVE")
    {
        found <- (fun(v < 0))
    }
    if(find.what == "#DIV/0!")
    {
        found <- (fun(v == "#DIV/0!"))
    } 
    return(found)
}


dirty.columns <- function(df, find.what = "NA", find.how = "any", output = "list", verbose = FALSE) 
{
    # (c) Allen H Nugent, 27/12/2015.
    #
    # Returns indices of all columns meeting specified criteria.
    # Return type can be a list or a vector.
    
    # if (verbose) {
    #     print(paste0("dirty.columns(): output format = ", output))
    # }
    
    out.l <- list()
    for (i in 1:ncol(df)) {
        test <- test.column(df[ , i], find.what = find.what, find.how = find.how)
        
        if (verbose) { print(paste0("i = ", i, ", test = ", test)) }
        
        if (test)   #: add this column index to the list
        {
            out.l[length(out.l) + 1] <- i
        }
    }
    if (output == "list") {
        out <- out.l
    }
    if (output == "vector") {
        out <- as.numeric(out.l)
    }
    return(out)
}


count.column <- function(v, find.what = "NA")
{
    # (c) Allen H Nugent, 24/08/2016.
    #
    # Used by column.counts().
    # Returns count of elements in the input vector containing the value specified in find.what:
    #
    #   "NA"        test for NA
    #   "NULL"      test for nulls
    #   "ZERO"      test for 0
    #   "NEGATIVE"  test for < 0
    #   "#DIV/0!"   test for string "#DIV/0!" (as exported by Excel)
    # TODO:
    #   "NUMERIC"   test for numeric values
    #   "NON-NUMERIC"   test for non-numeric values
    #
    find.what <- toupper(find.what)
    
    count <- 0
    
    if(find.what == "NA")
    {
        count <- sum(is.na(v))
    }
    if(find.what == "NULL")
    {
        count <- sum(is.null(v))
    }  
    if(find.what == "ZERO")
    {
        count <- sum(v == 0)
    }
    if(find.what == "NEGATIVE")
    {
        count <- sum(v < 0)
    }
    if(find.what == "#DIV/0!")
    {
        count <- sum(v == "#DIV/0!")
    } 
    return(count)
}


column.counts <- function(df, find.what = "NA", output = "list", verbose = FALSE) 
{
    # (c) Allen H Nugent, 24/08/2016.
    
    # Returns count of cells in each column meeting specified criteria.
    # Return type can be a list or a vector.
    
    # if (verbose) {
    #     print(paste0("column.counts(): output format = ", output))
    # }
    
    out.l <- list()
    for (i in 1:ncol(df)) {
        count <- count.column(df[ , i], find.what = find.what)
        if (verbose) { print(paste0("i = ", i, ", count = ", count)) }
        out.l[length(out.l) + 1] <- count
    }
    if (output == "list") {
        out <- out.l
    }
    if (output == "vector") {
        out <- as.numeric(out.l)
    }
    return(out)
}


format.path <- function(inpath) {

    # Converts a Windows path to the format used in R.
    
    outpath <- sub("\\\\","/", inpath)
    return(outpath)
}


lookup <- function(keyVector, lookupTable) 
{
    # Returns a vector of lookup values from lookupTable using keyVector as indices.
    #
    # Assumptions:
    #
    #   1. lookupTable is a the data frame with:
    #           1st column: name = "code"; contents = key codes  
    #           2nd column: name = "category"; contents = values ot be looked up, returned.
    #
    #   2. If NAs are to be handled explicitly, lookupTable should map replacement value to an NA level in the "code" column.
    #       ##> BUGGY!					  
    #
    
    # get lookup values:
    outVector <- lookupTable$category[keyVector]
    
    # handle empty strings, missing values:
    # (Nb. empty strings should already have been converted to NA by the inital lookup step, above)
    # BUGGY: THROWS ERROR: outVector[is.na(outVector) | outVector == ""] <- lookupTable$category[is.na(lookupTable$code)]
	# NEW ...
    outVector[outVector == ""] <- NA
    if (sum(is.na(lookupTable$code)) > 0) {
        na_alias <- lookupTable$category[is.na(lookupTable$code)]
        outVector[is.na(outVector)] <- na_alias
    }
    
    return(outVector)
}


get.col.num <- function(df, col.name)
{
    # Returns column number pertaining to the name provided.
    return(which(colnames(df) == col.name))
}

get.col.nums <- function(df, col.names)
{    
    # Returns a vector of column number pertaining to the names provided.
    #X: return(sapply(df, get.col.num, col.names)) >> output includes 0 & 1 results
    #X: return(vapply(df, get.col.num, col.names))
    #X: col.nums <- get.col.num(col.names)
    col.nums <- integer(length(col.names))
    for (i in 1:length(col.names))
    {
        col.nums[i] <- get.col.num(df, col.names[i])
    }
    return(col.nums)
}


set.factorsp <- function(x, x.levels.str, x.factors.str)
{
    # Defines a factor based on a set of levels and an arbitrary sequence,
    # and applies it to the input vector.
    # Both the levels and factors are supplied as newline-separated strings.
    
    # parse the input strings into vectors:
    x.levels <- unlist(strsplit(x.levels.str, '\n'))
    x.factors <- as.integer(unlist(strsplit(x.factors.str, '\n')))
    
    # reorder levels according to order of factors (in case non-monotonic):
    x.levels <- x.levels[order(x.factors)]
    
    # convert to factor:
    f <- x.levels[x]
    #?: x <- factor(x.levels[x], x.levels, ordered = TRUE)
    x <- factor(x.levels[x], x.levels)
 
    return(x)   
}


set.levels <- function(x, x.levels.str, x.factors.str, ordered = FALSE)
{
    # Replaces a factor-like variable based on a set of levels and an arbitrary sequence,
    # and applies it to the input vector.
    # Both the levels and factors are supplied as newline-separated strings.
    
    # parse the input strings into vectors:
    x.levels <- unlist(strsplit(x.levels.str, '\n'))
    x.factors <- as.integer(unlist(strsplit(x.factors.str, '\n')))
    
    # reorder levels according to order of factors (in case non-monotonic):
    x.levels <- x.levels[order(x.factors)]

    # apply to input vector:
    f <- x.levels[x]
    
    if (ordered) {
        f <- factor(f, x.levels, ordered = TRUE)
    } else {
        f <- factor(f, x.levels)
    }

    return(f)
}


na.replace <- function (x, withval = 0) 
{
    # Replaces NA within a vector with specified value.
    # (Overcomes issue with replacing elements of a data.frame)
    
    x[is.na(x)] <- withval
    return(x)
}


filldates <- function(all_dates, d, col.date = 1, col.data = 2)
{
    # Returns a data.frame with missing dates filled in (as per all_dates).
    # Replaces NAs in data column with 0.
    
    d <- merge(all_dates, d, by = col.date, all.x = TRUE)
    d[, col.data] <- na.replace(d[, col.data], 0)
    return(d)
}


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


