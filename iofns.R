# Script:       io.R
# Authors:      Allen H Nugent, 2015+
# Last edit:    2016-03-10
# Last test:    2016-03-10
#
# Purpose:      Library of I/O functions not covered by R base (or not obviously covered by standard libraries).
#
# Tips & tricks:
#
#   readChar(fileName, file.info(fileName)$size)   #: reads entire file into one string.
#
#   paste(readLines(fileName), collapse = "\n")   #: reads entire file into one string with no newline chars.
#
#   read_file(filename)   #: reads entire file into one string with no newline chars. WRONG: still need gsub("[\r\n]", "", x) or gsub("\r?\n|\r", " ", x)
#
#
# Contents:
#
#   get.longest.line    Gets longest line in a (text) file of any size.
#
#   get.maxlinelength   Gets length of longest line in a (text) file of any size.
#
#   get.num.lines       Gets number of lines in a (text) file of any size.
#
#   read.header         Reads header lines of a data file into a string list.
#
#


library(readr)



get.num.lines <- function(filepath, safe.read = TRUE, buffersize = 20000) {

    # Gets number of lines in a (text) file of any size.
    # Mar'16
    
    
    if (safe.read)
    {
        n.lines <- 0
        ( while((n.lines.in <- length(read_lines(filepath, n_max = buffersize))) > 0 )
            n.lines <- n.lines + n.lines.in )
        return(n.lines)
    } else
    {
      #  this truncates at soft EOF ...
        cxn <- file(filepath, open = "r")
        n.lines <- 0
        ( while((n.lines.in <- length(readLines(cxn, buffersize))) > 0 )
            n.lines <- n.lines + n.lines.in )
        close(cxn)
    }
    
    return(n.lines)
}


get.longest.line <- function(filepath, safe.read = TRUE, buffersize = 20000) {
    
    # UNTESTED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # Gets longest line in a (text) file of any size.
    # Mar'16
    
    if (safe.read)
    {
        # UNFINISHED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    }
    # cxn <- file(filepath, open = "r")
    length.max <- 0
    longest.line <- ""
    # while(length(lines.in <- readLines(cxn, buffersize)) > 0 )
    while(length(lines.in <- read_lines(filepath, n_max = buffersize)) > 0 )
    {    
        n.lines.in <- length(lines.in)
        for (i.line in 1:n.lines.in)
        {            
            if (nchar(lines.in[i.line]) > length.max) 
            { 
                length.max <- nchar(lines.in[i.line])
                longest.line <- lines.in[i.line]
            }
        }
        lengths.in.max <- max(nchar(lines.in))
        if (lengths.in.max > length.max) 
        { 
            length.max <- nchar(lines.in[i.line])
            longest.line <- lines.in[i.line]
        }
    }
    # close(cxn)
    
    return(longest.line)
}


get.maxlinelength <- function(filepath, safe.read = TRUE, buffersize = 20000) {
    
    # Gets length of longest line in a (text) file of any size.
    # Mar'16
    
    if (safe.read)
    {
        length.max <- 0
        longest.line <- ""
        while(length(lines.in <- read_lines(filepath, n_max = buffersize)) > 0 )
        {   
            lengths.in.max <- max(nchar(lines.in))
            length.max <- max(c(length.max, lengths.in.max))
        }
    } else
    {
        cxn <- file(filepath, open = "r")
        length.max <- 0
        longest.line <- ""
        while(length(lines.in <- readLines(cxn, buffersize)) > 0 )
        {   
            lengths.in.max <- max(nchar(lines.in))
            length.max <- max(c(length.max, lengths.in.max))
        }
        close(cxn)
    }
    
    return(length.max)
}


read.header <- function(filepath, header.line.prefix = NA, n = 0) {
    
    # Allen H. Nugent, Aug'2015.
    # Reads header lines of a data file into a string list.
    # Purpose: 
    #   (1) Header length known: Store file header and prepare for subsequent data reads.
    #   (2) Header length unknown: Inspect a number of lines from top of file to determine
    #       header length and column structure.
    #
    # header.line.prefix is a character string which, 
    #   prefixing a line in the file, denotes a header line.
    
    if (n != 0) {   # number of header lines is known 'a priori'
        y <- readLines(filepath, n)
    }
    else {
        if (is.na(header.line.prefix)) {
            stop("function = \'read.header\': args -> WTF?")
        }
        nheadermaxguess <- 10
        yp <- readLines(filepath, nheadermaxguess)
        for (i in 1:nheadermaxguess) {
            if (yp[i][1] == header.line.prefix) {
                y[i] <- yp[i]
            }
            else {
                break
            }
        }
    }
    
#     con <- file('yourInputFile', 'r') 
#     outfile <- file('yourOutputFile', 'w') 
#     while (length(input <- readLines(con, n=1000) > 0){ 
#         for (i in 1:length(input)){ 
#             ......your one line at a time processing 
#         } 
#         writeLines(output, con=outfile) 
#     } 
#     
    
    return(y)
}
