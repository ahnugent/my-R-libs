# File:         grafix.R
# Authors:      Allen H Nugent, Apr'16
# Authors:      Allen H. Nugent, 2015+
# Last edit:    2017-10-31
# Last test:    2017-06-10
# Purpose:      Reusable, high-level graphics functions.
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
# Contents:
#
#   apply.colourmap         Creates a vector of colours, mapped to a specified data column, 
#                           using a specified palette.
#                           
#   get.data.colour         Maps a colour palette to a vector of continuous or discrete numeric data.
#
#   col.as.number           Converts a string hexadecimal (as used in hsv values) to an integer.
#
#   plotlist                UNFINISHED: Plots a list of time series in a grid.
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# packages required:


col.as.number <- function(strhexcol) {
    
    # Converts a string hexadecimal (as used in hsv values) to an integer.
    
    colornbr <- as.numeric(sub("#", "0x", strhexcol))
    return(colornbr)
}


apply.colourmap <- function(df.in, columns.to.keep, column.to.colour, column.with.comments = 0, 
                            levels, fn.palette, do.barplot = TRUE, filepath = NULL)
{
    # Creates a vector of colours, mapped to a specified data column using a specified palette.
    #
    # Allen H Nugent, Jan'16+.
    #
    # Optional output includes:
    #
    #   a barplot depicting the colours as mapped to the data;
    #
    #   a csv file containing a subset of the data.frame, including specified columns as well as the new 
    #   (colour) vector and a comments vector; this is intended for colouring store-based geographic regions
    #   in a choropleth.
    #
    # Parameters:
    #
    #   df.in                   the input data.frame
    #
    #   columns.to.keep         the columns to be retained in the csv output
    #
    #   column.to.colour        the column of data to be mapped to colours
    #   
    #   column.with.comments    the column containing comments (to be used as hover hints in csv consumer app)
    #
    #   levels                  the intervals (created using cut()) for stratifying the input data prior to colour-mapping
    #
    #   fn.palette              a palette vector (e.g. created by colorRampPalette()) for mapping to the data.
    #
    # Usage Notes:
    #
    #    1. fn.palette and levels should have the same dimension.
    #
    #    2. See get.data.colour() for details of colour mapping (including how to create fn.palette).
    #
    
    if (typeof(column.to.colour) == "character")
    {
        col.colour <- which(names(df.in) == column.to.colour)
    } 
    else  # will fail if not numeric:
    {
        col.colour <- column.to.colour
    }
    #D print(paste0("col.colour = ", col.colour))
    
    if (typeof(column.with.comments) == "character")
    {
        col.comments <- which(names(df.in) == column.with.comments)
    } 
    else  # will fail if not numeric:
    {
        col.comments <- column.with.comments
    }
    #D print(paste0("col.comments = ", col.comments))
    
    dat <- cut(x = df.in[, col.colour], breaks = levels)
    dat.colour <- get.data.colour(dat, fn.palette, n.colours = NROW(levels), method = "discrete")
    #D print("dat.colour:")
    #D print(dat.colour)
    
    if(do.barplot)
    {
        barplot(rep(1, nrow(df.in)), col = dat.colour, space = 0, axes = FALSE, border = NA)
    }
    
    if (!is.null(filepath))
    {
        df.out <- cbind(subset(df.in, select = columns.to.keep), dat.colour)
        if(col.comments != 0)
        {
            df.out <- cbind(df.out, df.in[, col.comments])
        }
        write.csv(df.out, file = filepath, row.names = FALSE)
    }
    
    return(dat.colour)
}


# get.data.colour <- function(x, fn.palette, n.colours = 7, method = "continuous", 
#                             centre = "zero", x.min = NULL, x.max = NULL)
# {
#     # Maps a colour palette to a vector of continuous or discrete numeric data.
#     # AH Nugent, Jan'16+.
#     #
#     # Parameters:
#     #
#     #   fn.palette      a palette vector (e.g. created by the colorRampPalette() fn.).
#     #
#     #   n.colours       number of colours to actually use (<= NROW(fn.palette)).
#     #   
#     #   method          determines how colours map to data:
#     #       = "continuous"  input data are continuous; data will be scaled and mapped to the nearest colour;
#     #       = "discrete"    input data are levels (1 to n.colours); colours are exactly mapped to levels.
#     #
#     #   centre          (method="continuous") determines how centre of palette maps to data distribution:
#     #       = "zero"        the middle of the palette will be at x = 0;
#     #       = "auto"        the middle of the palette will be determined by the range of the input data.
#     #
#     #   x.min           (method="continuous") the effective minimum for mapping to the first colour.
#     #                   A hard limit is applied to input data so that x < x.min will still get mapped.
#     #
#     #   x.max           (method="continuous") the effective maximum for mapping to the last colour.
#     #                   A hard limit is applied to input data so that x > x.max will stil lget mapped.
#     #
#     # Usage:
#     # 
#     #   # create custom palette:
#     #   palette.fn <- colorRampPalette(c(rgb(1,0,0,0.5), rgb(1,1,1,0.5), rgb(0,1,0,0.5)), alpha=TRUE)
#     #   # create input data to be coloured:
#     #   xtest <- c(1,2,3,4,5,6,7)
#     #   gc <- get.data.colour(xtest, palette.fn)
#     #   gc
#     #   # plot the data:
#     #   barplot(rep(1, 7), col = gc, space = 0, border = NA)
#     #   # now squeeze the extrema:
#     #   gc <- get.data.colour(xtest, palette.fn, n.colours = 5, x.min = 2, x.max = 6)
#     #   gc
#     #   barplot(rep(1, 7), col = gc, space = 0, border = NA)
#     #
#     
#     if (method == "continuous")
#     {    
#         # compute limits if not specified ...
#         if(is.null(x.min))
#         {
#             x.min <- min(x)
#         }
#         if(is.null(x.max))
#         {
#             x.max <- max(x)
#         }
#         
#         # 19/04/16:
#         if (centre == "auto")
#         {
#             # do nothing
#         }
#         
#         if (centre == "zero")
#         {
#             # save this for a future optional parameter:::
#             #         mean.abs <- mean(abs(c(x.min, x.max)))
#             #         x.min <- mean.abs * sign(growth.min)
#             #         x.max <- mean.abs
#             
#             # define extreme values as 10th & 90th percentiles (to minimise leverage of outliers) ...
#             q <- quantile(x, probs = seq(0, 1, 0.1))
#             extreme10 <- q[2]
#             extreme90 <- q[10]
#             if (extreme10 * extreme90 < 0)  #: can't have symmetry unless there are data on either side of 0 
#             {
#                 # need a symmetric scale, so the mean absolute value of the extremes will be the half-width ...
#                 mean.extreme <- mean(abs(c(extreme10, extreme90)))
#                 x.min <- -mean.extreme
#                 x.max <- mean.extreme
#             }
#         }
#         
#         # apply hard limits ...
#         x[x < x.min] <- x.min
#         x[x > x.max] <- x.max
#         
#         # map input colour palette function based on specified number of colours and data range ...
#         i.out <- 1 + (n.colours - 1) * (x - x.min) / (x.max - x.min)
#         pal <- fn.palette(n.colours)
#         #D:    print(i.out)
#         x.out <- pal[i.out]
#         #D:    print(x.out)
#     }
#     
#     if (method == "discrete")
#     {
#         pal <- fn.palette(n.colours)   #: TODO: what happens if this <> NROW(x) ?
#         i.out <- unclass(x)
#         x.out <- pal[i.out]
#         #         print("palette:")
#         #         print(pal)
#         #         print("i.out:")
#         #         print(i.out)
#         #         print("x.out:")
#         #         print(x.out)
#     }
#     
#     return(x.out)
# }
# 

# Custom colour mapping for enhanced control of colour .................................................

# Problem: The above approach remaps the colour palette to the input vector, so no two 
# invocations will exhibit the same colour scale (unless the input data have the same range).
#
# Solution: Allen's function get.data.colour() maps a colour palette to a response variable,
# with the option to specify the effective the minimum and maximum values to be mapped to 
# the ends of the colour palette range. Data that exceed these values are clamped to them. 
# There is an additional option for specifying whether or not zero should map to the exact 
# middle of the spectrum of the colour palette; automatic handling of outliers is invoked 
# so as to avoid extreme skewing of the colour map.

get.data.colour <- function(x, fn.palette, n.colours = 7, method = "continuous", 
                            centre = "zero", x.min = NULL, x.max = NULL)
{
    # Maps a colour palette to a vector of continuous or discrete numeric data.
    # Allen H Nugent, Eureka Data Science 2016. 
    #     Latest edit:  01/12/2016
    #     Latest test:  01/12/2016
    #
    # Parameters:
    #
    #   fn.palette      a palette vector (e.g. created by the colorRampPalette() fn.).
    #
    #   n.colours       number of colours to actually use (<= NROW(fn.palette)).
    #   
    #   method          determines how colours map to data:
    #       = "continuous"  input data are continuous; data will be scaled and mapped to the nearest colour;
    #       = "discrete"    input data are levels (1 to n.colours); colours are exactly mapped to levels.
    #
    #   centre          (method="continuous") determines how centre of palette maps to data distribution:
    #       = "zero"        the middle of the palette will be at x = 0;
    #       = "auto"        the middle of the palette will be determined by the range of the input data.
    #
    #   x.min           (method="continuous") the effective minimum for mapping to the first colour.
    #                   A hard limit is applied to input data so that x < x.min will still get mapped.
    #
    #   x.max           (method="continuous") the effective maximum for mapping to the last colour.
    #                   A hard limit is applied to input data so that x > x.max will stil lget mapped.
    #
    # Usage:
    # 
    #   # create custom palette:
    #   palette.fn <- colorRampPalette(c(rgb(1,0,0,0.5), rgb(1,1,1,0.5), rgb(0,1,0,0.5)), alpha=TRUE)
    #   # create input data to be coloured:
    #   xtest <- 1:7
    #   gc <- get.data.colour(xtest, palette.fn)
    #   gc
    #   # plot the data:
    #   barplot(rep(1, 7), col = gc, space = 0, border = NA)
    #   # now squeeze the extrema:
    #   gc <- get.data.colour(xtest, palette.fn, n.colours = 5, x.min = 2, x.max = 6)
    #   gc
    #   barplot(rep(1, 7), col = gc, space = 0, border = NA)
    #
    
    if (method == "continuous")
    {    
        # compute limits if not specified ...
        if(is.null(x.min))
        {
            x.min <- min(x)
        }
        if(is.null(x.max))
        {
            x.max <- max(x)
        }
        
        # 19/04/16:
        if (centre == "auto")
        {
            # do nothing
        }
        
        if (centre == "zero")
        {
            # save this for a future optional parameter:::
            #         mean.abs <- mean(abs(c(x.min, x.max)))
            #         x.min <- mean.abs * sign(growth.min)
            #         x.max <- mean.abs
            
            # define extreme values as 10th & 90th percentiles (to minimise leverage of outliers) ...
            q <- quantile(x, probs = seq(0, 1, 0.1))
            extreme10 <- q[2]
            extreme90 <- q[10]
            if (extreme10 * extreme90 < 0)  #: can't have symmetry unless there are data on either side of 0 
            {
                # need a symmetric scale, so the mean absolute value of the extremes will be the half-width ...
                mean.extreme <- mean(abs(c(extreme10, extreme90)))
                x.min <- -mean.extreme
                x.max <- mean.extreme
            }
        }
        
        # apply hard limits ...
        x[x < x.min] <- x.min
        x[x > x.max] <- x.max
        
        # map input colour palette function based on specified number of colours and data range ...
        i.out <- 1 + (n.colours - 1) * (x - x.min) / (x.max - x.min)
        pal <- fn.palette(n.colours)
        #D:    print(i.out)
        x.out <- pal[i.out]
        #D:    print(x.out)
    }
    
    if (method == "discrete")
    {
        pal <- fn.palette(n.colours)   #: TODO: what happens if this <> NROW(x) ?
        i.out <- unclass(x)
        x.out <- pal[i.out]
        
        # Debugging code:
        #         print("palette:")
        #         print(pal)
        #         print("i.out:")
        #         print(i.out)
        #         print("x.out:")
        #         print(x.out)
    }
    
    return(x.out)
}


# # test the colour mapping function:
# par(mfrow=c(2,1), mar = c(1,1,4,1))
# dat1 <- 1:7
# dat.colour <- get.data.colour(dat1, cp_fn, n.colours = NROW(dat1), method = "continuous")
# barplot(rep(1, NROW(dat1)), col = dat.colour, space = 0, axes = FALSE, border = NA, main = do.call(paste, c(as.list(dat1), sep=" ")))
# dat2 <- 0:5
# barplot(rep(1, NROW(dat2)), col = dat.colour, space = 0, axes = FALSE, border = NA, main = do.call(paste, c(as.list(dat2), sep=" ")))
# #>> Nb. If called with method = "discrete", value=5 in dat2 maps to same colour as value=6 in dat1, which is unwanted behaviour!!!


plotlist <- function(signalist, timelist=NULL) {
    
    # TODO: Modify for use with Crust data in SparkR !!!!!!!!!!!!
    
    # Plots a list of time series in a grid.
    # If no timebase supplied, abscissa will be datum index.
    
    # TODO: Add paging when number of plots too high for one screen!
    
    Nplots <- length(signalist)
    
    # names of individual plots:
    if (is.null(names(signalist))) {
        plotnames <- paste0("P", as.character(c(1:Nplots)))
    }
    else
    {
        plotnames <- names(signalist)
    }
    
    num.rows <- floor(sqrt(Nplots))
    num.cols <- ceiling(Nplots / num.rows)
    par(mfrow = c(num.rows, num.cols), mar = c(2, 4, 2, 1), oma = c(1, 1, 0, 0))
    
    if (is.null(timelist)) {
        for (k in 1:Nplots) {
            plot(signalist[[k]], type = "l", ylab = plotnames[k], xlab = "")
            #plot(signalist[[k]], type = "l")
        }        
    } else {
        for (k in 1:Nplots) {
            plot(timelist[[k]], signalist[[k]], type = "l", ylab = plotnames[k], xlab = "")
            #plot(signalist[[k]], type = "l")
        }
    }
    
    # return(TRUE)
}


