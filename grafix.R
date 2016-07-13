# Script:       grafix.R
# Authors:      Allen H. Nugent, 2015+
# Last edit:    2015-09-01
# Last test:    2015-08-30
#
# Purpose:      Library of graphics functions.
#
# Contents:
#
#   col.as.number   Converts a string hexadecimal (as used in hsv values) to an integer.
#
#   perspectrum     Plots a spectral dispersion across a finite light sheet, in perspective
#                   (i.e. as an isosceles triangle).


col.as.number <- function(strhexcol) {
    
    # Converts a string hexadecimal (as used in hsv values) to an integer.
    
    colornbr <- as.numeric(sub("#", "0x", strhexcol))
    return(colornbr)
}


perspectrum <- function(a, b, db = 0, m = 256, p.alpha = 1, standalone = TRUE, file.name = NA, 
                        plot.width = 480, plot.height = 240) {
    
    # Spectral dispersion across a finite light sheet, in perspective.
    # Computes and plots a triangle with vertical (high) apex and left-to-right
    # dispersion of rainbow colours (red to indigo)
    #
    # parameters:
    # a = base of triangle enclosing specal dispersion (user units)
    # b = height of triangle enclosing specal dispersion (user units)
    # db = offset of apex from x-axis
    # db = vertical offset of apex of triangle
    # m = number of spectral divisions (precision)
    # alpha = opacity of image (0~1)
    # standalone = flag: treat as stand-alone plot (or secondary feature)
    # file.name = output file name (ignored if standalone = FALSE)
    # plot.width = width of plot (not triangle) in pixels (ignored if standalone = FALSE)
    # plot.height = height of plot (not triangle) in pixels (ignored if standalone = FALSE)
    #
    # variables:
    # x = horizontal coordinate (range = [-a/2, a/2])
    # dx = x-increment
    # y = vertical coordinate (range = [-b, 0])
    # i = index in x-range
    # j = index in y-range
    #
    
    # create palette ...
    
    cols = rainbow(m, alpha = p.alpha)

    # build vector of spectral-band endpoints ...
    
    x <- numeric(m+1)
    dx <- a / m
    for (i in 1:(m+1)) { 
        x[i] <- (i-1) * dx - (a/2) 
    }
    
    # set up new plot or add to open plot ?...
    
    if (standalone) {
        if (!is.na(file.name)) {
            png(filename = file.name, width = plot.width, height = plot.height, units = "px", type = "windows", bg = "transparent")
        }

        # prepare a pair of vectors for defining the plotting frame ...
        
        x.f <- c(-a/2, a/2)
        y.f <- c(-(b-db), 0)
        
        plot(x.f, y.f, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")   # NFG: ann = FALSE
        polygon(c(-a/2, a/2, a/2, -a/2), c(-(b-db), -(b-db), 0, 0), col = "#00000000", border = NA)
    }
    
    # plot the triangles in the colours of the spectrum ...
    
    for (i in 1:m) {
        xv <- c(x[i], x[i+1], 0)
        yv <- c(-(b-db), -(b-db), 0)
        polygon(xv, yv, col = cols[i], border = NA)
    }
    
    # close the device ...
    
    if (standalone) {
        if (!is.na(file.name)) {
            dev.off()
        }
    }
}


starburst <- function (x.c, y.c, x.s, y.s, n.p, p.alpha = 0.5, p.col = "random", standalone = FALSE) {

    # Plots a number of points randomly distributed about a central point.
    #
    # (x.c, y.c) is the centre (user units).
    # x.s, y.s are the standard deviations.
    # n.p is the number of points.
    # p.alpha is the transparency.
    # p.col is the color of the points
    #   "random" means colors will be randomly selected from a colour palette.
    #   "randomgrey" means colors will be randomly selected from a grey palette.
    #
    # Assumptions:
    #   1. The plot has alredy been set up outside.
    
    x.a <- rnorm(n.p, mean = x.c, sd = x.s)  # set of random x-coords
    y.a <- rnorm(n.p, mean = y.c, sd = y.s)  # set of random y-coords
    
    if (substr(p.col, 1, 6) == "random") {
        nc <- 20    
        if (p.col == "random") {
            cols <- sample(seq(from = (100 - nc), to = 99), size = n.p, replace = TRUE)
        } else {
            cols <- paste0("gray", as.character(sample(seq(from = (100 - nc), to = 99), size = n.p, replace = TRUE)))
        }
        for (i in 1:n.p) {
            points(x.a[i], y.a[i], type = "p", pch = 19, col = cols[i], cex = 0.03, lwd <- 1)
        }
    } else {
        points(x.a, y.a, type = "p", pch = 19, col = p.col, cex = 0.03, lwd <- 1) 
    }
        
}

