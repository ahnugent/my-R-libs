# File:       utilities.R
# Authors:    Allen H Nugent, others
# Last edit:  2016-03-29
# Last test:  2016-03-29
# Purpose:    Tools for managing R environment.



#-----------------------------------------------------------------------------
# Updating R
#-----------------------------------------------------------------------------
# author: Tal Galili 
# ref: http://www.r-statistics.com/2013/03/updating-r-from-r-on-windows-using-the-installr-package/

# installing/loading the package:
if(!require(installr)) {
    install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.

