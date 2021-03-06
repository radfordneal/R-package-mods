#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler, Christian Buchta, 
#                       Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

### categorize continuous variables

categorize <- function(x, breaks=4, quantile=TRUE, labels=NULL, ...) {
    if(!is.numeric(x)) stop("categorize needs numeric input")
    
    if(length(breaks)==1){
	if(quantile) breaks <- quantile(x, seq(0,1, length.out=breaks+1), na.rm=TRUE)
	else breaks <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=breaks+1)
    }else{
	if(quantile) breaks <- quantile(x, breaks, na.rm=TRUE)
	### else breaks is already values
    }
	
    
    x <- cut(x, breaks, include.lowest=TRUE, ...)
    
    if(!is.null(labels)) levels(x) <- labels
    x
}

