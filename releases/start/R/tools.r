# varios utility functions

ordered_pairs <- function (x) 
{
    if (length(x)==1)
        return (array(unlist(lapply(1:(x-1),function (k) rbind(k,(k+1):x))),dim=c(2,x*(x-1)/2)))
    n=length(x)
    return (array(unlist(lapply(1:(n-1),function (k) rbind(x[k],x[(k+1):n]))),dim=c(2,n*(n-1)/2)))
}

isFALSE <- function(x) identical(x,F)
