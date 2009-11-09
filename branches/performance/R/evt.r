# evt.r: Functions for applying Extreme Value Theory to Record Linkage

# simplified version of mrl.plot in package ismev
mrl<-
function(data, umin = min(data), umax = max(data) - 0.1, nint = 
	round(max(data)-min(data))*20)
{
#
# function to produce empirical mean residual life plot
# as function of threshold.
	x <- xu <- xl <- numeric(nint)
	u <- seq(umin, umax, length = nint)
	for(i in 1:nint) {
		data <- data[data > u[i]]
		x[i] <- mean(data - u[i])
	}
	return(list(x=u,y=x))
}

# Estimation of quantile in pareto distribution
gpdEst<- function(Wdata, thresh=-Inf, quantil=0.95)
{
    gpd=fpot(x=Wdata, threshold=thresh,std.err=FALSE)
    n=length(Wdata)
    scale=gpd$estimate[1]
    shape=gpd$estimate[2]
   	k=length(gpd$exceedances) # number of exceedances over thresh
    x_quantil=thresh+scale/shape*((n/k*(1-quantil))^(-shape) -1)
  # Falls Threshold das Intervall unterschreitet, nimm Median der Gewichte
    if (x_quantil < thresh) return (mean(c(thresh,max(Wdata))))
    if (x_quantil > -scale/shape) return (-scale/shape)
    return (x_quantil)
} 


# MRL plot
plotMRL <- function(rpairs,l=mrl(rpairs$Wdata))
{
  plot(l$x,l$y,type="l",lty="blank",xlab="Threshold",ylab="MRL")
  # Draw grid
  abline(v=pretty(extendrange(l$x),n=40),h=pretty(extendrange(l$y),n=40),col="lightgray")
  abline(v=pretty(extendrange(l$x),n=8),h=pretty(extendrange(l$y),n=8),col="gray")
  box()
  points(l$x,l$y,type="l")
}



getParetoThreshold <- function(rpairs, quantil=0.95, interval=NA)
{
  # Auswahl der R�nder. Gew�hlte Gewichte werden im Plot angezeigt
  l=mrl(rpairs$Wdata)
  if (!is.numeric(interval))
  {
    plotMRL(NULL,l=l)
    title(main=rpairs$description)
    message("Choose interval for pareto distribution")
    bringToTop()
    indices=sort(identify(l$x,l$y,n=2,labels=signif(l$x,4)))
    bringToTop(-1)
    if (length(indices)==0)
      stop("At least the left endpoint of the interval must be chosen!")
    interval=l$x[indices]
  }
  # Wenn nur der linke Rand ausgew�hlt wurde, bleibt der rechte offen
  if (length(interval)==1)
    interval=c(interval,max(rpairs$Wdata))
  fatTail=rpairs$Wdata[rpairs$Wdata <= interval[2]]
  threshold=gpdEst(fatTail,interval[1],quantil)  
  return(threshold)
}
