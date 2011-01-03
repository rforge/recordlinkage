setGeneric(
  name = "epiClassify",
  def = function(rpairs, threshold.upper, threshold.lower=threshold.upper, ...)
    standardGeneric("epiClassify")
)

setMethod(
  f = "epiClassify",
  signature = "RecLinkData",
  definition = function (rpairs,threshold.upper, 
                        threshold.lower=threshold.upper, ...)
  {    
  
    if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
      stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))
  
    if (nrow(rpairs$pairs) == 0)
      stop("No record pairs!")
  
    if (is.null(rpairs$Wdata))
      stop("No weights in rpairs!")
  
    if (!is.numeric(threshold.upper))
      stop(sprintf("Illegal type for threshold.upper: %s", class(threshold.upper)))
  
    if (!is.numeric(threshold.lower))
      stop(sprintf("Illegal type for threshold.lower: %s", class(threshold.lower)))
  
    if (threshold.upper < threshold.lower)
      stop(sprintf("Upper threshold %g lower than lower threshold %g",
      threshold.upper, threshold.lower))
      
    prediction=rep("P",nrow(rpairs$pairs))
    prediction[rpairs$Wdata>=threshold.upper]="L"
    prediction[rpairs$Wdata<threshold.lower]="N"
    
    ret=rpairs # keeps all components of rpairs
    ret$prediction=factor(prediction,levels=c("N","P","L"))
    ret$threshold=threshold.upper
    class(ret)="RecLinkResult"
    return(ret)
  }
) # end of setMethod

setMethod(
  f = "epiClassify",
  signature = "RLBigData",
  definition = function (rpairs,threshold.upper, 
                        threshold.lower=threshold.upper, e=0.01, 
                        f=getFrequencies(rpairs))
  {    
    if (!is.numeric(threshold.upper))
      stop(sprintf("Illegal type for threshold.upper: %s", class(threshold.upper)))
  
    if (!is.numeric(threshold.lower))
      stop(sprintf("Illegal type for threshold.lower: %s", class(threshold.lower)))
  
    if (threshold.upper < threshold.lower)
      stop(sprintf("Upper threshold %g lower than lower threshold %g",
      threshold.upper, threshold.lower))

    on.exit(clear(rpairs))
    rpairs <- begin(rpairs)
    nPairs <- 0
    n <- 10000
    i = n
    links <- matrix(nrow=0, ncol=2)
    possibleLinks <- matrix(nrow=0, ncol=2)
    while(nrow(slice <- nextPairs(rpairs, n)) > 0)
    {
#      message(i)
      flush.console()
      slice[is.na(slice)] <- 0
      e=e+rep(0,ncol(slice)-3)
      f=f+rep(0,ncol(slice)-3)
      # adjust error rate 
      # error rate
      w=log((1-e)/f, base=2)
      # 
      
      
      # weight computation
      row_sum <- function(r,w)
      {
        return(sum(r*w,na.rm=TRUE))
      }
    
      S=apply(slice[,-c(1,2,ncol(slice))],1,row_sum,w)/sum(w)
      if (any(is.na(S) | S < 0 | S > 1))
        warning("Some weights have illegal values. Check error rate and frequencies!")
#      message(range(slice[,1]))
#      message(range(slice[,2]))
#      message("----------------------")      
      links <- rbind(links, as.matrix(slice[S >= threshold.upper,1:2]))
      possibleLinks <- rbind(possibleLinks,
        as.matrix(slice[S >= threshold.lower & S < threshold.upper, 1:2]))
      i <- i + n
      nPairs <- nPairs + nrow(slice)
    }
    new("RLResult", data = rpairs, links = links, possibleLinks = possibleLinks,
      nPairs = nPairs)
  }
) # end of setMethod


setGeneric(
  name = "epiWeights",
  def = function(rpairs, e=0.01, f=getFrequencies(rpairs))
    standardGeneric("epiWeights")
)

setMethod(
  f = "epiWeights",
  signature = c("RLBigData", "ANY", "ANY"),
  definition = function (rpairs, e=0.01, f=getFrequencies(rpairs))
  {

    on.exit({
        clear(rpairs)
#        dbGetQuery(con2, "pragma wal_checkpoint")
      }
    )

    # create table where weights are stored
    dbGetQuery(rpairs@con, "drop table if exists weights")
    dbGetQuery(rpairs@con, "create table weights (id1 integer, id2 integer, weight real)")
    dbGetQuery(rpairs@con, "create index index_weights on weights(id1, id2)")
#    dbGetQuery(rpairs@con, "begin transaction")
    rpairs <- begin(rpairs)
    nPairs <- 0
    n <- 10000
    i = n

    # open a second connection to the database file
    con2 <- dbConnect(rpairs@drv, rpairs@dbFile)
    dbGetQuery(con2, "pragma journal_mode=wal")

#    dbGetQuery(con2, "pragma synchronous=off")
#    weightTable <- matrix(numeric(), ncol=3)
    while(nrow(slice <- nextPairs(rpairs, n)) > 0)
    {
#      message(i)
      flush.console()
      slice[is.na(slice)] <- 0
      e=e+rep(0,ncol(slice)-3)
      f=f+rep(0,ncol(slice)-3)
      # adjust error rate
      # error rate
      w=log((1-e)/f, base=2)
      #


      # weight computation
      row_sum <- function(r,w)
      {
        return(sum(r*w,na.rm=TRUE))
      }

      S=apply(slice[,-c(1,2,ncol(slice))],1,row_sum,w)/sum(w)
      if (any(is.na(S) | S < 0 | S > 1))
        warning("Some weights have illegal values. Check error rate and frequencies!")

#      weightTable <- rbind(weightTable, cbind(slice[,1:2], S))
      dbWriteTable(con2, "weights",
        cbind(slice[,1:2], S), row.names=FALSE, append=TRUE)
#      message(range(slice[,1]))
#      message(range(slice[,2]))
#      message("----------------------")

      nPairs <- nPairs + nrow(slice)
    }
#    browser()
    rpairs
  }
) # end of setMethod