  setGeneric(
  name = "emWeights",
  def = function(rpairs, cutoff=0.95, ...) standardGeneric("emWeights")
)


# em.r: Functions for Record Linkage with weights calculated by the EM algorithm

# Arguments:
#
#   rpairs  data pairs (class RecLinkPairs)
#   m       probability for an error (m-probability), either one value for
#           all attributes or a vector with distinct values
setMethod(
  f = "emWeights",
  signature = "RecLinkData",
  definition = function (rpairs, cutoff=0.95, ...)
  {
    # check for erronous input
    
    if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
      stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))
  
    if (nrow(rpairs$pairs) == 0)
      stop("No record pairs!")
      
    if (!is.numeric(cutoff))
      stop(sprintf("Illegal type for cutoff: %s", class(cutoff)))
    if (cutoff < 0 || cutoff > 1)
      stop(sprintf("Illegal value for cutoff: %g", cutoff))
  
  
    pairs=rpairs$pairs
    # ids und Matchingstatus rausnehmen
    pairs=pairs[,-c(1,2,ncol(pairs))]
    pairs=as.matrix(pairs)
    pairs[is.na(pairs)]=0
    is_fuzzy=!all(is.element(pairs,0:1))
    if (is_fuzzy)
    {
        if(length(cutoff)==1 || length(cutoff)==ncol(pairs)){
        	pairs_fuzzy=pairs
        	cutoff <- matrix(cutoff, ncol=ncol(pairs), nrow=nrow(pairs), byrow=TRUE)
        	pairs=(pairs>=cutoff)*1
  	}
  	else {
  	 stop("Cutoff must be a vector with length equal to the number of attributes or to one!")
  	 }
    }
  
    n_data=nrow(pairs)  
    observed_count=countpattern(pairs)
    n_attr=ncol(pairs)
    patterns=bincombinations(n_attr)  # Liste der Patterns
    x=c(rep(0,nrow(patterns)),rep(1,nrow(patterns)))
    s=c(1:length(observed_count), 1:length(observed_count))
    i=rep(1,nrow(patterns)) # Intercept
    X=cbind(i,x,rbind(patterns,patterns),rbind(patterns,patterns)*x) # Design Matrix
  
    u=rpairs$frequencies    
    m=0.97
    # Ad-hoc-Sch�tzung f�r Anteil an Matchen (Faktor 0.1 relativ beliebig)
    prob_M=1/sqrt(n_data)*0.1
    # Anzahl sch�tzen f�r Matche
    init_M=apply(patterns,1,function(a) prod(a*m+(1-a)*(1-m))*n_data*prob_M)
    init_U=apply(patterns,1,function(a) prod(a*u+(1-a)*(1-u))*n_data*(1-prob_M))
    expected_count=c(init_U,init_M)
  
    res=mygllm(observed_count,s,X,E=expected_count,...)
  
    n_patterns=length(res)/2
  
    # Anteil Matche/Non_Matche in einem Pattern
    matchrate=res[(n_patterns+1):(2*n_patterns)]/res[1:n_patterns]
    #matchrate=round(res[(n_patterns+1):(2*n_patterns)])/round(res[1:n_patterns])
  #    o=order(matchrate,res[(n_patterns+1):(2*n_patterns)],decreasing=T)
  
    n_matches=sum(res[(n_patterns+1):(2*n_patterns)])
    n_nonmatches=sum(res[1:n_patterns])
    U=res[1:n_patterns]/n_nonmatches
    M=res[(n_patterns+1):(2*n_patterns)]/n_matches
    W=log(M/U, base=2)
    indices=colSums(t(pairs)*(2^(n_attr:1-1)))+1    
    ret=rpairs # keeps all components of rpairs
    ret$M=M
    ret$U=U
    ret$W=W
    ret$Wdata=W[indices]
    if (is_fuzzy)
    {
        str_weights=apply(pairs_fuzzy^pairs,1,prod)
        ret$Wdata=ret$Wdata+log(str_weights, base=2)
    } 
    cat("\n")
    return(ret)
  }
) # end of setMethod 



setMethod(  
  f = "emWeights",
  signature = "RLBigData",
  definition = function (rpairs, cutoff=0.95, ...)
  {
    u=getFrequencies(rpairs)
    # get number of attributes from frequency vector: this way excluded
    # columns are not counted
    n_attr <- length(u)
    observed_count <- getPatternCounts(rpairs, cutoff=cutoff)
    n_patterns <- length(observed_count)
    n_data <- sum(observed_count)
    patterns=bincombinations(n_attr)  # Liste der Patterns
    x=c(rep(0,n_patterns),rep(1,n_patterns))
    s=c(1:n_patterns, 1:n_patterns)
    i=rep(1,n_patterns) # Intercept
    X=cbind(i,x,rbind(patterns,patterns),rbind(patterns,patterns)*x) # Design Matrix
  
    m=0.97
    # Ad-hoc-Sch�tzung f�r Anteil an Matchen (Faktor 0.1 relativ beliebig)
    prob_M=1/sqrt(n_data)*0.1
    # Anzahl sch�tzen f�r Matche
    init_M=apply(patterns,1,function(a) prod(a*m+(1-a)*(1-m))*n_data*prob_M)
    init_U=apply(patterns,1,function(a) prod(a*u+(1-a)*(1-u))*n_data*(1-prob_M))
    expected_count=c(init_U,init_M)
  
    res=mygllm(observed_count,s,X,E=expected_count,...)


    # Anteil Matche/Non_Matche in einem Pattern
    matchrate=res[(n_patterns+1):(2*n_patterns)]/res[1:n_patterns]
    #matchrate=round(res[(n_patterns+1):(2*n_patterns)])/round(res[1:n_patterns])
  #    o=order(matchrate,res[(n_patterns+1):(2*n_patterns)],decreasing=T)
  
    n_matches=sum(res[(n_patterns+1):(2*n_patterns)])
    n_nonmatches=sum(res[1:n_patterns])
    U=res[1:n_patterns]/n_nonmatches
    M=res[(n_patterns+1):(2*n_patterns)]/n_matches
    W=log(M/U, base=2)
    
    dbWriteTable(rpairs@con, "M", data.frame(id = 1:n_patterns, M = M), row.names = FALSE, overwrite = TRUE)
    dbWriteTable(rpairs@con, "U", data.frame(id = 1:n_patterns, U=U), row.names = FALSE, overwrite = TRUE)
    dbWriteTable(rpairs@con, "W", data.frame(id = 1:n_patterns, W=W), row.names = FALSE, overwrite = TRUE)

    # get weights for individual records and store in database
    dbGetQuery(rpairs@con, "drop table if exists Wdata")
    dbGetQuery(rpairs@con, "create table Wdata (id1 integer, id2 integer, W double)")
    # create index, this speeds up the join operation of getPairs
    # significantly
    dbGetQuery(rpairs@con, "create index index_Wdata on Wdata (id1, id2)")

#    clear(rpairs)
    rpairs <- begin(rpairs)

    # open a second connection to the database file
    con2 <- dbConnect(rpairs@drv, rpairs@dbFile)
    dbGetQuery(con2, "pragma journal_mode=wal")

    n <- 10000
    i = n
    while(nrow(slice <- nextPairs(rpairs, n)) > 0)
    {
      # auch hier vorl�ufiger Code! es muss noch ein tragf�higes Konzept her,
      # auf welche Weise Links und Possible Links ausgegeben werden!
      message(i)
      flush.console()
      slice[is.na(slice)] <- 0
      slice[slice < cutoff] <- 0
      slice[slice >= cutoff & slice < 1] <- 1
      indices=colSums(t(slice[,-c(1:2, ncol(slice))])*(2^(n_attr:1-1)))+1
      dbWriteTable(con2, "Wdata", data.frame(slice[,1:2], W[indices]),
        row.names = FALSE, append = TRUE)
      i <- i + n
    }
    dbDisconnect(con2)
    return(rpairs)
  }
) # end of setMethod


setGeneric(
  name = "emClassify",
  def = function(rpairs, threshold.upper = Inf, 
                        threshold.lower = threshold.upper, my = Inf, 
                        ny = Inf) standardGeneric("emClassify")
)

setMethod(
  f = "emClassify",
  signature = "RecLinkData",
  definition = function (rpairs, threshold.upper = Inf, 
                        threshold.lower = threshold.upper, my = Inf, 
                        ny = Inf)
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
        
    if (!is.numeric(my))
      stop(sprintf("Illegal type for my: %s", class(my)))
    if (!missing(my) && (my < 0 || my > 1))
      stop(sprintf("Illegal value for my: %g", my))
  
    if (!is.numeric(ny))
      stop(sprintf("Illegal type for ny: %s", class(ny)))
    if (!missing(ny) && (ny < 0 || ny > 1))
      stop(sprintf("Illegal value for ny: %g", ny))
  
    # if no threshold was given, compute them according to the error bounds
    if (missing(threshold.upper) && missing(threshold.lower))
    {
      o=order(rpairs$W,decreasing=TRUE) # order Weights decreasing
      FN=rev(cumsum(rev(rpairs$M[o]))) 
      FP=cumsum(rpairs$U[o])
      if (my==Inf && ny==Inf)
      {
          # no error bound given: minimize overall error
          cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
          if (length(cutoff_upper)==0)
              cutoff_upper=0
          cutoff_lower=cutoff_upper
          
      } else if (my==Inf)
      {  
          # only rate of false matches relevant
          cutoff_lower=head(which(FN<=ny),1)
          if (length(cutoff_lower)==0)
              cutoff_lower=length(o)
          cutoff_upper=cutoff_lower
      
      } else if (ny==Inf)
      {
          # only rate of false non-matches relevant
          cutoff_upper=tail(which(FP<=my),1)
          cutoff_lower=cutoff_upper
      } else
      {
          # both error bounds relevant
          cutoff_upper=tail(which(FP<=my),1)
          cutoff_lower=head(which(FN<=ny),1)
          if (length(cutoff_upper)==0)
              cutoff_upper=0
          if (length(cutoff_lower)==0)
              cutoff_lower=length(o)
          if (cutoff_lower<cutoff_upper)
          {
              cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
              cutoff_lower=cutoff_upper
          }
      } 
      print("Threshold berechnen und Klassifikation zuweisen")
      threshold.upper=rpairs$W[o][cutoff_upper]
      threshold.lower=rpairs$W[o][cutoff_lower]
    } # end if
     
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
  f = "emClassify",
  signature = "RLBigData",
  definition = function (rpairs, threshold.upper = Inf,
                        threshold.lower = threshold.upper, my = Inf,
                        ny = Inf)
  {

    if(!dbExistsTable(rpairs@con, "W"))
      stop("No EM weights have been calculated for rpairs! Call emWeights first.")

    W <- dbGetQuery(rpairs@con, "select W from W order by id asc")$W
    M <- dbGetQuery(rpairs@con, "select M from M order by id")$M
    U <- dbGetQuery(rpairs@con, "select U from U order by id asc")$U

    if (!is.numeric(threshold.upper))
      stop(sprintf("Illegal type for threshold.upper: %s", class(threshold.upper)))

    if (!is.numeric(threshold.lower))
      stop(sprintf("Illegal type for threshold.lower: %s", class(threshold.lower)))

    if (threshold.upper < threshold.lower)
      stop(sprintf("Upper threshold %g lower than lower threshold %g",
        threshold.upper, threshold.lower))

    if (!is.numeric(my))
      stop(sprintf("Illegal type for my: %s", class(my)))
    if (!missing(my) && (my < 0 || my > 1))
      stop(sprintf("Illegal value for my: %g", my))

    if (!is.numeric(ny))
      stop(sprintf("Illegal type for ny: %s", class(ny)))
    if (!missing(ny) && (ny < 0 || ny > 1))
      stop(sprintf("Illegal value for ny: %g", ny))

    # if no threshold was given, compute them according to the error bounds
    if (missing(threshold.upper) && missing(threshold.lower))
    {
      o=order(W,decreasing=TRUE) # order Weights decreasing
      FN=rev(cumsum(rev(M[o])))
      FP=cumsum(U[o])
      if (my==Inf && ny==Inf)
      {
          # no error bound given: minimize overall error
          cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
          if (length(cutoff_upper)==0)
              cutoff_upper=0
          cutoff_lower=cutoff_upper

      } else if (my==Inf)
      {
          # only rate of false matches relevant
          cutoff_lower=head(which(FN<=ny),1)
          if (length(cutoff_lower)==0)
              cutoff_lower=length(o)
          cutoff_upper=cutoff_lower

      } else if (ny==Inf)
      {
          # only rate of false non-matches relevant
          cutoff_upper=tail(which(FP<=my),1)
          cutoff_lower=cutoff_upper
      } else
      {
          # both error bounds relevant
          cutoff_upper=tail(which(FP<=my),1)
          cutoff_lower=head(which(FN<=ny),1)
          if (length(cutoff_upper)==0)
              cutoff_upper=0
          if (length(cutoff_lower)==0)
              cutoff_lower=length(o)
          if (cutoff_lower<cutoff_upper)
          {
              cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
              cutoff_lower=cutoff_upper
          }
      }
      print("Threshold berechnen und Klassifikation zuweisen")
      threshold.upper <- rpairs@W[o][cutoff_upper]
      threshold.lower <- rpairs@W[o][cutoff_lower]
    } # end if

#    on.exit(clear(rpairs))
#    rpairs <- begin(rpairs)
#    n <- 10000
#    i = n
#    links <- matrix(nrow=0, ncol=2)
#    possibleLinks <- matrix(nrow=0, ncol=2)
#    n_attr <- length(getFrequencies(rpairs))
#    nPairs <- 0
#    while(nrow(slice <- nextPairs(rpairs, n)) > 0)
#    {
#      # auch hier vorl�ufiger Code! es muss noch ein tragf�higes Konzept her,
#      # auf welche Weise Links und Possible Links ausgegeben werden!
#      message(i)
#      flush.console()
#      slice[is.na(slice)] <- 0
#      indices=colSums(t(slice[,-c(1:2, ncol(slice))])*(2^(n_attr:1-1)))+1
#      links <- rbind(links, as.matrix(slice[W[indices] >= threshold.upper,1:2]))
#      possibleLinks <- rbind(possibleLinks,
#        as.matrix(slice[W[indices] < threshold.upper &
#        W[indices] >= threshold.lower ,1:2]))
#      i <- i + n
#      nPairs <- nPairs + nrow(slice)
#    }

    query <- "select id1, id2 from Wdata where W >= :upper"
    links <- dbGetPreparedQuery(rpairs@con, query, data.frame(upper = threshold.upper))
    query <- "select id1, id2 from Wdata where W < :upper and W >= :lower"
    possibleLinks <- dbGetPreparedQuery(rpairs@con, query,
      data.frame(upper = threshold.upper, lower = threshold.lower))
    nPairs <- dbGetQuery(rpairs@con, "select count(*) from Wdata")[1,1]
    new("RLResult", data = rpairs, links = as.matrix(links),
      possibleLinks = as.matrix(possibleLinks), nPairs = nPairs)
  }
) # end of SetMethod


setGeneric(
  name = "getEMWeights",
  def = function(object) standardGeneric("getEMWeights")
)

setMethod(
  f = "getEMWeights",
  signature = "RLBigData",
  definition = function(object)
  {

  }
)