
# Arguments:
#
#   rpairs  data pairs (class RecLinkPairs)
#   m       probability for an error (m-probability), either one value for
#           all attributes or a vector with distinct values
emWeights <- function (rpairs, cutoff=0.95,...)
{
    library(e1071)
# t0=proc.time()
# print("Datenvorbereitung")
    pairs=rpairs$pairs
    # ids und Matchingstatus rausnehmen
    pairs=pairs[,-c(1,2,ncol(pairs))]
# print(proc.time()-t0)
# t0=proc.time()
    pairs=as.matrix(pairs)
    pairs[is.na(pairs)]=0
# print("Fuzzy umrechnen")
    is_fuzzy=!all(is.element(pairs,0:1))
    if (is_fuzzy)
    {
        pairs_fuzzy=pairs
        pairs=as.array((pairs>=cutoff)*1)
    }
# print(proc.time()-t0)
# t0=proc.time()

# print("Patterns z�hlen, em vorbereiten")
    n_data=nrow(pairs)  
    observed_count=countpattern(pairs)
    n_attr=ncol(pairs)
    patterns=bincombinations(n_attr)  # Liste der Patterns
    x=c(rep(0,nrow(patterns)),rep(1,nrow(patterns)))
    s=c(1:length(observed_count), 1:length(observed_count))
    i=rep(1,nrow(patterns)) # Intercept
    X=cbind(i,x,rbind(patterns,patterns),rbind(patterns,patterns)*x) # Design Matrix
# print(proc.time()-t0)
# t0=proc.time()

# print("H�ufigkeiten sch�tzen")
    u=rpairs$frequencies    
    m=0.97
    # Ad-hoc-Sch�tzung f�r Anteil an Matchen (Faktor 0.1 relativ beliebig)
    prob_M=1/sqrt(n_data)*0.1
    # Anzahl sch�tzen f�r Matche
    init_M=apply(patterns,1,function(a) prod(a*m+(1-a)*(1-m))*n_data*prob_M)
    init_U=apply(patterns,1,function(a) prod(a*u+(1-a)*(1-u))*n_data*(1-prob_M))
    expected_count=c(init_U,init_M)
# print(proc.time()-t0)
# t0=proc.time()

# print("EM ausf�hren")   
    res=mygllm(observed_count,s,X,E=expected_count,...)
# print(proc.time()-t0)
# t0=proc.time()

# print("Der Rest")
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
#    class(ret)="RecLinkResult"
# print(proc.time()-t0)
cat("\n")
    return(ret)
}


# Arguments:
#   rpairs      weighted record pairs (output of emWeights)
#   my      error bound: # False Matches / # Found Matches
#   ny      error bound  # False Non-Matches / # Found Non-Matches
#       If an error bound is Inf, it will not be considered, meaning that
#       "possible link" will not be assigned
emClassify <- function (rpairs,threshold.upper=Inf, 
                        threshold.lower=threshold.upper,my=Inf, ny=Inf)
{    

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


optimalThreshold <- function (rpairs, my=NULL, ny=NULL)
{
	o=order(rpairs$Wdata,decreasing=TRUE)
	weights=rpairs$Wdata[o]
  n_data=length(weights)
	is_match=rpairs$pairs$is_match[o]
	FP_err=cumsum(is_match!=1)#/as.numeric(1:n_data)
  FN_err=rev(cumsum(rev(is_match==1)))#/as.numeric(1:n_data)))
  error=FP_err+FN_err

    # nun baue Tabelle, in der Gewicht (unique) und Fehlerrate gegen�bergestellt
    # sind. Die Fehlerrate eines Gewichts ist in der sortierten Tabelle gleich
    # der Fehlerrate f�r den letzten Datensatz des Blocks
    # tapply() sortiert aufsteigend, das rev() stellt 
    # die absteigende Reihenfolge wieder her

    error_unique=rev(tapply(error,weights,tail,1))
    FP_err_unique=rev(tapply(FP_err/(1:n_data),weights,tail,1))
    FN_err_unique=rev(tapply(FN_err/(n_data:1),weights,tail,1))
    
    weights_unique=unique(weights)

    # Bestimme Gewicht des Datensatzes mit minimalem Fehler.

    if (is.null(my) && is.null(ny))
      return(as.numeric(weights_unique[which.min(error_unique)]))

    if (!is.null(my))
      return(tail(as.numeric(weights_unique[FP_err_unique<=my]),1))

    if (!is.null(ny))
      return(head(as.numeric(weights_unique[FN_err_unique<=ny]),1))
}



# EM mit externen Trainingsdaten ist erst mal auf Eis gelegt!

# supervised learning with EM
# emClassify2 <- function (rpairs, my=Inf, ny=Inf)
# {    
#     train=rpairs$train
#     train=train[,-c(1,2,ncol(train))] # delete ids and is_match
#     train[is.na(train)]=0 # convert NAs to 0
#     #o=order(rpairs$W,decreasing=T) # order Weights decreasing
#     n_attr=ncol(train) # number of attributes
#     # For each record pair, compute index of corresponding pattern in the
#     # table of binary combinations
#     indices=colSums(t(train)*(2^(n_attr:1-1)))+1    
#     train_weights=rpairs$W[indices]
# 
#     valid=rpairs$valid
#     valid=valid[,-c(1,2,ncol(valid))] # delete ids and is_match
#     valid[is.na(valid)]=0 # convert NAs to 0
#     #o=order(rpairs$W,decreasing=T) # order Weights decreasing
#     n_attr=ncol(valid) # number of attributes
#     # For each record pair, compute index of corresponding pattern in the
#     # table of binary combinations
#     indices=colSums(t(valid)*(2^(n_attr:1-1)))+1    
#     valid_weights=rpairs$W[indices]
# 
#     o=order(train_weights,decreasing=T)
#     threshold=getThresholdMinAll(train_weights[o],rpairs$train$is_match[o])
#     prediction=valid_weights>=threshold
# #     
# #     L_ind=o[1:cutoff_upper] # indices of detected links
# #     U_ind=o[(cutoff_lower+1):length(o)] # indices of detected non-links
# #     # links get result TRUE, non-links FALSE, possible links NA
# #     prediction=as.logical(rep(NA,nrow(pairs)))
# #     prediction[which(is.element(indices,L_ind))]=T
# #     prediction[which(is.element(indices,U_ind))]=F
# 
#     ret=rpairs # keeps all components of rpairs
#     ret$prediction=prediction
#     class(ret)="RecLinkResult"
#     return(ret)
# }
# 
# getThreshold <- function (weights, is_match, max_fehler=0.01)
# {
#   #return (getThresholdMaxFP(weights, is_match, max_fehler))
#   return (getThresholdMinAll(weights, is_match))
# }
# 
# getThresholdMinAll <- function (weights, is_match)
# {
# 	n_data=length(is_match)
# 	if (n_data!=length(weights))
# 	{
# 		stop("Argumente weights und is_match haben unterschiedliche L�nge")
# 	}
# 	# Zwei Fehlerraten werden gebildet: der erste die Fehlerrate f�r falsch
# 	# positive, die zweite f�r falsch negative; f�r jedes Datenpaar werden die
# 	# Fehler berechnet, die entstehen, wenn an diesem Datenpaar der Threshold
# 	# gesetzt wird. Die Summe der beiden Fehler ergibt den zu minimierenden
# 	# Gesamtfehler.
#         # no error bound given: minimize overall error
#         
#     fehlerrate_1=cumsum(is_match!=1)/as.numeric(1:n_data) # false positive
#     fehlerrate_2=rev(cumsum(rev(is_match==1)/as.numeric(1:n_data))) # false negative
#     fehlerrate=c(0,fehlerrate_1)+c(fehlerrate_2,0)
#     return(weights[which.min(fehlerrate)])
# 
#     # nun baue Tabelle, in der Gewicht (unique) und Fehlerrate gegen�bergestellt
#     # sind. Die Fehlerrate eines Gewichts ist in der sortierten Tabelle gleich
#     # der Fehlerrate f�r den letzten Datensatz des Blocks
#     # tapply() sortiert aufsteigend, das rev() stellt 
#     # die absteigende Reihenfolge wieder her
# 
#     fehlerrate_unique=rev(tapply(fehlerrate,c(weights,-Inf),tail,1))
# #     weights_unique=unique(weights)
# # 
# #     # Bestimme Gewicht des Datensatzes mit minimalem Fehler.
# #     threshold=as.numeric(weights_unique[which.min(fehlerrate_unique)])
# # 	return (threshold)
# }
# 
# getThresholdMaxFP <- function (weights, is_match, max_fehler=0.01)
# {
# 	n_data=length(is_match)
# 	if (n_data!=length(weights))
# 	{
# 		stop("Argumente weights und is_match haben unterschiedliche L�nge")
# 	}
#     fehlerrate=cumsum(is_match==0)/as.numeric(1:n_data)
#     # nehme den minimalen Threshold, f�r den die Fehlerschranke noch eingehalten
#     # wird
#     threshold=as.numeric(tail(weights[fehlerrate<max_fehler],1))
#     # Abfangen des Falls, dass nur Nicht-Matche in Stichprobe sind: setze
#     # Threshold auf Maximum der Gewichte
#     if(identical(threshold,numeric(0)))
#     {
#      threshold=max(weights)
#     }
# 	return (threshold)
# }
# 
