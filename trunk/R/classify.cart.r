# Epilink mit Kreuzvalidierung, mit echten Validierungsdaten

# Eingabe: 
#   Daten
#   method: rpart, bagging, boosting


classify.rpart = function (rpairs,model=NULL,...)
{
    library(rpart)
    library(ada)
    library(ipred)
    
    train=rpairs$train[,-c(1,2)]
    train$is_match=factor(train$is_match)
    valid=rpairs$valid[,-c(1,2)]
    if (missing(model))
    {
        if (nrow(rpairs$train)==0)
            stop("No training set in rpairs!")
        
        model=rpart(is_match ~ ., data=train,method="class",...)
    } else if (class(model) != "rpart")
        stop ("model must be of class 'rpart'")
    pred=predict(model, newdata=valid)
    ret=rpairs
#    ret$prediction=pred
    ret$prediction=pred[,"TRUE"]>pred[,"FALSE"]
    ret$model=model
    class(ret)="RecLinkResult"
    return(ret)
}


# classify.bagging = function (rpairs,model=NULL,...)
# {
#     library(ipred)
#     set.seed(1)    
#     
#        
#     # the columns of train and valid have to be coerced to factors with
#     # same levels, NA being a factor
#     train=rpairs$train[,-c(1,2)]
#     valid=rpairs$valid[,-c(1,2)]
#     # for bagging, all data has to be converted to factors with equal levels
#     # for train and valid, NA being a level
# #    levels=mapply(function(x,y) union(x,y),train,valid,SIMPLIFY=F)
#     # unklar, ob Sortieren wirklich etwas bringt, sp�ter testen
#     levels=mapply(function(x,y) sort(union(x,y),na.last=T),train,valid,SIMPLIFY=F)
#     train=as.data.frame(mapply(function(x,y) list(factor(x,levels=y,exclude=NULL)),train,levels))     
#     valid=as.data.frame(mapply(function(x,y) list(factor(x,levels=y,exclude=NULL)),valid,levels))
#     if (missing(model))
#     {
#         if (nrow(rpairs$train)==0)
#             stop("No training set in rpairs!")      
#         model=bagging(is_match ~ ., data=train,method="class",...)
#     } else if (class(model) != "classbagg")
#         stop ("model must be of class 'classbag'")
#     pred=predict(model, newdata=valid)
#     ret=rpairs
#     ret$model=model
#     ret$prediction=as.logical(levels(pred))[pred]
#     class(ret)="RecLinkResult"
#     return(ret)
# }

classify.bagging = function (rpairs,model=NULL,...)
{
    library(ipred)
    set.seed(1)    

    train=rpairs$train[,-c(1,2)]
    valid=rpairs$valid[,-c(1,2)]
    X=train[,-ncol(train)]
    y=factor(train$is_match)
    if (missing(model))
    {
        if (nrow(rpairs$train)==0)
            stop("No training set in rpairs!")      
        model=ipredbagg(y=y,X=X)
    } else if (class(model) != "classbagg")
        stop ("model must be of class 'classbag'")
    X=valid[,-ncol(valid)]
    pred=predict(model, newdata=X)
    ret=rpairs
    ret$model=model
    ret$prediction=as.logical(levels(pred))[pred]
    class(ret)="RecLinkResult"
    return(ret)
}

# classify.ada = function (rpairs,model=NULL,...)
# {
#     library(ada)
#     train=rpairs$train[,-c(1,2)]
#     train$is_match=factor(train$is_match)
#     valid=rpairs$valid[,-c(1,2)]
#     # for ada, all data has to be converted to factors with equal levels
#     # for train and valid, NA being a level
#     levels=mapply(function(x,y) union(x,y),train,valid)
#     train=as.data.frame(mapply(function(x,y) list(factor(x,levels=y,exclude=NULL)),train,levels))
#     valid=as.data.frame(mapply(function(x,y) list(factor(x,levels=y,exclude=NULL)),valid,levels))
#     if (missing(model))
#     {
#         if (nrow(rpairs$train)==0)
#             stop("No training set in rpairs!")      
#         model=ada(is_match ~ ., data=train,...)
#     } else if (class(model) != "ada")
#         stop ("model must be of class 'ada'")
#     pred=predict(model, newdata=valid)
#     ret=rpairs
#     ret$prediction=as.logical(levels(pred))[pred]
#     ret$model=model
#     class(ret)="RecLinkResult"
#     return(ret)
# }


classify.ada = function (rpairs,model=NULL,...)
{
    library(ada)
    set.seed(1)    

    train=rpairs$train[,-c(1,2)]
    valid=rpairs$valid[,-c(1,2)]
    X=train[,-ncol(train)]
    y=factor(train$is_match)
    if (missing(model))
    {
        if (nrow(rpairs$train)==0)
            stop("No training set in rpairs!")      
        model=ada(y=y,x=X)
    } else if (class(model) != "classbagg")
        stop ("model must be of class 'classbag'")
    X=valid[,-ncol(valid)]
    pred=predict(model, newdata=X)
    ret=rpairs
    ret$model=model
    ret$prediction=as.logical(levels(pred))[pred]
    class(ret)="RecLinkResult"
    return(ret)
}

