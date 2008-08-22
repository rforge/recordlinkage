# Bildet Vergleichsmuster von Datens�tzen
# Argumente:
#   dataset     data frame mit Datens�tzen, zeilenweise
#   blockfld    Liste von Vektor mit Feldern, die zum Blocking verwendet werden.
#               Ein Vektor enth�lt jeweils eine Menge von Feldern, die 
#               gleichzeitig �bereinstimmen m�ssen (Und-Verkn�pfung). F�r mehrere
#               Vektoren in der Liste wird das Blocking mehrmals mit den jeweiligen
#               Kriterien durchgef�hrt und alle so erhaltenen Mengen von 
#               Datensatzpaaren in die Ausgabe �bernommen (Oder-Verkn�pfung).
#               Wird nur ein Vektor oder Wert eingegeben, so wird dieser in
#               eine List mit einem Element (der Eingabe selbst) umgewandelt
#   blockfun    Benutzerdefiniertes Blockingkriterium. Eine Funktion,
#               die als Argumente zwei Zeilen von dataset hat und TRUE zur�ckgibt,
#               falls diese als Paar in die Vergleichsmuster �bernommen werden
#               sollen, ansonsten FALSE.
#  ids          TRUE, falls die erste Spalte ids enth�lt. Ansonsten werden die 
#               Zeilen von dataset durchnummeriert.                                     
#  phonetic     Entweder ein Vektor mit Indizes von Spalten, die vor der Bildung der 
#               Vergleichsmuster in einen phonetischen Code transformiert werden
#               sollen, oder TRUE, falls dies f�r alle Felder geschehen soll.
#  strcmp       Entweder ein Vektor mit Indizes von Feldern, f�r die bei der
#               Bildung der Vergleichsmuster eine Stringmetrik verwendet werden
#               soll, oder TRUE, falls dies f�r alle Felder geschehen soll.


# R�ckgabe: Stringvergleich gibt bei "NA" 0 zur�ck!

# blockfun, phonetic: noch nicht implementiert 

# Bin�r und Fuzzy in Ergebnisliste? (auf Wunsch)


# externe IDs gehen noch nicht! (Es wird mit id indiziert)




# Weitere m�gliche Parameter:
#  keep            Vektor von Feldern, die unver�ndert in die Paare �bernommen werden
#                  sollen, oder TRUE, falls dies f�r alle geschehen soll (damit k�nnen
#                  Paare gebildet werden, f�r die erst sp�ter Vergleichsmuster erstellt
#                  werden sollen)
#  strcmpfun       Funktion, die als Argumente zwei Strings hat und einen
#                  Vergleichswert im Intervall [0,1] zur�ckgibt. Default: jarowinkler
#  phonfun         Funktion, die als Argument eine String hat und dessen phonetische
#                  Codierung zur�ckgibt.


# R�ckgabe: Liste mit Elementen patterns (Tabelle aus Vergleichsmustern), 
# frequencies (durchschnittliche relative H�ufigkeiten der Attribute)



compare <- function(dataset, blockfld=FALSE, blockfun=NULL,ids=1:nrow(dataset),phonetic=FALSE,strcmp=FALSE)
{
    ids # Argument wird erst initialisiert, wenn es gebraucht wird,
        # deshalb muss er hier auftauchen, bevor dataset transponiert wird 
    # format conversions
    ndata=nrow(dataset)
    dataset=as.matrix(dataset)
    dataset[dataset==""]=NA
    
    if (!isFALSE(phonetic))
    {
        if (isTRUE(phonetic))
        {    
            dataset=pho_h(dataset)
        } else
        dataset[,phonetic]=pho_h(dataset[,phonetic])
    }
    
    if (!is.function(blockfun))
        blockfun=jarowinkler
    
    if (!identical(blockfld,F))
    {
        keys=ids
        pairs=list()
        
        for (blockelem in blockfld)
        {
          blockstr=apply(dataset[,blockelem,drop=F],1,paste,collapse=" ")
          names(keys)=blockstr
          
          blocktable=list()
          
          for (i in 1:ndata)
          {
              x=blocktable[[blockstr[i],exact=T]]
              if (is.null(i))
              {
                  blocktable[[blockstr[i]]]=keys[i]
              } else
              {
                  blocktable[[blockstr[i]]]=c(x,keys[i])
              }
          }
      
      
          for (e in blocktable)
          {
              if (length(e) > 1)
              {
                  pairs[[length(pairs)+1]]=ordered_pairs(e)    
              }
          }  
        }
        if (length(pairs)==0)
            return (NULL)
        pairs=unlist(pairs)
        dim(pairs)=c(2,length(pairs)/2)  
        pairs=t(pairs)
    } else # blockfld==FALSE
    {
        pairs=t(ordered_pairs(ndata))
    }    

    if (isTRUE(strcmp))
    {
       patterns=unique(cbind(pairs,blockfun(dataset[pairs[,1],,drop=F],
                                               dataset[pairs[,2],,drop=F])))
    } else 
    {
        patterns=unique(cbind(pairs,dataset[pairs[,1],,drop=F]==dataset[pairs[,2],,drop=F]))
        if (!identical(strcmp,F))
            patterns[,strcmp+2]=blockfun(dataset[pairs[,1],strcmp,drop=F],
                                               dataset[pairs[,2],strcmp,drop=F])
    }                    
    
    if (nrow(patterns)>1)
        patterns=patterns[order(patterns[,1],patterns[,2]),]
    colnames(patterns)=c("id1","id2",colnames(dataset))
    rownames(patterns)=NULL
    return(patterns)
}


# erste Version der Funktion: quadratische Laufzeit, f�r realistische L�ngen
# von dataset deutlich langsamer

# compare <- function(dataset, blockfld=FALSE, blockfun=NULL,ids=1:nrow(dataset),phonetic=FALSE,strcmp=FALSE)
# {
#     ids # Argument wird erst initialisiert, wenn es gebraucht wird,
#         # deshalb muss er hier auftauchen, bevor dataset transponiert wird 
#     # format conversions
#     dataset=t(as.matrix(dataset))     
#     dataset[dataset==""]=NA
#       
#     ndata=ncol(dataset)
# 
#     if (!identical(blockfld,F))
#     {
#         if (!is.list(blockfld))
#             blockfld=list(blockfld)
#     
#         outerexpr=character()    
#         for (blockel in blockfld)
#         {
#             innerexpr=character()
#             for (fld in blockel)
#             {
#                 innerexpr=append(innerexpr, sprintf("X[%d,]==1",fld+2))
#             }
#             outerexpr=append(outerexpr, paste("(",paste(innerexpr,collapse=" & "),")",sep=""))
#         }
#         expr=paste(outerexpr,collapse=" | ")
# #        print(expr)
#         
#         eval(parse(text=sprintf("blockfun <- function(X) return(X[,%s,drop=F])",expr))) 
#     
#     }
# 
#     if (length(ids)!=ndata)
#     {
#        if (length(ids)!=1)
#        {
#           stop("length(ids) must be either nrow(data) or 1")
#        }
#        idcol=ids
#        ids=dataset[,ids]
#        dataset[[idcol]]=NULL
#     } else
# #        if (!identical(blockfld,F))
# #           blockfld=lapply(blockfld,"+",2) # muss noch besser gemacht werden! 
# 
# 
# #  if (phonetic)... erst Phonetik in R implementieren
# 
#     patterns=list()
#     for (i in 1:(ndata-1))
#     {        
#         temp_patterns=rbind(ids[i],ids[(i+1):ndata],
#             (dataset[,i]==dataset[,(i+1):ndata,drop=F]))
#         if (!identical(blockfld,F))
#         {
#               temp_ids=blockfun(temp_patterns)[1:2,,drop=F]          
#             if (ncol(temp_ids)==0)
#                 next
#         } else
#         {
#             temp_ids=temp_patterns[1:2,,drop=F]
#             if (ncol(temp_ids)==0)
#                 next
#         }
#             
# 
#         if (isTRUE(strcmp))
#         {
#             temp_patterns=rbind(temp_ids,
#                 apply(dataset[,temp_ids[2,],drop=F],2,jarowinkler,dataset[,i]))
#         } else
#         {       
#             temp_patterns=rbind(temp_ids,
#                 (dataset[,i]==dataset[,temp_ids[2,],drop=F]))
#             if (!identical(strcmp,F))
#                 temp_patterns[strcmp+2,]=apply(dataset[strcmp,temp_ids[2,],drop=F],2,jarowinkler,dataset[strcmp,i])
#          }     
#         patterns[[i]]=temp_patterns
#     }
#     if (length(patterns)==0)
#         return (NULL)
#     # convert back to matrix
#     patterns=unlist(patterns)
#     patterns=unique(matrix(patterns,ncol=nrow(dataset)+2,nrow=length(patterns)/(nrow(dataset)+2),byrow=T))
#     colnames(patterns)=c("id1","id2",rownames(dataset))
#     return(patterns)
# }



 