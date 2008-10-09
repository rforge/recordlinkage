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
#  phonetic     Entweder ein Vektor mit Indizes von Spalten, die vor der Bildung der 
#               Vergleichsmuster in einen phonetischen Code transformiert werden
#               sollen, oder TRUE, falls dies f�r alle Felder geschehen soll, oder
#               FALSE, falls keine Phonetik verwendet wird.
#  strcmp       Entweder ein Vektor mit Indizes von Feldern, f�r die bei der
#               Bildung der Vergleichsmuster eine Stringmetrik verwendet werden
#               soll, oder TRUE, falls dies f�r alle Felder geschehen soll.
#
#  strcmpfun    Funktion, die als Argumente zwei Strings hat und einen 
#               Vergleichswert im Interval [0,1] zur�ckgibt. Standard: Jaro-Winkler
#  phonfun      Funktion, die f�r einen String einen phonetischen Code berechnet
#               Standard: "Hannoveraner" Phonetik
#
#               strcmpfun und phonfun m�ssen vektorisierbar sein. 
#
#  exclude      Vektor mit Indizes von Spalten, die von Paarbildung und Matching
#               ausgenommen sind. Erlaubt zum Beispiel, externe identifier zu
#               verwenden.
#
#               Indizes, die in exclude erscheinen, sollten nicht in 
#               phonetic, strcmp und blockfld verwendet werden. F�r strcmp
#               und blockfld werden solche Indizes stillschweigend entfernt,
#               in blockfld verursachen sie einen Fehler.

# R�ckgabe: Stringvergleich gibt bei "NA" 0 zur�ck!

# blockfun, phonetic: noch nicht implementiert 

# Bin�r und Fuzzy in Ergebnisliste? (auf Wunsch)






# Weitere m�gliche Parameter:
#  phonfun         Funktion, die als Argument eine String hat und dessen phonetische
#                  Codierung zur�ckgibt.


# R�ckgabe: Liste mit Elementen patterns (Tabelle aus Vergleichsmustern), 
# frequencies (durchschnittliche relative H�ufigkeiten der Attribute)



compare <- function(dataset, blockfld=FALSE, phonetic=FALSE,
                    phonfun=F, strcmp=FALSE,strcmpfun=FALSE, exclude=F)
{
    # various catching of erronous input
    if (!is.data.frame(dataset) && !is.matrix(dataset))
        stop ("Illegal format of dataset")
    ndata=nrow(dataset) # number of records
    nfields=ncol(dataset)
    if (ndata<2) 
        stop ("dataset must contain at least two records")
    if (!is.numeric(strcmp) && !is.logical(strcmp))
        stop ("strcmp must be numeric or a single logical value")
    if (!is.numeric(phonetic) && !is.logical(phonetic))
        stop ("phonetic must be numeric or a single logical value")
    if (max(strcmp)>nfields|| any(is.na(strcmp)))
        stop ("strcmp contains out of bounds index")
    if (max(phonetic)>nfields || any(is.na(phonetic)))
        stop ("phonetic contains out of bounds index")
    if (!is.numeric(exclude) && !isFALSE(exclude))
        stop ("exclude must be numeric or FALSE")
    if (max(exclude)>nfields)
        stop ("exclude contains out of bounds index")
    
    ret=list()  # return object
    # rownames(ret$data)=1:ndata
    # handle excluded columns
    full_data=as.matrix(dataset)


    # keep phonetics for blocking fields
    if (is.numeric(phonetic))
    {
        phonetic_block=intersect(phonetic,unlist(blockfld))
    }
    if (is.numeric(exclude))
    {        
        dataset=dataset[,-exclude]  # remove excluded columns
        # adjust indices to list of included fields
        if (is.numeric(phonetic)) 
        {
            phonetic=setdiff(phonetic,exclude)
            phonetic=sapply(phonetic,function(x) return (x-length(which(exclude<x))))
        }
        if (is.numeric(strcmp))
        {
            strcmp=setdiff(strcmp,exclude)
            strcmp=sapply(strcmp,function(x) return (x-length(which(exclude<x))))       
        }
        # no longer neccessary:
        #blockfld=lapply(blockfld,function(x) return (x-length(which(exclude<x))))
    }
    # issue a warning if both phonetics and string metric are used on one field
    if ((length(intersect(phonetic,strcmp))>0 && !isFALSE(strcmp) && !isFALSE(phonetic)) ||
         (isTRUE(strcmp) && !isFALSE(phonetic)) ||
         (isTRUE(phonetic) && !isFALSE(strcmp)))
    {
        warning(sprintf("Both phonetics and string metric are used on some fields",length(intersect(phonetic,strcmp))))
    }
    dataset[dataset==""]=NA # label missing values
    full_data[full_data==""]=NA # label missing values
    dataset=as.matrix(dataset)        


    if (!is.function(phonfun))
        phonfun=pho_h

    if (!isFALSE(phonetic)) # true, if phonetic is T or not a logical value
    {
        if (isTRUE(phonetic)) # true, if phonetic is a logical value and T
        {    
            dataset=pho_h(dataset)
        } else # phonetic is not a logical value
        dataset[,phonetic]=pho_h(dataset[,phonetic])
    }
    
    if (!is.function(strcmpfun))
        strcmpfun=jarowinkler

        
    
    pair_ids=matrix(0,nrow=0,ncol=2) # each row holds indices of one record pair
    if (!is.list(blockfld)) blockfld=list(blockfld)
    for (blockelem in blockfld) # loop over blocking definitions
    {
      if (isTRUE(phonetic))
      {
        block_data=phonfun(full_data)
      } else if (is.numeric(phonetic))
      {
        block_data=full_data
        block_data[,phonetic_block]=phonfun(full_data[,phonetic_block])
      } else
      {
        block_data=full_data
      }
      # for each record, concatenate values in blocking fields
      blockstr=apply(block_data,1,function(x) paste(x[blockelem],collapse=" "))
      # delete.NULLs(tapply(...)) gives for each value of the blocking string
      # indices of matching records. From these lapply builds record pairs via
      # unordered_pairs (defined in tool.r). unlist makes a vector from the
      # resulting list which is reshaped as a matrix in the following line
      id_vec=unlist(lapply(delete.NULLs(tapply(1:ndata,blockstr,function(x) if(length(x)>1) return(x))),unordered_pairs))
      # reshape vector and attach to matrix of record pairs
      if (!is.null(id_vec))
        pair_ids=rbind(pair_ids,t(matrix(id_vec,ncol=length(id_vec)/2,nrow=2)))
    }
    # return empty data frame if no pairs are obtained
    if (length(pair_ids)==0)
    {
        ret$pairs=data.frame() 
    } else
    {
      pair_ids=unique(matrix(as.integer(pair_ids),ncol=ncol(pair_ids),nrow=nrow(pair_ids)))
      pair_ids=cbind(1:nrow(pair_ids),pair_ids) # add index for each pair
      colnames(pair_ids)=c("pair_id","id_1","id_2")
      
      # get data for left hand side of record pairs    
      left=merge(pair_ids[,c(1,2),drop=F],cbind(1:nrow(dataset),dataset),by.x=2,by.y=1,sort=F)
      left=left[order(left[,"pair_id"]),] # merge scrambles rows, reorder
      # get data for right hand side of record pairs
      right=merge(pair_ids[,c(1,3),drop=F],cbind(1:nrow(dataset),dataset),by.x=2,by.y=1,sort=F)
      right=right[order(right[,"pair_id"]),] # merge scrambles rows, reorder
      # matrix to hold comparison patterns
      patterns=matrix(0,ncol=ncol(left)-2,nrow=nrow(left)) 
      if (isTRUE(strcmp))
      {
          patterns=strcmpfun(as.matrix(left[,-(1:2)]),as.matrix(right[,-(1:2)]))
      } else if (is.numeric(strcmp)) 
      {
          patterns[,-strcmp]=(as.matrix(left[,-c(1,2,strcmp+2)])==as.matrix(right[,-c(1,2,strcmp+2)]))*1
          patterns[,strcmp]=strcmpfun(as.matrix(left[,strcmp+2]),as.matrix(right[,strcmp+2])) #*1
      } else
      {
          patterns=(as.matrix(left[,-c(1,2)])==as.matrix(right[,-c(1,2)]))*1    
      }                    
      patterns=cbind(pair_ids[,-1,drop=F],as.data.frame(patterns))
      patterns=patterns[order(patterns[,1],patterns[,2]),] # muss nicht unbedingt sein, evtl Argument
      colnames(patterns)=c("id1","id2",colnames(dataset))
      rownames(patterns)=NULL
      ret$pairs=patterns
    }
    ret$data=as.data.frame(full_data)
    frequencies=apply(dataset,2,function(x) 1/length(unique(x)))
    ret$frequencies=frequencies
    class(ret)="RecLinkPairs"
    return(ret)
}


 