#before the TDMs can be created, must be separated into training and test sets

split.em <- function(doc){#characer vector
        inTrain <- rbinom(length(doc),1,.9)
        list(train = doc[as.logical(inTrain)], test = doc[!inTrain])
}

multisplit <- function(docs){#expects a list
        if(!is.list(docs))stop()
        for(i in 1:length(docs)){
                docs[[i]] <- split.em(docs[[i]])
        }
        docs
}


#3rd version of TDM creation functions
TermDocumentMatrices <- function(corp,n){#corp is list of documents for which you want TDMs:
        #must be already split into $train and $test
        #n is a vector of unique integers n>=1, n<=5
        require(magrittr)
        corp %>% TDMsforManydocs(n) %>%
                combineByType()
}

TDMsforManydocs <- function(corp,n){#corp is list of documents for which you want TDMs:
        #must be already split into $train and $test
        #n is a vector of unique integers n>=1, n<=5
        require(magrittr)
        
        TDMs <- list()
        for(i in seq_along(corp)){
                TDMs[[i]] <- getTrainingSet(corp[[i]]) %>% 
                        docAsDT("name") %>%
                        createtokens(n) %>%
                        TDMsforOneDoc(n,names(corp)[i])
        }
        gc()#the previous is a very memory intensive procedure. Make sure to reclaim some memory.
        names(TDMs) <- names(corp)
        c("uni", "bi", "tri", "four", "five")[1:length(n)]
        TDMs
        
}

getTrainingSet <- function(splitDoc){#splitDoc is a document split into a two entry list:
        #$train and $test
        splitDoc$train
        
}

docAsDT <- function(doc,docname){#doc is a character vector,at this point docname names the output file
        require(quanteda)
        require(stringi)
        require(data.table)
        # filename <- paste(docname,".csv",sep="")
        #in trying to make things faster down the road i attempted to make the function
        #read in a previously made document. Unfortunately it either reads or writes the
        #data incorrectly, so the time saving was abandonned in favor of reproducibility
        #if(file.exists(filename)) return(fread(filename))
        tokens <- tokenize(stri_trans_tolower(doc))
        data.1 <- tokenstoDT(tokens = tokens)
        data.1 <- data.1[theterms != ""]#remove the several empty cells
        #fwrite(data.1,filename)
        data.1
}

tokenstoDT <- function(tokens){
        tokens <- unlist(tokens)
        data.table(theterms=tokens)
}

createtokens <- function(DTdoc,N){#DTdoc is a document that has been uni tokenized, 
        #N is vector of unique integers N>=1, N<=5
        if(!is.data.table(DTdoc))stop()
        if(length(DTdoc) > 1)stop()
        if(names(DTdoc)!="theterms")stop()
        possibilities <- c("uni","bi","tri","four","five")
        DTdoc[,newf := theterms]
        for(i in 1:max(N)){
                DTdoc[,newf:=c(theterms[i:.N],rep(NA,i-1))]
                setnames(DTdoc, "newf",possibilities[i])
        }
        DTdoc[,theterms:=NULL]
}

TDMsforOneDoc <- function(doc,n,docname){#doc is a document as a datatable; tokenized, 
        #n is vector of unique integers n>=1, n<=5
        gc()#this function is usually run many times and once a document has been categorized
                #it is not longer needed. memory doesn't seem to be automatically freed up, so
                #we do that here.
        name <- paste(deparse(substitute(doc)),docname,sep="")
        TDMlist <- list()
        for(i in n){
                TDMlist[[i]] <- createTDM2.1(doc, i, name)
                
        }
        TDMlist
        #assign(paste(name,".TDMlist",sep=""),)
}



createTDM2.1 <- function(doc,N,name){#doc is a tokenized document in data.table form,
        #N is the type of N-gram, 
        #name is the name of the document
        possibilities <- c("uni","bi","tri","four","five",1,2,3,4,5)
        if(!N %in% possibilities)stop()
        if(is.character(N)) N=grep(N,possibilities)
        TDM <- doc[,.N,by=eval(names(doc)[1:N])]
        names(TDM) <- c(possibilities[1:N],name)
        filename <- paste(name,possibilities[N],".TDM",sep="")
        #fwrite(TDM,filename)
        
        #assign(filename,TDM,parent.frame(2))#this makes it not work when called from global env
        TDM
} 

combineByType <- function(TDMs){#TDMs is a list of TDMs arranged by document
        
        #find a non null TDM to 
        index <- which(sapply(TDMs[[1]],length) == max(sapply(TDMs[[1]],length)))
        finallist <- list()
        
        for(i in 1:(length(TDMs[[1]][[index]])-1)){
                #if(!is.null(TDMs[[i]][[i]])){
                finallist[[i]] <- lapply(TDMs, "[[", i) %>%
                        combiner()
                f_dowle2.1(finallist[[i]])
                #}
        }
        finallist
}

combiner <- function(TDMs){ #takes a list of documents to make into TDM
        Reduce(mymerge,TDMs)
}

mymerge = function(x,y) merge(x,y,all=TRUE)#function suggested by SO for DT merges

#The function bellow was borrowed from a stack overflow answer: 
        #https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
        #It replaces all NA values in the table with 0.
f_dowle2.1 = function(DT) {#function modified from SO answer
        types <- c("uni","bi","tri","four","five")
        for (i in setdiff(names(DT), types))
                DT[is.na(get(i)), (i):=0]
}
