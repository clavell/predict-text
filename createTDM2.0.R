library(data.table)

#create a new way of storing ngrams: Just offset the corpus by one.
#first make a data table of eachword in the corpus
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

#check against fread document to see which rows aren't reading in right
        # them <- which(!(document.1.frea[,theterms] == document.1[,theterms]))
        # quotes <- document.1[them,unique(theterms)]
        # quotes.frea <- document.1.frea[them,unique(theterms)]
        # document.1[(ind-6):(ind+6)]
        # sum(as.integer(counts))
        # sum(as.integer(counts.frea))
        # counts <- character()
        # counts.frea <- character()
        # 
        # document.1[them]
        # 
        # for(i in quotes){
        #         counts[i] <- document.1[theterms == i,.N]
        # }
        # for(i in quotes.frea){ 
        #         counts.frea[i] <- document.1.frea[theterms == i,.N]
        # }
        # 
        # sum(as.integer(counts.frea[2:length(counts.frea)]))
        # sum(as.integer(counts[2:length(counts)]))
        #didn't find a solution as the counts found are different
        
        

#should make a training and test set
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

        set.seed(39384)
        doclist <- multisplit(doclist)


               

#create a function to turn a document into a TDM
createTDM2.0 <- function(doc,N,name){#document is a character vector, N is the type of N-gram
        possibilities <- c("uni","bi","tri","four","five",1,2,3,4,5)
        if(!N %in% possibilities)stop()
        if(is.character(N)) N=grep(N,possibilities)
        if(!"document" %in% ls())docAsDT(doc,name)
        document[,newf := theterms]
        for(i in 1:N){
                document[,newf:=c(theterms[i:.N],rep(NA,i-1))]
                names(document) <- c("theterms",possibilities[1:i])
        }
        document[,theterms:=NULL]
        TDM <- document[,.N,by=names(document)]
        names(TDM) <- c(possibilities[1:N],name)
        filename <- paste(name,possibilities[N],".TDM.csv",sep="")
        fwrite(TDM,filename)
        TDM
}



#check the blogTDM made by createTDM2.0 against the TDM fread in.
        # uniTDMblog <- createTDM2.0(shortdocs$`final/en_US/en_US.blogs.txt`$train,1,"blog")
        # uniTDMblog.frea <- fread("blog.TDM.csv")
        # identical(uniTDMblog,uniTDMblog.frea)
        # #yet again these are not identical..
        # all(uniTDMblog[,blog] == uniTDMblog.frea[,blog])#all counts are the same
        # uniTDMblog[715];uniTDMblog.readcsv[715] #it's just the tokens that aren't all the same
        # 
        # #try reading in with read.csv and changing to data.table
        # uniTDMblog.readcsv <- read.csv("blog.TDM.csv")
        # uniTDMblog.readcsv <- as.data.table(uniTDMblog.readcsv)
        # identical(uniTDMblog,uniTDMblog.readcsv) 
        # #this says they aren't identical but if we check their columns..
        # identical(uniTDMblog[,blog],uniTDMblog.readcsv[,blog]) #TRUE
        # identical(uniTDMblog[,uni], uniTDMblog.readcsv[,uni]) #FALSE 
        # all(uniTDMblog[,uni] == uniTDMblog.readcsv[,uni]) #TRUE (this is weird 
        #that all and identical get different results. oh well.. it seems fread is the problem, not
        #fwrite)

#now that we have a somewhat working TDM creator for a single document, let's make 
#functions that make a TDM with multiple documents!
combineTDM <- function(TDMlist,docname,toGlobal=TRUE){#takes a list of TDMs(data.tables)
        TDMtype <- names(TDMlist[[1]])[length(TDMlist[[1]])-1]
        TDMname <- paste(docname,TDMtype,"TDM",sep="")
        mymerge = function(x,y) merge(x,y,all=TRUE)#function suggested by SO for DT merges
        
        fullTDM <- Reduce(mymerge,TDMlist)
        f_dowle2.1(fullTDM)
        if(toGlobal) assign(TDMname,fullTDM,.GlobalEnv)

        if(!toGlobal) assign(TDMname,fullTDM,parent.frame(2))
}

f_dowle2.1 = function(DT) {
        types <- c("uni","bi","tri","four","five")
        for (i in setdiff(names(DT), types))
                DT[is.na(get(i)), (i):=0]
}


TDMlister <- function(docs,N){#argument docs is a list of documents,N is passed to 
                                                                        #createTDM2.0
        TDMList <- list()
        for(i in seq_along(docs)){
                TDMList[[i]] <- createTDM2.0(doc = docs[[i]]$train,N,name = names(docs)[i])
        }
        TDMList#list of data.table TermdocumentMatrices (one for each document)
}


corpusTDMcreator <- function(corpus,n,name=NULL,toGlobal=TRUE){#corpus is a list of character vectors, n is the ngram
        if(is.null(name)) name <- deparse(substitute(corpus))#get the name of the corpus object (list of docs)
        combineTDM(TDMlister(corpus,n),name,toGlobal)#give the TDM a name
      
}

#now let's make all of the possible TDM's up to five-grams at once for the corpus
multiTDMs <- function(corp,ngrams,List=FALSE){#corp is a list of documents(corpus), ngrams is a sequence
                                                                     #min 1, max 5
        if(max(ngrams)>5|min(ngrams)<1) stop()
        name <- deparse(substitute(corp))
        for(i in ngrams){
            corpusTDMcreator(corp,i,name,!List)    
        }
        if(List){
                ListofTDMs <- list()
                possibilities <- c("uni","bi","tri","four","five")
                TDMname <- character()
                for(i in ngrams){
                        TDMname[i] <- paste(name,possibilities[i],"TDM",sep="")
                        ListofTDMs[[i]] <- get(TDMname[i])
                }
                names(ListofTDMs) <- TDMname
                assign(paste(name,"TDMs",sep=""),ListofTDMs, .GlobalEnv)
        }
}

#to make multi-TDM creation more efficient, use docAsDT only once per doc and create the TDM
#for each desired n-gram tokenizer before moving on to the next doc. This is no more space
#inefficient and computationally much more efficient.
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
   
#overall function layout
        #takes document list element by element
                #creates tokenized version
                #creates a TDM for each type of ngram
        #combines the TDMs by ngram tyoe
        #returns TDMs or list of TDMs

TDMsforOneDoc <- function(doc,n,docname){#doc is a document as a datatable; tokenized, 
                                        #n is vector of unique integers n>=1, n<=5

        name <- paste(deparse(substitute(doc)),docname,sep="")
        TDMlist <- list()
        for(i in n){
               TDMlist[[i]] <- createTDM2.1(doc, i, name)

        }
        TDMlist
        #assign(paste(name,".TDMlist",sep=""),)
}

getTrainingSet <- function(splitDoc){#splitDoc is a document split into a two entry list:
                                        #$train and $test
        splitDoc$train
        
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
        
        names(TDMs) <- names(corp)
                c("uni", "bi", "tri", "four", "five")[1:length(n)]
        TDMs
        
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

TermDocumentMatrices <- function(corp,n){#corp is list of documents for which you want TDMs:
                                        #must be already split into $train and $test
                                        #n is a vector of unique integers n>=1, n<=5
        require(magrittr)
        corp %>% TDMsforManydocs(n) %>%
                combineByType()
}

#use just a small set to test functions
        shortdocs2 <- lapply(doclist,lapply,head,100)

#Now, let's use the shortdocs with our functions to check if the functions work properly
        #to keep separate from the real thing change names slightly
        names(shortdocs2) <- paste("short-",names(shortdocs2),sep="")

       

#Maybe a function to make the first n-1 words of ngrams into one column to save space
combineInitial <- function(TDM){
        columns <- ncol(TDM)
        possibilities <- c("uni","bi","tri","four","five")
        type <- sum(possibilities %in% names(TDM))
        TDM[,FirstTerms := do.call(paste,.SD),.SDcols=1:(type-1)]
        setcolorder(TDM,c(columns+1,1:columns))
        TDM[,possibilities[1:(type-1)]:=NULL]
}

getTotals <- function(TDM,deleteOthers = FALSE){
        possibilities <- c("uni","bi","tri","four","five")
        if(is.null(TDM$Sum)){
        TDM[,Sum := Reduce(`+`, .SD),.SDcols = setdiff(names(TDM),possibilities)]
        }
        if(deleteOthers){
                columnstoremove <- setdiff(names(TDM),c("Sum",possibilities))
                TDM[,(columnstoremove):=NULL]
        }
        
}

Tablemaker <- function(DT,wordfreqs=c(1,2,100,200)){#takes TDM, desired ngram counts
        DTname <- deparse(substitute(DT))
        if(!exists(paste(DTname,"$Sum")))getTotals(DT)
        totalngrams <- DT[,sum(Sum)]
        thetable <- data.table(tablename = DTname)
        for(i in wordfreqs){
                        columnname <- paste("frequency >",i)
                        thetable[,get("columnname") := DT[Sum>i,.(sum(Sum))]/totalngrams]
        }
        thetable
}

doclistTDMs[[1]][grep("^/",uni)]
