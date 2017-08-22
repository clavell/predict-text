#create a new way of storing ngrams: Just offset the corpus by one.
#first make a data table of eachword in the corpus
docAsDT <- function(doc,docname){#doc is a character vector
        require(quanteda)
        require(stringi)
        require(data.table)
        filename <- paste(docname,".csv",sep="")
        if(file.exists(filename)) return(fread(filename))
        tokens <- tokenize(stri_trans_tolower(doc))
        data.1 <- tokenstoDT(tokens = tokens)
        data.1 <- data.1[theterms != ""]
        fwrite(data.1,filename)
        data.1
}

tokenstoDT <- function(tokens){
        tokens <- unlist(tokens)
        data.table(theterms=tokens)
}

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


               
#use just a small set to test functions
        shortdocs <- lapply(doclist,lapply,head,100)
        shortdoc <- shortdocs[[1]]$train
#make a document to test with
        document.1 <- docAsDT(doclist$`final/en_US/en_US.blogs.txt`$train,"blog")
#after we have the document in a data table form we can replicate the column offset by
#one to show the bigrams
        
        
        document.1[,bi:=c(theterms[2:.N],NA)]
        document.1[,tri:=c(theterms[3:.N],NA,NA)]
        document.1[,conc := paste(theterms,bi,sep="_")]
        document.1[,docname := NULL]
        document.1 <- document.1[theterms != ""]
        fwrite(document.1,file = "blogasDT.csv")
        document.1.frea <- fread("blogasDT.csv")
        TDMbi <- document.1[,.N,by=.(theterms,bi)]

createTDM2.0 <- function(doc,N,name){#document is a character vector, N is the type of N-gram
        possibilities <- c("uni","bi","tri","four","five",1,2,3,4,5)
        if(!N %in% possibilities)stop()
        if(is.character(N)) N=grep(N,possibilities)
        if(file.exists(paste(name,".csv",sep=""))){
                
        }
        document <- docAsDT(doc)
        document[,newf := theterms]
        for(i in 1:N){
                document[,newf:=c(theterms[i:.N],rep(NA,i-1))]
                names(document) <- c("theterms",possibilities[1:i])
        }
        document[,theterms:=NULL]
        document[,.N,by=names(document)]
}

docAsDT(shortdoc,"blah")

triTDMblog <- createTDM2.0(doclist$`final/en_US/en_US.blogs.txt`$train,3)
document.1[25412963]
