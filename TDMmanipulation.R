#TDM manipulation functions
removeTokes <- function(DT){#function removes unwanted tokens from TDMs
        require(data.table)
        possibilities <- c("uni", "bi", "tri", "four", "five")
        for(i in possibilities[possibilities %in% names(DT)]){
                setkeyv(DT, i)
                DT <- DT[grep("[a-zA-Z]",get(i))]
                #we also want to remove profanity. Here is a large list of bad words.
                if(!file.exists("swearWords.csv")){
                        download.file("http://www.bannedwodlist.com/lists/swearWords.csv",
                                      destfile = "swearWords.csv")
                }
                swears <- fread("swearWords.csv")
                swears <- names(swears)
                swears
                DT <- DT[!swears]
                #remove words that start with # as they are mostly just twitter hashtags and appear at the end
                #of entries
                DT <- DT[!grep("^#",get(i))]
        }
        DT
}

combineInitial <- function(TDM){
        columns <- ncol(TDM)
        possibilities <- c("uni","bi","tri","four","five")
        type <- sum(possibilities %in% names(TDM))
        if(type > 2){
                TDM[,FirstTerms := do.call(paste,.SD),.SDcols=1:(type-1)]
                setcolorder(TDM,c(columns+1,1:columns))
                TDM[,possibilities[1:(type-1)]:=NULL]
        }
}


uncombine <- function(TDM){#make sure to set this to an object
                                #does the opposite of combine initial
        if(!is.null(TDM$FirstTerms)){
                require(quanteda)
                columnNums <- 1:length(unlist(tokenize(TDM$FirstTerms[1])))
                possibilities <- c("uni", "bi", "tri", "four", "five")
                columns <- tstrsplit(TDM$FirstTerms,split = " ") %>% 
                        as.data.table() %>% setnames(possibilities[columnNums])
                TDM[,FirstTerms:=NULL]
                cbind(columns,TDM)
        }else{TDM}
}


bestMaker <- function(TDM){#chooses the best (most frequent) result from given predictors for a TDM
        combineInitial(TDM)
        x <- if(!is.null(TDM$FirstTerms)){"FirstTerms"}else{"uni"}
        setkeyv(TDM,c(x,"Sum"))
        
        TDM <- TDM[TDM[,unique(.SD),.SDcols=x],mult="last"]
        uncombine(TDM)
}

indexifyTable = function(TDM,unigrams) {#changes a TDM into a bunch of indices referring to 
                                                #a key with corresponding words
        possibilities <- c("uni","bi","tri","four","five")
        type <- sum(names(TDM) %in% possibilities)
        
        for (i in possibilities[1:type])
                TDM[,(i) := unigrams[TDM[[i]],which=TRUE]]
}

wordifyTable <- function(TDM,unigrams){#opposite of indexifyTable()
        possibilities <- c("uni","bi","tri","four","five")
        type <- sum(names(TDM) %in% possibilities)
        
        for (i in possibilities[1:type]){
                TDM[,(i) := unigrams[TDM[[i]]]$uni]
                
        }        
}

TDMsetkey <- function(TDM){   #sets a useful key for prediction for given TDM
        possibilities <- c("uni","bi","tri","four","five")
        
        type <- sum(names(TDM) %in% possibilities)
        setkeyv(TDM,possibilities[1:(type-1)])
}

WordtoIndex <- function(predictor,conversionTable){#gets the index of a word from the key
        conversionTable[predictor,which=TRUE]
}

IndextoWord <- function(index, conversionTable){#opposite of WordtoIndex()
        conversionTable[index]$uni
}

getTotals <- function(TDM,deleteOthers = FALSE){#sums the total appearances of each token throughout the TDM
        possibilities <- c("uni","bi","tri","four","five")
        if(is.null(TDM$Sum)){
                TDM[,Sum := Reduce(`+`, .SD),.SDcols = setdiff(names(TDM),possibilities)]
        }
        if(deleteOthers){
                columnstoremove <- setdiff(names(TDM),c("Sum",possibilities))
                TDM[,(columnstoremove):=NULL]
        }
        TDM
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
        thetable#outputs a table with token coverage
}