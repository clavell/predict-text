#functions useful in shiny development
makePrediction <- function(TDM,wordIndices,conversionTable){
        # require(data.table)
        # require(magrittr)
        possibilities <- c("uni","bi","tri","four","five")
        last <- length(wordIndices)
        TDM[(wordIndices[last:1])][[sum(possibilities %in% names(TDM))]] %>% #the prediction has to be the last column?
                IndextoWord(conversionTable)
}

multiPred <- function(TDMs,wordIndices,conversionTable){
        lapply(TDMs,makePrediction,wordIndices,conversionTable)
        }

predictionchooser <- function(Predictions,wordlist){#Predictions is a list of predictions,
                                                        #wordlist is a data.table full of words
        n = length(Predictions)
        pred <- Predictions[[n]]
        
        while(any(is.na(pred))|length(pred)>1|any(is.null(pred))){
        n = n - 1
        if(n > 1){
                pred <- Predictions[[n]]
                }else{
                pred <- wordlist[round(runif(1,min = 1, max = wordlist[,.N]))]$uni
                }
                
        
        }
        pred
}

TDMsetkey <- function(TDM){   #sets a useful key for prediction for given TDM
        possibilities <- c("uni","bi","tri","four","five")
        
        type <- sum(names(TDM) %in% possibilities)
        setkeyv(TDM,possibilities[(type-1):1])#set key backwards for ease of use in prediction
}

WordtoIndex <- function(predictor,conversionTable){#gets the index of a word from the key
        conversionTable[predictor,which=TRUE]
}

IndextoWord <- function(index, conversionTable){#opposite of WordtoIndex()
        conversionTable[index]$uni
}



createModel <- function(Docs,...){#takes a list of character vectors and makes a prediction
        arguments <- list(...)
        TDMs <- TermDocumentMatrices(Docs,arguments$n)
        lapply(TDMs,getTotals,deleteOthers=TRUE)
        TDMs <- lapply(TDMs,removeTokes)
        TDMs <- lapply(TDMs, bestMaker)
        #lapply(TDMs,function(x) x[,Sum:=NULL])#remove sum column as the function relies on the
        #last column being the prediction (makePrediction())
        lapply(TDMs,TDMsetkey)
        lapply(TDMs[2:max(arguments$n)],indexifyTable,TDMs[[1]])
        lapply(TDMs,TDMsetkey)
        TDMs
}
