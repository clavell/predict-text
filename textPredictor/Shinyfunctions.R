#functions useful in shiny
makePrediction <- function(TDM,wordIndices,conversionTable){
        # require(data.table)
        # require(magrittr)
        last <- length(wordIndices)
        TDM[(wordIndices[last:1])][[length(TDM)]] %>% 
                IndextoWord(conversionTable)
}

multiPred <- function(TDMs,wordIndices,conversionTable){
        lapply(TDMs,makePrediction,wordIndices,conversionTable)
        }

predictionchooser <- function(Predictions,wordlist){#Predictions is a list of predictions,
                                                        #wordlist is a data.table full of words
        n = length(Predictions)
        pred <- Predictions[[n]]
        
        while(is.na(pred)|length(pred)>1|is.null(pred)){
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