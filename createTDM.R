#Finding creating a TermDocument Matrix with the tm package quite tedious, i decided to
# create my own function to create the matrix. It only takes about 2 minutes to turn 800 MB
#of documents (3 total) into a TDM matrix

library(tm)
#Function to download relevant files
downloadFiles <- function(){
        if(!dir.exists("final")){
                dataURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                download.file(dataURL, destfile = "textdata.zip")
                unzip("textdata.zip")
        }
}
loadDocs <- function(){ #this function is unfinished! Fix it later
        
        allFiles <- grep("final/*",dir(".",recursive = TRUE, 
                                       include.dirs = FALSE),value = TRUE)
        
        filenums <- grep("en",allFiles)
        doclist <- list()
        for(i in seq_along(filenums)){
                doclist[[i]] <- readLines(allFiles[filenums[i]])
        }
        names(doclist) <- allFiles[filenums]
        doclist
}
#Here is code from SO answer
#https://stackoverflow.com/questions/25330753/more-efficient-means-of-creating-a-corpus-and-dtm-with-4m-rows

        library(stringi)
        library(SnowballC)
        out <- stri_extract_all_words(stri_trans_tolower(SnowballC::wordStem(data[[1]], "english"))) #in old package versions it was named 'stri_extract_words'
        names(out) <- paste0("doc", 1:length(out))
        
        lev <- sort(unique(unlist(out)))
        dat <- do.call(cbind, lapply(out, function(x, lev) {
                tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
        }, lev = lev))
        rownames(dat) <- sort(lev)
        
        library(tm)
        dat <- dat[!rownames(dat) %in% tm::stopwords("english"), ] 
        
        library(slam)
        dat2 <- slam::as.simple_triplet_matrix(dat)
        
        tdm <- tm::as.TermDocumentMatrix(dat2, weighting=weightTf)
        tdm
        
        ## or...
        dtm <- tm::as.DocumentTermMatrix(dat2, weighting=weightTf)
        dtm

#from this i see that maybe the stringi package might do this faster
#try with my own things

#playing with small document
        doc <- doclist[[1]]
        library(ngram)
        words1 <- stri_extract_all_words(concatenate(stri_trans_tolower(doc1)))
        data <- data.table(terms1 = unlist(words1))
        setkey(data,terms1)
        data[1:100]
        data[,doc1 := .N,by=terms1]
        uniquewords <- data[,unique(terms1)]
        data <- unique(data)
        
        words1
#function to make a TermDocumentMatrix in data.table form from 1 document
singledoc <- function(doc,docnum){
        if(is.null(docnum))stop()
        require(ngram)
        require(stringi)
        require(data.table)
      
        words <- stri_extract_all_words(concatenate(stri_trans_tolower(doc)))
        data <- data.table(theterms = unlist(words))
        data[,new :=.N,by=theterms]
        names(data) <- c("theterms", paste0("doc",docnum))
        data <- data[,unique(data)]
        setkey(data,theterms)
        data
}

        data <- singledoc(doc,1)
        
        smalldoc <- list()
        smalldoc[[1]] <- doclist[[1]][1:2500]
        smalldoc[[2]] <- doclist[[1]][2501:5000]

#now make a list of data.tables. Each element is a dtm for a single document
        smalldocList <- list()
        for(i in seq_along(smalldoc)){
                        smalldocList[[i]] <- singledoc(smalldoc[[i]], i)
                        }
#here is the function to do it generally
TDMlister <- function(docs){#argument docs is a list of documents
        TDMList <- list()
        for(i in seq_along(docs)){
                TDMList[[i]] <- singledoc(docs[[i]], i)
        }
        TDMList #list of data.table TermdocumentMatrices (one for each document)
}

        TDMList <- TDMlister(doclist)
        TDM <- merge(TDMList[[1]],TDMList[[2]],all=TRUE)
        length


#Below we need a way to replace NA with 0. This function was borrowed from a stack overflow
# answer. https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
f_dowle2 = function(DT) {
        for (i in names(DT))
                DT[is.na(get(i)), (i):=0]
}
#this function

#Now make a function that does it all from start to finish
mymerge = function(x,y) merge(x,y,all=TRUE)#function suggested by SO for DT merges
createTDM <- function(docs){ #takes a list of documents to make into TDM
        Reduce(mymerge,TDMlister(docs))
}
        TDM <- createTDM(doclist)


#Now that there is a TDM for unigrams it has to be done with bigrams and trigrams
#Maybe the ngram package has something for me here

key(TDM)

#The URL below is for a github repo that contains a list of english 
#words in a newline delimited text file. (for sourcing)
        "https://github.com/dwyl/english-words"
        #the direct link is below
        englishWordsURL <- "https://github.com/dwyl/english-words/blob/master/words_alpha.txt"