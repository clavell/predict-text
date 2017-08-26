#Finding creating a TermDocument Matrix with the tm package quite tedious, i decided to
# create my own function to create the matrix. It only takes about 2 minutes to turn 800 MB
#of documents (3 total) into a TDM matrix

#Function to download relevant files
downloadFiles <- function(){
        if(!dir.exists("final")){
                dataURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                download.file(dataURL, destfile = "textdata.zip")
                unzip("textdata.zip")
        }
}
loadDocs <- function(){
        downloadFiles()
        if(!any(grepl("doclist",ls(.GlobalEnv)))){
                allFiles <- grep("en",dir("./final",recursive = TRUE, 
                                               include.dirs = FALSE),value = TRUE)
                
                filenums <- grep("en",allFiles)
                doclist <- list()
                for(i in seq_along(filenums)){
                        doclist[[i]] <- readLines(paste("final/",allFiles[filenums[i]],sep=""))
                }
                names(doclist) <- allFiles[filenums]
                #to avoid creating new directories replace the slashes with dashes
                names(doclist) <- gsub(x=names(doclist),"/","-")
                assign("doclist",doclist,envir = .GlobalEnv)
        }   
}
loadDocs()
#code from SO answer provided a good insight into how to make a TDM more efficiently
#https://stackoverflow.com/questions/25330753/more-efficient-means-of-creating-a-corpus-and-dtm-with-4m-rows

#function to make a TermDocumentMatrix in data.table form from 1 document
singledoc <- function(doc,docnum){#doc is a character vector
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

#now make a list of data.tables. Each element is a dtm for a single document
#here is the function to do it generally
TDMlister <- function(docs){#argument docs is a list of documents
        TDMList <- list()
        for(i in seq_along(docs)){
                TDMList[[i]] <- singledoc(doc = docs[[i]], docnum = i)
        }
        TDMList #list of data.table TermdocumentMatrices (one for each document)
}

#Now make a function that does it all from start to finish
mymerge = function(x,y) merge(x,y,all=TRUE)#function suggested by SO for DT merges
createTDM <- function(docs){ #takes a list of documents to make into TDM
        Reduce(mymerge,TDMlister(docs))
}

#Create the Term document matrix and remove all of the extraneous things
        TDM <- createTDM(doclist)
        
#This creates a data.table full of NAs where there are no entries for a particular 
#term and doc. The function bellow was borrowed from a stack overflow answer: 
#https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
#It replaces all NA values in the table with 0.
        f_dowle2 = function(DT) {
                for (i in names(DT))
                        DT[is.na(get(i)), (i):=0]
        }
        
        f_dowle2(TDM)
#Now let's clean up the environment.
        rm(list = grep("[^(doclist|TDM)]",ls(),value = TRUE))
        
#Now that there is a TDM for unigrams it has to be done with bigrams and trigrams
#Maybe the ngram package has something for me here. To be continued


#The URL below is for a github repo that contains a list of english 
#words in a newline delimited text file. (for sourcing)
        #"https://github.com/dwyl/english-words"
        #the direct link is below
        #englishWordsURL <- "https://github.com/dwyl/english-words/blob/master/words_alpha.txt"