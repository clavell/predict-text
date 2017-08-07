library(tm)
dataURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(dataURL, destfile = "textdata.zip")
unzip("textdata.zip")
allFiles <- grep("final/*",dir(".",recursive = TRUE, 
                               include.dirs = FALSE),value = TRUE)

filenums <- grep("en",allFiles)
filenums <- 4
doclist <- list()
for(i in seq_along(filenums)){
        doclist[[i]] <- readLines(allFiles[filenums[i]])
}
#Make a small corpus to test out lists as a source.
smalllist <- lapply(doclist,head)
source1 <- VectorSource(smalllist)
smallcorp <- VCorpus(source1)

#make corpus with the directory source instead to automatically get ID metadata
ensource <- DirSource(recursive = TRUE,pattern="en")
DirCorp <- VCorpus(ensource)

#make a term document matrix (hope it doesn't crash)
TDM <- TermDocumentMatrix(DirCorp, control = list(removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE,
                                                  tolower = TRUE)
)
#it crashed, let's try the RWeka pacakage
library(RWeka)

#I don't really understand RWeka. Looking at an example on SO
data <- data.frame(
        text=c("Let the big dogs hunt",
               "No holds barred",
               "My child is an honor student"
        ), stringsAsFactors = F)

## eliminate this step to work as a MWE
data <- data[rep(1:nrow(data), 100000), , drop=FALSE]

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
library(data.table)
lowerfirst <- stri_trans_tolower(doclist[[1]])
words <- stri_extract_all_words(lowerfirst)
words <- unlist(words)
uniquewords <- sort(unique(words))
data.table(doc1 <- sort(words))

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
        require(ngram)
        require(stringi)
        require(data.table)
      
        words <- stri_extract_all_words(concatenate(stri_trans_tolower(doc)))
        data <- data.table(theterms = unlist(words))
        data<-data[,.N,by=theterms]
        names(data) <- c("theterms", paste0("doc",docnum))
        unique(data)
        setkey(data,theterms)
}

data1 <- singledoc(doc1)

#make small documents to test out the function
doc1 <- doclist[[1]][1:2500]
doc2 <- doclist[[1]][2501:5000]
data1 <- singledoc(doc1,1)
data2 <- singledoc(doc2,docnum)
data1

#remake the small documents as lists for indexability
smalldoc <- list()
smalldoc[[1]] <- doclist[[1]][1:2500]
smalldoc[[2]] <- doclist[[1]][2501:5000]

#now make a list of data.tables. Each element is a dtm for a single document
smalldocList <- list()
for(i in seq_along(smalldoc)){
                smalldocList[[i]] <- singledoc(smalldoc[[i]], i)
                }
#here is the function to do it generally
TDMlister <- function(docs){
        TDMList <- list()
        for(i in seq_along(docs)){
                TDMList[[i]] <- singledoc(docs[[i]], i)
        }
        TDMList #list of data.table TermdocumentMatrices (one for each document)
}

TDMList <- TDMlister(smalldoc)
TDM <- merge(TDMList[[1]],TDMList[[2]],all=TRUE)
length

createTDM <- function(TDMs){ #TDMs is a list of TDMs for single docs to be spliced together
        numberOfindices <- 1:(length(TDMs) - 1)
        for(i in seq_along(TDMs)){
                
                if(i==1){
                        
                }else{
                        
                }
                
        }
}
#This seems complicated. Maybe try the Reduce function suggested by SO
mymerge = function(x,y) merge(x,y,all=TRUE)
Reduce(mymerge,TDMList)

#Below we need a way to replace NA with 0
f_dowle2 = function(DT) {
        for (i in names(DT))
                DT[is.na(get(i)), (i):=0]
}


#Aha it works! Now make a function that does it all from start to finish
createTDM <- function(docs){ #takes a list of documents to make into TDM
        Reduce(mymerge,TDMlister(docs))
}
TDM <- createTDM(doc)

#Now that there is a TDM for unigrams it has to be done with bigrams and trigrams
#Maybe the ngram package has something for me here
ngram::

Rprof("Rprof2.out")
dat <- lapply(words[1:2000], function(x, lev) {
        tabulate(factor(x, levels = lev, ordered = TRUE))
}, lev = sort(unique(words[1:2000])))
Rprof(NULL)
dat <- as.data.table(dat) 
dat[,words:=sort(unique(words[1:2000]))]
dat[,c(tweet1:tweet6,words)]












#Messing around
write.csv(data.frame(x = 1:2, y = 1:2), tf1 <- tempfile(fileext = ".csv"))
write.csv(data.frame(x = 2:3, y = 2:3), tf2 <- tempfile(fileext = ".csv"))
write.csv(data.frame(x = 3:4, y = 3:4), tf3 <- tempfile(fileext = ".csv"))
zip(zipfile <- tempfile(fileext = ".zip"), files = c(tf1, tf2))
zip(zipfile <- tempfile(fileext = ".zip"), files = c(tf1, tf3))
zip(zipfile <- tempfile(fileext = ".zip"), files = c(tf2, tf3))

list_of_txts<-unzip(zipfile,list=TRUE)[,1]
list_of_txts<-list_of_txts[str_detect(list_of_txts,".xml")]

ind <- list(c(1, 2, 2), c("A", "A", "B"))
table(ind)
tapply(1:3, ind) #-> the split vector
tapply(1:3, ind, sum)

library(stringr)
library(reshape2)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

require(stats)
groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
tapply(groups, groups, length) #- is almost the same as
table(groups)


fileFun <- function(theDir) {
        ## Look for files (directories included for now)
        allFiles <- list.files(theDir, no.. = TRUE)
        ## Look for directory names
        allDirs <- list.dirs(theDir, full.names = FALSE, recursive = FALSE)
        ## If there are any directories,
        if(length(allDirs)) {
                ## then call this function again
                moreFiles <- lapply(file.path(theDir, allDirs), fileFun)
                ## Set names for the new list
                names(moreFiles) <- allDirs
                ## Determine files found, excluding directory names
                outFiles <- allFiles[!allFiles %in% allDirs]
                ## Combine appropriate results for current list
                if(length(outFiles)) {
                        allFiles <- c(outFiles, moreFiles)
                } else {
                        allFiles <- moreFiles
                }
        }
        return(allFiles)
}




library(tm)
source1 <- DirSource(directory = "final", pattern = "[a-za-z]_[A-ZA-Z]",recursive = TRUE)
corp <- SimpleCorpus(source1)
corptest2 <- PCorpus(DirSource(txt),dbControl = list(dbName = "db"))
predictionCorp <- PCorpus(source1,dbControl = list(dbName = "predictionCorp"))
predictionCorp <- readRDS("predictionCorp.rds")

txt <- system.file("texts", "txt", package = "tm")
ovid <- SimpleCorpus(DirSource(txt))
ovid <- VCorpus(DirSource(txt),
                        readerControl = list(language = "lat"))
ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"),
                        readerControl = list(language = "lat"))
help.search(keyword = "character", package = "base")

getIDs <- function(x){
                y <- character()
                for(i in 1:length(x)) {
                       y[i] <- meta(x[[i]],"id")
                }
                y
}
Ids <-  getIDs(corp)


UStwitter <- readLines(paste("final/en_US/", sep ="", Ids[6]))
USblogs <- readLines(paste("final/en_US/", sep ="", Ids[4]))
USNews <- readLines(paste("final/en_US/", sep ="", Ids[5]))

UScharacter <- lapply(corp[4:6],as.character)

corp <- VCorpus(source1)
class(corp[[9]])

maxind <- which(nchar(fitwtchr) == max(nchar(fitwtchr)))
fitwtchr[maxind]

lapply(UScharacter, function(x)which(nchar(x) == max(nchar(x))))
length(grep("love",UScharacter$en_US.twitter.txt,value =TRUE))/
        length(grep("hate",UScharacter$en_US.twitter.txt,value =TRUE))

grep("biostats",UScharacter$en_US.twitter.txt,value=TRUE)

kickboxstring <- "A computer once beat me at chess, but it was no match for me at kickboxing"
grep(kickboxstring,UScharacter$en_US.twitter.txt,value=TRUE)
str <- concatenate(lapply(ovid, "[", 1))

oviddtm <- DocumentTermMatrix(ovid)
