#

library(ggplot2)
#here is the order of the 
renameDocs <- function(){
        allFiles <- grep("final/*",dir(".",recursive = TRUE, 
                                       include.dirs = FALSE),value = TRUE)
        
        filenums <- grep("en",allFiles)
        
        x <- c("terms",allFiles[filenums])
        x <- gsub("final/en_US/en_US.","",x)
        x <- gsub(".txt", "",x)
        x
}
        names(TDM) <- renameDocs()
        #clean up the environment
        rm(list = grep("[^(doclist|TDM|quantCorpus)]",ls(),value = TRUE))
        
        
#word count function
        library(stringi)
        library(ngram)
        doc<-c("a hairy brwon motorcycle fell out of a purple monster","sinep sinep")
sepdoc <- function(doc){
        require(ngram)
        require(stringi)
        require(data.table)
                
        words <- stri_extract_all_words(concatenate(stri_trans_tolower(doc)))
        data <- data.table(theterms = unlist(words))
        data
}

docs <- lapply(doclist,sepdoc)
lapply(docs,nrow)
#let's look at the intersection of words in the documents
        #removing the entries with punctuation (except apostrophe) we reduce the number of entries by 11%
        TDMnopunct <- TDM[!grep("[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]",terms)]
        #nrow(TDMnopunct)/nrow(TDM)
        #removing entries with numbers as well 
        TDMnonum <- TDMnopunct[!grep("\\d",terms)]
        #nrow(TDMnonum)/nrow(TDM)
        #now let's look at 
        intersect <- TDM[,.N,.(blogs >0,news>0,twitter>0)][,type := "unaltered"]
        intersectnopunct <- TDMnopunct[,.N,.(blogs >0,news>0,twitter>0)][,type := "nopunct"]
        intersectnonum <-TDMnonum[,.N,.(blogs >0,news>0,twitter>0)][,type:="nonum"]
        #make a table showing intersection of words found in the documents in an unaltered
        #state, with punctuation removed and as with numbers removed.
        intersections <- Reduce(mymerge,list(intersect,intersectnopunct,intersectnonum))[order(-type)]
        dcast(intersections,blogs+news+twitter~type,value.var = "N")

lapply(as.data.table(TDM[,.SD>0][,2:4]),sum)
TDM[,sum(blogs>0)]
concatente(doclist[[1]])
object.size(TDMnopunct)
colnames(TDM)
super <- TDM[,.SD>0]

TDMlister(doclist)

TDM[,totals := rowSums(TDM[,2:4])]
TDM[,percentage := totals/sum(totals)*100]
TDM[order(-totals)][1:100]
TDM[sum(percentage)]