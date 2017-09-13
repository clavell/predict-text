#now that we have the tdms let's remove words that might not be so useful: Start with profanity
library(data.table)
library(stringi)
library(magrittr)
library(googlesheets)

#pull apart a string of words and put those words in the thing

shortdocs2TDMs[[4]][uni == "!" & bi=="!"]
grub <- c("the","time")
grub <- as.list(grub)
shortdocs2TDMs[[3]][grub]
lapply(shortdocs2TDMs,getTotals,TRUE)

getTotals(shortdocs2TDMs[[3]])
shortdocs2TDMs[[3]][,Sum := Reduce(`+`, .SD),.SDcols = setdiff(names(shortdocs2TDMs[[3]]),
                                                               possibilities)]
shortdocs2TDMs[[3]]

TermDocumentMatrices(shortdocs2)


#seeing why my function won't work after project closed
library(data.table)
shortdocs2TDMs <- TermDocumentMatrices(shortdocs2,1:4)
lapply(shortdocs2TDMs,getTotals,TRUE)
shortdocs2TDMs[[3]]

#get the totals and leave behind the rest
lapply(doclistTDMs,getTotals,TRUE)
doclistTDMs

setkey(shortdocs2TDMs[[3]],uni,bi)
shortdocs2TDMs[[3]][.(uni,bi),allow.cartesian=TRUE]
key(shortdocs2TDMs[[3]])
setkey(shortdocs2TDMs[[3]],uni,bi,Sum)
key(doclistTDMs[[3]])
shortdocs2TDMs[[3]][.(unique(uni)),mult="first"]

doclistTDMs[[2]][]
key(doclistTDMs[[2]])
setkey(doclistTDMs[[2]],uni,Sum)
thefirst <- doclistTDMs[[2]][unique(uni),mult="first"]
thelast <- doclistTDMs[[2]][unique(uni),mult="last"]
thelast

#ok so there's the way to get just the most frequently used terms
#it would be fun to make a sentence creator
#choose a random word, then use the bigram to choose the next, the trigram after that, etc.

wordchooser <- function(){
        
        sentence <- character()
        i <- 1
        sentence[1] <- thelast[i = sample(1:nrow(thelast),1),uni]
        while(sentence[i] != "."){
        sentence[i+1] <- thelast[sentence[i],bi]
        i <- i+1
        if(i==20)return(sentence)
        }
        sentence
}
wordchooser()
#from this function there is a lot of repetition. Let's find words that occur a lot as the
#second word in the bigrams
thelast[,.N,by=bi][order(-N)][1:100]

#Make a similar function but also using the trigrams data
thelast <- doclistTDMs[[3]][unique(uni,bi),mult="last"]
setkey(shortdocs2TDMs[[3]],uni,bi,Sum)
key(shortdocs2TDMs[[3]])
shortdocs2TDMs[,uni]
unique(shortdocs2TDMs[[2]])
shortdocs2TDMs[[2]][unique(uni,bi)]
doclistTDMs[[3]]
shortdocs2TDMs[[3]][,unique(paste(uni,bi))]
doclistTDMs[[3]][,unique(paste0(uni,bi))]
trisentence <- function(){
        
        sentence <- character()
        i <- 1
        sentence[1] <- thelast[i = sample(1:nrow(thelast),1),uni]
        while(sentence[i] != "."){
                sentence[i+1] <- thelast[sentence[i],bi]
                i <- i+1
                if(i==20)return(sentence)
        }
        sentence
}

#I'm thinking maybe I shouldn't have done it this way..
library(quanteda)
library(magrittr)
library(stringi)
newt <- doclist[[1]]$train %>% stri_trans_tolower() %>%
        tokenize() %>% unlist()

system.time(newtDFM <- dfm(newt))

#making the dfm seemed to take quite a while (>30 mins),(just for one of the three docs) 
#so back to the old way
#it seems difficult to make the prediction using each word in a column, so let's try 
#combining the first two words and making the prediction from that

doclistTDMs[[3]]
shortdocs2TDMs[[2]][, uni := paste0(uni,bi)]
shortdocs2TDMs[[3]][,uni:=paste0(uni,bi)]
shortdocs2TDMs[[3]][,bi := NULL]
doclistTDMs

#let's make something that works with words with types that have at least one letter in them
doclistTDMs[[1]] <- doclistTDMs[[1]][grep("[a-zA-Z]",uni)]
#we also want to remove profanity. Here is a large list of bad words.
download.file("http://www.bannedwordlist.com/lists/swearWords.csv",
              destfile = "swearWords.csv")
swears <- fread("swearWords.csv")
swears <- names(swears)
swears
doclistTDMs[[1]] <- doclistTDMs[[1]][!swears]

#remove words that start with # as they are mostly just twitter hashtags and appear at the end
#of entries
doclistTDMs[[1]] <- doclistTDMs[[1]][!grep("^#",uni)]

#we'll probably need a way to deal with numbers as they come up, but think about that later
#let's find the most common endings to our trigrams and see how they compare to the whole
#thing.

#Let's see if modifying the TDM after it was created lets function work on it now:
getTotals(doclistTDMs[[1]])
doclistTDMs[[1]]
#it worked!
#try to get it to work just by setting a DT to itself..
doclistTDMs[[2]] <- doclistTDMs[[2]]
#no luck

#remove the offending words from the columns of the bigram TDM
doclistTDMs[[2]] <- doclistTDMs[[2]][doclistTDMs[[1]]$uni]
getTotals(doclistTDMs[[2]])
#now that works! Excellent!
#see if removing the words from the TDM is faster with grep or by using previously made 
#list of possible words.

system.time(test <- doclistTDMs[[2]][(doclistTDMs[[1]]$uni)])
system.time(doclistTDMs[[2]][unique(uni), doclistTDMs[[1]]$uni])

#hmmm not working as i hoped..
#maybe try to set key just to the bi column to get what i want
setkey(doclistTDMs[[2]],bi)
doclistTDMs[[2]]
system.time(test <- doclistTDMs[[2]][unique(uni)][!is.na(uni)])
doclistTDMs[[1]]
#i get it now. If there was a token removed that removed two words then the result using
#the adjusted uni list would produce NAs naturally! I'm not going to understand this
#sentence later...
#if we just apply all the same transformations to each column, that shuld remove all of the
#tokens that we don't want.
#ready to make a function, maybe?
#by doing previous steps, I inadvertently removed all punctuation, so now there are no
#tokens with punctuation.. I'd like punctuation to have an effect. periods and other sentence
#finishing punctuation can give an idea of what tokens to throw away and other punctuation
#like commas can affect the words that are coming next.
#in the future, could ad a period to the end of each entry before tokenizing to make sure
#we're drawing lines between entries and not affecting the prediction.
#look at the ngram function..

library(ngram)
?ngram
gram <- ngram("the little boy, though stupid was a boy none the less.")

#First remove the rest of the trigrams that contain any of the banned words from above
#here is a function to remove tokens that we don't want as above.
removeTokes <- function(DT){
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


shortdocs2TDMs[[1]]

 
shortdocs2TDMs[[1]] <- removeTokes(shortdocs2TDMs[[1]])       

#return the first TDM to what it was in the beginnning
doclistTDMs[[1]] <- TermDocumentMatrices(doclist,1)
doclistTDMs[[1]] <- doclistTDMs[[1]][[1]]

#remove the tokens from the different TDMs
removeTokes(doclistTDMs[[3]])

#can use lapply to remove the tokens from all of the DTMs
doclistTDMs <- lapply(doclistTDMs, removeTokes)

#now can find the most common last word in the trigram
lapply(doclistTDMs,getTotals,TRUE)


setkey(doclistTDMs[[3]],Sum)
hi <- doclistTDMs[[3]][,.N,by=tri]

top <- hi[order(-N)]$tri[1:100]
#The top 5000 third terms cover 78% of the trigram tokens
#The top 100 third terms cover 33% of the trigram tokens

#if we only use the top 100 third words, how much coverage do we have of the first terms?
#we can remove the tokens that don't contain the most common words, but we only want the most
#common for each set of first terms.
#first we get rid of all of the extraneous tokens
setkey(doclistTDMs[[3]],tri)
top100 <- doclistTDMs[[3]][top]
combineInitial(top100)
setkey(top100,FirstTerms)
bestTop100 <- top100[unique(FirstTerms),mult="last"]
top100
bestTop100[order(-Sum)]
bestTop100[,.N]/doclistTDMs[[2]][,.N]
object.size(bestTop100$FirstTerms)/10^6

#There is a large number of numbers with units. Those could be replaced with a single marker
#i.e. length could represent all numbers ending in mm, cm, in, etc.

        times <- c("^[0-9]+[aApP][mM]$")
        lengths <- c("^[0-9].*m$",)
        position#(first, second, etc.)
        date #1920s
#could also consolidate synonyms

uncombine <- function(TDM){#make sure to set this to an object
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


        bestTop100 <- uncombine(bestTop100)
        
        object.size(bestTop100[!grep("^[0-9]",FirstTerms)])
        key(bestTop100)
        
        bestTop100[,unique(bi)]
        setkey(doclistTDMs[[2]],uni)
        bipred <- doclistTDMs[[2]][!bestTop100[Sum>2,unique(bi)]]
        bestTop100[Sum>2][order(-Sum)] %>% object.size()/10^6
        bestTop100[Sum>2,unique(bi)]
        
        bipred <- bipred[unique(uni),mult="last"]
        fwrite(bipred,"bigrampredictiontable.csv")
        fread("bigrampredictiontable.csv")



#to get the app to put out a value, we need to take an input, take the last two values, then
#make a prediction with the trigram prediction model. If there's nothing there then, fall 
#back to the bigram model with the just the last term.
#
#When trying the above got nothing for the word fell... should be pretty common. I think I
# chipped too much away.
        key(doclistTDMs[[2]])
        bipred <- doclistTDMs[[2]][unique(uni),mult="last"]
        object.size(bipred)/10^6
        fwrite(bipred,"bigrampredictiontable.csv")
#now try to get the prediction
        key(bipred)
        bipred["fell"]
#got something but it's weird
        doclistTDMs[[2]]["fell"]
#didn't set a very good order
        setkey(doclistTDMs[[2]],uni,Sum)
        bipred <- doclistTDMs[[2]][unique(uni),mult="last"]
        fwrite(bipred,"bigrampredictiontable.csv")
#looks like there are a lot of strange words at the end
        bipred[,which(grepl("z",uni))]
        bipred
        setkey()
        besttri <- doclistTDMs[[3]]
#need a function that will find the best option for a predictive token
bestMaker <- function(TDM){
        combineInitial(TDM)
        x <- if(!is.null(TDM$FirstTerms)){"FirstTerms"}else{"uni"}
        setkeyv(TDM,c(x,"Sum"))
        
        TDM <- TDM[TDM[,unique(.SD),.SDcols=x],mult="last"]
        uncombine(TDM)
}


        shortdocs2TDMs <- shortdocs2 %>% TermDocumentMatrices(1:5)
        lapply(shortdocs2TDMs,getTotals,deleteOthers = TRUE)
        shortdocs2TDMs[[4]] <- bestMaker(shortdocs2TDMs[[4]])
#trying with copy so not to mess up the original
        testdoclistTDM3 <- copy(doclistTDMs[[3]])
        testdoclistTDM3 <- bestMaker(testdoclistTDM3)
        setkey(testdoclistTDM3,uni,bi)
        fwrite(testdoclistTDM3,"tripredict.csv")
        
        
        
        drive_upload("tripredict.csv")
        (chicken <- drive_upload(
                "./tripredict.csv.zip",
                "tripredict.csv.zip"
        ))
        
        drive_download("tripredict.csv.zip")
        drive_download(chicken)#pretty slow

        boring_ss <- gs_new("boring", ws_title = "iris-gs_new", input = head(iris),
                            trim = TRUE, verbose = FALSE)

#giving up on google sheets we want to store the tdms on mysql server
        library(RMySQL)
        library(data.table)
        capstoneDb <- dbConnect(MySQL(),user="capstone", dbname = "capstone",
                            host="localhost",password = "posTrooF!2")
        
        allTables <- dbListTables(capstoneDb)
        length(allTables)
        dbWriteTable(capstoneDb,name="triTDM",value = testdoclistTDMs[[3]],overwrite=TRUE)
        
        dbGetQuery(capstoneDb, "select tri from testshortbi where uni = '!' and .short-en_US-en_US.news.txt > 1")
        dbListFields(capstoneDb,"testshortbi")
        
        dbGetQuery(capstoneDb,"select tri from triTDM where uni = 'the' and bi = 'one'")
        dbDisconnect(capstoneDb)
#so the mysql db approach is real slow
#see if the zip file can be downloaded quickly from google drive

#the file takes about 90 seconds to download
#tried to do binary indexing on the mysql db but no luck because of the limit on
#character vectors.
#try coding each word as an integer in the unigram list.
        object.size(unigrams)/10^6
        unigrams <- copy(doclistTDMs[[1]])
        unigrams[,Sum := NULL]
        fwrite(doclistTDMs[[1]])
        key(unigrams)
# replace all of the words in the uni and bi columns of the trigram tdm with numbers as they
# relate to the unigram tdm.. 
        length(unique(doclistTDMs[[3]]$uni))
        unigrams[,length(unique(uni))]
#these aren't the same length which is weird.. I'll figure that out later. Right now
#just need to do this, so use the unigrams i have

        all(testdoclistTDM3$uni %in% unigrams$uni )
        lapply(testdoclistTDM3, function(x) all(x %in% unigrams$uni))

#try this out with a small dataset
        shortunigrams <- shortdocs2TDMs[[1]][,.(uni)]
        setkey(shortunigrams,uni)
        shorttrigrams <- shortdocs2TDMs[[3]]
        lapply(shortdocs2TDMs[[3]], function(x) all(x %in% shortunigrams$uni))
        firstShortTri <- shortdocs2TDMs[[3]]$uni
        firstShortTri <- shortunigrams[firstShortTri,which=TRUE]
        identical(shortunigrams$uni[firstShortTri],shortdocs2TDMs[[3]]$uni)
        shortdocs2TDMs[[3]]
#ok so now replace the words in the tri TDM with numbers
        shortdocs2triTDM <-shortdocs2TDMs[[3]]#checking to see if changing this, changes the initial
        firstShortTri <- shortdocs2TDMs[[3]]$uni
        shortdocs2triTDM[,uni := shortunigrams[uni,which=TRUE]]
        shortdocs2triTDM[[3]] #it is changed!
        
#now need a function to do it for the whole table
indexifyTable = function(TDM,unigrams) {
        possibilities <- c("uni","bi","tri","four","five")
        type <- sum(names(TDM) %in% possibilities)
        
        for (i in possibilities[1:type])
                TDM[,(i) := unigrams[TDM[[i]],which=TRUE]]
}

#try things out with short doclist
        loadDocs()
        set.seed(39384)
        doclist <- multisplit(doclist)
        shortdocs2 <- lapply(doclist,lapply,head,100)
        names(shortdocs2) <- paste("short-",names(shortdocs2),sep="")
        shortdocs2TDMs <- TermDocumentMatrices(shortdocs2,1:5)
        lapply(shortdocs2TDMs,getTotals, deleteOthers = TRUE)
        shortdocs2TDMs <- lapply(shortdocs2TDMs,removeTokes)
        shortdocs2TDMs <- lapply(shortdocs2TDMs, bestMaker)
        shortunigrams <- shortdocs2TDMs[[1]][,.(uni)]
        #get a testable sample to check if it goes back to its original self
        firstShortTri <- shortdocs2TDMs[[3]]$uni
        object.size(shortdocs2TDMs[[3]])/10^6 #check size
        indexifyTable(shortdocs2TDMs[[3]],shortunigrams)
        object.size(shortdocs2TDMs[[3]])/10^6 #and again after transformation: about 20% of orginal size!
        identical(shortunigrams[shortdocs2TDMs[[3]]$uni]$uni,firstShortTri)#it works!
        shortdocs2TDMs
        
#now need to see if the TDMs can be indexed as mysql DBs
        testdoclistTDMs <- lapply(doclistTDMs,copy)
        testdoclistTDMs <- lapply(testdoclistTDMs,removeTokes)
#need to make sure that the keys are correct
TDMsetkey <- function(TDM){   
        possibilities <- c("uni","bi","tri","four","five")
        
        type <- sum(names(TDM) %in% possibilities)
        setkeyv(TDM,possibilities[1:(type-1)])
}
        lapply(doclistTDMs,TDMsetkey)
        lapply(doclistTDMs,key)
        object.size(doclistTDMs[[3]])/10^6
        indexifyTable(doclistTDMs[[2]],testdoclistTDMs[[1]])       
        indexifyTable(doclistTDMs[[3]],testdoclistTDMs[[1]])

# I messed up and didn't do everything I should have to the matrix before conversion
wordifyTable <- function(TDM,unigrams){
        possibilities <- c("uni","bi","tri","four","five")
        type <- sum(names(TDM) %in% possibilities)
        
        for (i in possibilities[1:type]){
                TDM[,(i) := unigrams[TDM[[i]]]$uni]

                }        
}

        wordifyTable(shortdocs2TDMs[[3]],shortdocs2TDMs[[1]]) #it workds
        #prepare TDMs for shiny
        testdoclistTDMs[[3]] <- bestMaker(testdoclistTDMs[[3]])
        lapply(doclistTDMs,key)
        lapply(doclistTDMs,TDMsetkey)
        doclistTDMs <- lapply(doclistTDMs,bestMaker)
        lapply(doclistTDMs[2:3],indexifyTable,doclistTDMs[[1]])
        lapply(doclistTDMs,function(x)object.size(x)/10^6)
        lapply(doclistTDMs,function(x) x[,Sum:=NULL])
        
WordtoIndex <- function(predictor,conversionTable){
                conversionTable[predictor,which=TRUE]
}
        testdoclistTDMs[[3]]
        convertWord(testdoclistTDMs[[1]],"0.03mm")

#going to try with the mysql version first so just nee conversion table
        zip("conversionTable.zip",filenames[1])
        (convTable <- drive_upload("conversionTable.zip"))

        drive_get(as_id(convTable$id))
        (convtable <- convTable %>% 
                        drive_share(role = "reader", type = "anyone"))
        library(dplyr)
        write.csv(select(convtable,name,id,shared),"conversiontableDribble.csv")
        convtable$id
IndextoWord <- function(index, conversionTable){
        conversionTable[index]$uni
}


#want to get quadragram prediction into the mix
        doclistTDMs[[4]] <- TermDocumentMatrices(doclist,4)
        gc()
        doclistTDMs[[4]] <- doclistTDMs[[4]][[4]] #remove sublist
        lapply(doclistTDMs,getTotals,deleteOthers = TRUE)#add sum column
        doclistTDMs <- lapply(doclistTDMs,removeTokes)#remove unwanted tokens
        lapply(doclistTDMs,key)
        lapply(doclistTDMs,TDMsetkey)#set key appropriateley
        doclistTDMs <- lapply(doclistTDMs,bestMaker)#take only the best prediction for each set of predictors
        preindexSize <- lapply(doclistTDMs,function(x)object.size(x)/10^6)#check size
        lapply(doclistTDMs,function(x) x[,Sum:=NULL])#remove the Sum column It's done its duty
        #in following do iteration manually. Write a function for it someday maybe :)
        lapply(doclistTDMs[[4]], function(x) all(x %in% doclistTDMs[[1]]$uni))#check if all columns are covered by unigrams
        #everything looks good. Now time to index the table
        lapply(doclistTDMs[2:4],indexifyTable,doclistTDMs[[1]])#reduce size by creating indexed version
        postindexSize <- lapply(doclistTDMs,function(x)object.size(x)/10^6)#check size
        lapply(doclistTDMs,function(x) x[,Sum:=NULL])#remove sum column as it is no longer needed
        postSumremovedSize <- lapply(doclistTDMs,function(x)object.size(x)/10^6)#check size
        #trying to make a word prediction
        
        lapply(doclistTDMs,key)
        lapply(doclistTDMs,TDMsetkey)
        words <- list("my","leg","fell")
        indices <- lapply(words,WordtoIndex,doclistTDMs[[1]])
        
        makePrediction(doclistTDMs[[4]],indices,doclistTDMs[[1]])#function from Shinyfunctions.R
        preds <- multiPred(doclistTDMs[2:4],indices,doclistTDMs[[1]])
        (pred<-predictionchooser(preds,shortdocs2TDMs[[1]]))
        #this can be used to write files
        possibilities <- c("uni","bi","tri","four","five")
        filenames <- unlist(lapply(doclistTDMs,function(x) {
                paste0("./textPredictor/",names(x)[sum(names(x) %in% possibilities)],".TDM.csv")
        }))
        fwrite(data.table(filenames = filenames),"./textPredictor/filenames.csv")#so shiny can access them
        lapply(doclistTDMs,function(x) {
                fwrite(x,paste0("textPredictor/", 
                                names(x)[sum(names(x) %in% possibilities)],".TDM.csv"))
        })
        fread("./textPredictor/textPredictor/bi.TDM.csv")
        zip("textPredictor/TDMsforShinynoSum.zip",paste0("textPredictor/",filenames))
        lapply(doclistTDMs,str)
        
        #in case there are too many mySQL connections open
        lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

        #try things out with short doclist
        #if doclist doesn't exist
        loadDocs()
        set.seed(39384)
        doclist <- multisplit(doclist)
        #if doclist exists
        shortdocs2 <- lapply(doclist,lapply,head,100)
        names(shortdocs2) <- paste("short-",names(shortdocs2),sep="")
        shortdocs2TDMs <- TermDocumentMatrices(shortdocs2,1:5)
        lapply(shortdocs2TDMs,getTotals, deleteOthers = TRUE)
        shortdocs2TDMs <- lapply(shortdocs2TDMs,removeTokes)
        shortdocs2TDMs <- lapply(shortdocs2TDMs, bestMaker)
        lapply(shortdocs2TDMs[2:5],indexifyTable,shortdocs2TDMs[[1]])
        lapply(shortdocs2TDMs,key)
        lapply(shortdocs2TDMs,TDMsetkey)
        
        swords <- list("go")
        sindices <- lapply(words,WordtoIndex,shortdocs2TDMs[[1]])
        shortdocs2TDMs[[3]][(indices[2:3])]$tri %>% IndextoWord(shortdocs2TDMs[[1]])
        
        makePrediction(shortdocs2TDMs[[4]],indices,shortdocs2TDMs[[1]])#function from Shinyfunctions.R
        indices[3:1]
        
        