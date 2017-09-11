#now that we have the tdms let's remove words that might not be so useful: Start with profanity
library(data.table)
library(stringi)
library(magrittr)

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
#could also consolidate synonyms

uncombine <- function(TDM){#make sure to set this to an object
        require(quanteda)
        columnNums <- 1:length(unlist(tokenize(TDM$FirstTerms[1])))
        possibilities <- c("uni", "bi", "tri", "four", "five")
        columns <- tstrsplit(TDM$FirstTerms,split = " ") %>% 
                as.data.table() %>% setnames(possibilities[columnNums])
        TDM[,FirstTerms:=NULL]
        cbind(columns,TDM)
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

fwrite([bestTop100[Sum>3][order(-Sum)],"trigrampredictiontable.csv")


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
        setkey(TDM,FirstTerms,Sum)
        temp <- TDM[unique(uni),mult="last"]
        uncombine(temp)
}

test <- bestMaker(shortdocs2TDMs[[3]])
shortdocs2TDMs[[3]] <- getTotals(shortdocs2TDMs[[3]],TRUE)
shortdocs2TDMs[[3]]
