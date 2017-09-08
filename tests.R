#now that we have the tdms let's remove words that might not be so useful: Start with profanity
library(data.table)
download.file("http://www.bannedwordlist.com/lists/swearWords.csv",
              destfile = "swearWords.csv")
swears <- fread("swearWords.csv")
swears <- names(swears)
swears
doclistTDMs[[1]]
doclistTDMs[[1]][!doclistTDMs[[1]]$uni %in% swears]
shortdocs2TDMs[[1]]
setnames(d)
#to greatly reduce the size of the tdm's could remove the ngrams that don't feature prominently
#do that in a bit

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


#example from faq vignette in data.table
X = data.table(grp = c("a", "a", "b",
                       "b", "b", "c", "c"), foo = 1:7)
setkey(X, grp)
Y = data.table(c("b", "c"), bar = c(4, 2))
X
#    grp foo
# 1:   a   1
# 2:   a   2
# 3:   b   3
# 4:   b   4
# 5:   b   5
# 6:   c   6
# 7:   c   7
Y
#    V1 bar
# 1:  b   4
# 2:  c   2
X[Y]
# [1] 74
X[Y, sum(foo*bar), by = .EACHI, drop=TRUE]
key(Y)

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
#we also want to remove profanity

#there is a problem with the function for 