##figured out a decent way to do this maybe
library(tm)
#Read in the twitter data
con <- file("final/en_US/en_US.twitter.txt")
UStwitter <- readLines(con)
rm(con)
#Create small sample
set.seed(39384)
USTwitSamp <- sample(UStwitter, size=floor(length(UStwitter)*.1))
rm(UStwitter)
#Create tokenizers
BigramTokenizer <-
        function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
                           use.names = FALSE)
UnigramTokenizer <-
        function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), 
                           use.names = FALSE)
TrigramTokenizer <-
        function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), 
                           use.names = FALSE)
#Make sample into corpus with each tweet being a document
USTwitSampCorp <- VCorpus(VectorSource(USTwitSamp))


#create a character vector where every tweet from the sample is stored in one entry
library(ngram)
allinone <- concatenate(USTwitSamp)
#create a corpus from the vector
corpallinone <- VCorpus(VectorSource(allinone))
#make a function to create Document term matrices that use only part of the corpus
createTDM <- function(corp,end=2,start=1,tokenizer=UnigramTokenizer){
                TermDocumentMatrix(corp[start:end], control = list(tokenize = tokenizer,
                                                                   removeNumbers = TRUE,
                                                                   removePunctuation = TRUE,
                                                                   stripWhitespace = TRUE,
                                                                   wordLengths = c(1,10),
                                                                   bounds = list(local = c(2,Inf)))
                )
}
#Create document term matrices (DTM) from all in one corpus
UniTDMallinone <- createTDM(corpallinone, end = 1)
BiTDMallinone <- createTDM(corpallinone, end = 1, tokenizer = BigramTokenizer)
TriTDMallinone <- createTDM(corpallinone, end = 1, tokenizer = TrigramTokenizer)

#remove unwanted terms from DTMs
testTDM <- BiTDMallinone[sample()]
#check out the allinone dtm's
freqTerms <- findFreqTerms(UniDTMallinone,10000)

#create function to convert document termmatrices to data frames for analysis
library(data.table);library(dplyr)
dtmToDf <- function(dtm){
                DTerms <- dimnames(dtm)$Terms
                DT <- dtm %>% as.TermDocumentMatrix() %>% 
                        as.matrix() %>% as.data.table() 
                DT[,GDTerms := DTerms]
}

uniaioDF <- dtmToDf(UniTDMallinone)

BiaioDF <- dtmToDf(BiTDMallinone)
TriaioDF <- dtmToDf(TriTDMallinone)

#BiaioDF has some terms that are just single words with spaces before or after them
#let's remove those.
adjustedBiaioDF <- BiaioDF[3:(.N-10)]
adjustedBiaioDF <- BiaioDF[!(GDTerms %in% grep("^[^ ]+ $",BiaioDF$GDTerms,value = TRUE) |
                             GDTerms %in% grep("^ [^ ]+$",BiaioDF$GDTerms, value=TRUE))]
#try with str_detect from stringr package to see if there is a better result
library(stringr)
sadjustedBiaioDF <- adjustedBiaioDF[!(str_detect(string = 
                                GDTerms,regex(pattern = " $|^ ",TRUE)))][4:.N]
#inspecting we see that the first 88 and last 333 rows seem to be garbage, so let's get 
#rid of them
#remove some words in the original as well
uniaioDF <- uniaioDF[23:(nrow(uniaioDF)-44)]
#same idea with TriaioDF. This time there are two-word chains that have to be removed
adjustedTriaioDF <- TriaioDF[!(GDTerms %in% grep("^[^ ]+ [^ ]+ $",GDTerms,value = TRUE) | 
                                GDTerms %in% grep("^ [^ ]+ [^ ]+$",GDTerms,value = TRUE) |
                                GDTerms %in% grep("^ [^ ]+ $",GDTerms,value= TRUE) | 
                                GDTerms %in% grep("  ",GDTerms,value = TRUE))]
adjustedTriaioDF[GDTerms %in% grep("^a blue ", GDTerms, value=TRUE)]

adjustedTriaioDF2 <- adjustedTriaioDF[!(GDTerms %in% grep("^ [^ ]+ $",GDTerms,value= TRUE))]
adjustedTriaioDF[GDTerms %in% grep(adjustedBiaioDF[`1` == max(`1`)]$GDTerms, GDTerms, value=T)]

library(ggplot2)
g <- uniaioDF %>% ggplot(aes(x = terms, y = `1`,fill = terms))  + geom_col()
g + fte_theme()
biG <- adjustedBiaioDF[GDTerms %in% names(findMostFreqTerms(BiDTMallinone)$`1`)] %>% 
        ggplot(aes(x = GDTerms, y = `1`, fill = GDTerms)) 
biG + geom_col()
triG <- adjustedTriaioDF[GDTerms %in% names(findMostFreqTerms(TriDTMallinone)$`1`)] %>% 
        ggplot(aes(x = GDTerms, y = `1`, fill = GDTerms))               
triG + geom_col()
 annotate("text",x=1:15, 
                         y = uniaioDF[1:15,]$`1`, label = uniaioDF[1:15,]$terms)

MFT <- findMostFreqTerms(UniDTMallinone)

ggplot(uniaioDF[`1` >= 25000], aes(x = `1`)) + 
        geom_histogram(bins=25) + fte_theme()


#stuff that i'm not sure what to do with
UniDTMallinonenoSparse <- createDTM(corpallinone, end=1) %>% tm_map(removeSparseTerms)
#create DTM with bigram tokenizer from original corpus (divided by tweet)
BigramDTM <- createDTM(USTwitSampCorp,end = length(USTwitSampCorp),
                        tokenizer = BigramTokenizer)
BiTDMallinone[!(BiTDMallinone %in% grep("^[^ ]+ $",dimnames(as.matrix(BiTDMallinone))$Terms,
                                        value = TRUE))]
grep("a", uniaioDF)
blueMoon <- grep("^blue moon", adjustedTriaioDF$GDTerms,value=TRUE)
blueMoonwords <- strsplit(blueMoon,split = " ")
setkey(adjustedBiaioDF,GDTerms)
adjustedBiaioDF[GDTerms %in% grep(paste("^",uniaioDF$GDTerms[3000], sep=""), 
                                  GDTerms, value=TRUE)]$GDTerms

uniaioDF$GDTerms[100]

#that's going to take too long. Second try
#split the strings
#tstrsplit(adjustedBiaioDF$GDTerms, " ")[[1]]

#Maybe try making markov chains for each starting word
grep("^a$",uniaioDF$GDTerms,value=TRUE)
UniTDMallinone
#lets see if there are any entries in the bigram DTM that have nothing from the unigram one
oneword <- uniaioDF$GDTerms[1:8000]
system.time(arethey <- sapply(twoword,str_detect,oneword))
all(unique(wordlists[[2]]) %in% unique(wordlists[[1]]))
sum(unique(wordlists[[1]]) %in% unique(wordlists[[2]]))

shortSAIODF <- sadjustedBiaioDF
twoword <- shortSAIODF$GDTerms
wordlists <- tstrsplit(twoword, " ")
shortSAIODF <- cbind(shortSAIODF, sapply(wordlists, strsplit," "))
shortSAIODF <- shortSAIODF[4:nrow(shortSAIODF)] #remove the weird bits at front
shortSAIODF[,V1 := as.character(V1) ]
shortSAIODF[,V2 := as.character(V2) ]
shortSAIODF[,unique(V1)]

Markovprot <- dcast(shortSAIODF,V2 ~ V1, value.var = "1",drop=FALSE,fun=sum)
Markovprot[1:10,1:10]
test <- dcast(shortSAIODF[1:5000],V2~V1,value.var="1",fun=sum)[1:10,1:10]
v <- t(test["a",-1])[,1]
sims <- sample(1:length(v),1,prob=v,replace=TRUE)
names(v[sims])
Markovprot[3111,V2]
sims <- sample(1:length(v),1,prob=v,replace=TRUE)

wordguess <- Markovprot[,c("V2","poop"),with=FALSE]
wordguess[which(wordguess[,.SD>0, .SDcols = "poop"])] #This is ugly but gets what #
#i want with character vector

dice.roll <- sample(1:nrow(wordguess),1,prob=wordguess$poop)
wordguess[dice.roll]$V2

guessinWrds <- function(x){
        wordguess <- Markovprot[,c("V2",x),with=FALSE]
        dt <- wordguess[which(wordguess[,.SD>0, .SDcols = x])]
        sims <- sample(1:nrow(dt),1,prob=dt[[2]])
        dt[sims]$V2
}



