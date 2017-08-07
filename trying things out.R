tdmfitwit <- TermDocumentMatrix(corp[9])
ovidToken <- NGramTokenizer(ovid)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
oviddtm2gram <- DocumentTermMatrix(ovid, control = list(tokenize = BigramTokenizer))
BigramTokenizer <-
        function(x)
                unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

ovidtdm2gram <- TermDocumentMatrix(ovid, control = list(tokenize = BigramTokenizer))
inspect(removeSparseTerms(tdm[, 1:10], 0.7))

## A simple text.
s <- String("  First sentence.  Second sentence.  ")
##           ****5****0****5****0****5****0****5**

## Use a pre-built regexp (span) tokenizer:
wordpunct_tokenizer
wordpunct_tokenizer(s)
## Turn into a token tokenizer:
tt <- as.Token_Tokenizer(wordpunct_tokenizer)
tt
tt(s)
## Of course, in this case we could simply have done
s[wordpunct_tokenizer(s)]
## to obtain the tokens from the spans.
## Conversion also works the other way round: package 'tm' provides
## the following token tokenizer function:
scan_tokenizer <- function(x)
        scan(text = as.character(x), what = "character", quote = "", 
             quiet = TRUE)
## Create a token tokenizer from this:
tt <- Token_Tokenizer(scan_tokenizer)
tt(s)
## Turn into a span tokenizer:
st <- as.Span_Tokenizer(tt)
st(s)
## Checking tokens from spans:
s[st(s)]

s <- "The quick brown fox jumps over the lazy dog"
## Split into words:
w <- strsplit(s, " ", fixed = TRUE)[[1L]]
## Word tri-grams:
ngrams(w, 3L)
## Word tri-grams pasted together:
vapply(ngrams(w, 3L), paste, "", collapse = " ")

library("tm")
data("crude")

BigramTokenizer <-
        function(x)
                unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

system.time(UStwitsampleDTM <- DocumentTermMatrix(twitterSampleCorp, 
                                                   control = list(tokenize = BigramTokenizer)))
inspect(removeSparseTerms(tdm[, 1:10], 0.7))

UStwit <- readLines(con)

UStwitsample <- character()
con <- file("final/en_US/en_US.twitter.txt", "r") 
        j=1
        coin <- rbinom(length(UStwit),1,0.2)
for(i in 1:length(UStwit)){
        if(coin[i] == 1){
                UStwitsample[j] <- readLines(con,1)
                j <- j + 1
        } else {
                sink("flile")
                readLines(con,1)
                sink()
        }
}
close(con)
UStwitsample2 <- UStwit[coin]

sink("flile")
readLines(con, 5)## Read in the next 5 lines of text 
readLines(con)
sink()
readLines(con,5)
close(con) ## It's important to close the connection when you are done

first10 <- readLines(con, 10) ## Read the first line of text 

#try annotate function in ggplot2
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + annotate("text", x = 4, y = 25, label = "Some text")
p + annotate("text", x = 2:5, y = 25, label = "Some text")
p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21,
             alpha = .2)
p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,
             colour = "blue")
p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
             colour = "red", size = 1.5)

p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))

p + annotate("text", x = 4, y = 25, label = "italic(R) ^ 2 == 0.75",
             parse = TRUE)
p + annotate("text", x = 4, y = 25,
             label = "paste(italic(R) ^ 2, \" = .75\")", parse = TRUE)

##hadley wikham presentation
qplot(long, lat, data = expo, geom="tile", fill = ozone,
      facets = year ~ month) +
        scale_fill_gradient(low="white", high="black") + map

#playing with data.table
DT <- data.table(strings = c(" good","great ", " amazing ","  great and","and good "))
DT[!(strings %in% grep("^[^ ]+ $",strings,value = TRUE) | 
           strings %in% grep("^ [^ ]+$",strings,value = TRUE))]

strings = c(" good","great ", " amazing ","  great and","and good ")
testCorp <- VCorpus(VectorSource(strings))


#datatable fun
DT <- data.table( ID = 1:50,
                  Capacity = sample(100:1000, size = 50, replace = F),
                  Code = sample(LETTERS[1:4], 50, replace = T),
                  State = rep(c("Alabama","Indiana","Texas","Nevada"), 50))

DT[Code == "C", mean(Capacity)]
DT[Code == "D"]
DT[, mean(Capacity), by = State]

urlbig <- "http://download.geonames.org/export/zip/GB_full.csv.zip"
download.file(url = urlbig, destfile = "GB_full.csv.zip")
DT[Code == "A", mean(Capacity)]

unzip("GB_full.csv.zip")
DT <- fread("GB_full.csv")
 DT[order(V4, -V8)]

 sub_columns <- DT[,.(V2,V3,V4)]
 DT[, V_New := V10 + V11]
 DT[, .N, by = V4]
 DT[.("Shetland South Ward",on ="Scotland")]
 DT[,V_d := rep("3",times=nrow(DT))]
 
testTable <- data.table(letters2 = sample(letters,100,replace = TRUE),
                        monthName = sample(month.name,100,replace = TRUE))

for(i in 1:nrow(testTable)){
        print(testTable$monthName[i])
}
oneword <- "torrential"
twowords <- c("ghastly", "orbital")
data.table(as.list(oneword), as.list(twowords))
data.table(V1 = lastwords)

DT
twoword <- sadjustedBiaioDF$GDTerms[3:8]
oneword <- uniaioDF$GDTerms[1:5]
sapply(twoword,str_detect,oneword)
systemdat <- data.frame(yabba=c(1000,2000,4000), dabba = c(8.5,34,165))
expFit <- lm(data = systemdat, log(y) ~ x)
detach(systemdat)
plot(systemdat$yabba,log(systemdat$dabba), xlim = c(0,5000),ylim=c(0,10))
abline(coef=expFit$coefficients)

names(ChickWeight) <- tolower(names(ChickWeight))
DT <- melt(as.data.table(ChickWeight), id=2:4) # calls melt.data.table

# dcast is a S3 method in data.table from v1.9.6
dcast(DT, time ~ variable, fun=mean)
dcast(DT, diet ~ variable, fun=mean)
dcast(DT, diet+chick ~ time, drop=FALSE)
dcast(DT, diet+chick ~ time, drop=FALSE, fill=0)

# using subset
dcast(DT, chick ~ time, fun=mean, subset=.(time < 10 & chick < 20))
dcast(shortSAIODF,)
x <- matrix(runif(100), ncol = 5)
group <- sample(1:8, 20, TRUE)
(xsum <- rowsum(x, group))

#function aggregation
dt = data.table(x=sample(5,20,TRUE), y=sample(2,20,TRUE), 
                z=sample(letters[1:2], 20,TRUE), d1 = runif(20), d2=1L)
# multiple value.var
dcast(dt, x + y ~ z, fun=sum, value.var=c("d1","d2"))
# multiple fun.aggregate
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var="d1")
# multiple fun.agg and value.var (all combinations)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=c("d1", "d2"))
# multiple fun.agg and value.var (one-to-one)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=list("d1", "d2"))

#probability table
throws <- sample(1:6, 1000, replace=TRUE, prob=c(1,1,1,1,1,2) )
Markovprot[1]
shortSAIODF

char <- "guts and glory for you"
last(unlist(strsplit(char, " ")))

#accessing a column with character
DT = data.table(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)
cols = c("x","y")
DT[,lapply(.SD[,list(x,y)], min) ]

DT[, .SD, .SDcols=x:y]
DT[, .SD[1]] 
DT[, which(.SD[1]>0), by=x]  

X[, DT[.BY, y, on="x"], by=x]          # join within each group
X = data.table(x=c("c","b"), v=8:7, foo=c(4,2))

