#Exploring the 2010 cohort data
        ten <- fread("./CollegeScorecard_Raw_Data/MERGED2009_10_PP.csv",na.strings = c("NA","NULL"))
        h <- ggplot(data=ten,aes(x=SAT_AVG,y=MN_EARN_WNE_P8)) + 
                geom_point() 
        h + scale_y_discrete(breaks = seq(0,100000,10000)) + 
                geom_hline(yintercept = 50000)#this won't make the line i want..

        str(ten)
#from this we see an overall increasing trend with average SAT score of an institution
# correlated with earning after 6 years.
#There are, however, a fair number of missing values (about a third)
#Before going on, we should probably break the data into training and testing sets
        set.seed(383)
        in.train <- as.logical(rbinom(ten[,.N],1,.9))
        training <- ten[in.train]
        testing <- ten[!in.train]

#To tidy data, the minority servingness should be put into a single variable if possible
        lapply(training[,HBCU:NANTI],function(x)all(is.na(x)))
#All of the values, however are NA, so no use in that.

#Looking at missingness, we need to find if there are certain factors that predict missingness
#I have to do some review..

        #looking at the summaries of the variables
        summaries <- summary(ten)
        #it's been a while, so going to look at what this "summaries" object is really like
        class(summaries)
        attributes(summaries)
        numericFeatures <- sapply(ten,is.numeric) %>% which()
        #use showSummaries() function defined in UefulFunctions.R
        showSummaries(summaries[,numericFeatures])

#boxplot(summaries[,numericFeatures[1:4]])#this doesn't work.. You can't make boxplots
#from summaries.
        dic[grep("^UGDS",AllVarNames,value=TRUE)]
        boxplot(ten[,grep("^COST",names(ten),value=TRUE),with=FALSE])
#Use function DictionarySearch() from  UsefulFunctions.R to find meaning of terms
        DictionarySearch("EARN",dictionary = dic)

        showSummaries(summaries[,grep("^UGDS",dic$AllVarNames,value=TRUE),with=FALSE])
#the above didn't work, now trying with a data.table version

        summaries <- as.data.table(summaries)
        summaries[,V1:=NULL]
        names(summaries) <- c("features", "values")
        setkey(summaries,features)
        summaries[grep("UGDS",summaries$features,value=TRUE),features]
# there is a little whitespace in front of all of the entries
#not work: lapply(summaries,function(x)summaries[,(x):=trimws(x,"both")])
#summaries[,x:=NULL]
        for(i in names(summaries)) summaries[,(i) := trimws(get(i))]
        summaries[grep("^UGDS",features,value = TRUE)]
# Adding up all of the fractions of students from the different provided demographics
# adds up to 1, so looks like things are good there.
        racialDem <- ten[,.(INSTNM,UGDS_HISP,UGDS_BLACK,UGDS_WHITE,UGDS_ASIAN,UGDS_UNKN,UGDS_2MOR,
                            UGDS_NHPI,UGDS_AIAN,UGDS_API,UGDS_WHITENH,UGDS_BLACKNH,UGDS_NRA,
                            UGDS_HISPOLD,UGDS_AIANOLD)]
        rowSums(racialDem[,2:ncol(racialDem)],na.rm = TRUE) %>% is.na() %>% any()
#it seems that some schools without any of a particular demographic recorded the
#percentage as a missing value as all of the demographic info adds up to 1

        boxplot(racialDem) #taking a look at the demographics

#these boxplots leave a bit to be desired. It would be nice to know have all of
#the plots labelled. Maybe it can be done in ggplot2 with the gridExtra package?

# boxplotgg <- function(DT,variable) {
#         ggplot(data=DT,aes(x=get(variable))) + geom_boxplot()      
# }
# ggplot(data=racialDem,aes(x=UGDS_WHITE)) + geom_quantile()#hmmm this is not working
#maybe tstrsplit will help if i can give the x value as the feature name and
#the quantile divisions as they are given from the summary function
        summaries[(names(racialDem)),c("type","value") := tstrsplit(values,split=":")]
        summaries[,value := as.numeric(value)]
        demSummaries <- dcast(summaries[(names(racialDem))],features ~ type)
        setcolorder(demSummaries,c(1,7,2,5,6,3,4,8))#set in more useful order
        dimnames(demSummaries)[[2]] <- trimws(dimnames(demSummaries)[[2]],"both")
        demSummaries[,features := gsub("UGDS_","",features)]
#try ggplot solution
        ggplot(demSummaries,aes(features)) + 
        geom_boxplot(
                aes(ymin = Min., lower = `1st Qu.`, middle = Median, upper = `3rd Qu.`, 
                    ymax = Max.),
                stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
#the above could be made into a function for other situations.

        demSummaries
#instead of doing this with summary data it could be done with the actual data instead. This
# would allow the data to be plotted over the boxplots.
        meltedRacialDem <- racialDem %>% melt(id = "INSTNM",variable.name = "Race",
                                              value.name = "Population Fraction")
        meltedRacialDem[,Race := gsub("UGDS_","",Race)]
        str(meltedRacialDem)
        g <- meltedRacialDem %>% ggplot(aes(x=Race,y=`Population Fraction`))
        g + geom_boxplot(alpha=0.2) + 
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
        #geom_jitter(y=(value)width=.2)
        meltedRacialDem[Race == "HISP",unique(INSTNM) %>% length()]  
        meltedRacialDem[,unique(INSTNM)] %>% length()
#looks like the institution name is repeated.
#It's strange that there are 554 missing values in each of the types of demographic
#It seemed while I was compiling the list of demographics that the missing values were
#spread out between the schools.
#let's compare two variable side by side on a plot maybe?
        rowsWithNAs <- rowSums(racialDem[,2:ncol(racialDem)]) %>% is.na() %>% which()
        racialDem[rowsWithNAs,2:ncol(racialDem)] %>% is.na() %>% all()
#that's kind of confusing because there are some of the sums that were NA
#but when na.rm was set to TRUE then they became values.. This accounts for
#all of the missing values, though..
#let's look at the rowsums again
        racialDem[,2:ncol(racialDem)] %>% rowSums(na.rm=TRUE)#ok i see now the 0's are spread out

#now looking at schools with missing demographic information. It will be necessary to impute
# this data as demographics are very likely an important factor in the success of 
# the students at a college.
        ten[rowsWithNAs,.N,by=STABBR] %>% leaflet() %>%
                leaflet::addTiles()
#looking up info, it seems like it's better documented for plotly. Doing some examples
#on the side in exploringstudentLoans.R. Use the dt made in previous attempt
        missingDemInfobyState <- ten[rowsWithNAs,.N,by=STABBR]
        g <- list(
                scope = 'usa',
                projection = list(type = 'albers usa'),
                showlakes = TRUE,
                lakecolor = toRGB('white')
        )
        
        p <- plot_geo(missingDemInfobyState, locationmode = 'USA-states') %>%
                add_trace(
                        z = ~N, locations = ~STABBR,
                        color = ~N, colors = 'Purples'
                ) %>%
                colorbar(title = "Number of Schools") %>%
                layout(
                        title = "Number of Schools With Missing Demographic Info",
                        geo = g
                )
        
        plotPlotly(p)
#that was fun. Cool looking plot!

#Looking now at scatter plots of earnings vs. costs.
        str(ten)
#MN_EARN_WNE_P8 vs. each of 


#Now look at the individual schools for trends in missingness
        ten[rowsWithNAs]$INSTNM
        summary(ten$COSTT4_P)
        DictionarySearch(dictionary = dic,searchterm = "^RPY_3YR_N")[
                ,c("developer-friendly name","AllVarNames")]
        key(ten)
        ten[,(DictionarySearch(dic, "^GT")$AllVarNames),with=FALSE]        
        DictionarySearch("^MN_EARN_WNE_P8$",dic)$AllVarNames


#Want to make a bunch of scatter plots on the same plot plotting the variables determined in
#this note https://www.evernote.com/l/AYj6Ge83GUVIrIxpVRFot-BgerrEMn2cZN8
#see the variableChooser function in UsefulFunctions.R
        searchTerms <- c("GT_25K_P8","^MN_EARN_WNE_P8$","^RPY", "^CDR",              
                         "$DEBT_MDN", "GRAD_DEBT_MDN_SUPP", "^MEDIAN_HH_INC","^COSTT4_"  )
        variables <- variableChooser(ten,searchTerms,dictionary=dic)   
        scatterVars <- ten[,c("INSTNM",variableChooser(ten,searchTerms,dictionary=dic)),with=FALSE] %>% 
                copy()
        scatterVars %>% ncol()
        dic[variables,.(`developer-friendly name`,AllVarNames)]
        
        melt(scatterVars,id=c("INSTNM","GT_25K_P8"))
#this is giving me warnings that the variables aren't all of the same type.
#theproblem may be columns that are entirely NAs.
#let's remove them using the function created in UsefulFunctions.R:

        removeNAcolumns(scatterVars)

#finally time to plot the data!
        meltedScatterVars <- melt(scatterVars,id=c("INSTNM","GT_25K_P8"))
#still has a problem.. Must be the Privacy Suppressed entries

        str(scatterVars)
        #i think that ggplot can probably handle it
        #let's just use a couple of variables at first
        setkey(meltedScatterVars,variable)
        meltedScatterVars[,unique(variable)]#this looks funny..NVM
        firstplotDat<- meltedScatterVars[c("CDR2","RPY_3YR_N")] %>% copy()
        firstplotDat[,value := as.numeric(value)]#must be numeric or ggplot can't handle it
        firstplotDat[,GT_25K_P8 := as.numeric(GT_25K_P8)]
        scatter <- ggplot(firstplotDat,aes(x=value,y=GT_25K_P8,color=variable ))
        scatter + geom_line()      #this is going to look terrible
#it looks fine, just not working on the same scale

#next, I'll make an app with plotly and shiny to make the plots easier to create