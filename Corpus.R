library(rjson)
library(BBmisc)
library(tm)
library(SnowballC)
library(wordcloud)
require(RColorBrewer)

setwd("~/Documents/S2DS-interns/s2ds2015")
filenames <- list.files(getwd(), pattern="*.json") #Gets all the files names in the folder. 
sumWords <- function(body) {
        sumVector <- as.data.frame(sapply(gregexpr("\\S+", body), length), optional = T)
        return(sumVector)
}
numberOfFilesToRead <- 11500
bar <- makeProgressBar(max=numberOfFilesToRead, label="ETA")
countFiles = 0

for (i in 1:numberOfFilesToRead) {
        setwd("~/Documents/S2DS-interns/s2ds2015")
        sampleFile <- lapply(filenames[i], function(x) fromJSON(file=x)) 
        vectorPubmeds = names(sampleFile[[1]])
        
        if(length(vectorPubmeds) ==0){
                next
        }
        
        getBody <- lapply(vectorPubmeds, function(t) as.character(sampleFile[[1]][[t]]$body))  
        sumWordsMatrix <- cbind(vectorPubmeds, WordsPerBody = sumWords(getBody))
        bodyAbove5000 <- which(sumWordsMatrix[,2] >= 5000 | sumWordsMatrix[,2] <= 1000)
        
        if(length(bodyAbove5000) >= 1) {
                vectorPubmeds <- vectorPubmeds[-c(bodyAbove5000)] 
        }
        
        #Fetching the main body of the paper, tagging it each with an <eoc> and eliminating many "\"
        getBody <- lapply(vectorPubmeds, function(t) as.character(sampleFile[[1]][[t]]$body))
        getBody <- lapply(1:length(getBody), function(w) paste(getBody[w], "<eoc>", collapse = "\""))
        getBody <- gsub("\\", "", getBody, fixed = T) # process included to much "\" and "\\", thus eliminated them.
        getBody <- gsub("\"", "", getBody, fixed = T)      
        
        #Doing the same with answers, question and dictionary
        getQueries <- lapply(vectorPubmeds, function(t) matrix(as.character(unlist(sampleFile[[1]][[t]]$querie)),ncol=2,byrow=TRUE)[,2])
        getQueries <- lapply(1:length(getQueries), function(w) paste(unlist(getQueries[[w]]), "<eoq>", collapse = " "))
        getQueries <- unlist(getQueries)
        
        getAnswers <- lapply(vectorPubmeds, function(t) matrix(as.character(unlist(sampleFile[[1]][[t]]$querie)),ncol=2,byrow=TRUE)[,1])
        getAnswers <- lapply(1:length(getAnswers), function(w) paste(unlist(getAnswers[[w]]), "<eoa>", collapse = " "))
        getAnswers <- unlist(getAnswers)
                
        getCode <- lapply(vectorPubmeds, function(t) names(sampleFile[[1]][[t]]$entity_dictionary))
  	getEntity <- lapply(vectorPubmeds, function(t) sampleFile[[1]][[t]]$entity_dictionary)
        getDictionary <- pairlist(getCode, getEntity)
        getDictionary <- getDictionary[2][[1]]
        
        #bodyCorpus = Corpus(VectorSource(getBody))
        #querieCorpus = Corpus(VectorSource(getQueries))
        
        if (!exists("body") | !exists("queries") | !exists("answers") | !exists("dictionary")) {
        	#superBodyCorpus <- bodyCorpus
                #superQuestionCorpus <- querieCorpus
        	body <- vector()
                queries <- vector()
                answers <- vector()
                dictionary <- vector()      
        } 
        
        #superBodyCorpus <- c(superBodyCorpus, bodyCorpus)
        #superQuestionCorpus <- c(superQuestionCorpus, querieCorpus)
        body <- append(body, getBody)
        queries <- append(queries, getQueries)
        answers <- append(answers, getAnswers)
        dictionary <- append(dictionary, getDictionary)

        if(i %% 500 == 0) {

                setwd("~/Documents/S2DS-interns/clean_data")
                #Writing Body of paper
                exportJson <- toJSON(body)
                write(exportJson, paste("body", countFiles, ".json", sep = ""))
                #Writing Queries
                exportJson <- toJSON(queries)
                write(exportJson, paste("queries", countFiles, ".json", sep = ""))
                #Writing Answers
                exportJson <- toJSON(answers)
                write(exportJson, paste("answers", countFiles, ".json", sep = ""))
                #Writing Dictionaries
                exportJson <- toJSON(dictionary)
                write(exportJson, paste("dictionary", countFiles, ".json", sep = ""))
                
                rm(body); rm(getBody);rm(queries); rm(getQueries);rm(answers); rm(getAnswers); rm(getDictionary); rm(dictionary)
                countFiles = countFiles + 1           
        }
                
        #Status or progress about the process
        progress <- round(i/numberOfFilesToRead*100, 2)
        # create progress bar
        print(paste("Progress......", progress, " % ", i,"json files"))
        print(paste("Analysing file", filenames[i]))
        cat("\n")
        bar$set(i)
        Sys.sleep(0.1)
        cat("\n")
}       

#Corpus analysis
superCorpus = tm_map(superBodyCorpus, tolower)
superCorpus = tm_map(superCorpus, removePunctuation)
superCorpus = tm_map(superCorpus, removeWords, stopwords("english"))

frequencies = DocumentTermMatrix(superBodyCorpus)
findFreqTerms(frequencies, lowfreq=30)
sparse = removeSparseTerms(frequencies, 0.995)
frequency <- as.matrix(sparse)    
frequency <- colSums(frequency) 
frequency <- sort(frequency, decreasing = T)
word <- names(frequency)
pal <- brewer.pal(8,"Dark2")
wordcloud(word[1:500], frequency[1:500], colors=pal)

#For reading saved Json files
toBody <- unlist(lapply(list.files(getwd(), pattern= c("*body")), function(x) fromJSON(file=x)))
toQueries <- unlist(lapply(list.files(getwd(), pattern= c("*querie")), function(x) fromJSON(file=x)))
toAnswers <- unlist(lapply(list.files(getwd(), pattern= c("*answers")), function(x) fromJSON(file=x)))
toDictionary <- lapply(list.files(getwd(), pattern= c("*dictionary")), function(x) fromJSON(file=x))[[1]]

str(toQueries)
str(toBody)
str(toAnswers)
str(toDictionary)


