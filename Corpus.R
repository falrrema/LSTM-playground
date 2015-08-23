library(rjson)
library(BBmisc)
library(tm)
library(SnowballC)
library(wordcloud)
require(RColorBrewer)

setwd("~/Documents/S2DS-interns/s2ds2015")
filenames <- list.files(getwd(), pattern="*.json") #Gets all the files names in the folder. 
filenames <- sample(filenames, 3000, replace = F)
sumWords <- function(body) {
        sumVector <- as.data.frame(sapply(gregexpr("\\S+", body), length), optional = T)
        return(sumVector)
}
bar <- makeProgressBar(max=3000, label="ETA")

for (i in 1:3000) {
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
        getBody <- lapply(vectorPubmeds, function(t) as.character(sampleFile[[1]][[t]]$body))  
        getBody <- lapply(getBody, function(w) paste(w, "<eoc>"))
        #getQueries <- lapply(vectorPubmeds, function(t) matrix(as.character(unlist(sampleFile[[1]][[t]]$querie)),ncol=2,byrow=TRUE)[,2])
        #getAnswers <- lapply(vectorPubmeds, function(t) matrix(as.character(unlist(sampleFile[[1]][[t]]$querie)),ncol=2,byrow=TRUE)[,1])
        
        #getCode <- lapply(vectorPubmeds, function(t) names(sampleFile[[1]][[t]]$entity_dictionary))
  	#getEntity <- lapply(vectorPubmeds, function(t) sampleFile[[1]][[t]]$entity_dictionary)
        #getDictionary <- pairlist(getCode, getEntity)
        
        #bodyCorpus = Corpus(VectorSource(getBody))
        #querieCorpus = Corpus(VectorSource(getQueries))
        
        if (!exists("body")) {
        	#superBodyCorpus <- bodyCorpus
                #superQuestionCorpus <- querieCorpus
        	body <- getBody
                #queries <- getQueries
                #answers <- getAnswers
                #dictionary <- getDictionary       
        } 
        
        #superBodyCorpus <- c(superBodyCorpus, bodyCorpus)
        #superQuestionCorpus <- c(superQuestionCorpus, querieCorpus)
        body <- append(body, getBody)
        #queries <- append(queries, getQueries)
        #answers <- append(answers, getAnswers)
        #dictionary <- append(dictionary, getDictionary)

                
        #Status or progress about the process
        progress <- round(i/length(filenames)*100, 2)
        # create progress bar
        print(paste("Progress......", progress, " % ", i,"json files"))
        print(paste("Analysing file", filenames[i]))
        cat("\n")
        bar$set(i)
        Sys.sleep(0.1)
        cat("\n")
}       


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
word