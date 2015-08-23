library(rjson)
library(BBmisc)
setwd("~/Documents/S2DS-interns/s2ds2015")
filenames <- list.files(getwd(), pattern="*.json") #Gets all the files names in the folder. 

sumWords <- function(body) {
        sumVector <- as.data.frame(sapply(gregexpr("\\S+", body), length), optional = T)
        return(sumVector)
}
bar <- makeProgressBar(max=length(filenames), label="ETA")

for (i in 1:length(filenames)) {
        
        sampleFile <- lapply(filenames[i], function(x) fromJSON(file=x)) 
        vectorPubmeds = names(sampleFile[[1]])
        getBodyJson <- lapply(vectorPubmeds, function(t) as.character(sampleFile[[1]][[t]]$body))
        sumWordsMatrix <- cbind(vectorPubmeds, WordsPerBody = sumWords(getBodyJson))
        
        if(!exists("numberWords")) {
                numberWords <- sumWordsMatrix
        } 
        numberWords <- rbind(numberWords, sumWordsMatrix, deparse.level = 0)
        
        #Status or progress about the process
        progress <- round(i/length(filenames)*100, 2)
        # create progress bar
        print(paste("Progress......", progress, " % of", length(filenames),"json files"))
        print(paste("Running mean of", round(mean(numberWords[,2]),2)))
        print(paste("Analysing file", filenames[i],".json files"))
        cat("\n")
        bar$set(i)
        Sys.sleep(0.1)
        cat("\n")
        
        if(i == length(filenames)) {
             averageNumberWords <- mean(numberWords[,2])
             sdNumberWords <- sd(numberWords[,2])
             print(c(mean = averageNumberWords, sd = sdNumberWords))
             hist(numberWords[,2], xlab = "Number of Words per Body per Document", ylab = "Frequency", breaks = 800, main = "", xlim = c(0,10000), col = "lightblue")
             abline(v = averageNumberWords, col = "darkorange", lty = 2, lwd = 5)
             legend(3500, 60000, cex = 0.8, legend = c(expression(paste(italic("Average = 3177"))), expression(paste(italic("Median = 2767"))), expression(italic("Standard deviation = 2206"))), col = c("black", "black", "black"))
             
             rm(sampleFile);rm(vectorPubmeds);rm(sumWordsMatrix);rm(getBodyJson)
            
        } 
}

