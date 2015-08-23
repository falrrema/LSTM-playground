library(rjson)

# Setting the workspace -- Must be were the uncleaned data is stored. 
setwd("~/Documents/S2DS-interns/cleaned")
filenames <- list.files(getwd(), pattern="*.json") #Gets all the files names in the folder. 
numberOfAnswers = 0
error <- data.frame()

for (i in 1:length(filenames)){
        
        #Read file one by one from the uncleaned folder
        setwd("~/Documents/S2DS-interns/s2ds2015") #Since we will me changing folders
        filenamesUncleaned <- list.files(getwd(), pattern="*.json")
        sampleFile <- lapply(filenames[i], function(x) fromJSON(file=x)) 
        
        #Read file one by one from the cleaned folder
        setwd("~/Documents/S2DS-interns/cleaned")
        filenamesCleaned <- list.files(getwd(), pattern="*.json")
        sampleFileCleaned <- lapply(filenames[i], function(x) fromJSON(file=x)) 
        
        #Extract dictionary to data frames from both cleaned and uncleaned data
        for (t in 1:length(names(sampleFile[[1]]))) {
                pubmedArticle <- names(sampleFile[[1]])[[t]]
                dictionary <- t(data.frame(sampleFile[[1]][[pubmedArticle]]$entity_dictionary))
                dictionaryCleaned <- t(data.frame(sampleFileCleaned[[1]][[pubmedArticle]]$entity_dictionary))
                
                dictionary <- as.data.frame(dictionary)
                dictionary$code <- row.names(dictionary)
                dictionaryCleaned <- as.data.frame(dictionaryCleaned)
                dictionaryCleaned$code <- row.names(dictionaryCleaned)
                row.names(dictionary) <- NULL; row.names(dictionaryCleaned) <- NULL
                names(dictionary)[1] <- "entity"; names(dictionaryCleaned)[1] <- "entity"
                
                #Make data frame with the eliminated entities 
                eliminated <- data.frame()
                counter <- 0
                for (k in 1:nrow(dictionary)) {
                        codename <- dictionary$code[k]
                        if(codename %in% dictionaryCleaned$code) {
                                counter <- counter + 1 
                        } else {
                                eliminated <- rbind(eliminated, dictionary[k,])
                        }
                }
                
                #Make data frame with possible Hits on answers that had an entity which was eliminated in the dictionary
                #The first IF is checking for NULL instances, posible when file is lost 
                if(length(sampleFileCleaned[[1]][[pubmedArticle]][["queries"]]) == 0) {
                        error <- rbind(error, as.data.frame(cbind(NA, "NONE", pubmedArticle, filenames[i])))
                } else {
                        for (n in 1:length(sampleFileCleaned[[1]][[pubmedArticle]][["queries"]])) {
                                if (sampleFileCleaned[[1]][[pubmedArticle]][["queries"]][[n]]$answer %in% eliminated$code) { 
                                        
                                        error <- rbind(error, as.data.frame(cbind(sampleFileCleaned[[1]][[pubmedArticle]][["queries"]][[n]]$answer, "ERROR", pubmedArticle, filenames[i])))
                                        numberOfAnswers = numberOfAnswers + 1
                                } else {
                                        error <- rbind(error, as.data.frame(cbind(sampleFileCleaned[[1]][[pubmedArticle]][["queries"]][[n]]$answer, "OK", pubmedArticle, filenames[i])))
                                        numberOfAnswers = numberOfAnswers + 1
                                }
                        }    
                }
                #Status or progress about the process
                progress <- round(i/length(filenames)*100, 2)
                print(paste("Progress......", progress, " % of", length(filenames),"json files"))
                print(paste("Analysing article", pubmedArticle,"from a total of", length(names(sampleFile[[1]]))))
                print(paste("Checked", numberOfAnswers, "answers from", i, ".json files"))
                cat("\n")
        }
        #Cleaning up of global variables and assigning names to columns of the error matrix
        if(i == length(filenames)){
                names(error) <- c("Entity", "Status", "PubmedArticle", "Filename")
                rm(sampleFile); rm(eliminated); rm(sampleFileCleaned); rm(pubmedArticle); rm(dictionary); rm(dictionaryCleaned); rm(codename); rm(counter)
        }  
}