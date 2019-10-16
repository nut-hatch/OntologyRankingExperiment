### This file generates the benchmark for the NCBO recommender 2.0 of the BioPortal.
### Important: The data collection requires a valid API key of the BioPortal API.
### Usage: Set source file location as working directory and run script.

library(jsonlite)
library(tidyverse)
library(udpipe)

# Get metadata about all ontologies
bioportalBase <- "http://data.bioontology.org/"
apikey <- "ENTER YOUR API KEY"
allOntsUrl <- paste0(bioportalBase,"ontologies/","?apikey=",apikey)

ontsMetadataFile <- "./output/BioPortal_Ontologies_Metadata.json"
dir.create(dirname(ontsMetadataFile), showWarnings = FALSE)
download.file(allOntsUrl,ontsMetadata)

ontologyMetadata <- fromJSON(ontsMetadataFile,flatten = FALSE)
ontologyMetadataSimple <- ontologyMetadata %>% select(-links,-`@context`)

ontologyFolder <- "./output/ontologies/"
dir.create(ontologyFolder, showWarnings = FALSE)

unaccessibleOntologiesFile <- "./output/UnaccessibleOntologies.txt"
if (file.exists(unaccessibleOntologiesFile)) {
    unaccessibleOntologies <- readLines(unaccessibleOntologiesFile)    
}

# Download the ontology collection
for (row in c(1:nrow(ontologyMetadataSimple))) {
    acronym <- ontologyMetadataSimple[row,]$acronym
    print(acronym)
    ontologyFile <- paste0(ontologyFolder,acronym,".owl")
    if(!file.exists(ontologyFile) && (!acronym %in% unaccessibleOntologies)) {
        downloadOntologyUrl <- paste0(bioportalBase,"ontologies/",acronym,"/download","?apikey=",apikey)
        tryCatch(download.file(downloadOntologyUrl, ontologyFile, mode = "wb", quiet = FALSE), 
                 error = function(e) write(acronym,file=unaccessibleOntologiesFile,append=TRUE))    
    }
}

# Get NLP model to extract nouns and verbs
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model("./english-ewt-ud-2.3-181115.udpipe")
x <- udpipe_annotate(ud_model, x = ontologyMetadataSimple$name)
x <- as.data.frame(x)
tokens <- x %>% filter(upos=="NOUN" | upos=="VERB") %>% select(token, upos) %>% filter(!str_detect(token, "^http"))

queries <- unique(tolower(tokens$token))

# Init vars
folderRaw <- "./output/rankings/raw/"
folderFiltered <- "./output/rankings/filtered/"
filenameFilteredAll <- "./output/RankingScoresBioPortal_All.csv"

queriesFileName <- paste0(folderRaw,"queries.txt")
dir.create(folderRaw,recursive=TRUE);
dir.create(folderFiltered,recursive=TRUE);
writeLines(queries, con = file(queriesFileName), sep = "\n", useBytes = FALSE)
localVocabs <- read.csv("./input/BioPortalOntologiesInLocalTripleStore.csv", stringsAsFactors = FALSE)

# Get ranking for each query
for (i in c(1:length(queries))) {
    query <- queries[i]
    if (nchar(query) > 1) { 
        url <- URLencode(paste0(bioportalBase,"recommender?input=",query,"&input_type=2&pagesize=200","&apikey=",apikey))#&wd=0&ws=0
        print(url)
        filename <- paste0(folderRaw,gsub('[^a-zA-Z]', '_',query),".json")
        if (!file.exists(filename)) {
            download.file(url,filename)
            Sys.sleep(10)
        }
        filenameFiltered <- paste0(folderFiltered,gsub('[^a-zA-Z]', '_',query),".csv")
        
        # Processing crawled ranking
        if (!file.exists(filenameFiltered)) {
            result <- fromJSON(filename, flatten = TRUE)
            if (is.data.frame(result) && nrow(result) > 0) {
                resultDF <- data.frame(matrix(0,nrow = nrow(result),ncol=7))
                colnames(resultDF) <- c("uri", "score", "acronym", "coverageScore", "specializationScore", "acceptanceScore", "query")
                for (i in c(1:nrow(result))) {
                    print(i)
                    resultDF[i,] <- c(result$ontologies[[i]]$`@id`, result$evaluationScore[i],result$ontologies[[i]]$acronym,result$coverageResult.score[i],result$specializationResult.score[i],result$acceptanceResult.normalizedScore[i],query)
                }
                
                # only keep vocabularies that could be inserted to local repository
                filterVec <- sapply(resultDF$uri, FUN = function(x) any(x %in% localVocabs$g))
                resultDF <- resultDF[filterVec,]

                # save
                if (nrow(resultDF) > 0) {
                    write.table(resultDF, file=filenameFiltered, sep=",", col.names = TRUE, row.names = FALSE)
                    if (!file.exists(filenameFilteredAll)) {
                        write.table(resultDF, file=filenameFilteredAll, sep=",", col.names = TRUE, row.names = FALSE)
                    } else {
                        write.table(resultDF, file=filenameFilteredAll, sep=",", col.names = FALSE, row.names = FALSE, append=TRUE)
                    }
                }
            }
        }
    }
}

# Create NameSpace2FileName file
unaccessibleOntologies <- readLines(unaccessibleOntologiesFile)
ontologyMetadataSimpleFiltered <- ontologyMetadataSimple %>% select(`@id`,acronym) %>% filter(!acronym %in% unaccessibleOntologies)

# Map BioPortal names to actual ontology IRI
BP2Namespace <- read.csv("./BPNameToOntologyNamespace.csv",stringsAsFactors = FALSE)
ontologyMetadataSimpleFiltered <- ontologyMetadataSimpleFiltered %>% right_join(BP2Namespace, by=c("@id" = "g"))

# Read all rankings
rankings <- read.table(filenameFilteredAll, sep = ",", stringsAsFactors = FALSE, header = TRUE)
rankings <- rankings %>% right_join(BP2Namespace, by=c("uri" = "g"))

# Filter all queries that have less than 8 results
rankingsFiltered <- rankings %>% group_by(query) %>% filter(n()>6) %>% filter(nchar(query) > 2)

# Prepare ground truth table
groundTruth <- data.frame(matrix(0,0,3))
colnames(groundTruth) <- c("query", "vocabulary","relevance")
groundTruth$query <- as.character(groundTruth$query)
groundTruth$vocabulary <- as.character(groundTruth$vocabulary)
groundTruth$relevance <- as.numeric(groundTruth$relevance)


vocabs <- as.data.frame(unique(localVocabs$g), stringsAsFactors = FALSE)
colnames(vocabs) <- c("uri")
vocabs <- vocabs %>% right_join(BP2Namespace, by=c("uri" = "g"))

# Iterate through all queries
GTqueries <- unique(rankingsFiltered$query)
set.seed(11111)
for (GTquery in GTqueries) {
    rankingsFilteredQuery <- rankings %>% filter(query==GTquery)
    
    # Compute relevance score
    maxScore <- max(rankingsFilteredQuery$score)
    minScore <- min(rankingsFilteredQuery$score)
    intervalRange <- (maxScore - minScore) / 4
    lowerLimitScore4 <- maxScore - intervalRange
    lowerLimitScore3 <- lowerLimitScore4 - intervalRange
    lowerLimitScore2 <- lowerLimitScore3 - intervalRange
    rankingsFilteredQuery <- rankingsFilteredQuery %>% mutate(relevance=ifelse(score > lowerLimitScore4,4,
                                                                               ifelse(score > lowerLimitScore3,3,
                                                                                      ifelse(score > lowerLimitScore2,2,
                                                                                             1))))
    groundTruth <- bind_rows(groundTruth,rankingsFilteredQuery %>% select(query,vocabulary=s,relevance))
    
    # Add non-relevant documents
    nonRelevantVocabs <- vocabs %>% filter(!uri %in% rankingsFilteredQuery$uri)
    if (nrow(nonRelevantVocabs) > nrow(rankingsFilteredQuery)) {
        randomSelection <- sample(1:nrow(nonRelevantVocabs), nrow(rankingsFilteredQuery), replace=F)
        nonRelevantVocabs <- as.data.frame(nonRelevantVocabs[randomSelection,], stringsAsFactors = FALSE)
        names(nonRelevantVocabs) <- c("uri", "s")
    }
    nonRelevantVocabs <- nonRelevantVocabs %>% mutate(query=as.character(GTquery), relevance=0)
    groundTruth <- bind_rows(groundTruth,nonRelevantVocabs %>% select(query,vocabulary=s,relevance))
    
}

# Generate ground truth file
groundTruthArranged <- groundTruth %>% group_by(query,vocabulary) %>% arrange(query,desc(relevance))
groundTruthFileName <- "./output/BioPortalGroundTruth.csv"
write.table(groundTruthArranged,groundTruthFileName,sep = ",",col.names = FALSE, row.names = FALSE)
