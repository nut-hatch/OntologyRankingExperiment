### This file generates the benchmark for the LOV portal.
### Usage: Set source file location as working directory and run script.

library(jsonlite)
library(tidyverse)
library(udpipe)

# Read metadata extracted from LOV SPARQL endpoint about collection
vocabs_meta <- fromJSON("./input/LOVOntologyCollection-Metadata.json",flatten = TRUE)
vocabs_meta <- vocabs_meta %>% filter(`description.xml:lang` == "en") %>% select(uri.value,keyword.value,description.value,reusedByDatasets.value)

# Get NLP model to extract nouns and verbs
keywords <- unique(vocabs_meta$keyword.value)
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model("./english-ewt-ud-2.3-181115.udpipe")
x <- udpipe_annotate(ud_model, x = vocabs_meta$description.value)
x <- as.data.frame(x)
tokens <- x %>% filter(upos=="NOUN" | upos=="VERB") %>% select(token, upos) %>% filter(!str_detect(token, "^http"))
    
queries <- unique(tolower(tokens$token))

# Init vars
folderRaw <- "./output/rankings/raw/"
folderFiltered <- "./output/rankings/filtered/"
filenameFilteredAll <- "./output/RankingScoresLOV_All.csv"
queriesFileName <- paste0(folderRaw,"queries.txt")
dir.create(folderRaw,recursive=TRUE);
dir.create(folderFiltered,recursive=TRUE);
writeLines(queries, con = file(queriesFileName), sep = "\n", useBytes = FALSE)
lovSearchAPI <- "https://lov.linkeddata.es/dataset/lov/api/v2/vocabulary/search?page_size=500"

# Get ranking for each query
for (i in c(1:length(queries))) {
    query <- queries[i]
    if (nchar(query) > 1) { 
        url <- URLencode(paste0(lovSearchAPI,"&q=",query))
        print(url)
        filename <- paste0(folderRaw,gsub('[^a-zA-Z]', '_',query),".json")
        if (!file.exists(filename)) {
            download.file(url,filename)
            Sys.sleep(10)
        }
        
        # Filtering all ontologies of problematic domains without reuses
        filenameFiltered <- paste0(folderFiltered,gsub('[^a-zA-Z]', '_',query),".csv")
        if (!file.exists(filenameFiltered)) {
            result <- fromJSON(filename, flatten = TRUE)$results
            if (length(result) > 0) {
                #result$query <- query
                result <- result %>% select(`_id`,`_score`,`_source.prefix`,`_source.http://purl.org/dc/terms/title@en`, `_source.http://purl.org/dc/terms/description@en`, `_source.tags`) %>% mutate(query=query)
                
                # only keep ontologies that have at least one tag of the considered keywords
                filterVec <- sapply(result$`_source.tags`, FUN = function(x) any(x %in% keywords))
                result <- result[filterVec,]
                
                # save
                result$`_source.tags` <- vapply(result$`_source.tags`, paste, collapse = ", ", character(1L))
                write.table(result, file=filenameFiltered, sep=",", col.names = TRUE, row.names = FALSE)
                if (!file.exists(filenameFilteredAll)) {
                    write.table(result, file=filenameFilteredAll, sep=",", col.names = TRUE, row.names = FALSE)
                } else {
                    write.table(result, file=filenameFilteredAll, sep=",", col.names = FALSE, row.names = FALSE, append=TRUE)
                }
            }
        }
    }
}

# Read all rankings
rankings <- read.table(filenameFilteredAll, sep = ",", stringsAsFactors = FALSE, header = TRUE)

# remove open311 vocabulary, as it cannot be imported to stardog
open311IRI <- "http://ontology.eil.utoronto.ca/open311.owl"
rankings <- rankings %>% filter(!X_id == open311IRI)

# Filter all queries that have less than 8 results
rankingsFiltered <- rankings %>% group_by(query) %>% filter(n()>6) %>% filter(nchar(query) > 2)

# Prepare ground truth table
groundTruth <- data.frame(matrix(0,0,3))
colnames(groundTruth) <- c("query", "vocabulary","relevance")
groundTruth$query <- as.character(groundTruth$query)
groundTruth$vocabulary <- as.character(groundTruth$vocabulary)
groundTruth$relevance <- as.numeric(groundTruth$relevance)

vocabs <- as.data.frame(unique(vocabs_meta$uri.value), stringsAsFactors = FALSE)
colnames(vocabs) <- c("uri")

# Iterate through all queries
GTqueries <- unique(rankingsFiltered$query)
set.seed(11111)
for (GTquery in GTqueries) {
    rankingsFilteredQuery <- rankings %>% filter(query==GTquery)
    
    # Compute relevance score
    maxScore <- max(rankingsFilteredQuery$X_score)
    minScore <- min(rankingsFilteredQuery$X_score)
    intervalRange <- (maxScore - minScore) / 4
    lowerLimitScore4 <- maxScore - intervalRange
    lowerLimitScore3 <- lowerLimitScore4 - intervalRange
    lowerLimitScore2 <- lowerLimitScore3 - intervalRange
    rankingsFilteredQuery <- rankingsFilteredQuery %>% mutate(relevance=ifelse(X_score > lowerLimitScore4,4,
                                                      ifelse(X_score > lowerLimitScore3,3,
                                                             ifelse(X_score > lowerLimitScore2,2,
                                                                    1))))
    groundTruth <- bind_rows(groundTruth,rankingsFilteredQuery %>% select(query,vocabulary=X_id,relevance))
    
    # Add non-relevant documents
    nonRelevantVocabs <- vocabs %>% filter(!uri %in% rankingsFilteredQuery$X_id)
    if (nrow(nonRelevantVocabs) > nrow(rankingsFilteredQuery)) {
        randomSelection <- sample(1:nrow(nonRelevantVocabs), nrow(rankingsFilteredQuery), replace=F)
        nonRelevantVocabs <- as.data.frame(nonRelevantVocabs[randomSelection,], stringsAsFactors = FALSE)
        names(nonRelevantVocabs) <- c("uri")
    }
    nonRelevantVocabs <- nonRelevantVocabs %>% mutate(query=as.character(GTquery), relevance=0)
    groundTruth <- bind_rows(groundTruth,nonRelevantVocabs %>% select(query,vocabulary=uri,relevance))
    
}

# Generate ground truth file
groundTruthFileName <- "./output/LOVGroundTruth.csv"
write.table(groundTruth, groundTruthFileName, sep = ",", col.names = FALSE, row.names = FALSE)
