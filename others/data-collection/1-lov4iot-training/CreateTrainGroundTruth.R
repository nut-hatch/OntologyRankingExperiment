### This file generates the ground truth based on the LOV4IoT catalog.
### Usage: Set source file location as working directory and run script.

library(tidyverse)

# Prepare ground truth table
groundTruth <- data.frame(matrix(0,0,3))
colnames(groundTruth) <- c("query", "vocabulary","relevance")
groundTruth$query <- as.character(groundTruth$query)
groundTruth$vocabulary <- as.character(groundTruth$vocabulary)
groundTruth$relevance <- as.numeric(groundTruth$relevance)

# Get all relevant ontologies
relevantByDomain <- read.csv("./input/RelevantOntologiesByDomain.csv", stringsAsFactors = FALSE)
relevantByDevice <- read.csv("./input/RelevantOntologiesByDevice.csv", stringsAsFactors = FALSE)
relevantVocs <- rbind(relevantByDomain, relevantByDevice)

# Remove ontologies that are not in the triple store
ignoreVocs <- readLines("./input/SyntacticallyIncorrectVocabs.txt")
relevantVocs <- relevantVocs %>% filter(!ontology %in% ignoreVocs)

# Compute popularity scores
popularityScores <- read.csv("./input/OntologyScoresNormalized.csv", stringsAsFactors = FALSE)
relevantVocsScores <- left_join(relevantVocs, popularityScores, by = c("ontology" = "VocabularyIRI"))
# Remove vocs without score information
relevantVocsScores <- relevantVocsScores[complete.cases(relevantVocsScores), ]
relevantVocsScores <- relevantVocsScores %>% mutate(popularityScore=0.5*cites_norm+0.25*a_norm+0.25*b_norm)
relevantVocsScores <- relevantVocsScores %>% group_by(query) %>% filter(n()>6) %>% filter(nchar(query) > 2)

vocabs <- as.data.frame(unique(relevantVocsScores$ontology), stringsAsFactors = FALSE)
colnames(vocabs) <- c("uri")

# Iterate through queries
GTqueries <- unique(relevantVocsScores$query)
set.seed(11111)
for (GTquery in GTqueries) {
    rankingsFilteredQuery <- relevantVocsScores %>% filter(query==GTquery)
    
    # Compute relevance score
    maxScore <- max(rankingsFilteredQuery$popularityScore)
    minScore <- min(rankingsFilteredQuery$popularityScore)
    intervalRange <- (maxScore - minScore) / 4
    lowerLimitScore4 <- maxScore - intervalRange
    lowerLimitScore3 <- lowerLimitScore4 - intervalRange
    lowerLimitScore2 <- lowerLimitScore3 - intervalRange
    rankingsFilteredQuery <- rankingsFilteredQuery %>% mutate(relevance=ifelse(popularityScore > lowerLimitScore4,4,
                                                                               ifelse(popularityScore > lowerLimitScore3,3,
                                                                                      ifelse(popularityScore > lowerLimitScore2,2,
                                                                                             1))))
    groundTruth <- bind_rows(groundTruth,rankingsFilteredQuery %>% select(query,vocabulary=ontology,relevance))
    
    # Add non-relevant ontologies randomly
    nonRelevantVocabs <- vocabs %>% filter(!uri %in% rankingsFilteredQuery$ontology)
    numberOfNonRelevantVocabs <- floor(nrow(rankingsFilteredQuery) * 4)
    if (nrow(nonRelevantVocabs) > numberOfNonRelevantVocabs) {
        randomSelection <- sample(1:nrow(nonRelevantVocabs), numberOfNonRelevantVocabs, replace=F)
        nonRelevantVocabs <- as.data.frame(nonRelevantVocabs[randomSelection,], stringsAsFactors = FALSE)
        names(nonRelevantVocabs) <- c("uri")
    }
    nonRelevantVocabs <- nonRelevantVocabs %>% mutate(query=as.character(GTquery), relevance=0)
    groundTruth <- bind_rows(groundTruth,nonRelevantVocabs %>% select(query,vocabulary=uri,relevance))
    
}

groundTruth <- groundTruth %>% arrange(query, desc(relevance))

# Create Ground Truth file
groundTruthFileName <- "./output/GroundTruth.csv"
write.table(groundTruth,groundTruthFileName,sep = ",",col.names = FALSE, row.names = FALSE)

