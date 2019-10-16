### This script generate the feature frequency statistics for the discussion of the ranking model.
### It requires not only RankLib but further the commons-math library.

library(utils)
library(dplyr)

featureStats <- function(
    ranklibJar="./bin/RankLib-2.10.jar",
    mathJar="./bin/commons-math3-3.6.1/commons-math3-3.6.1.jar",
    modelFolder="../train/output/models/",
    modelName="full-model.txt",
    kcv="10",
    featureCount="11",
    tmpfile=tempfile()
) {
    # Prepare table
    featureStatsDF <- data.frame(matrix(0,as.numeric(kcv),as.numeric(featureCount)))
    featureStatsDF <- as.data.frame(sapply(featureStatsDF, as.numeric))
    
    # Iterate through folds
    colnames(featureStatsDF) <- c("Availability", "Interlinking", "W2VGlove", "Consistency", "PageRank", "Understandability", "Lucene", "Believability", "Width", "WordNet", "Depth")
    for(i in c(1:kcv)) {
        statsCommand <- paste0("java -cp ", ranklibJar, ":", mathJar, " ciir.umass.edu.features.FeatureManager -feature_stats ", modelFolder, "f", i, ".", modelName)
        statsCommand <- paste(statsCommand, ">", tmpfile)
        system(statsCommand)
        result <- readLines(tmpfile)
        for (j in c(1:featureCount)) {
            featureStatsDF[i,j] = as.numeric(tail(strsplit(result[j+5],split=" ")[[1]],1))
        }
    }
    
    # Compute means for each feature
    means <- colMeans(featureStatsDF)
    
    # Write to file
    filename <- "./output/feature-stats.csv"
    dir.create(dirname(filename),showWarnings = FALSE);
    write.table(means, file=filename, append = FALSE, sep=",", col.names = FALSE, row.names = TRUE, fileEncoding = "UTF-8", quote = FALSE, qmethod = "double")
}

featureStats(ranklibJar="../bin/RankLib-2.10.jar", mathJar="../bin/commons-math3-3.6.1/commons-math3-3.6.1.jar", modelFolder="../train/output/models/", modelName="full-model.txt", kcv="10")