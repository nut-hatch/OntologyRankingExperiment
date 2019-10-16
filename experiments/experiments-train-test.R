### This R script contains the code to train and test the relevance and full ranking model.
### In order to reproduce the results, it is recommended to delete the following folders first:
###     ./train/output/
###     ./test-lov/output/
###     ./test-bioportal/output/
### Before execution the working directory needs to be set to the location of this file (setwd("path")).

library(utils)
library(dplyr)

# Function name: validateOnMetric
# Function that performs a test with a given metric on a trained model
validateOnMetric <- function(metric,
                         ranklibJar="./bin/RankLib-2.10.jar",
                         trainingFile,
                         kcv,
                         kcvmd,
                         kcvmn,
                         gmax,
                         norm,
                         foldFolder,
                         log,
                         resultFolder,
                         tmpfile=tempfile()) {
    
    allFoldsResults <- c()
    
    # Init result table
    colNames <- c("Model","Fold",metric)
    scoreTable <- data.frame(matrix(ncol = length(colNames), nrow = 0), stringsAsFactors = FALSE)
    colnames(scoreTable) <- colNames
    
    # Compute metric for each fold
    for (fold in c(1:kcv)) {
        # The trained ranking model for respective fold
        modelFile <- paste0(kcvmd,"/f",fold,".",kcvmn)
        # The test data for respective fold
        testFile <- paste0(foldFolder,"/f",fold,".test.",basename(trainingFile))
        
        # Call ranklib for testing
        testCommand <- paste("java -jar", ranklibJar, "-load", modelFile, "-test", testFile, "-metric2T", metric, "-norm", norm)
        if (gmax != '') {
            testCommand <- paste(testCommand,"-gmax",gmax)
        }           
        testCommand <- paste(testCommand, ">", tmpfile)
        system(testCommand)
        
        # Read the test results
        result <- readLines(tmpfile)
        resultLast <- result[length(result)]
        score <- tail(strsplit(resultLast, split=" ")[[1]],1)
        scoreTable[fold,] <- c(kcvmn,fold,score)
        
        # Logging detailed results (test for each fold)
        if (log) {
            filename <- paste0(resultFolder,"/validation_",metric,"_",kcv,"_",norm,"_",substring(basename(trainingFile),1,nchar(basename(trainingFile))-4),".csv")
            if(!file.exists(filename)) {
                write.table(scoreTable, file=filename, append = TRUE, sep=",", col.names = TRUE, row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE, qmethod = "double")
            } else {
                write.table(scoreTable[fold,], file=filename, append = TRUE, sep=",", col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE, qmethod = "double")
            }
        }
        allFoldsResults <- c(allFoldsResults, score)
    }
    
    # Return the mean of the test over all folds
    mean(as.numeric(allFoldsResults))
}


# Function name: validate
# Function that initialises the tests for a trained model, by generating the folds and calling tests with chosen metrics.
validate <- function(at="10",
                 ranklibJar="./bin/RankLib-2.10.jar",
                 trainingFile,
                 kcv,
                 kcvmd,
                 kcvmn,
                 gmax,
                 norm,
                 foldFolder,
                 resultFolder,
                 ...) {
    
    # Generate fold files.
    generateFoldsCommand <- paste("java -cp", ranklibJar, "ciir.umass.edu.features.FeatureManager -input", trainingFile, "-output", foldFolder, "-k", kcv)
    print(generateFoldsCommand)
    system(generateFoldsCommand)
    
    # For our experiments we are especially interested in the @10 scores, and log the detailed results for all folds 
    log <- FALSE
    if (at == '10') {
        log <- TRUE
    }
    
    #Run test for all metrics
    c(validateOnMetric(paste0("P@",at), ranklibJar, trainingFile, kcv, kcvmd, kcvmn, gmax, norm, foldFolder, log, resultFolder),
      validateOnMetric(paste0("MAP"), ranklibJar, trainingFile, kcv, kcvmd, kcvmn, gmax, norm, foldFolder, log, resultFolder),
      validateOnMetric(paste0("DCG@",at), ranklibJar, trainingFile, kcv, kcvmd, kcvmn, gmax, norm, foldFolder, log, resultFolder),
      validateOnMetric(paste0("NDCG@",at), ranklibJar, trainingFile, kcv, kcvmd, kcvmn, gmax, norm, foldFolder, log, resultFolder),
      validateOnMetric(paste0("RR@",at), ranklibJar, trainingFile, kcv, kcvmd, kcvmn, gmax, norm, foldFolder, log, resultFolder),
      validateOnMetric(paste0("ERR@",at), ranklibJar, trainingFile, kcv, kcvmd, kcvmn, gmax, norm, foldFolder, log, resultFolder))
}

# Function name: train
# Function that trains a ranking model and initializes the test on the learned model. Results are logged to a csv file.
train<-function(ranklibJar="./bin/RankLib-2.10.jar",
                trainingFile,
                ranker,
                feature,
                kcv,
                kcvmd,
                kcvmn,
                metric2t,
                gmax,
                norm,
                tmpfile = tempfile()
)
{
    # Creating the command to train the model with ranklib
    command <- paste("java -jar",ranklibJar,"-train",trainingFile,"-ranker",ranker)
    features <- "all"
    if (feature != '') {
        command <- paste(command,"-feature",feature)
        featureFile <- read.table(feature)
        features <- paste0("\"",paste0(featureFile, collapse=" "),"\"")
    }
    if (kcv != '') {
        command <- paste(command,"-kcv",kcv)
    }
    if (kcvmd != '') {
        command <- paste(command,"-kcvmd",kcvmd)
    }
    if (kcvmn != '') {
        command <- paste(command,"-kcvmn",kcvmn)
    }
    if (metric2t != '') {
        command <- paste(command,"-metric2t",metric2t)
    }
    if (gmax != '') {
        command <- paste(command,"-gmax",gmax)
    }
    if (norm != '') {
        command <- paste(command,"-norm",norm)
    }
    command <- paste(command,">",tmpfile)
    
    # Run training
    system(command)
    
    # Read training results
    lines <- R.utils::countLines(tmpfile)
    learningResults <- read.table(tmpfile, header=FALSE, skip = lines-3, nrows = 1, sep = "|", stringsAsFactors = FALSE)
    train <- learningResults$V2
    
    
    # Return test and training results
    c(trainingFile, 
      ranker, 
      features, 
      kcv, 
      metric2t, 
      gmax, 
      norm
    )
}

# Initialize table to log the results
colNames <- c("TrainingSet", 
              "Ranker", 
              "Features", 
              "X-foldCV", 
              "MetricForTraining", 
              "Gmax", 
              "Normalization", 
              "TestP10", 
              "TestMAP10", 
              "TestDCG10", 
              "TestNDCG10", 
              "TestRR10", 
              "TestERR10", 
              "TestP5", 
              "TestMAP5", 
              "TestDCG5", 
              "TestNDCG5", 
              "TestRR5", 
              "TestERR5", 
              "TestP1", 
              "TestMAP1", 
              "TestDCG1", 
              "TestNDCG1", 
              "TestRR1", 
              "TestERR1", 
              "TestP12", 
              "TestMAP12", 
              "TestDCG12", 
              "TestNDCG12", 
              "TestRR12", 
              "TestERR12")

evaluationTable <- data.frame(matrix(ncol = length(colNames), nrow = 0), stringsAsFactors = FALSE)
colnames(evaluationTable) <- colNames



trainAndValidate <- function(ranklibJar,trainingFile,ranker,kcv,kcvmd,kcvmn,metric2t,gmax,norm) {
    
    # 1:Availability 2:Interlinking 3:W2VGlove 4:Consistency 5:PageRank 6:Understandability 7:Lucene 8:Believability 9:Width 10:WordNet 11:Depth
    features = list(c(1:11),
                    c(3,7,10)
    )
    
    # Run the ranklib experiments
    for (i in c(1:length(kcvmn))) {
        
        # Features to be considered need to be stored in a file and specified (unless all features are used)
        featureFile <- "./train/output/tmp/featuresUsedForExperiment.txt"
        dir.create(dirname(featureFile), showWarnings = FALSE, recursive = TRUE)
        featureTable <- matrix(features[i][[1]])
        write.table(featureTable, featureFile, row.names = FALSE, col.names=FALSE, sep = "\n")
        
        
        # Initiation training
        trainResults <- train(ranklibJar,
                               trainingFile,
                               ranker,
                               featureFile,
                               kcv,
                               kcvmd,
                               kcvmn[i],
                               metric2t,
                               gmax,
                               norm,
                               tmpfile = tempfile())
        
        resultFolder <- "./train/output/results"
        validationFolder <- "./train/output/validation-folds"
        dir.create(resultFolder, showWarnings = FALSE, recursive = TRUE)
        dir.create(validationFolder, showWarnings = FALSE, recursive = TRUE)
        
        # Run validation
        validation10 <- validate("10",ranklibJar,trainingFile,kcv,kcvmd,kcvmn[i],gmax,norm,validationFolder,resultFolder)
        validation5 <- validate("5",ranklibJar,trainingFile,kcv,kcvmd,kcvmn[i],gmax,norm,validationFolder,resultFolder)
        validation1 <- validate("1",ranklibJar,trainingFile,kcv,kcvmd,kcvmn[i],gmax,norm,validationFolder,resultFolder)
        validation12 <- validate("12",ranklibJar,trainingFile,kcv,kcvmd,kcvmn[i],gmax,norm,validationFolder,resultFolder)
        
        evaluationTable[nrow(evaluationTable)+1,] <- c(trainResults,validation10,validation5,validation1,validation12)
        
        # Log the results
        filename <- paste0(resultFolder,"/ValidationResults.csv")
        if(!file.exists(filename)) {
            write.table(evaluationTable, file=filename, append = TRUE, sep=",", col.names = TRUE, row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE, qmethod = "double")
        } else {
            write.table(evaluationTable[i,], file=filename, append = TRUE, sep=",", col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE, qmethod = "double")
        }
    }
}

testOnMetric <- function(ranklibJar,testFile,kcv,kcvmd,kcvmn,norm,resultFolder,metric,tmpfile=tempfile()) {
    dir.create(kcvmd, showWarnings = FALSE, recursive = TRUE)
    dir.create(resultFolder, showWarnings = FALSE, recursive = TRUE)
    # Init result table
    colNames <- c("Model","Fold",metric)
    scoreTable <- data.frame(matrix(ncol = length(colNames), nrow = 0), stringsAsFactors = FALSE)
    colnames(scoreTable) <- colNames
    
    # Compute metric for each fold
    for (i in c(1:length(kcvmn))) {
        for (fold in c(1:kcv)) {
            # The trained ranking model for respective fold
            modelFile <- paste0(kcvmd,"/f",fold,".",kcvmn[i])
            print(modelFile)
            # Call ranklib for testing
            testCommand <- paste("java -jar", ranklibJar, "-load", modelFile, "-test", testFile, "-metric2T", metric, "-norm", norm) 
            print(testCommand)
            testCommand <- paste(testCommand, ">", tmpfile)
            system(testCommand)
            
            # Read the test results
            result <- readLines(tmpfile)
            resultLast <- result[length(result)]
            print(resultLast)
            score <- tail(strsplit(resultLast, split=" ")[[1]],1)
            scoreTable[fold,] <- c(kcvmn[i],fold,score)
            
            filename <- paste0(resultFolder,"/test_",metric,"_",kcv,"_",norm,"_",substring(basename(testFile),1,nchar(basename(testFile))-4),".csv")
            if(!file.exists(filename)) {
                write.table(scoreTable, file=filename, append = TRUE, sep=",", col.names = TRUE, row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE, qmethod = "double")
            } else {
                write.table(scoreTable[fold,], file=filename, append = TRUE, sep=",", col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE, qmethod = "double")
            }
        }
    }
}

test <- function(ranklibJar,testFile,kcv,kcvmd,kcvmn,norm,resultFolder) {
    dir.create(resultFolder, showWarnings = FALSE, recursive = TRUE)
    testOnMetric(ranklibJar,testFile,kcv,kcvmd,kcvmn,norm,resultFolder,"MAP")
    testOnMetric(ranklibJar,testFile,kcv,kcvmd,kcvmn,norm,resultFolder,"NDCG@10")
    testOnMetric(ranklibJar,testFile,kcv,kcvmd,kcvmn,norm,resultFolder,"ERR@10")
}


# Define names for models that are trained
kcvmn <- c("full-model.txt", "relevance-model.txt")

trainAndValidate("./bin/RankLib-2.10.jar", "./train/input/TrainingSet-shuffled.txt", "6", "10", "./train/output/models", kcvmn, "ERR@10", "", "sum")
test("./bin/RankLib-2.10.jar", "./test-lov/input/LOVTestSet.txt", "10", "./train/output/models", kcvmn, "sum", "./test-lov/output/results")
test("./bin/RankLib-2.10.jar", "./test-bioportal/input/BioPortalTestSet.txt", "10", "./train/output/models", kcvmn, "sum", "./test-bioportal/output/results")
