### This script generates the boxplots for the validation of the relevance and full model.
### It uses as input the detailed results for each fold per metric, and generates plots as *.tex files.

library(ggplot2)
library(tikzDevice)

metrics <- c("MAP","NDCG@10", "ERR@10")
kcv <- "10"
norm <- "sum"
trainFile <- "TrainingSet-shuffled"
filename <- "./output/boxplot_validation.tex"
dir.create(dirname(filename),showWarnings = FALSE);

# Generate boxplots with *.tex export 
for (metric in c(1:length(metrics))) {
    # Read data for metric
    data <- read.table(paste0("../train/output/results/validation_",metrics[metric],"_",kcv,"_",norm,"_",trainFile,".csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
    print(str(data))
    # Define order for boxplot
    data$Model <- factor(data$Model,levels = c("relevance-model.txt", "full-model.txt"), ordered = TRUE)
    yCol <- gsub("@", ".", metrics[metric])
    
    # Create plot
    plot <- ggplot(data, aes(x=Model, y=!!ensym(yCol), fill=Model)) + 
        # scale_colour_grey() +
        geom_boxplot(outlier.shape = 4, outlier.colour=NA) + #outlier.colour=NA 
        geom_point(aes(fill=Model),position = position_jitterdodge(), size=0.5) +
        #geom_line(aes(group = Fold), alpha = 0.6, colour = "black") +
        #geom_smooth() +
        stat_summary(fun.y=mean, geom="line", aes(group=1), alpha = 0.8, colour = "red", size=1.2)  + 
        stat_summary(fun.y=mean, geom="point", shape=4, size=2) +
        scale_fill_manual(values=c("#DCDCDC","#A9A9A9")) +
        # scale_fill_manual(values=c("#969696","#74c476")) +
        ggtitle("") + xlab("") + 
        ylab(metrics[metric]) + 
        stat_boxplot(geom ='errorbar', width = 0.3, coef=1.5) + 
        coord_cartesian(ylim = c(0, 1)) + 
        theme_bw() +
        theme(
            legend.position = "none",
            plot.title = element_blank(),
            axis.title.x = element_blank()
        )
    plot <- plot + theme(axis.title.y = element_text( vjust=-1))
    plot <- plot + scale_x_discrete(labels=c("(a) Relevance","(b) Full")) + theme(plot.margin = unit(c(0,0,0,0), "cm"))
    if (metrics[metric] == "MAP") {
        plotMap <- plot
    } else if (metrics[metric] == "NDCG@10") {
        plotNdcg <- plot
    } else if (metrics[metric] == "ERR@10") {
        plotErr <- plot
    }
    #print(plot)
    
}

# Generate output file
tikz(file = filename, width = 1.55*3, height = 1.2)
grid.arrange(plotMap, plotNdcg, plotErr, nrow = 1)
dev.off()
