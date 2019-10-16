### This script generate the figure of the introduction section based on the LOV metadata extracted via SPARQL.

library(jsonlite)
library(tidyverse)
library(tikzDevice)

filename <- "./output/LOVOntologyReuses.tex"             

# Read LOV data
vocabCounts <- fromJSON("./input/LOVreusesSPARQLresult.json", flatten = TRUE)
vocabCounts <- vocabCounts %>% select(keyword.value, cntReused.value, cntTotal.value)
vocabCounts$cntReused.value <- as.numeric(vocabCounts$cntReused.value)
vocabCounts$cntTotal.value <- as.numeric(vocabCounts$cntTotal.value)
vocabCounts <- vocabCounts %>% 
    rowwise() %>% 
    mutate( mymean = mean(c(cntReused.value,cntTotal.value) )) %>% 
    arrange(mymean) %>% 
    mutate(keyword.value=factor(keyword.value, keyword.value)) %>%
    mutate("segment"="segment")

cols <- c( "c1" = rgb(0.2,0.7,0.1,0.5), "c2" = rgb(0.7,0.2,0.1,0.5))

g <- ggplot(vocabCounts) +
    geom_segment( aes(x=keyword.value, xend=keyword.value, y=cntReused.value, yend=cntTotal.value, linetype="c3"), color="grey", size=1) +
    geom_point( aes(x=keyword.value, y=cntReused.value, color="c1"), size=2 ) +
    geom_point( aes(x=keyword.value, y=cntTotal.value, color="c2"), size=2 ) +
    # scale_linetype_manual("segment",values=c("segment"=2))
    ylab("Ontology Count") +
    xlab("Domain/Tag") +
    coord_flip() +
    theme_light() +
    theme(
        # legend.position = "right",
        legend.position = c(0.7, 0.15),
        legend.text = element_text(size = 12),
        # legend.box = "horizontal",
        panel.border = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 12,vjust = 0.2),
        axis.text.x = element_text(size = 12),
        aspect.ratio = 2/2
    ) +
    scale_color_manual(
        name = "Legend:",
                           breaks = c("c1", "c2"),
                           values = cols,
                           labels = c("Total number of ontologies that have been reused at least once", "Total number of ontologies for the domain"))+
    scale_linetype_manual(
        name = "Legend:",
        breaks = c("c3"),
        values = c(1),
        # labels = c("Total count", "At least 1 reuse", "Never reused"))  
        labels = c("Amount of ontologies that were never reused")) + 
     guides(color = guide_legend(element_blank()), linetype = guide_legend(element_blank()))
tikz(file = filename, sanitize=TRUE)
print(g)
dev.off()
lines <- readLines(con=filename)
lines <- lines[-which(grepl("\\path\\[clip\\]*", lines,perl=F))]
lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines,perl=F))]
writeLines(lines,con=filename)

pdf("./output/LOVOntologyReuses.pdf")
print(g)
dev.off()
