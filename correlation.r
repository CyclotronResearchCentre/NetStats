library(ggplot2)
library(plyr)
library(reshape)
library(car)

plot_network_metric_correlation <- function(filename, ntwk_metrics) {
    title = 'Correlation between Network Metrics'
    df = read.csv(filename, header=T) # Read in the comma-separated value file
    node_data = df[1:(1+length(ntwk_metrics))] # Separate the nodal metric data
    mean_per_node_metrics = aggregate(. ~ Node, data = node_data, mean) # Compute the mean for each metric for each subject
    mean_per_node_metrics[, match("Isolates", names(mean_per_node_metrics))] = length(levels(node_data$Node))*mean_per_node_metrics[, match("Isolates", names(mean_per_node_metrics))]
    rownames(mean_per_node_metrics)=mean_per_node_metrics[,1] # Set subject name as row names

    per_node_metrics_noNA <- na.omit(mean_per_node_metrics)
    per_node_metrics_noNA.cor <- cor(per_node_metrics_noNA[2:length(per_node_metrics_noNA)])

    metrics.m <- melt(per_node_metrics_noNA.cor)
    metrics.o <- metrics.m[order(metrics.m$value), ]
    metrics.o$X1 <- factor(metrics.o$X1, levels = unique(metrics.o$X1))
    metrics.o$X2 <- factor(metrics.o$X2, levels = unique(metrics.o$X2))
    breaks = c(-1, -0.5, 0, 0.5, 1)
    colours=c("red", "white", "steelblue")
    #pdf(file=paste("./",tractnums[a],subjectlist[idx],"cmatrix.pdf"))
    print(p <- ggplot(metrics.o) + geom_tile(aes(X1, X2, fill = value)) + scale_fill_gradientn(colours=colours, breaks=breaks, labels=format(breaks)) + labs(x = "Metric", y = "Metric", fill= "Correlation") + opts(title=title, axis.text.x=theme_text(angle=-90, hjust=0)))
}

plot_clinical_metric_correlation <- function(filename, ntwk_metrics, group_metrics, clinical_metrics, omit=TRUE) {
    title = 'Correlation between Psychological Metrics'
    df = read.csv(filename, header=T) # Read in the comma-separated value file
    node_data = df[2:(2+length(ntwk_metrics))] # Separate the nodal metric data
    mean_node_metrics = aggregate(. ~ Subject, data = node_data, mean) # Compute the mean for each metric for each subject
    mean_node_metrics[, match("Isolates", names(mean_node_metrics))] = length(levels(node_data$Node))*mean_node_metrics[, match("Isolates", names(mean_node_metrics))]
    rownames(mean_node_metrics)=mean_node_metrics[,1] # Set subject name as row names

    # Choose a single node so we can pull the subject data
    datasubset <- subset(df, Node=='Brain-Stem')
    subject_data = datasubset[,(2+length(ntwk_metrics)):length(datasubset)] # Only non-nodal measures
    rownames(subject_data) <- subject_data$Subject # Set subject name as row names
#    attach(subject_data)

    # Merge the subject-specific data with the subjects' mean nodal metrics
    complete_data = merge(mean_node_metrics, subject_data, by = "Subject")

    clinical_metrics <- clinical_metrics[-match(group_metrics,clinical_metrics)]
    clinical_independent = complete_data[match(clinical_metrics, names(complete_data))]
    clinical_independent = clinical_independent[sapply(clinical_independent, is.numeric)]
    if (omit == TRUE) {
        clinical_noNA = na.omit(clinical_independent)
        clinical.cor <- cor(clinical_noNA)
    } else {
        clinical.cor <- cor(clinical_independent)
    }
   
    metrics.m <- melt(clinical.cor)
    metrics.o <- metrics.m[order(metrics.m$value), ]
    metrics.o$X1 <- factor(metrics.o$X1, levels = unique(metrics.o$X1))
    metrics.o$X2 <- factor(metrics.o$X2, levels = unique(metrics.o$X2))
    #pdf(file=paste("./",tractnums[a],subjectlist[idx],"cmatrix.pdf"))
    breaks = c(-1, -0.5, 0, 0.5, 1)
    colours=c("red", "white", "steelblue")
    dev.new()
    print(p <- ggplot(metrics.o) + geom_tile(aes(X1, X2, fill = value)) + scale_fill_gradientn(colours=colours, breaks=breaks, labels=format(breaks)) + labs(x = "Metric", y = "Metric", fill= "Correlation") + opts(title=title, axis.text.x=theme_text(angle=-90, hjust=0)))
}

plot_all_metric_correlation <- function(filename, ntwk_metrics, group_metrics, clinical_metrics, omit=TRUE) {
    title = 'Correlation between Psychological and Network Metrics'
    df = read.csv(filename, header=T) # Read in the comma-separated value file
    node_data = df[2:(2+length(ntwk_metrics))] # Separate the nodal metric data
    mean_node_metrics = aggregate(. ~ Subject, data = node_data, mean) # Compute the mean for each metric for each subject

    mean_node_metrics[, match("Isolates", names(mean_node_metrics))] = length(levels(node_data$Node))*mean_node_metrics[, match("Isolates", names(mean_node_metrics))]
    rownames(mean_node_metrics)=mean_node_metrics[,1] # Set subject name as row names

    # Choose a single node so we can pull the subject data
    datasubset <- subset(df, Node=='Brain-Stem')
    subject_data = datasubset[,(2+length(ntwk_metrics)):length(datasubset)] # Only non-nodal measures
    rownames(subject_data) <- subject_data$Subject # Set subject name as row names

    # Merge the subject-specific data with the subjects' mean nodal metrics
    complete_data = merge(mean_node_metrics, subject_data, by = "Subject")
    complete_metrics = names(complete_data)

    complete_independent = complete_data[match(c(clinical_metrics, ntwk_metrics), names(complete_data))]
    complete_independent <- complete_independent[-match("Isolates", names(complete_independent))]
    complete_independent = complete_independent[sapply(complete_independent, is.numeric)]

    if (omit == TRUE) {
        complete_noNA = na.omit(complete_independent)
        complete.cor <- cor(complete_noNA)
    } else {
        complete.cor <- cor(complete_independent)
    }
   
    complete.m <- melt(complete.cor)
    complete.o <- complete.m[order(complete.m$value), ]
    complete.o$X1 <- factor(complete.o$X1, levels = unique(complete.o$X1))
    complete.o$X2 <- factor(complete.o$X2, levels = unique(complete.o$X2))

    dev.new()
    breaks = c(-1, -0.5, 0, 0.5, 1)
    colours=c("red", "white", "steelblue")
    print(p <- ggplot(complete.o) + geom_tile(aes(X1, X2, fill = value)) + scale_fill_gradientn(colours=colours, breaks=breaks, labels=format(breaks)) + labs(x = "Metric", y = "Metric", fill= "Correlation") + opts(title=title, axis.text.x=theme_text(angle=-90, hjust=0)))
}
