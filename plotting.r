library(ggplot2)
library(plyr)
library(reshape)
library(car)

plot_network_graphs <- function(filename, metric, group, ntwk_metrics, clinical_metrics) {
    title = paste('Network Metrics vs.', metric, 'by', group)

    clinical_metrics <- clinical_metrics[-match(metric,clinical_metrics)]
    clinical_metrics <- clinical_metrics[-match(group,clinical_metrics)]

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

    # Removes specific fields from the dataframe
    complete_data <- complete_data[,-match(clinical_metrics, names(complete_data))]

    complete_noNA <- na.omit(complete_data)
    m <- melt(complete_noNA, id = c('Subject', metric, group), na.rm=TRUE)
    m$titles <- ordered(m$variable,
            levels = ntwk_metrics,
            labels = ntwk_metrics)
    dev.new()
    g <- ggplot(m, aes_string(x = metric, y = "value", colour = group))
    print(g + geom_point() +
        facet_wrap( ~ titles, ncol = 3, scales = 'free_y') +
        ylab("") + xlab(metric) +
        stat_smooth(method = "lm", size=2) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
    
#    ggsave("nodal.png", width=11, height=8.5)
}

plot_extra_graphs <- function(filename, metric, group, group_metrics, ntwk_metrics, clinical_metrics) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = paste('Psychological Metrics vs.', metric)
    title = paste(title, 'by')
    title = paste(title, group)
    clinical_metrics <- clinical_metrics[-match(metric,clinical_metrics)]
    clinical_metrics <- clinical_metrics[-match(group,clinical_metrics)]
    unneccessary_group_metrics <- group_metrics[-match(group,group_metrics)]
    clinical_metrics <- clinical_metrics[-match(unneccessary_group_metrics,clinical_metrics)]

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

    # Removes specific fields from the dataframe
    complete_data <- complete_data[,-match(ntwk_metrics, names(complete_data))]
    complete_data <- complete_data[,-match(unneccessary_group_metrics, names(complete_data))]

    m <- melt(complete_data, id = c('Subject', metric, group), na.rm=TRUE)
    m$titles <- ordered(m$variable,
            levels = clinical_metrics,
            labels = clinical_metrics)

    dev.new()
    g <- ggplot(m, aes_string(x = metric, y = "value", fill=group))
    print(g + geom_point() +
        facet_wrap( ~ titles, ncol = 3, scales = 'free_y') +
        ylab("") + xlab(metric) +
        stat_smooth(method = "lm") +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
    
    ggsave("extra.png", width=11, height=8.5)
}

plot_nodal_bargraph <- function(filename, node_id, metric, group, group_metrics, ntwk_metrics, clinical_metrics) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = paste(node_id, metric, 'by', group)
    ntwk_metrics <- ntwk_metrics[-match(metric,ntwk_metrics)]
    clinical_metrics <- clinical_metrics[-match(group,clinical_metrics)]
    unneccessary_group_metrics <- group_metrics[-match(group,group_metrics)]
    clinical_metrics <- clinical_metrics[-match(unneccessary_group_metrics,clinical_metrics)]
    data = read.csv(file=filename, header=TRUE)
    data.nodal <- subset(data, Node==node_id)   
    complete_data <- data.nodal
    complete_data <- complete_data[,-match(clinical_metrics, names(complete_data))]
    dev.new()
    g <- ggplot(complete_data, aes_string(x = "Subject", y = metric, fill = group))
    print(g + geom_bar() +
        coord_flip() + 
        labs(x='Subject', y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
    
    ggsave("bargraph.png")
}

plot_nodal_bargraph_bygroup <- function(filename, node_id, metric, group1, group2, group_metrics, ntwk_metrics, clinical_metrics) {
    title = paste(node_id, metric, 'by', group)
    ntwk_metrics <- ntwk_metrics[-match(metric,ntwk_metrics)]
    clinical_metrics <- clinical_metrics[-match(c(group1, group2),clinical_metrics)]
    unneccessary_group_metrics <- group_metrics[-match(c(group1, group2),group_metrics)]
    clinical_metrics <- clinical_metrics[-match(unneccessary_group_metrics,clinical_metrics)]

    data = read.csv(file=filename, header=TRUE)    
    node_data <- subset(data, Node==node_id)

    complete_data <- node_data
    complete_data <- complete_data[,-match(clinical_metrics,names(complete_data))]
    complete_data <- complete_data[,-match("Node",names(complete_data))]
    rownames(complete_data)=complete_data[,match("Subject",names(complete_data))] # Set subject name as row names    
    attach(complete_data)

    grp1_eq <- paste(group1, "==")
    g1d1 = levels(get(group1))[1]
    g1d2 = levels(get(group1))[2]
    grp1_eq1 <- paste(grp1_eq, "'", g1d1, "'", sep="")
    grp1_eq2 <- paste(grp1_eq, "'", g1d2, "'", sep="")

    grp1_data1 = subset(complete_data, eval(parse(text = grp1_eq1)))
    grp1_data2 = subset(complete_data, eval(parse(text = grp1_eq2)))

    grp2_eq <- paste(group2, "==")
    g2d1 = levels(get(group2))[1]
    g2d2 = levels(get(group2))[2]
    grp2_eq1 <- paste(grp2_eq, "'", g2d1, "'", sep="")
    grp2_eq2 <- paste(grp2_eq, "'", g2d2, "'", sep="")

    grp1_data1_grp2_data1 = subset(grp1_data1, eval(parse(text = grp2_eq1)))
    grp1_data1_grp2_data2 = subset(grp1_data1, eval(parse(text = grp2_eq2)))

    grp1_data2_grp2_data1 = subset(grp1_data2, eval(parse(text = grp2_eq1)))
    grp1_data2_grp2_data2 = subset(grp1_data2, eval(parse(text = grp2_eq2)))

    split_by = c(group1, group2) 
    datasumm <- ddply(complete_data, split_by, function(d) data.frame(mean_metric=mean(d[,metric]), sd_metric=sd(d[,metric])))
    attach(datasumm)

    limits <- aes(ymax = mean_metric + sd_metric, ymin = mean_metric - sd_metric) 
    
    g1d1_test_results <- t.test(grp1_data1_grp2_data1[, metric], grp1_data1_grp2_data2[, metric], alternative="two.sided")
    g1d2_test_results <- t.test(grp1_data2_grp2_data1[, metric], grp1_data2_grp2_data2[, metric], alternative="two.sided")

    if (g1d1_test_results$p.value < 0.001) {
        g1d1_stat_result = '***'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d1_test_results$p.value < 0.01) {
        g1d1_stat_result = '**'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d1_test_results$p.value < 0.05) {    
        g1d1_stat_result = '*'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d1_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d1, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d1_test_results)

    if (g1d2_test_results$p.value < 0.001) {
        g1d2_stat_result = '***'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d2_test_results$p.value < 0.01) {
        g1d2_stat_result = '**'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d2_test_results$p.value < 0.05) {    
        g1d2_stat_result = '*'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d2_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d2, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d2_test_results)
    
    height = max(mean_metric)/5
    label_hover = height/5
     
    # Create significance segments and labels
    # First label
    label_y = max(mean_metric[1:2]) + height
    y2_point = max(mean_metric[1:2]) + height
    x_mid1 = 0.75
    x_mid2 = 1.25
    coords_g1d1 <- data.frame(x1 = x_mid1, y1 = mean_metric[1], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[2])
    label_x_g1d1 = (x_mid1 + x_mid2)/2
    label_y_g1d1 = label_y + label_hover
    
    # Second label
    label_y = max(mean_metric[3:4]) + height
    y2_point = max(mean_metric[3:4]) + height
    x_mid1 = x_mid1 + 1
    x_mid2 = x_mid2 + 1
    coords_g1d2 <- data.frame(x1 = x_mid1, y1 = mean_metric[3], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[4])
    label_x_g1d2 = (x_mid1 + x_mid2)/2
    label_y_g1d2 = label_y + label_hover

    dev.new()
    g <- ggplot(datasumm, aes_string(x = group1, y = "mean_metric", fill = group2))      
    print(g + geom_bar(position='dodge', stat = "identity") + 
        # Coords 1
        geom_segment(data=coords_g1d1, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x1, y=y2, xend=x3, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x3, y=y3, xend=x4, yend=y4)) +
        # Coords 2
        geom_segment(data=coords_g1d2, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d2, aes(x=x1, y=y2, xend=x3, yend=y2)) +
        geom_segment(data=coords_g1d2, aes(x=x3, y=y3, xend=x4, yend=y4)) +
        # Error bars
        geom_errorbar(limits, width=0.4, position=position_dodge(width=0.85)) + 
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d2, ylab = label_y_g1d2, stat_text = g1d2_stat_result)) +
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d1, ylab = label_y_g1d1, stat_text = g1d1_stat_result)) +
        labs(x=group1, y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        geom_text(size=3.5, aes(x, y, label = desc_text), 
            data.frame(x = 1.5, y = (label_y_g1d2 + label_y_g1d2/5), desc_text = 'Error bars show one standard deviation')) +
        #geom_text(aes(x, y, label = desc_text), 
        #    data.frame(x = 1.4, y = label_y_M + label_y_M/5, desc_text = 'n.s. : p > 0.05   * : p < 0.05   ** : p < 0.01   *** : p < 0.05')) +
        opts(title = title))
            
    ggsave(paste(title,'.png', sep=""))
    cat(paste(title,'.png\n', sep=""))
}

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

plot_average_bargraph_bygroup <- function(filename, metric, group1, group2, group_metrics, ntwk_metrics, clinical_metrics, node_id) {
    title = paste('Average', metric, 'by', group)

    clinical_metrics <- clinical_metrics[-match(metric,clinical_metrics)]
    clinical_metrics <- clinical_metrics[-match(group,clinical_metrics)]

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    node_data = df[2:(2+length(ntwk_metrics))] # Separate the nodal metric data
    mean_node_metrics = aggregate(. ~ Subject, data = node_data, mean) # Compute the mean for each metric for each subject
    mean_node_metrics[, match("Isolates", names(mean_node_metrics))] = length(levels(node_data$Node))*mean_node_metrics[, match("Isolates", names(mean_node_metrics))]
    rownames(mean_node_metrics)=mean_node_metrics[,1] # Set subject name as row names

    # Choose a single node so we can pull the subject data
    datasubset <- subset(df, Node==node_id)
    subject_data = datasubset[,(2+length(ntwk_metrics)):length(datasubset)] # Only non-nodal measures
    rownames(subject_data) <- subject_data$Subject # Set subject name as row names

    # Merge the subject-specific data with the subjects' mean nodal metrics
    complete_data = merge(mean_node_metrics, subject_data, by = "Subject")
	attach(complete_data)
	
    grp1_eq <- paste(group1, "==")
    g1d1 = levels(get(group1))[1]
    g1d2 = levels(get(group1))[2]
    grp1_eq1 <- paste(grp1_eq, "'", g1d1, "'", sep="")
    grp1_eq2 <- paste(grp1_eq, "'", g1d2, "'", sep="")

    grp1_data1 = subset(complete_data, eval(parse(text = grp1_eq1)))
    grp1_data2 = subset(complete_data, eval(parse(text = grp1_eq2)))

    grp2_eq <- paste(group2, "==")
    g2d1 = levels(get(group2))[1]
    g2d2 = levels(get(group2))[2]
    grp2_eq1 <- paste(grp2_eq, "'", g2d1, "'", sep="")
    grp2_eq2 <- paste(grp2_eq, "'", g2d2, "'", sep="")

    grp1_data1_grp2_data1 = subset(grp1_data1, eval(parse(text = grp2_eq1)))
    grp1_data1_grp2_data2 = subset(grp1_data1, eval(parse(text = grp2_eq2)))

    grp1_data2_grp2_data1 = subset(grp1_data2, eval(parse(text = grp2_eq1)))
    grp1_data2_grp2_data2 = subset(grp1_data2, eval(parse(text = grp2_eq2)))

    split_by = c(group1, group2) 
    datasumm <- ddply(complete_data, split_by, function(d) data.frame(mean_metric=mean(d[,metric]), sd_metric=sd(d[,metric])))
    attach(datasumm)

    limits <- aes(ymax = mean_metric + sd_metric, ymin = mean_metric - sd_metric) 
    
    g1d1_test_results <- t.test(grp1_data1_grp2_data1[, metric], grp1_data1_grp2_data2[, metric], alternative="two.sided")
    g1d2_test_results <- t.test(grp1_data2_grp2_data1[, metric], grp1_data2_grp2_data2[, metric], alternative="two.sided")

    if (g1d1_test_results$p.value < 0.001) {
        g1d1_stat_result = '***'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d1_test_results$p.value < 0.01) {
        g1d1_stat_result = '**'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d1_test_results$p.value < 0.05) {    
        g1d1_stat_result = '*'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d1_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d1, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d1_test_results)

    if (g1d2_test_results$p.value < 0.001) {
        g1d2_stat_result = '***'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d2_test_results$p.value < 0.01) {
        g1d2_stat_result = '**'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d2_test_results$p.value < 0.05) {    
        g1d2_stat_result = '*'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d2_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d2, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d2_test_results)
    
    height = max(mean_metric)/5
    label_hover = height/5
     
    # Create significance segments and labels
    # First label
    label_y = max(mean_metric[1:2]) + height
    y2_point = max(mean_metric[1:2]) + height
    x_mid1 = 0.75
    x_mid2 = 1.25
    coords_g1d1 <- data.frame(x1 = x_mid1, y1 = mean_metric[1], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[2])
    label_x_g1d1 = (x_mid1 + x_mid2)/2
    label_y_g1d1 = label_y + label_hover
    
    # Second label
    label_y = max(mean_metric[3:4]) + height
    y2_point = max(mean_metric[3:4]) + height
    x_mid1 = x_mid1 + 1
    x_mid2 = x_mid2 + 1
    coords_g1d2 <- data.frame(x1 = x_mid1, y1 = mean_metric[3], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[4])
    label_x_g1d2 = (x_mid1 + x_mid2)/2
    label_y_g1d2 = label_y + label_hover

    dev.new()
    g <- ggplot(datasumm, aes_string(x = group1, y = "mean_metric", fill = group2))      
    print(g + geom_bar(position='dodge', stat = "identity") + 
        # Coords 1
        geom_segment(data=coords_g1d1, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x1, y=y2, xend=x3, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x3, y=y3, xend=x4, yend=y4)) +
        # Coords 2
        geom_segment(data=coords_g1d2, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d2, aes(x=x1, y=y2, xend=x3, yend=y2)) +
        geom_segment(data=coords_g1d2, aes(x=x3, y=y3, xend=x4, yend=y4)) +
        # Error bars
        geom_errorbar(limits, width=0.4, position=position_dodge(width=0.85)) + 
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d2, ylab = label_y_g1d2, stat_text = g1d2_stat_result)) +
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d1, ylab = label_y_g1d1, stat_text = g1d1_stat_result)) +
        labs(x=group1, y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        geom_text(size=3.5, aes(x, y, label = desc_text), 
            data.frame(x = 1.5, y = (label_y_g1d2 + label_y_g1d2/5), desc_text = 'Error bars show one standard deviation')) +
        #geom_text(aes(x, y, label = desc_text), 
        #    data.frame(x = 1.4, y = label_y_M + label_y_M/5, desc_text = 'n.s. : p > 0.05   * : p < 0.05   ** : p < 0.01   *** : p < 0.05')) +
        opts(title = title))
            
    ggsave(paste(title,'.pdf', sep=""))
    cat(paste(title,'.pdf\n', sep=""))
}

plot_fiber_bargraph <- function(filename, metric, group, group_metrics) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = paste(metric, 'by', group)
    unneccessary_group_metrics <- group_metrics[-match(group,group_metrics)]
    data = read.csv(file=filename, header=TRUE)
    complete_data <- data
    dev.new()
    g <- ggplot(complete_data, aes_string(x = "Subject", y = metric, fill = group))
    print(g + geom_bar() +
        coord_flip() + 
        labs(x='Subject', y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
    
    ggsave(paste(title,'.pdf', sep=""))
}

plot_fiber_bargraph_bygroup <- function(filename, metric, group1, group2, group_metrics, clinical_metrics) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = paste(metric, 'by', group1, 'bars')

    clinical_metrics <- clinical_metrics[-match(c(group1, group2),clinical_metrics)]
    unneccessary_group_metrics <- group_metrics[-match(c(group1, group2),group_metrics)]
    clinical_metrics <- clinical_metrics[-match(unneccessary_group_metrics,clinical_metrics)]
	
    data = read.csv(file=filename, header=TRUE)    
    complete_data <- data
    complete_data <- complete_data[,-match(clinical_metrics,names(complete_data))]
    rownames(complete_data)=complete_data[,match("Subject",names(complete_data))] # Set subject name as row names    
    attach(complete_data)

    
    grp1_eq <- paste(group1, "==")
    g1d1 = levels(get(group1))[1]
    g1d2 = levels(get(group1))[2]
    grp1_eq1 <- paste(grp1_eq, "'", g1d1, "'", sep="")
    grp1_eq2 <- paste(grp1_eq, "'", g1d2, "'", sep="")

    grp1_data1 = subset(complete_data, eval(parse(text = grp1_eq1)))
    grp1_data2 = subset(complete_data, eval(parse(text = grp1_eq2)))

    grp2_eq <- paste(group2, "==")
    g2d1 = levels(get(group2))[1]
    g2d2 = levels(get(group2))[2]
    grp2_eq1 <- paste(grp2_eq, "'", g2d1, "'", sep="")
    grp2_eq2 <- paste(grp2_eq, "'", g2d2, "'", sep="")

    grp1_data1_grp2_data1 = subset(grp1_data1, eval(parse(text = grp2_eq1)))
    grp1_data1_grp2_data2 = subset(grp1_data1, eval(parse(text = grp2_eq2)))

    grp1_data2_grp2_data1 = subset(grp1_data2, eval(parse(text = grp2_eq1)))
    grp1_data2_grp2_data2 = subset(grp1_data2, eval(parse(text = grp2_eq2)))

    split_by = c(group1, group2) 
    datasumm <- ddply(complete_data, split_by, function(d) data.frame(mean_metric=mean(d[,metric]), sd_metric=sd(d[,metric])))
    attach(datasumm)

    limits <- aes(ymax = mean_metric + sd_metric, ymin = mean_metric - sd_metric) 
    
    g1d1_test_results <- t.test(grp1_data1_grp2_data1[, metric], grp1_data1_grp2_data2[, metric], alternative="two.sided")
    g1d2_test_results <- t.test(grp1_data2_grp2_data1[, metric], grp1_data2_grp2_data2[, metric], alternative="two.sided")

    if (g1d1_test_results$p.value < 0.001) {
        g1d1_stat_result = '***'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d1_test_results$p.value < 0.01) {
        g1d1_stat_result = '**'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d1_test_results$p.value < 0.05) {    
        g1d1_stat_result = '*'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d1_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d1, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d1_test_results)

    if (g1d2_test_results$p.value < 0.001) {
        g1d2_stat_result = '***'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d2_test_results$p.value < 0.01) {
        g1d2_stat_result = '**'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d2_test_results$p.value < 0.05) {    
        g1d2_stat_result = '*'
        cat(paste('The difference in means for', g1d2, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d2_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d2, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d2_test_results)
    
    height = max(mean_metric)/5
    label_hover = height/5
     
    # Create significance segments and labels
    # First label
    label_y = max(mean_metric[1:2]) + height
    y2_point = max(mean_metric[1:2]) + height
    x_mid1 = 0.75
    x_mid2 = 1.25
    coords_g1d1 <- data.frame(x1 = x_mid1, y1 = mean_metric[1], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[2])
    label_x_g1d1 = (x_mid1 + x_mid2)/2
    label_y_g1d1 = label_y + label_hover
    
    # Second label
    label_y = max(mean_metric[3:4]) + height
    y2_point = max(mean_metric[3:4]) + height
    x_mid1 = x_mid1 + 1
    x_mid2 = x_mid2 + 1
    coords_g1d2 <- data.frame(x1 = x_mid1, y1 = mean_metric[3], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[4])
    label_x_g1d2 = (x_mid1 + x_mid2)/2
    label_y_g1d2 = label_y + label_hover

    dev.new()
    g <- ggplot(datasumm, aes_string(x = group1, y = "mean_metric", fill = group2))      
    print(g + geom_bar(position='dodge', stat = "identity") + 
        # Coords 1
        geom_segment(data=coords_g1d1, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x1, y=y2, xend=x3, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x3, y=y3, xend=x4, yend=y4)) +
        # Coords 2
        geom_segment(data=coords_g1d2, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d2, aes(x=x1, y=y2, xend=x3, yend=y2)) +
        geom_segment(data=coords_g1d2, aes(x=x3, y=y3, xend=x4, yend=y4)) +
        # Error bars
        geom_errorbar(limits, width=0.4, position=position_dodge(width=0.85)) + 
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d2, ylab = label_y_g1d2, stat_text = g1d2_stat_result)) +
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d1, ylab = label_y_g1d1, stat_text = g1d1_stat_result)) +
        labs(x=group1, y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        geom_text(size=3.5, aes(x, y, label = desc_text), 
            data.frame(x = 1.5, y = (label_y_g1d2 + label_y_g1d2/5), desc_text = 'Error bars show one standard deviation')) +
        #geom_text(aes(x, y, label = desc_text), 
        #    data.frame(x = 1.4, y = label_y_M + label_y_M/5, desc_text = 'n.s. : p > 0.05   * : p < 0.05   ** : p < 0.01   *** : p < 0.05')) +
        opts(title = title))
            
    ggsave(paste(title,'.pdf', sep=""))
    cat(paste(title,'.pdf\n', sep=""))
}

plot_average_bargraph <- function(filename, metric, group, group_metrics, ntwk_metrics, clinical_metrics, node_id) {
    title = paste('Average', metric, 'by', group)

    clinical_metrics <- clinical_metrics[-match(metric,clinical_metrics)]
    clinical_metrics <- clinical_metrics[-match(group,clinical_metrics)]

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    node_data = df[2:(2+length(ntwk_metrics))] # Separate the nodal metric data
    mean_node_metrics = aggregate(. ~ Subject, data = node_data, mean) # Compute the mean for each metric for each subject
    mean_node_metrics[, match("Isolates", names(mean_node_metrics))] = length(levels(node_data$Node))*mean_node_metrics[, match("Isolates", names(mean_node_metrics))]
    rownames(mean_node_metrics)=mean_node_metrics[,1] # Set subject name as row names

    # Choose a single node so we can pull the subject data
    datasubset <- subset(df, Node==node_id)
    subject_data = datasubset[,(2+length(ntwk_metrics)):length(datasubset)] # Only non-nodal measures
    rownames(subject_data) <- subject_data$Subject # Set subject name as row names

    # Merge the subject-specific data with the subjects' mean nodal metrics
    complete_data = merge(mean_node_metrics, subject_data, by = "Subject")
	attach(complete_data)
	
    grp1_eq <- paste(group, "==")
    g1d1 = levels(get(group))[1]
    g1d2 = levels(get(group))[2]
    grp1_eq1 <- paste(grp1_eq, "'", g1d1, "'", sep="")
    grp1_eq2 <- paste(grp1_eq, "'", g1d2, "'", sep="")

    grp1_data1 = subset(complete_data, eval(parse(text = grp1_eq1)))
    grp1_data2 = subset(complete_data, eval(parse(text = grp1_eq2)))

    split_by = group
    datasumm <- ddply(complete_data, split_by, function(d) data.frame(mean_metric=mean(d[,metric]), sd_metric=sd(d[,metric])))
    attach(datasumm)

    limits <- aes(ymax = mean_metric + sd_metric, ymin = mean_metric - sd_metric) 
    
    g1d1_test_results <- t.test(grp1_data1[, metric], grp1_data2[, metric], alternative="two.sided")

    if (g1d1_test_results$p.value < 0.001) {
        g1d1_stat_result = '***'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d1_test_results$p.value < 0.01) {
        g1d1_stat_result = '**'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d1_test_results$p.value < 0.05) {    
        g1d1_stat_result = '*'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d1_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d1, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d1_test_results)
    
    height = max(mean_metric)/5
    label_hover = height/5
     
    # Create significance segments and labels
    # First label
    label_y = max(mean_metric[1:2]) + height
    y2_point = max(mean_metric[1:2]) + height
    x_mid1 = 1
    x_mid2 = 2
    coords_g1d1 <- data.frame(x1 = x_mid1, y1 = mean_metric[1], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[2])
    label_x_g1d1 = (x_mid1 + x_mid2)/2
    label_y_g1d1 = label_y + label_hover

    dev.new()
    g <- ggplot(datasumm, aes_string(x = group, y = "mean_metric", fill = group))      
    print(g + geom_bar(stat = "identity") + 
        # Coords 1
#        geom_segment(data=coords_g1d1, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x1, y=y2, xend=x3, yend=y2)) +
#        geom_segment(data=coords_g1d1, aes(x=x3, y=y3, xend=x4, yend=y4)) +
        # Error bars
        geom_errorbar(limits, width=0.4, position=position_dodge(width=0.85)) + 
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d1, ylab = label_y_g1d1, stat_text = g1d1_stat_result)) +
        labs(x=group, y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
            
    ggsave(paste(title,'.pdf', sep=""))
    cat(paste(title,'.pdf\n', sep=""))
}

plot_total_edges_bargraph_bysubject <- function(filename) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = 'Total edges by Subject'

    df = read.csv(filename, header=T) # Read in the comma-separated value file
	df <- df[match(c("Node", "Subject","Degree"), names(df))]
    degree_sum_per_subj = aggregate(. ~ Subject, data = df, sum)
  	degree_sum_per_subj <- degree_sum_per_subj[-match(c("Node"), names(degree_sum_per_subj))]
    rownames(degree_sum_per_subj)=degree_sum_per_subj[,1] # Set subject name as row names    
    degree_sum_per_subj$edges <- degree_sum_per_subj$Degree / 2 #The total number of edges in a graph is half of the sum of the nodal degrees
    total_edge_data = degree_sum_per_subj
    
    dev.new()
    g <- ggplot(total_edge_data, aes_string(x = "Subject", y = "edges", fill = "edges"))
    print(g + geom_bar() +
        coord_flip() + 
        labs(x='Subject', y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
    
    ggsave(paste(title,'.pdf', sep=""))
}

plot_total_edges_histogram_bygroup <- function(filename, group) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = paste('Histogram of Number of Edges vs.', group)

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    groups = df[match(c("Subject", group), names(df))]

	df <- df[match(c("Node", "Subject","Degree"), names(df))]
    degree_sum_per_subj = aggregate(. ~ Subject, data = df, sum)
  	degree_sum_per_subj <- degree_sum_per_subj[-match(c("Node"), names(degree_sum_per_subj))]
    rownames(degree_sum_per_subj)=degree_sum_per_subj[,1] # Set subject name as row names    
    degree_sum_per_subj$edges <- degree_sum_per_subj$Degree / 2 #The total number of edges in a graph is half of the sum of the nodal degrees
    total_edge_data = degree_sum_per_subj

    groups = groups[!duplicated(groups),]
    rownames(groups) = groups[!duplicated(groups),][,1]
    complete_data = merge(groups, total_edge_data, by = "Subject")    

    split_by = group
    metric = "edges"
    
    complete_data$dummy <- complete_data[, group]
    complete_data <- complete_data[-match(c("Degree"), names(complete_data))]
    
    dev.new()
    g <- ggplot(complete_data, aes_string(x = "edges"))
    print(g +
        geom_histogram(aes(y=..count..),      # Histogram with density instead of count on y-axis
                   binwidth=2500,
                   colour="black", fill="white") +
  		#geom_line(aes(y = ..density..), stat="density",
        #    size = 1, colour="red", linetype=1) + #, alpha = 0.2) + 
        stat_density(aes(y = ..scaled..), alpha=.4, fill="black") + 
        #geom_density(aes(y = ..density..), alpha=.2, fill="black") +
    	facet_wrap(~ dummy, nrow = 2) + #, scales = 'free_y', nrows=2) +
        theme_bw() + 
        labs(x='Number of Edges', y='Number of Subjects') +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
    
    ggsave(paste(title,'.pdf', sep=""))
}

plot_average_edges_bargraph_bygroup <- function(filename, group) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = paste('Total edges by', group)

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    groups = df[match(c("Subject", group), names(df))]

	df <- df[match(c("Node", "Subject","Degree"), names(df))]
    degree_sum_per_subj = aggregate(. ~ Subject, data = df, sum)
  	degree_sum_per_subj <- degree_sum_per_subj[-match(c("Node"), names(degree_sum_per_subj))]
    rownames(degree_sum_per_subj)=degree_sum_per_subj[,1] # Set subject name as row names    
    degree_sum_per_subj$edges <- degree_sum_per_subj$Degree / 2 #The total number of edges in a graph is half of the sum of the nodal degrees
    total_edge_data = degree_sum_per_subj
    
    complete_data = merge(groups, total_edge_data, by = "Subject")
    attach(complete_data)    
    metric = 'edges'
	
    grp1_eq <- paste(group, "==")
    g1d1 = levels(get(group))[1]
    g1d2 = levels(get(group))[2]
    grp1_eq1 <- paste(grp1_eq, "'", g1d1, "'", sep="")
    grp1_eq2 <- paste(grp1_eq, "'", g1d2, "'", sep="")

    grp1_data1 = subset(complete_data, eval(parse(text = grp1_eq1)))
    grp1_data2 = subset(complete_data, eval(parse(text = grp1_eq2)))

    split_by = group
    datasumm <- ddply(complete_data, split_by, function(d) data.frame(mean_metric=mean(d[,metric]), sd_metric=sd(d[,metric])))
    attach(datasumm)

    limits <- aes(ymax = mean_metric + sd_metric, ymin = mean_metric - sd_metric) 
    
    g1d1_test_results <- t.test(grp1_data1[, metric], grp1_data2[, metric], alternative="two.sided")

    if (g1d1_test_results$p.value < 0.001) {
        g1d1_stat_result = '***'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d1_test_results$p.value < 0.01) {
        g1d1_stat_result = '**'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d1_test_results$p.value < 0.05) {    
        g1d1_stat_result = '*'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d1_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d1, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d1_test_results)
    
    height = max(mean_metric)/5
    label_hover = height/5
     
    # Create significance segments and labels
    # First label
    label_y = max(mean_metric[1:2]) + height
    y2_point = max(mean_metric[1:2]) + height
    x_mid1 = 1
    x_mid2 = 2
    coords_g1d1 <- data.frame(x1 = x_mid1, y1 = mean_metric[1], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[2])
    label_x_g1d1 = (x_mid1 + x_mid2)/2
    label_y_g1d1 = label_y + label_hover

    dev.new()
    g <- ggplot(datasumm, aes_string(x = group, y = "mean_metric", fill = group))      
    print(g + geom_bar(stat = "identity") + 
        # Coords 1
#        geom_segment(data=coords_g1d1, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x1, y=y2, xend=x3, yend=y2)) +
#        geom_segment(data=coords_g1d1, aes(x=x3, y=y3, xend=x4, yend=y4)) +
        # Error bars
        geom_errorbar(limits, width=0.4, position=position_dodge(width=0.85)) + 
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d1, ylab = label_y_g1d1, stat_text = g1d1_stat_result)) +
        labs(x=group, y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
            
    ggsave(paste(title,'.pdf', sep=""))
    cat(paste(title,'.pdf\n', sep=""))
}

plot_metric_histogram_bygroup <- function(filename, metric, group) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = paste('Histogram of', metric, 'vs.', group)

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    groups = df[match(c("Subject", group), names(df))]
    node_data = df[match(c(metric, "Subject"), names(df))]
    mean_node_metrics = aggregate(. ~ Subject, data = node_data, mean) 
    rownames(mean_node_metrics)=mean_node_metrics[,1] # Set subject name as row names

    # Choose a single node so we can pull the subject data
    datasubset <- subset(df, Node=='Brain-Stem')
    subject_data = df[match(c(group, "Subject"), names(df))]

    groups = df[match(c("Subject", group), names(df))]
    groups = groups[!duplicated(groups),]
    rownames(groups) = groups[!duplicated(groups),][,1]
    complete_data = merge(groups, mean_node_metrics, by = "Subject") 
        
    # Merge the subject-specific data with the subjects' mean nodal metrics
    complete_data = merge(groups, mean_node_metrics, by = "Subject")
    
    split_by = group
    
    complete_data$dummy <- complete_data[, group]
    
    dev.new()
    g <- ggplot(complete_data, aes_string(x = metric))
    print(g +
        geom_histogram(aes(y=..count..),      # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
  		#geom_line(aes(y = ..density..), stat="density",
        #    size = 1, colour="red", linetype=1) + #, alpha = 0.2) + 
        stat_density(aes(y = ..scaled..), alpha=.4, fill="black") + 
        #geom_density(aes(y = ..density..), alpha=.2, fill="black") +
    	facet_wrap(~ dummy, nrow = 2) + #, scales = 'free_y', nrows=2) +
        theme_bw() + 
        labs(x=metric, y='Number of Subjects') +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
    
    ggsave(paste(title,'.pdf', sep=""))
}

plot_average_metric_bargraph_bygroup <- function(filename, metric, group) {
    library(ggplot2)
    library(reshape)
    library(car)
    title = paste('Average',metric, 'by', group)

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    groups = df[match(c("Subject", group), names(df))]
    node_data = df[match(c(metric, "Subject"), names(df))]
    mean_node_metrics = aggregate(. ~ Subject, data = node_data, mean) 
    rownames(mean_node_metrics)=mean_node_metrics[,1] # Set subject name as row names

    # Choose a single node so we can pull the subject data
    datasubset <- subset(df, Node=='Brain-Stem')
    subject_data = df[match(c(group, "Subject"), names(df))]

    groups = df[match(c("Subject", group), names(df))]
    groups = groups[!duplicated(groups),]
    rownames(groups) = groups[!duplicated(groups),][,1]
    complete_data = merge(groups, mean_node_metrics, by = "Subject") 
        
    # Merge the subject-specific data with the subjects' mean nodal metrics
    complete_data = merge(groups, mean_node_metrics, by = "Subject")
    
    split_by = group
    
    complete_data$dummy <- complete_data[, group]
    
    attach(complete_data)    
	
    grp1_eq <- paste(group, "==")
    g1d1 = levels(get(group))[1]
    g1d2 = levels(get(group))[2]
    grp1_eq1 <- paste(grp1_eq, "'", g1d1, "'", sep="")
    grp1_eq2 <- paste(grp1_eq, "'", g1d2, "'", sep="")

    grp1_data1 = subset(complete_data, eval(parse(text = grp1_eq1)))
    grp1_data2 = subset(complete_data, eval(parse(text = grp1_eq2)))

    split_by = group
    datasumm <- ddply(complete_data, split_by, function(d) data.frame(mean_metric=mean(d[,metric]), sd_metric=sd(d[,metric])))
    attach(datasumm)

    limits <- aes(ymax = mean_metric + sd_metric, ymin = mean_metric - sd_metric) 
    
    g1d1_test_results <- t.test(grp1_data1[, metric], grp1_data2[, metric], alternative="two.sided")

    if (g1d1_test_results$p.value < 0.001) {
        g1d1_stat_result = '***'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.001). Adding ***\n'))
    } else if (g1d1_test_results$p.value < 0.01) {
        g1d1_stat_result = '**'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.01). Adding **\n'))
    } else if (g1d1_test_results$p.value < 0.05) {    
        g1d1_stat_result = '*'
        cat(paste('The difference in means for', g1d1, 'is statistically significant (p < 0.05). Adding *\n'))
    } else {
        g1d1_stat_result = 'n.s.'
        cat(paste('The difference in means for', g1d1, 'is not significant (p > 0.05). Adding n.s.\n'))
    }
    print(g1d1_test_results)
    
    height = max(mean_metric)/5
    label_hover = height/5
     
    # Create significance segments and labels
    # First label
    label_y = max(mean_metric[1:2]) + height
    y2_point = max(mean_metric[1:2]) + height
    x_mid1 = 1
    x_mid2 = 2
    coords_g1d1 <- data.frame(x1 = x_mid1, y1 = mean_metric[1], x2 = x_mid1, y2 = y2_point,
        x3 = x_mid2, y3 = y2_point, x4 = x_mid2, y4 = mean_metric[2])
    label_x_g1d1 = (x_mid1 + x_mid2)/2
    label_y_g1d1 = label_y + label_hover

    dev.new()
    g <- ggplot(datasumm, aes_string(x = group, y = "mean_metric", fill = group))      
    print(g + geom_bar(stat = "identity") + 
        # Coords 1
#        geom_segment(data=coords_g1d1, aes(x=x1, y=y1, xend=x1, yend=y2)) +
        geom_segment(data=coords_g1d1, aes(x=x1, y=y2, xend=x3, yend=y2)) +
#        geom_segment(data=coords_g1d1, aes(x=x3, y=y3, xend=x4, yend=y4)) +
		theme_bw() +
        # Error bars
        geom_errorbar(limits, width=0.4, position=position_dodge(width=0.85)) + 
        geom_text(aes(xlab, ylab, label = stat_text), 
            data.frame(xlab = label_x_g1d1, ylab = label_y_g1d1, stat_text = g1d1_stat_result)) +
        labs(x=group, y=title) +
        opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        opts(title = title))
            
    ggsave(paste(title,'.pdf', sep=""))
}
