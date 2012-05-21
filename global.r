library(ggplot2)
library(plyr)
library(reshape)
library(car)

plot_global_metric_bargraph_bygroup <- function(filename, metric, group) {
    title = paste('Average',metric, 'by', group)

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    complete_data = df[match(c("Subject", group, metric), names(df))]
    
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

plot_global_metric_histogram_bygroup <- function(filename, metric, group) {
    title = paste('Histogram of', metric, 'vs.', group)

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    complete_data = df[match(c("Subject", group, metric), names(df))]
   
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
