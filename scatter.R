library(ggplot2)

plot_correlation_scatter <- function(filename, x, y, colour) {
    title = paste(y,' vs. ', x, 'by', colour)

    df = read.csv(filename, header=T) # Read in the comma-separated value file
    complete_data = df[match(c("SubjectID", x, y, colour), names(df))]
    attach(complete_data)    
    
    test_result <- cor.test(complete_data[x][,1],complete_data[y][,1])

    test_name = paste(y, " vs. ", x)
    stat_result = ""

    if (test_result$p.value < 0.001) {
        stat_result = paste('R=', round(test_result$estimate,3),'*** p=', round(test_result$p.value,4))
        cat(paste('Correlation coefficient for', test_name, 'is ',  round(test_result$estimate,3), 'and is statistically significant (p = ',round(test_result$p.value,4),'). Adding ***\n'))
    } else if (test_result$p.value < 0.01) {
        stat_result = paste('R=', round(test_result$estimate,3),'** p=', round(test_result$p.value,4))
        cat(paste('Correlation coefficient for', test_name, 'is ',  round(test_result$estimate,3), 'and is statistically significant (p = ',round(test_result$p.value,4),'). Adding **\n'))
    } else if (test_result$p.value < 0.05) {    
        stat_result = paste('R=', round(test_result$estimate,3),'* p=', round(test_result$p.value,4))
        cat(paste('Correlation coefficient for', test_name, 'is ',  round(test_result$estimate,3), 'and is statistically significant (p = ',round(test_result$p.value,4),'). Adding *\n'))
    } else {
        stat_result = paste('R=', round(test_result$estimate,3),'n.s. p=', round(test_result$p.value,4))
        cat(paste('Correlation coefficient for', test_name, 'is ',  round(test_result$estimate,3), 'and is not statistically significant (p = ',round(test_result$p.value,4),'). Adding n.s.\n'))
    }
    print(test_result)
    
    lab_x = min(complete_data[x]) + min(complete_data[x])/4
    lab_y = max(complete_data[y]) - max(complete_data[y])/10

    dev.new()
    g <- ggplot(complete_data, aes_string(x = x, y = y, colour = colour))      
    print(g + geom_point(size=3) + 
        geom_smooth(color="black", aes(group = 1), method="lm", formula = y ~ x, se=TRUE) +
        #geom_segment(data=coords_g1d1, aes(x=x1, y=y2, xend=x3, yend=y2)) +
        #theme_bw() +
        geom_text(colour="black", data=data.frame(x2=lab_x, y2=lab_y, stat_result=stat_result),
            aes(x2, y2, label=stat_result)) +
        labs(x=x, y=y) +
        #scale_fill_manual(values = c("grey","white")) +
        #opts(axis.title.x = theme_text(size = 15, vjust = 0.3, hjust = 0.5)) +
        labs(title = title))
    ggsave(paste(title,'.pdf', sep=""))
}