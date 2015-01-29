require(ggplot2)
require(reshape)


betaplots <- function(mean=0.25, sample.size=2^seq(3, 12)){
    df <- data.frame(x = seq(0, 1, length=500))
    cn <- c('x')
    for (N in sample.size){
        a <- mean * N
        b <- N - a
        df <- cbind(df, dbeta(df$x, a, b) )    
        colnames(df)[ncol(df)] <- paste('N', N, sep='')
    }

    plotdf <- melt(df, id='x', variable_name='sample.size')
    colnames(plotdf)[3] <- 'pdf'
    ggplot(plotdf, aes(x, pdf)) + geom_line() + facet_grid(sample.size ~ ., scales='free')    
}



input.data <- list(A=list(impressions=5000, clicks=80),
                   B=list(impressions=2500, clicks=50),
                   C=list(impressions=1000, clicks=35))

abtest.posteriorplot <- function(inp){
    df <- data.frame(x = seq(0, 0.1, length=500))
    for (sample in names(inp)){
        a <- inp[[sample]]$clicks
        b <- inp[[sample]]$impressions - a
        df[[sample]] <- dbeta(df$x, a, b)        
    }
    plotdf <- melt(df, id='x', variable_name='sample')
    colnames(plotdf)[3] <- 'pdf'
    ggplot(plotdf, aes(x, pdf)) + geom_line(aes(colour = sample))
}


