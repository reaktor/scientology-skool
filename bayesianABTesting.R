require(ggplot2)
require(reshape)


betaplots <- function(mean=0.25, sample.size=2^seq(3, 12), prior=c(1, 1)) {
    prior.a <- prior[[1]]; prior.b <- prior[[2]]
    df <- data.frame(x = seq(0, 1, length=500))
    cn <- c('x')
    for (N in sample.size){
        a <- mean * N
        b <- N - a
        df <- cbind(df, dbeta(df$x, a + prior.a, b + prior.b) )    
        colnames(df)[ncol(df)] <- paste('N', N, sep='')
    }

    plotdf <- melt(df, id='x', variable_name='sample.size')
    colnames(plotdf)[3] <- 'pdf'
    ggplot(plotdf, aes(x, pdf)) + geom_line() + facet_grid(sample.size ~ ., scales='free')    
}



abtest.posteriorplot <- function(inp, prior = c(1, 1)) {
    prior.a <- prior[[1]]; prior.b <- prior[[2]]
    df <- data.frame(x = seq(0, 0.1, length=500))
    for (sample in names(inp)){
        a <- inp[[sample]]$clicks
        b <- inp[[sample]]$impressions - a
        df[[sample]] <- dbeta(df$x, a + prior.a, b + prior.b)
    }
    plotdf <- melt(df, id='x', variable_name='sample')
    colnames(plotdf)[3] <- 'pdf'
    ggplot(plotdf, aes(x, pdf)) + geom_line(aes(colour = sample))
}


simulate.data <- function(p, N=10){
    sample(c(0,1), N, replace=TRUE, prob=c(1-p, p))
}

get.counts <- function(x){
    list(impressions=length(x), clicks=sum(x))
}

visits.a.day <- function(mu){
    floor(rnorm(1, mu, mu*0.5))
}


cnts1 <- get.counts(simulate.data(0.03, N=visits.a.day(2000)))
cnts2 <- get.counts(simulate.data(0.025, N=visits.a.day(2000)))

input.data <- list(A=cnts1, B=cnts2)
abtest.posteriorplot(input.data)


betaplots()
betaplots(mean=0.5, sample.size=0)
abtest.posteriorplot(input.data)


input.data <- list(A=list(impressions=5000, clicks=80),
                   B=list(impressions=2500, clicks=50),
                   C=list(impressions=1000, clicks=35))
