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



abtest.posteriorplot <- function(df, prior = c(1, 1)) {
    xmax <- max(df$click.rate)
    xmin <- min(df$click.rate)
    xx <- reshape(df, idvar = "day", timevar="variant", direction = "wide")
    cumulative <- cumsum(xx[, -1])
    cumulative$day <- as.numeric(levels(xx$day))[xx$day]

    prior.a <- prior[[1]]; prior.b <- prior[[2]]

    all.days <- data.frame()
    for (row in row.names(cumulative)){
        df <- data.frame(x = seq(xmin*0.5, xmax*1.5, length=500))
        df$day <- cumulative[row, 'day']
        a <- cumulative[row, 'clicks.A'] + prior.a
        b <- cumulative[row, 'visits.A'] - cumulative[row, 'clicks.A'] + prior.b
        df$A <- dbeta(df$x, a, b)

        a <- cumulative[row, 'clicks.B'] + prior.a
        b <- cumulative[row, 'visits.B'] - cumulative[row, 'clicks.B'] + prior.b
        df$B <- dbeta(df$x, a, b)
        all.days <- rbind(all.days, df)
    }

    df <- melt(all.days, id.vars = c('x', 'day'), variable_name='variant')
    colnames(df)[4] <- 'pdf'
    gp <- ggplot(df, aes(x, pdf)) + geom_line(aes(colour = variant))
    gp <- gp + facet_wrap( ~day, scales='free')
    quartz()
    print(gp)
}

betapdf <- function(a, b){
    df <- data.frame(x = seq(0, 0.1, length=500))
    df$pdf <- dbeta(df$x, a + prior.a, b + prior.b)
}

simulate.data <- function(p, N=10){
    sample(c(0,1), N, replace=TRUE, prob=c(1-p, p))
}

get.counts <- function(x){
    list(visits=length(x), clicks=sum(x))
}

visits.a.day <- function(mu){
    max(0, floor(rnorm(1, mu, mu*0.25)))
}


click.data <- function(p.A, p.B, mean.visits=2000, days=2){
    visits = c()
    clicks = c()
    for (day in seq(1,days)){
        cnts <- get.counts(simulate.data(p.A, N=visits.a.day(mean.visits)))
        visits <- c(visits, cnts$visits)
        clicks <- c(clicks, cnts$clicks)
        cnts <- get.counts(simulate.data(p.B, N=visits.a.day(mean.visits)))        
        visits <- c(visits, cnts$visits)
        clicks <- c(clicks, cnts$clicks)        
    }
    df <- data.frame(day=as.character(rep(seq(1, days), each=2)),
               variant=rep(c('A', 'B'), days),
               visits,
               clicks)
    df$click.rate = df$clicks/df$visits
    df
}

example <- click.data(0.025, 0.035, mean.visits=2000, days=4)

zz <- melt(example, id.vars=c('day', 'variant'))
gp <- ggplot(zz, aes(day, value))
gp <- gp + geom_bar(stat='identity', aes(fill=variant), alpha=0.9, position='dodge')
gp <- gp + facet_grid(variable ~ ., scales = 'free_y')
print(gp)


xx <- reshape(example, idvar = "day", timevar="variant", direction = "wide")
example.cumulative <- cumsum(xx[, -1])
example.cumulative$day <- xx$day
example.cumulative$click.rate.A <- example.cumulative$click.rate.B <-  NULL
