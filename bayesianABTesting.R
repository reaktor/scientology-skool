require(ggplot2)
require(reshape)


betaplots <- function(mean=0.25, sample.size=2^seq(3, 12), prior=c(1, 1)) {
    prior.a <- prior[[1]]; prior.b <- prior[[2]]
    df <- data.frame(x = seq(0, 1, length=5000))
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



abtest.posteriorplot <- function(df, prior = c(1, 1), plot.prior=FALSE) {
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

    if(plot.prior){
        x <- seq(xmin*0.5, xmax*1.5, length=500)
        prior.df <- data.frame(x=x, pdf=dbeta(x, prior.a, prior.b))
        gp <- gp + geom_line(data=prior.df, linetype='dashed', size=.5)
    }

    list(cumulative=cumulative,
         plot=gp + facet_wrap( ~day, scales='free'))
}

betapdf <- function(a, b){
    df <- data.frame(x = seq(0, 0.1, length=500))
    df$pdf <- dbeta(df$x, a + prior.a, b + prior.b)
}

theta.samples <- function (views, clicks, a.prior=1, b.prior=1, n=100000)
  rbeta(n, a.prior + clicks, b.prior + views-clicks)

simulate.data <- function(p, N=10){
    sample(c(0,1), N, replace=TRUE, prob=c(1-p, p))
}

get.counts <- function(x){
    list(visits=length(x), clicks=sum(x))
}

visits.a.day <- function(mu){
    max(0, floor(rnorm(1, mu, mu*0.25)))
}

click.data <- function(p.A, p.B, days=2, mean.visits=2000, test.group=0.2){
    mean.visits.a <- mean.visits * (1-test.group)
    mean.visits.b <- mean.visits * test.group
    visits = c()
    clicks = c()
    for (day in seq(1,days)){
        cnts <- get.counts(simulate.data(p.A, N=visits.a.day(mean.visits.a)))
        visits <- c(visits, cnts$visits)
        clicks <- c(clicks, cnts$clicks)
        cnts <- get.counts(simulate.data(p.B, N=visits.a.day(mean.visits.b)))
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

click.plots <- function(df){
    out <- list()
    df$visits_no_click <- df$visits - df$clicks
    zz <- melt(df, id.vars=c('day', 'variant'))
    gp <- ggplot(subset(zz, variable %in% c('visits_no_click', 'clicks')), aes(day, value))
    gp <- gp + geom_bar(stat='identity', aes(fill=variable), alpha=0.9, position='stack')
    gp <- gp + scale_fill_manual(values=c("darkorchid3",  "darkolivegreen4", "dodgerblue4", "goldenrod1"))
    out$histograms <- gp + facet_grid(variant ~ ., scales = 'free_y') + ylab("count")

    gp <- ggplot(subset(zz, variable == 'click.rate'), aes(x=day, y=value, group=variant))
    out$rates <- gp + geom_line(size=1.5, aes(colour=variant)) + ylab("click rate (raw)")

    out
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

colours <- gg_color_hue(2)
