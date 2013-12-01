# EXAMPLES

source('slopegraph.r')

## Tufte's Cancer Graph (to the correct scale)
cancer <- read.csv('tufte-cancer-survival-data.csv')
rownames(cancer) <- cancer[,1]
cancer <- cancer [,-1]
pdf('tufte-cancer-survival-plot.pdf',height=16, width=12, family='Palatino')
slopegraph(cancer, col.line='gray', xlim=c(-.5,5.5), labels=c('5 Year','10 Year','15 Year','20 Year'), binval=2.5)
dev.off()


## Tufte's GNP Graph
gnp <- read.csv('tufte-gnp-data.csv')
gnp[,3] <- NULL
pdf('tufte-gnp-plot.pdf',height=12, width=8, family='Palatino')
slopegraph(gnp, col.line='gray', labels=c('1970','1979'), 
    main='Current Receipts of Goverment as a Percentage of Gross Domestic Product', binval=3.75)
dev.off()
