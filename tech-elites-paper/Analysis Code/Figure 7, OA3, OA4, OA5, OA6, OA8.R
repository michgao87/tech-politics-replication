rm(list=ls())

require(ggplot2)
require(scales)
require(stringr)
require(plyr)
require(dplyr)
require(haven)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

data <- read_dta('Tech Donor and Public Survey Data/combined_anon.dta')

data$pid3 <- -1
data$pid3[data$pid1 == 1 | data$pid2leanselectedchoice == 1] <- 1
data$pid3[data$pid1 == 2 | data$pid2leanselectedchoice == 2] <- 3

data$massdems <- data$sample == 1 & data$pid3 == 1
data$masseducdems <- data$massdems & data$education >= 4
data$massreps <- data$sample == 1 & data$pid3 == 3
data$donordems <- data$sample == 2 & data$party == 'D'
data$donorreps <- data$sample == 2 & data$party == 'R'
data$tech <- data$sample == 3

data <- rbind.data.frame(filter(data, massdems == 1),
                         filter(data, masseducdems == 1),
                         filter(data, massreps == 1),
                         filter(data, donordems == 1),
                         filter(data, donorreps == 1),
                         filter(data, tech == 1))

data$group <- ''
data$group[data$massdems == 1] <- 'Democrats (Mass Sample)'
data$group[data$masseducdems == 1] <- 'College-Ed. Dems (Mass Sample)'
data$group[data$donordems == 1] <- 'Democratic Donors'
data$group[data$massreps == 1] <- 'Republicans (Mass Sample)'
data$group[data$donorreps == 1] <- 'Republican Donors'
data$group[data$tech == 1] <- 'Technology Entrepreneurs'

data$group <- factor(data$group, ordered = TRUE,
                     levels = unique(data$group)[c(1,2,4,3,5,6)])


make.fig <- function(var, qname){
  data.tmp <- data[!is.na(data[,var]),]
  
  data.tmp$outcome <- as_factor(data.tmp[,var])
  
  data.tmp <- data.frame(outcome = data.tmp$outcome, group = data.tmp$group)
  names(data.tmp) <- c('outcome', 'group')
  
  dfl <- data.tmp %>% 
    group_by(group, outcome) %>% 
    summarise(n=n()) %>% 
    group_by(group) %>% 
    mutate(perc=n/sum(n))
  
  #print(dfl)
  
  g <- ggplot(dfl, aes(x=outcome, y=perc)) +
    geom_bar(stat = 'identity', aes(fill = group)) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_y_continuous(labels=percent) + ylab('') + #Percent Selecting\nEach Option') +
    facet_wrap(~ group) +
    xlab('') + theme_bw() +
    scale_fill_manual(values = c('dodgerblue1', 'dodgerblue2', 'dodgerblue4',
                                 'firebrick1', 'firebrick4', 'darkolivegreen3'))  +
    theme(legend.position="none") +
    ggtitle(qname)
  return(g)
}

setwd('figures')

# Figure 7
pdf('ussrq.pdf', width = 7, height = 5.5)
multiplot(make.fig('uberussrq', 'Uber surge pricing fair.'),
          make.fig('flowersussrq', 'Florists raising prices on holidays fair.'),
          cols = 1)
dev.off()

# Figure OA4
pdf('redist.pdf', width = 10, height = 13)
multiplot(make.fig('fedspend_poor', 'Increase federal spending on the poor.'),
          make.fig('fedprograms_poorest', 'Support programs benefiting only poorest Americans.'),
          make.fig('taxes_250k', 'Increase taxes on those making >$250k per year.'),
          make.fig('taxes_million', 'Increase taxes on those making >$1MM per year.'),
          make.fig('healthcare', 'Support for universal healthcare, even if means raising taxes.'),
          cols = 1)
dev.off()

# Figure OA3
pdf('globalism.pdf', width = 10, height = 11)
multiplot(make.fig('concentrateonproblemsathome', 'Pay less attention to problems overseas and concentrate on problems at home.'),
          make.fig('tradevsjobs', 'American jobs vs. free trade and foreign jobs trade-off.'),
          make.fig('freetradeagreementsgood', 'Free trade agreements a good thing.'),
          make.fig('immigration', 'Immigration.'),
          cols = 1)
dev.off()

# Figure OA5
pdf('social.pdf', width = 10, height = 11)
multiplot(make.fig('ssm', 'Same-sex marriage.'),
          make.fig('deathpenalty', 'Death penalty.'),
          make.fig('guncontrol', 'Gun control.'),
          make.fig('abortion', 'View on abortion.'),
          cols = 1)
dev.off()

# Figure OA6a
pdf('regulation.pdf', width = 10, height = 11)
multiplot(make.fig('reg_uber_like_taxis', 'Regulate Uber like taxis.'),
          make.fig('gig_workers', 'Regulate gig workers like regular workers.'),
          make.fig('too_hard_fire_workers', 'It is too hard to fire workers.'),
          make.fig('govt_reg_business_harm', 'Government regulation of business does more harm than good.'),
          cols = 1)
dev.off()

# Figure OA6b
pdf('regulation2.pdf', width = 10, height = 11)
multiplot(make.fig('reg_drones', 'Regulations on drones should...'),
          make.fig('reg_selfdriving', 'Regulations on self-driving cars should...'),
          make.fig('reg_internetdata', 'Regulatons on how internet companies store data should...'),
          cols = 1)
dev.off()


# Figure OA8
pdf('laborinfluenceq.pdf', width = 7, height = 5.5)
multiplot(make.fig('laborinfluencepublic',
                   expression(paste("Would like to see ", bold("public"), " labor unions have..."))),
          make.fig('laborinfluenceprivate',
                   expression(paste("Would like to see ", bold("private"), " labor unions have..."))),
          cols = 1)
dev.off()

