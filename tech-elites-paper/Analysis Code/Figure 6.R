library(haven)
library(ggplot2)
library(stringr)
require(reshape)
require(plyr)
require(dplyr)
require(tidyr)
require(ggrepel)

data <- read_dta('Tech Donor and Public Survey Data/combined_withmeans_anon.dta')

data$medianvoter <- data$sample == 1
data$massdems <- data$sample == 1 & data$pid3 == 1
data$massreps <- data$sample == 1 & data$pid3 == 3
data$masseducdems <- data$massdems & data$education >= 4
data$donordems <- data$sample == 2 & data$party == 'D'
data$donorreps <- data$sample == 2 & data$party == 'R'
data$tech <- data$sample == 3

data <- rbind.data.frame(filter(data, medianvoter == 1),
                         filter(data, massdems == 1),
                         filter(data, massreps == 1),
                         filter(data, masseducdems == 1),
                         filter(data, donordems == 1),
                         filter(data, donorreps == 1),
                         filter(data, tech == 1))

data$group <- ''
data$group[data$medianvoter] <- 'General Public'
data$group[data$massdems] <- 'Democrats (Mass Sample)'
data$group[data$massreps] <- 'Republicans (Mass Sample)'
data$group[data$masseducdems] <- 'College-Educated Democrats (Mass Sample)'
data$group[data$donordems] <- 'Democratic Donors'
data$group[data$donorreps] <- 'Republican Donors'
data$group[data$tech] <- 'Technology Entrepreneurs'

data$group <- factor(data$group, ordered = TRUE,
                     levels = unique(data$group)[c(3,2,4,5,1,6,7)])

d <- data %>% 
  group_by(group) %>% 
  summarize_each(funs(mean(., na.rm = TRUE), se = sd(., na.rm = TRUE) / sqrt(sum(!is.na(.)))),
                 authoritarianism, racialresentment, cosmopolitanism2, entrepreneurstoomuchcredit) %>%
  data.frame()

d$racialresentment_mean = 1 - d$racialresentment_mean
d$authoritarianism_mean = 1 - d$authoritarianism_mean

d <- reshape(d, direction = 'long',
             varying = names(d)[-1],
             v.names = 'value',
             timevar = 'statname',
             times = names(d)[-1]) %>%
  separate(statname, c('construct', 'stat'), '_') %>%
  select(-id) %>%
  reshape(direction = 'wide',
          idvar = c('group', 'construct'),
          timevar = 'stat')

d$ebymax <- with(d, value.mean + value.se * 1.96)
d$ebymin <- with(d, value.mean - value.se * 1.96)

d$construct <- as.character(d$construct)
d$construct <- revalue(d$construct, c(
  racialresentment = 'Racial Resentment',
  entrepreneurstoomuchcredit = 'Value of Entrepreneurs to Economy Relative to Others',
  cosmopolitanism2 = 'Cosmopolitanism',
  authoritarianism = 'Authoritarianism'))

g <- ggplot(data = d, aes(x = group)) +
  geom_col(aes(y = value.mean, fill = group)) + 
  scale_fill_manual(values = c('grey50',
                               'dodgerblue1', 'dodgerblue3', 'dodgerblue4',
                               'firebrick1', 'firebrick4', 
                               'darkolivegreen3')) +
  geom_errorbar(aes(ymax = ebymax, ymin = ebymin)) +
  ylab("Liberalism (0-1 Scales)") + xlab("Group") + theme_bw() +
  facet_wrap(~construct, scales = 'free') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  #  scale_y_continuous(breaks = seq(0, 1, by = .1)) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16))

ggsave('figures/db_predispos.pdf', g, scale = 1.35, width = 8, height = 5)
