library(haven)
library(ggplot2)
library(stringr)
require(reshape)
require(plyr)
require(dplyr)
require(tidyr)
require(ggrepel)


data <- read_dta('Tech Donor and Public Survey Data/combined_withmeans_anon.dta')

data$massdems <- data$sample == 1 & data$pid3 == 1
data$masseducdems <- data$massdems & data$education >= 4
data$donordems <- data$sample == 2 & data$party == 'D'
data$techdems <- data$sample == 3 & (data$pid3 == 1) 
data$tech <- data$sample == 3

data <- rbind.data.frame(filter(data, massdems == 1),
                         filter(data, masseducdems == 1),
                         filter(data, donordems == 1),
                         filter(data, techdems == 1),
                         filter(data, tech == 1))

data$group <- 'MISSING'
data$group[data$massdems] <- 'Democrats (Mass Sample)'
data$group[data$masseducdems] <- 'College-Educated Democrats (Mass Sample)'
data$group[data$donordems] <- 'Democratic Donors'
data$group[data$tech] <- 'All Technology Entrepreneurs'
data$group[data$techdems] <- 'Democratic Technology Entrepreneurs'

table(data$group)

data$group <- factor(data$group, ordered = TRUE,
                     levels = unique(data$group)[c(1:3,5,4)])

d <- data %>% 
  group_by(group) %>% 
  summarize_each(funs(mean(., na.rm = TRUE), se = sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))) ),
                 redistribution, social, globalism, regulation) %>%
  data.frame()
d$regulation_mean <- 1 - d$regulation_mean
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
d$construct <- revalue(d$construct, c(globalism = 'Support for Globalism', regulation = 'Support for Regulation',
                                      social = 'Liberalism on Social Issues', redistribution = 'Support for Redistribution'))


g <- ggplot(data = d, aes(x = group)) +
  geom_col(aes(y = value.mean, fill = group)) + 
  scale_fill_manual(values = c('dodgerblue1', 'dodgerblue2', 'dodgerblue4',
                               'darkolivegreen3', 'darkolivegreen4')) +
  geom_errorbar(aes(ymax = ebymax, ymin = ebymin)) +
  ylab("Liberalism") + xlab("Group") + theme_bw() +
  facet_wrap(~construct, scales = 'free') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  #  scale_y_continuous(breaks = seq(0, 1, by = .1)) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.title.x = element_blank())

ggsave('figures/db_scalescores_withtechdems.pdf', g, scale = 1.35, width = 8, height = 5)

