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
data$donordems <- data$sample == 2 & data$party == 'D'
data$donorreps <- data$sample == 2 & data$party == 'R'
data$tech <- data$sample == 3
data$masseducdems <- data$massdems & data$education >= 4
data$wealthydems <- data$massdems & data$millionaire == 1 
data$wealthydemdonors <- data$donordems & data$millionaire == 1

data <- rbind.data.frame(filter(data, masseducdems == 1),
                         filter(data, wealthydems == 1),
                         filter(data, wealthydemdonors == 1),
                         filter(data, tech == 1))

data$group <- ''
data$group[data$masseducdems] <- 'College-Educated Democrats (Mass Sample)'
data$group[data$wealthydems] <- 'Millionaire Democrats (Mass Sample)'
data$group[data$wealthydemdonors] <- 'Millionaire Democratic Donors'
data$group[data$tech] <- 'Technology Entrepreneurs'

data$group <- factor(data$group, levels = unique(data$group))

d <- data %>% 
  group_by(group) %>% 
  summarize_each(funs(mean(., na.rm = TRUE), se = sd(., na.rm = TRUE) / sqrt(sum(!is.na(.)))),
                 redistribution, social, globalism, regulation) %>%
  data.frame()

# Recode regulation so that higher values are more LIBERAL (i.e. support for regulation);
# previously higher values were more conservative (i.e., opposition to regulation).
d$regulation_mean <- 1 - d$regulation_mean

# Reshape data to respondent-by-outcome level for easier plotting.
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
  scale_fill_manual(values = c('dodgerblue2', 'dodgerblue3', 'dodgerblue4',
                               'darkolivegreen3')) +
  geom_errorbar(aes(ymax = ebymax, ymin = ebymin)) +
  ylab("Liberalism") + xlab("Group") + theme_bw() +
  facet_wrap(~construct, scales = 'free') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.title.x = element_blank())

ggsave('figures/db_scalescores_robustness_v2.pdf', g, scale = 1.35, width = 8, height = 5)


