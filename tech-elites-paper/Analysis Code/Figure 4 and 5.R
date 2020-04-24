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
data$techdems <- data$sample == 3 & data$pid3 == 1
data$techreps <- data$sample == 3 & data$pid3 == 3
data$massmillionaires <- data$sample == 1 & data$millionaire == 1

get.data.subset <- function(filtered.df, group.name) {
  filtered.df$group <- group.name
  return(filtered.df)
}

data <- rbind.data.frame(get.data.subset(filter(data, medianvoter == 1), 'General Public'),
                         get.data.subset(filter(data, massdems == 1), 'Democrats (Mass Sample)'),
                         get.data.subset(filter(data, masseducdems == 1),
                                         'College-Educated Democrats (Mass Sample)'),
                         get.data.subset(filter(data, massreps == 1), 'Republicans (Mass Sample)'),
                         get.data.subset(filter(data, donordems == 1), 'Democratic Donors'),
                         get.data.subset(filter(data, donorreps == 1), 'Republican Donors'),
                         get.data.subset(filter(data, tech == 1), 'Technology Entrepreneurs'),
                         get.data.subset(filter(data, techdems == 1), 'Democratic Technology Entrepreneurs'))


data$group <- factor(data$group, ordered = TRUE,
                     levels = unique(data$group)[c(1,2,3,5,4,6,7,8)])


data$regulation <- 1 - data$regulation # in prior file this var was coded such that positive = conservative

# for use in the next graph
d.copy <- data %>% 
  rbind.data.frame(get.data.subset(filter(data, massmillionaires == 1), 'Millionaires in Mass Public')) %>%
  group_by(group) %>% 
  summarize_each(funs(mean(., na.rm = TRUE), se = sd(., na.rm = TRUE) / sqrt(sum(!is.na(.)))),
                 redistribution, social, globalism, regulation) %>%
  data.frame()


# main analysis data
data <- filter(data, group != 'Democratic Technology Entrepreneurs')
data$group <- factor(data$group, ordered = TRUE,
                     levels = levels(data$group)[1:7])

d <- data %>% 
  group_by(group) %>% 
  summarize_each(funs(mean(., na.rm = TRUE), se = sd(., na.rm = TRUE) / sqrt(sum(!is.na(.)))),
                 redistribution, social, globalism, regulation) %>%
  data.frame()

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
  scale_fill_manual(values = c('grey50',
                               'dodgerblue1', 'dodgerblue2', 'dodgerblue4',
                               'firebrick1', 'firebrick4', 
                               'darkolivegreen3')) +
  geom_errorbar(aes(ymax = ebymax, ymin = ebymin)) +
  ylab("Liberalism") + xlab("Group") + theme_bw() +
  facet_wrap(~construct, scales = 'free') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
  #  scale_y_continuous(breaks = seq(0, 1, by = .1)) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 16),
        axis.text=element_text(size=9),
        axis.title=element_text(size=16),
        axis.title.x = element_blank())

ggsave('figures/db_scalescores_v2.pdf', g, scale = 1.35, width = 7.75, height = 5)


### reg vs. redist plots

d.copy$tech <- grepl('Techn', d.copy$group)

ggplot(data = d.copy, aes(y = regulation_mean, x = redistribution_mean)) +
  geom_smooth(data = subset(d.copy, !tech),
              method='lm', se = FALSE) +
  scale_color_manual(values = c('grey50',
                               'dodgerblue1', 'dodgerblue2', 'dodgerblue4',
                               'firebrick1', 'firebrick4', 
                               'darkolivegreen4', 'darkolivegreen3', 'grey')) +
  geom_text_repel(aes(label = stringr::str_wrap(group, width = 30),
                y = regulation_mean, color = group), size = 3) +
  geom_point(aes(color = group)) +
  theme_classic() +
  theme(legend.position="none",
        strip.background = element_blank()) +
  scale_y_continuous(name = 'Liberalism on Regulation (0-1 Scale)', limits = c(.3, .8)) +
  scale_x_continuous(name = 'Liberalism on Redistribution (0-1 Scale)', limits = c(.41, .95))
ggsave('figures/reg_redist_x_y_plot.pdf', scale = .75, width = 9, height = 9, units = 'in')






