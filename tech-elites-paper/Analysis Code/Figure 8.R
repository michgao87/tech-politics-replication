library(haven)
library(ggplot2)
library(stringr)
require(reshape)
require(plyr)
require(dplyr)
require(tidyr)

data <- read_dta('CS Undergrad Survey Data/combined_withmeans_withugrads.dta')

data$regulation <- 1 - data$regulation # in prior file this var was coded such that positive = conservative

# main analysis data
#data$group <- factor(data$group, ordered = TRUE, levels = levels(data$group)[1:7])

groups <- c('Dem College Public', 'Dem Donors', 'Technology Entrepreneurs', 'Computer Science Majors', 'Biology Majors')
data$group <- groups[data$sample]
data$group <- factor(data$group, ordered = TRUE, levels = groups)

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
  scale_fill_manual(values = c('dodgerblue2', 'dodgerblue4',
                               'darkolivegreen3',
                               'darkkhaki', 'sienna4')) +
  geom_errorbar(aes(ymax = ebymax, ymin = ebymin)) +
  ylab("Liberalism") + xlab("Group") + theme_bw() +
  facet_wrap(~construct, scales = 'free') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  #  scale_y_continuous(breaks = seq(0, 1, by = .1)) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.title.x = element_blank())

ggsave('figures/withugrad_scalescores.pdf', g, scale = 1.35, width = 8, height = 5)




