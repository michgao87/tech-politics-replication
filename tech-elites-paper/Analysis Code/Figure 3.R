require(ggplot2)
require(haven)
require(labelled)
require(scales)

data <- read_dta('Tech Donor and Public Survey Data/combined_anon.dta')
data <- subset(data, sample == 3) # Subset to tech sample.

setwd('figures')

ggplot(subset(data, !is.na(millionaire)),
       aes(x = to_factor(millionaire))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab('Millionaire?') + ylab('Percentage') + scale_y_continuous(labels=scales::percent)
ggsave('millionaire.pdf', scale = .6)
   
ggplot(subset(data, !is.na(topposition)),
       aes(x=to_factor(topposition))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Respondent's Top Position Held") + ylab('Percentage') + scale_y_continuous(labels=scales::percent)
ggsave('topposition.pdf', scale = .6)

ggplot(subset(data, !is.na(maxpeopleworkedunderr)),
       aes(x=to_factor(maxpeopleworkedunderr))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab('Max Employees Worked For Respondent') + ylab('Percentage') + scale_y_continuous(labels=scales::percent)
ggsave('maxemployees.pdf', scale = .6)

ggplot(subset(data, !is.na(worksintech)),
       aes(x=to_factor(worksintech))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab('Works in Technology Industry?') + ylab('Percentage') + scale_y_continuous(labels=scales::percent)
ggsave('worksintech.pdf', scale = .6)

ggplot(subset(data, !is.na(startedorrunbusiness)),
       aes(x=to_factor(startedorrunbusiness))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab('Started on Run a Business?') + ylab('Percentage') + scale_y_continuous(labels=scales::percent)
ggsave('startedorrunbiz.pdf', scale = .6)

