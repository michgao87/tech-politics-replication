require(ggplot2)
require(haven)
require(labelled)
require(scales)

data <- read_dta('Tech Donor and Public Survey Data/combined_anon.dta')
data <- subset(data, sample == 2) # Subset to donor sample.


deminf <- data[,startsWith(names(data), 'deminf')]
deminf.more <- deminf == 1
deminf.less <- deminf == 3

colMeans(deminf.more, na.rm = TRUE)
colMeans(deminf.less, na.rm = TRUE)


df <- data.frame(colMeans(deminf.less, na.rm = TRUE))
names(df) <- 'Less'
df$group <- rownames(df)
df$group[df$group == 'deminfluence_tech'] <- 'Technology entrepreneurs'
df$group[df$group == 'deminfluence_smallbiz'] <- 'Small businesses'
df$group[df$group == 'deminfluence_bigbiz'] <- 'Big businesses'
df$group[df$group == 'deminfluence_labor'] <- 'Labor unions'
df$group[df$group == 'deminfluence_lgbt'] <- 'LGBT people and organizations'
df$group[df$group == 'deminfluence_banks'] <- 'Big banks'
df$group[df$group == 'deminfluence_civilrights'] <- 'Civil rights organizations'
df$group[df$group == 'deminfluence_blacks'] <- 'African-Americans'
df$group[df$group == 'deminfluence_latinos'] <- 'Latinos'
df <- df[order(df$Less),]
df$group <- factor(df$group, ordered = TRUE, levels = df$group)

ggplot(df) + geom_col(aes(x = group, y = Less)) + coord_flip() + 
  xlab('') + ylab('') + ggtitle("Share of Democratic Donors Expecting Below\nGroup's Influence in Party to Decrease") +
  scale_y_continuous(labels=scales::percent)
ggsave('figures/donors_think_lessinf.pdf', width = 5.5)


df <- data.frame(colMeans(deminf.more, na.rm = TRUE))
names(df) <- 'More'
df$group <- rownames(df)
df$group[df$group == 'deminfluence_tech'] <- 'Technology entrepreneurs'
df$group[df$group == 'deminfluence_smallbiz'] <- 'Small businesses'
df$group[df$group == 'deminfluence_bigbiz'] <- 'Big businesses'
df$group[df$group == 'deminfluence_labor'] <- 'Labor unions'
df$group[df$group == 'deminfluence_lgbt'] <- 'LGBT people and organizations'
df$group[df$group == 'deminfluence_banks'] <- 'Big banks'
df$group[df$group == 'deminfluence_civilrights'] <- 'Civil rights organizations'
df$group[df$group == 'deminfluence_blacks'] <- 'African-Americans'
df$group[df$group == 'deminfluence_latinos'] <- 'Latinos'
df <- df[order(df$More, decreasing = TRUE),]
df$group <- factor(df$group, ordered = TRUE, levels = df$group)

ggplot(df) + geom_col(aes(x = group, y = More)) + coord_flip() + 
  xlab('') + ylab('') + ggtitle('Share of Democratic Donors Expecting Below\nGroup\'s Influence in Party to Increase') +
  scale_y_continuous(labels=scales::percent)
ggsave('figures/donors_think_moreinf.pdf', width = 5.5)
