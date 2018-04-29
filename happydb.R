setwd('/home/zhixing/Documents/dataincubator/happy_project')
library(readr)
library(dplyr)
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(ggplot2)
hm <- read_csv('./input/data/cleaned_hm.csv')
demo_data = read_csv('./input/data/demographic.csv')
glimpse(hm)
glimpse(demo_data)

##add word length
full_word_count <- hm %>%
  unnest_tokens(word, cleaned_hm) %>%
  group_by(hmid) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))
hm <- right_join(hm,full_word_count, by='hmid')

hist(full_word_count$num_words,breaks = 200, xlim=c(0,100))

##add age stage
demo_data$age <- as.numeric(demo_data$age)
hist(demo_data$age,breaks=10)
demo_data <- demo_data %>%
  mutate(agestage = 
           ifelse(demo_data$age %in% 12:22, "Teenager", 
           ifelse(demo_data$age %in% 23:30, "Young adult", 
           ifelse(demo_data$age %in% 31:45, "Middle aged", 
           ifelse(demo_data$age %in% 46:80, "Mature adult", 
           "NA")))))

hm <- left_join(hm,select(demo_data,wid,agestage), by='wid')
hm$agestage <- factor(hm$agestage)
hm$agestage <- factor(hm$agestage, levels = c('Teenager','Young adult','Middle aged','Mature adult','NA'))
ggplot(hm[hm$agestage!='NA',], aes(x=agestage) )+
  geom_bar(fill='lightgreen')

## check if it is affection
hm <-  hm %>% mutate(Affection=ifelse(hm$predicted_category!='affection','not related','related to Affection'))
ggplot(hm[hm$agestage!='NA',], aes(x=agestage)) +
  geom_histogram(aes(fill = Affection ),stat='count') +
  ylab("Count") + 
  xlab("age stage") +
  ggtitle("age stage Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}
# fix (expand) contractions
hm$cleaned_hm <- sapply(hm$cleaned_hm, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
hm$cleaned_hm <- sapply(hm$cleaned_hm, removeSpecialChars)
hm$cleaned_hm <- sapply(hm$cleaned_hm, tolower)

undesirable_words <- c('happy', 'day', 'got', 'went', 'today', 'made', 'one', 'two', 'time', 'last', 
                       'first', 'going', 'getting', 'took', 'found', 'lot', 'really', 'saw', 'see', 
                       'month', 'week', 'day', 'yesterday', 'year', 'ago', 'now', 'still', 'since',
                       'something', 'great', 'good', 'long', 'thing', 'toi', 'without', 'yesteri', 
                       '2s', 'toand', 'ing')
hm_words_filtered <- hm %>%
  unnest_tokens(word, cleaned_hm) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words)


hm_words_filtered <- hm_words_filtered %>%
  count(word, sort = TRUE) 

wordcloud2(hm_words_filtered[1:300, ], size = .5)
