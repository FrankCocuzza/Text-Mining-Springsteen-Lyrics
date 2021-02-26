
### Sentiment Analysis of Bruce Springsteen's Lyrics Over the Years
### Frank Cocuzza
### First Published 12 February 2021

### Credit to Preetish P's Blog Post on KDNuggets for the inspiration
### https://www.kdnuggets.com/2018/09/sentiment-analysis-adele-songs.html


library(tidyverse)
library(here)
library(geniusr)
library(genius)
library(tm)
library(wordcloud)
library(tidytext)
library(textdata)
library(syuzhet)
library(ggrepel)


### Data Collection


# Set your Genius API token for accessing the Genius API (this will be unique to your account)
# See here fore more info on using the Genius API (https://docs.genius.com/#/getting-started-h1)

GENIUS_API_TOKEN <-"your token here"

# Load album list. I put this together myself, but you could also 
# scrape this info from the internet
albums <- read_csv(here("albums10.csv"), col_names = TRUE)

# Combine artist name on albums data
albums_tbl <- tibble(artist = c(rep("Bruce Springsteen", 20)), album = albums$Title, year = albums$Year)

# Map genius data (which includes lyrics) on album data
# This step take awhile as R maps the genius_album function
# from the 'genius' package and scrapes the lyrics from the internet)
album_lyrics <- albums_tbl %>% 
                mutate(tracks = map2(artist, album, genius_album))

# Pull out the lyrics for processing
lyrics <- album_lyrics %>% 
  unnest(tracks) %>%    
  arrange(desc(artist))
lyrics <- as.data.frame(lyrics)
lyrics_text <- lyrics$lyric


### Prepare data for text mining


# Remove punctuation marks and alphanumeric characters from lyrics
lyrics_text<- gsub('[[:punct:]]+', '', lyrics_text)
lyrics_text<- gsub("([[:alpha:]])\1+", "", lyrics_text)

# Define the song lyrics as our corpus
docs <- Corpus(VectorSource(lyrics_text))

# Convert all lyrics to lowercase
docs <- tm_map(docs, content_transformer(tolower))

# Remove common English stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# create 'Term Document Matrix' (TDM)
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)


### Word Frequency Overview


# Count word frequencies in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# Create a table with words and their frequencies
lyrics_wc_df <- data.frame(word=names(word_freqs), freq=word_freqs)
lyrics_wc_df <- lyrics_wc_df[1:300,]

# Plot in a word cloud
set.seed(22)
wordcloud(words = lyrics_wc_df$word, freq = lyrics_wc_df$freq, 
          min.freq = 1,scale=c(1.8,.5),
          max.words=400, random.order=FALSE, rot.per=0.05, 
          colors=brewer.pal(6, "Paired"))


### Emotion and Sentiment Analysis
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ### Associate emotions with lyrics

# Using NRC sentiment lexicon. This lexicon labels words as being associated with 1
# of 2 sentiments and 1 or more of 8 emotions
# The 8 emotions correspond to those proposed by pscyhologist
# Robert Plutchik (https://en.wikipedia.org/wiki/Robert_Plutchik)

ty_sentiment <- get_nrc_sentiment((lyrics_text))

# Create table with cumulative value of each emotions
Sentimentscores <- data.frame(colSums(ty_sentiment[,]))

# Create table with emotion and score as columns
names(Sentimentscores) <- "Score"
Sentimentscores <- cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(Sentimentscores) <- NULL


### Visualize emotions by song and album

# Plot for the cumulative emotions for all lyrics, overall
# I filtered out positive/negative sentiment categories for now
# as I'm focusing on the 8 emotions 

cumulative_nrc <- Sentimentscores %>%
  filter(sentiment !="positive", sentiment != "negative") 

cumulative_nrc %>%
ggplot(aes(x=sentiment,y=Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position="none") +
  xlab("Emotion")+ylab("Word Count") +
  ggtitle("Total words associated with each emotion") 


### Analyze emotional trends by song and album level

# Count how many words each song has
# We'll use this as the denominator in determining the degree that each song expresses
# each type of emotion

# total words (regardless whether they are associated with a sentiment)
tidy_lyrics <- lyrics %>% 
  unnest_tokens(word,lyric)
song_wrd_count <- tidy_lyrics %>% count(track_title, sort=TRUE, name = "song_word_count")

# Join the song word count and word sentiment  onto song-level data
lyric_counts <- tidy_lyrics %>%
  left_join(song_wrd_count, by = "track_title") 

# Join emotion tags onto lyrics data, using the NRC lexicon
lyric_sentiment_nrc <- lyric_counts %>% 
  inner_join(get_sentiments("nrc"),by="word")

# Calculate value for each 'bipolar' pair of emotions for each song
# Normalize by total words in a song because
# I didn't want any songs to have more influence just because they had more words
plot_nrc <- lyric_sentiment_nrc %>%
  group_by(track_title) %>%
  mutate(   anger_count = sum(sentiment == "anger"),
            fear_count = sum(sentiment == "fear"),
            ant_count = sum(sentiment == "anticipation"),
            surp_count = sum(sentiment == "surprise"),
            joy_count = sum(sentiment == "joy"),
            sad_count = sum(sentiment == "sadness"),
            trust_count = sum(sentiment == "trust"),
            disgust_count = sum(sentiment == "disgust"),
            anger_pct = (anger_count - fear_count)/song_word_count*100,
            ant_pct = (ant_count - surp_count)/song_word_count*100,
            joy_pct = (joy_count - sad_count)/song_word_count*100,
            trust_pct = (trust_count - disgust_count)/song_word_count*100
         ) %>% 
  distinct(track_title, .keep_all=TRUE) 

# Aggregate by album and plot albums on 2 charts
chart_nrc <-  plot_nrc %>% 
              group_by(album) %>%
              summarize(median_anger = median(anger_pct),
                      median_ant = median(ant_pct),
                      median_joy = median(joy_pct),
                      median_trust = median(trust_pct) 
                      )
# Joy/Anger Plot
plot_joy_anger <-
  ggplot(chart_nrc, 
       aes(x = median_anger, y = median_joy)) + 
  geom_point(color = "dodgerblue4", size=2.0) +
  geom_text_repel(data = chart_nrc,
                  aes(label = album),
                  size=3.5,
                  box.padding = 1.5) +
  coord_cartesian(xlim =c(-2, 2), ylim = c(-1.5, 5)) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
plot_joy_anger

# Trust/Anticipation Plot
plot_trust_ant <- 
       ggplot(chart_nrc, 
       aes(x = median_ant, y = median_trust)) + 
  geom_point(color = "dodgerblue4", size=2.0) +
  geom_text_repel(data = chart_nrc,
                  aes(label = album),
                  size=3.5,
                  box.padding = 1.5,
                  max.overlaps = 20) +
  coord_cartesian(xlim =c(-0.5, 2), ylim = c(0, 2.5)) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab("Less Than Zero = More Surprise, Greater Than Zero = More Anticipation")+ 
  ylab("Less Than Zero = More Disgust, Greater Than Zero = More Trust") 
plot_trust_ant


### Analyze sentiment trends by song and album level


# Using the 'afinn' sentiment lexicon, which assigns a value of -5 (most negative) 
# to +5 (most positive)
lyric_sentiment_afinn <- lyric_counts %>% 
  inner_join(get_sentiments("afinn"),by="word")

# Calculate the sentiment scores of all tracks and normalize by total words in a song
# I didn't want songs to have more influence just because they made more words
plot <- lyric_sentiment_afinn %>%
  group_by(track_title) %>%
  summarize(track_title,
            album, 
            year,
            sent_score = sum(value),
            normalized_afinn_score = sent_score/song_word_count*100) %>% 
  distinct(track_title, .keep_all=TRUE)

song_afinn <- plot

# Identify top 10 most positive and negative songs in terms of sentiment
plot3 <- head(plot[order(plot$normalized_afinn_score),],15)
view(plot3)
plot4 <- head(plot[order(plot$normalized_afinn_score, decreasing = TRUE),],15)
view(plot4)

# Calculate the median sentiment score of songs by album/year released
album_median_afinn <- plot %>%
  group_by(album) %>%
  summarize(
    album =  album,
    year = year,
    normalized_afinn_score = median(normalized_afinn_score)) %>%
  distinct(album, year, normalized_afinn_score)
view(album_median_afinn)

# Visualization
timeline <-
ggplot(album_median_afinn, 
        aes(x = year, y = normalized_afinn_score)) + 
        geom_point(color = "green", size=2.0) +
        geom_line(color = "green", size=1) + 
        geom_text_repel(data = album_median_afinn,
                  aes(label = album),
                  size=3.5,
                  box.padding = 1.5) +
        coord_cartesian(xlim =c(1960, 2030), ylim = c(-3, 9)) +
        theme(legend.position = "none") 
timeline

### Write all derived tables to .csv files for viz's in other programs

# https://en.wikipedia.org/wiki/File:Plutchik-wheel.svg

write.csv(lyrics_wc_df, file = "word_frequency.csv")
write.csv(cumulative_nrc, file = "cumulative_nrc.csv")
write.csv(lyric_sentiment_nrc, file = "lyric_sentiment_nrc.csv")
write.csv(song_afinn, file = "song_afinn.csv")
write.csv(lyric_sentiment_afinn, file = "lyric_sentiment_afinn.csv") 
write.csv(album_median_afinn, file = "album_median_afinn.csv")














### Extraneous items

# Plot for each sentiment
plot %>%
  group_by(sentiment) %>%
  filter(sentiment == "anger") %>%
  top_n(n=5) %>%
  ggplot(aes(x=reorder(track_title, sent_pct),y=sent_pct), fill = sentiment) + 
  geom_bar(stat="identity",show.legend = FALSE) +
  xlab("Sentiments") + ylab("Scores")+
  ggtitle("Top 5 Songs 'Anger' word Frequency") +
  coord_flip()

plot2 %>% filter(sentiment == "positive") %>%
ggplot(aes(x = year, y = median_track_sent)) + 
  geom_line(aes(color = sentiment))

# Analysis by Album
flat_album <- album_lyrics %>% 
  unnest(tracks) %>%
  select(album, track_n, track_title) %>%
  distinct(track_title, .keep_all = TRUE) %>%
  left_join(song_wrd_count, by = "track_title") %>% 
  left_join(albums_tbl, by = "album")

# experiment with plotting all songs, it's a bit messy
plot_joy_anger_songs <-
  ggplot(plot_nrc, 
         aes(x = anger_pct, y = joy_pct)) + 
  geom_point(aes(color = album), size=2.0) 













