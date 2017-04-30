suppressWarnings(suppressMessages(library('plyr')))
suppressWarnings(suppressMessages(library('dplyr')))
suppressWarnings(suppressMessages(library('readr')))
suppressWarnings(suppressMessages(library('RTextTools')))
suppressWarnings(suppressMessages(library('tm')))
suppressWarnings(suppressMessages(library('tidytext')))
suppressWarnings(suppressMessages(library('ggplot2')))
suppressWarnings(suppressMessages(library('gridBase')))
suppressWarnings(suppressMessages(library('grid')))


ds <- read.csv('climb_spider/climbs.csv', stringsAsFactors = FALSE,
                  strip.white = TRUE, na.strings = c("NA",""))
dim(ds)
summary(ds)
str(ds)
 ##for now 
eda_features <- c("area", "average_rating", "grade", "name", "region", "climb_info", "number_of_votes", "crag")
dseda <- ds[,eda_features]
unique(dseda$region)
unique(dseda$location)


##let's create climb_type: Trad/Sport/Boulder/TR/Alpine/Ice/Aid/Mix
dseda$trad <- 0
dseda$trad[grep("Trad",dseda$climb_info)] <- 1
sum(dseda$trad)
dseda$sport <- 0
dseda$sport[grep("Sport",dseda$climb_info)] <- 1
dseda$toprope <- 0
dseda$toprope[grep("TR",dseda$climb_info)] <- 1
dseda$boulder <- 0 
dseda$boulder[grep("Boulder",dseda$climb_info)] <- 1
dseda$alpine <- 0
dseda$alpine[grep("Alpine",dseda$climb_info)] <- 1
dseda$aid <- 0
dseda$aid[grep("Aid",dseda$climb_info)] <- 1
dseda$ice <- 0
dseda$ice[grep("Ice",dseda$climb_info)] <- 1
dseda$mixed <- 0
dseda$mixed[grep("Mixed",dseda$climb_info)] <- 1

##let's get pitches:
dseda$pitch <- 0
m <- gregexpr("\\d pitch", dseda$climb_info)
m1 <- regmatches(dseda$climb_info, m)
dseda$pitch <- as.numeric(gsub("[^\\d]+", "", m1 ,perl=TRUE))

##and climb length in feet
dseda$length <-0
m <- gregexpr("[0-9]+'", dseda$climb_info) #"\\d pitch"
m1 <- regmatches(dseda$climb_info, m)
dseda$length <- as.numeric(gsub("[^\\d]+", "", m1 ,perl=TRUE))

##and now grade... or let it be...but clean the strings of the space in the beginning
unique(dseda$grade)
# grades <- c(" 5.15a/b",   " 5.14c/d",    "5.14b")
# gsub("(\\s+)","", grades)
dseda$grade <- gsub("(\\s+)","", dseda$grade)




## for now only trad, TR, sport climbs and instead of grades do difficulty level
dseda$grade_class <- "none"
easy <- c("5.1", "5.2", "5.3", "5.4", "5.5", "5.6", "5.7")
moderate <- c("5.8", "5.9","5.10")
difficult <- c("5.11", "5.12")
expert <- c("5.14", "5.15")



dseda$grade_class[grep(paste(easy,collapse="|"), dseda$grade, value=FALSE)] <- "easy"
dseda$grade_class[grep(paste(moderate,collapse="|"), dseda$grade, value=FALSE)] <- "moderate"
dseda$grade_class[grep(paste(difficult,collapse="|"), dseda$grade, value=FALSE)] <- "difficult"
dseda$grade_class[grep(paste(expert,collapse="|"), dseda$grade, value=FALSE)] <- "expert"

dseda$grade_class_det <- "none"
less5.5 <- c("5.1", "5.2", "5.3", "5.4", "5.5")
more5.13 <- c("5.13", "5.14", "5.15")
dseda$grade_class_det[grep(paste(less5.5,collapse="|"), dseda$grade, value=FALSE)] <- "<=5.5"
dseda$grade_class_det[grep("5.6", dseda$grade, value=FALSE)] <- "5.6"
dseda$grade_class_det[grep("5.7", dseda$grade, value=FALSE)] <- "5.7"
dseda$grade_class_det[grep("5.8", dseda$grade, value=FALSE)] <- "5.8"
dseda$grade_class_det[grep("5.9", dseda$grade, value=FALSE)] <- "5.9"
dseda$grade_class_det[grep("5.10", dseda$grade, value=FALSE)] <- "5.10"
dseda$grade_class_det[grep("5.11", dseda$grade, value=FALSE)] <- "5.11"
dseda$grade_class_det[grep("5.12", dseda$grade, value=FALSE)] <- "5.12"
dseda$grade_class_det[grep(paste(more5.13,collapse="|"), dseda$grade, value=FALSE)] <- ">=5.13"


## instead of states for now let's look at location: East/West US and international
dseda$geo <-"none"
summary(as.factor(dseda$region))
west<-c("Arizona","California","Colorado","Idaho","Montana",
        "Nevada","Washington", "Oregon",  "South Dakota",
        "Texas","Utah", "New Mexico", "Wyoming")
east <- c("Connecticut", "Kentucky", "Maine",  "Maryland",
         "Massachusetts", "New York", "North Carolina","Virginia",
         "West Virginia", "New Hampshire")
intern <-c("International")                
dseda$geo[grep(paste(west,collapse="|"), dseda$region, value=FALSE)] <- "west"
dseda$geo[grep(paste(east,collapse="|"), dseda$region, value=FALSE)] <- "east"
dseda$geo[grep(paste(intern,collapse="|"), dseda$region, value=FALSE)] <- "international"
     

##some common eda, all stat's there
ggplot(dseda, aes(x = grade_class_det, fill = geo)) +
  geom_bar(position="dodge")

## only toprope, trad or sport (since we only did those for grades)
ds_tradsport <- dseda %>%
  filter(trad==1 | toprope ==1 | sport == 1) 

## Graph ##1
ds_tradsport$grade_class_det <- factor(ds_tradsport$grade_class_det,
                       levels = c('<=5.5','5.6', '5.7','5.8', '5.9','5.10','5.11','5.12','>=5.13'),
                       ordered = TRUE)
ds_tradsport$geo <- factor(ds_tradsport$geo, 
                           levels = c('east', 'west','international'),
                           ordered = TRUE)
ggplot(ds_tradsport, aes(x = grade_class_det, y=average_rating, fill=geo)) +
  geom_boxplot()+
  theme_minimal() +
  #stat_boxplot(geom ='errorbar')+
  labs(x='Difficulty level', y='Average rating', title='Average rating (climber\'s satisfaction) vs grade (difficulty)')+
  scale_fill_hue(l=55)
  #scale_fill_brewer(palette = "Set1", name='Location')

# ggplot(ds_tradsport, aes(x = grade_class, y=ds_tradsport$length)) +
#   geom_boxplot()


##let's have some fun with the names of the climbs for starters
##looking at sample first

names_df <- dseda %>%
  select(grade_class, crag, name, average_rating,geo)

#  group_by(region, crag)
# mutate(text = dseda$name) %>%

##tokenization
data(stop_words)
tidy_names <- names_df %>%
  unnest_tokens(word, name) %>%
  anti_join(stop_words)



tidy_names %>%
 # filter(grade_class=="expert")%>%
  #filter(!(word %in% c("crack","unknown"))) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

##compare word frequency  - graph ##2

frequency <- tidy_names %>%
  count(grade_class, word) %>%
  mutate(proportion = n / sum(n))%>%
  select(-n) %>% 
  spread(grade_class, proportion) %>% 
  gather(grade_class, proportion, easy:difficult)


frequency$grade_class <- factor(frequency$grade_class , 
                                levels = c('easy', 'moderate','difficult','expert','none'),
                                ordered = TRUE)

library(scales)
ggplot(frequency, aes(x = proportion, y = `moderate`, color = abs(`moderate` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_fill_gradientn(colours = terrain.colors(10))+#limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~grade_class, ncol = 2) +
  theme(legend.position="none") +
  labs(color = "Moderate-Easy/Difficult",title = "Compare word frequency in the climb names depending on the climb difficulty",y = "moderate", x = NULL)




library(wordcloud)
tidy_names %>%
  filter(grade_class=="difficult")%>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

## some sentiments...
names_sent <- tidy_names %>%
  filter(grade_class=="expert")%>%
  inner_join(get_sentiments("nrc"))

ggplot(names_sent, aes(x = sentiment)) +
  geom_bar()

##commonality word cloud
GR<-c("difficult","easy","expert","moderate")
GR1<-c("easy","expert")
 tidy_names %>%
    select(word, grade_class)%>%
    filter(grade_class %in% GR)%>%
    count(word, grade_class, sort = TRUE)%>%
    acast(word ~ grade_class, value.var = "n", fill = 0)%>%
      commonality.cloud(random.order=FALSE, 
                        #colors = c("blue", "red", "orange","black"),#
                        colors =brewer.pal(8, "Dark2"),
                       title.size=1.5,
                       max.words = 200)

## draw negative-positive cloud
tidy_names %>%
  #filter(grade_class=="easy")%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   max.words = 200)


