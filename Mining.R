########Tweet Mining Full Script#######

#For the purposes of commenting this code, I have divided the 31 teams into 
  #their 4 respective divisions: Atlantic, Central, Metropolitan, and Pacific.
  #I will comment the code for the Atlantic Division only, but the other three
  #follow the same exact coding blocks.



#Use libraries in order to mine tweets

x <- c("tidyverse", "tidytext", "purrr", "twitteR", "ROAuth", "devtools", 
       "stringr", "RColorBrewer", "wordcloud", "gridExtra", "tm", 
       "sp", "RgoogleMaps", "ggmap", "maptools", "tigris", "leaflet", 
       "webshot", "RCurl", "reshape2", "scales", "kableExtra", "knitr");

lapply(x, library, character.only = TRUE);


#Keys and tokens accessed from Twitter developer app 

api_key <- 	"6MgJS9u6EFi1hrWKMAlRhK4e0"
api_secret <- "MIiI07L6L2HsUtQYnOzOjfBLsgto8ydKjTBgvomFp5usSVbX1u"
access_token <- "204625038-1vqJjU8rx0mNp9STUaL6NNiFmQGxkWQduqEwMIzj"
access_token_secret <- "IR0TdMnSGRFfWELnemdYaUq0wKLbZhleauqgpgWYXvOnk"

#Setup Twitter authorization for tweet mining
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


#Atlantic Division

#Retrieve 500 tweets, in English, using the official hashtag for each individual team
#from the October 04 2017 (beginning of the season) to December 15 2017, filtering out retweets
Lightning <- searchTwitter('#GoBolts -filter:retweets',  since = "2017-10-04", until = "2017-12-15" , n=500, lang="en")
Leafs <- searchTwitter('#TMLtalk -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Habs <- searchTwitter('#GoHabsGo -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Bruins <- searchTwitter("#NHLBruins -filter:retweets",  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
RedWings <- searchTwitter("#LGRW -filter:retweets",  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Sens <- searchTwitter('#Sens -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Panthers <- searchTwitter('#FlaPanthers -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Sabres <- searchTwitter('#Sabres -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")


#Turn the lists into data frams
Lightning.df <- twListToDF(Lightning)
Leafs.df <- twListToDF(Leafs)
Habs.df <- twListToDF(Habs)
Bruins.df <- twListToDF(Bruins)
RedWings.df <- twListToDF(RedWings)
Sens.df <- twListToDF(Sens)
Panthers.df <- twListToDF(Panthers)
Sabres.df <- twListToDF(Sabres)


#Add a teamname field for filtering later on
Lightning.df$teamname <-'Tampa Bay Lightning'
Leafs.df$teamname <- 'Toronto Maple Leafs'
Habs.df$teamname <- 'Montreal Canadiens'
Bruins.df$teamname <- 'Boston Bruins'
RedWings.df$teamname <- 'Detroit Red Wings'
Sens.df$teamname <- 'Ottawa Senators'
Panthers.df$teamname <- 'Florida Panthers'
Sabres.df$teamname <- 'Buffalo Sabres'

#Bind all division teams together into one variable 
atlantic <- rbind(Lightning.df, Leafs.df, Habs.df, Bruins.df, 
                  RedWings.df, Sens.df, Panthers.df, Sabres.df)


#The next lines are cleaning the individual tweets
#Remove @UserName
atlantic$text <- gsub("@\\w+", "", atlantic$text)
#Remove all non ASCII or latin characters, i.e. #
atlantic$text <- sapply(atlantic$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
#turn all Tweets lower case
atlantic$text <- tolower(atlantic$text)
#Remove "RT", the retweet preface
atlantic$text <- gsub("rt", "", atlantic$text)
#Remove URL links
atlantic$text <- gsub("http[^[:space:]]*", "", atlantic$text)
atlantic$text <- gsub("http[[:alnum:][:punct:]]*", "", atlantic$text) 
atlantic$text <- gsub("http\\w+", "", atlantic$text)
#Remove punctuation
atlantic$text <- gsub("[[:punct:]]", "", atlantic$text)
#Remove AMP 
atlantic$text <- gsub("amp", "", atlantic$text)
#Remove tabs
atlantic$text <- gsub("[ |\t]{2,}", " ", atlantic$text)
#Remove blank spaces at the beginning and end of some tweets
atlantic$text <- gsub("^ ", "", atlantic$text)
atlantic$text <- gsub(" $", "", atlantic$text)

#Save tweets to a clean data structure
CleanAtlantic <- atlantic

#Save clean data structure in separate RData file
save(CleanAtlantic, file='atlantic.RData')


#REPEAT FOR THE FOLLOWING 3 DIVISIONS (skip to line 316)

###Central Division###
Preds <- searchTwitter('#Preds -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Jets <- searchTwitter('#GoJetsGo -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Blues <- searchTwitter('#AllTogetherNowSTL -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Stars <- searchTwitter('#GoStars -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Wild <- searchTwitter('#mnwild -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Hawks <- searchTwitter('#Blackhawks -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Avalanche <- searchTwitter('#GoAvsGo -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")


Preds.df <- twListToDF(Preds)
Jets.df <- twListToDF(Jets)
Blues.df <- twListToDF(Blues)
Stars.df <- twListToDF(Stars)
Wild.df <- twListToDF(Wild)
Hawks.df <- twListToDF(Hawks)
Avalanche.df <- twListToDF(Avalanche)


Preds.df$teamname <- 'Nashville Predators'
Jets.df$teamname <- 'Winnipeg Jets'
Blues.df$teamname <- 'St. Louis Blues'
Stars.df$teamname <- 'Dallas Stars'
Wild.df$teamname <- 'Minnesota Wild'
Hawks.df$teamname <- 'Chicago Blackhawks'
Avalanche.df$teamname <- 'Colorado Avalanche'

central<- rbind(Preds.df, Jets.df, Blues.df, Stars.df, Wild.df, Hawks.df, Avalanche.df)

# Replace @UserName
central$text <- gsub("@\\w+", "", central$text)

central$text <- sapply(central$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

central$text <- tolower(central$text)


# Replace blank space (“rt”)
central$text <- gsub("rt", "", central$text)

# Remove links

central$text <- gsub("http[^[:space:]]*", "", central$text)

central$text <- gsub("http[[:alnum:][:punct:]]*", "", central$text) 

central$text <- gsub("http\\w+", "", central$text)


# Remove punctuation
central$text <- gsub("[[:punct:]]", "", central$text)

# Remove AMP 
central$text <- gsub("amp", "", central$text)



# Remove tabs
central$text <- gsub("[ |\t]{2,}", " ", central$text)

# Remove blank spaces at the beginning
central$text <- gsub("^ ", "", central$text)

# Remove blank spaces at the end
central$text <- gsub(" $", "", central$text)

CleanCentral <- central

save(CleanCentral, file='central.RData')

###Metropolitan Division###
Devils <- searchTwitter('#NJDevils -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
BlueJackets <- searchTwitter('#CBJ -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Caps <- searchTwitter('#ALLCAPS -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Islanders <- searchTwitter('#Isles -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Pens <- searchTwitter('#LetsGoPens -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Rangers <- searchTwitter('#NYR -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Canes <- searchTwitter('#Redvolution -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Flyers <- searchTwitter('#LetsGoFlyers -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")

Devils.df <- twListToDF(Devils)
BlueJackets.df <- twListToDF(BlueJackets)
Caps.df <- twListToDF(Caps)
Islanders.df <- twListToDF(Islanders)
Pens.df <- twListToDF(Pens)
Rangers.df <- twListToDF(Rangers)
Canes.df <- twListToDF(Canes)
Flyers.df <- twListToDF(Flyers)

Devils.df$teamname <- 'New Jersey Devils'
BlueJackets.df$teamname <- 'Columbus Blue Jackets'
Caps.df$teamname <- 'Washington Capitals'
Islanders.df$teamname <- 'New York Islanders'
Pens.df$teamname <- 'Pittsburgh Penguins'
Rangers.df$teamname <- 'New York Rangers' 
Canes.df$teamname <- 'Carolina Hurricanes'
Flyers.df$teamname <- 'Philadelphia Flyers'

metropolitan <- rbind(Devils.df, BlueJackets.df, Caps.df, Islanders.df, Pens.df, Rangers.df, Canes.df, Flyers.df)

# Replace @UserName
metropolitan$text <- gsub("@\\w+", "", metropolitan$text)

metropolitan$text <- sapply(metropolitan$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

metropolitan$text <- tolower(metropolitan$text)


# Replace blank space (“rt”)
metropolitan$text <- gsub("rt", "", metropolitan$text)

# Remove links

metropolitan$text <- gsub("http[^[:space:]]*", "", metropolitan$text)

metropolitan$text <- gsub("http[[:alnum:][:punct:]]*", "", metropolitan$text) 

metropolitan$text <- gsub("http\\w+", "", metropolitan$text)


# Remove punctuation
metropolitan$text <- gsub("[[:punct:]]", "", metropolitan$text)

# Remove AMP 
metropolitan$text <- gsub("amp", "", metropolitan$text)



# Remove tabs
metropolitan$text <- gsub("[ |\t]{2,}", " ", metropolitan$text)

# Remove blank spaces at the beginning
metropolitan$text <- gsub("^ ", "", metropolitan$text)

# Remove blank spaces at the end
metropolitan$text <- gsub(" $", "", metropolitan$text)

CleanMet <- metropolitan

save(CleanMet, file = 'metropolitan.RData')

###Pacific Division###
Kings <- searchTwitter('#LAKings -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
GK <- searchTwitter('#VegasBorn -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Canucks <- searchTwitter('#Canucks -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Sharks <- searchTwitter('#SJSharks -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Flames <- searchTwitter('#CofRed -filter:retweets', since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Ducks <- searchTwitter('#LetsGoDucks -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Oilers <- searchTwitter('#LetsGoOilers -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")
Coyotes <- searchTwitter('#Yotes -filter:retweets',  since = "2017-10-04", until = "2017-12-15", n=500, lang="en")

Kings.df <- twListToDF(Kings)
GK.df <- twListToDF(GK)
Canucks.df <- twListToDF(Canucks)
Sharks.df <- twListToDF(Sharks)
Flames.df <- twListToDF(Flames)
Ducks.df <- twListToDF(Ducks)
Oilers.df <- twListToDF(Oilers)
Coyotes.df <- twListToDF(Coyotes)

Kings.df$teamname <- 'Los Angeles Kings'
GK.df$teamname <- 'Vegas Golden Knights'
Canucks.df$teamname <- 'Vancouver Canucks'
Sharks.df$teamname <- 'San Jose Sharks'
Flames.df$teamname <- 'Calgary Flames'
Ducks.df$teamname <- 'Anaheim Ducks'
Oilers.df$teamname <- 'Edmonton Oilers'
Coyotes.df$teamname <- 'Arizona Coyotes'


pacific <- rbind(Kings.df, GK.df, Canucks.df, Sharks.df, Flames.df, Ducks.df, Oilers.df, Coyotes.df)

# Replace @UserName
pacific$text <- gsub("@\\w+", "", pacific$text)

pacific$text <- sapply(pacific$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

pacific$text <- tolower(pacific$text)


# Replace blank space (“rt”)
pacific$text <- gsub("rt", "", pacific$text)

# Remove links

pacific$text <- gsub("http[^[:space:]]*", "", pacific$text)

pacific$text <- gsub("http[[:alnum:][:punct:]]*", "", pacific$text) 

pacific$text <- gsub("http\\w+", "", pacific$text)


# Remove punctuation
pacific$text <- gsub("[[:punct:]]", "", pacific$text)

# Remove AMP 
pacific$text <- gsub("amp", "", pacific$text)



# Remove tabs
pacific$text <- gsub("[ |\t]{2,}", " ", pacific$text)

# Remove blank spaces at the beginning
pacific$text <- gsub("^ ", "", pacific$text)

# Remove blank spaces at the end
pacific$text <- gsub(" $", "", pacific$text)

CleanPacific <- pacific

save(CleanPacific, file ='pacific.RData')

#Create data structure with all Tweets combined
alltweets <- rbind(CleanAtlantic, CleanMet, CleanCentral, CleanPacific)

#Save into RData
save(alltweets, file='alltweets.RData')

rm(list = ls())

###Filtering and Cleaning###



#Load necessary libraries 
x <- c("tidyverse", "tidytext", "purrr", "twitteR", "ROAuth", "devtools", 
       "stringr", "RColorBrewer", "wordcloud", "gridExtra", "tm", 
       "sp", "RgoogleMaps", "ggmap", "maptools", "tigris", "leaflet", 
       "webshot", "RCurl", "reshape2", "scales", "kableExtra", "knitr");

lapply(x, library, character.only = TRUE);

#Load data from Mining file

load('atlantic.RData')
load('metropolitan.RData')
load('central.RData')
load('pacific.RData')

#Recreate each team variable using pipe operator and filter 

#ATLANTIC TEAMS
Lightning <-CleanAtlantic %>% filter(teamname == 'Tampa Bay Lightning')
Leafs <- CleanAtlantic %>% filter(teamname == 'Toronto Maple Leafs')
Habs <- CleanAtlantic %>% filter(teamname == 'Montreal Canadiens')
Bruins <- CleanAtlantic %>% filter(teamname == 'Boston Bruins')
RedWings <- CleanAtlantic %>% filter(teamname == 'Detroit Red Wings')
Sens <- CleanAtlantic %>% filter(teamname == 'Ottawa Senators')
Panthers  <- CleanAtlantic %>% filter(teamname == 'Florida Panthers')
Sabres <- CleanAtlantic %>% filter(teamname == 'Buffalo Sabres')

#MET TEAMS

Devils <- CleanMet %>% filter(teamname == 'New Jersey Devils')
BlueJackets <- CleanMet %>% filter(teamname == 'Columbus Blue Jackets')
Caps <- CleanMet %>% filter(teamname == 'Washington Capitals')
Islanders <- CleanMet %>% filter(teamname == 'New York Islanders')
Pens <- CleanMet %>% filter(teamname == 'Pittsburgh Penguins')
Rangers <- CleanMet %>% filter(teamname == 'New York Rangers')
Canes <- CleanMet %>% filter(teamname == 'Carolina Hurricanes')
Flyers <- CleanMet %>% filter(teamname == 'Philadelphia Flyers')

#CENTRAL TEAMS

Preds <- CleanCentral %>% filter(teamname == 'Nashville Predators')
Jets <- CleanCentral %>% filter(teamname == 'Winnipeg Jets')
Blues <- CleanCentral %>% filter(teamname == 'St. Louis Blues')
Stars <- CleanCentral %>% filter(teamname == 'Dallas Stars')
Wild <- CleanCentral %>% filter(teamname == 'Minnesota Wild')
Hawks <- CleanCentral %>% filter(teamname == 'Chicago Blackhawks')
Avalanche <- CleanCentral %>% filter(teamname == 'Colorado Avalanche')

#PACIFIC TEAMS
Kings <- CleanPacific %>% filter(teamname == 'Los Angeles Kings')
GK <- CleanPacific %>% filter(teamname == 'Vegas Golden Knights')
Canucks <- CleanPacific %>% filter(teamname == 'Vancouver Canucks')
Sharks <- CleanPacific %>% filter(teamname == 'San Jose Sharks')
Flames <- CleanPacific %>% filter(teamname == 'Calgary Flames')
Ducks  <- CleanPacific %>% filter(teamname == 'Anaheim Ducks')
Oilers <- CleanPacific %>% filter(teamname == 'Edmonton Oilers')
Coyotes <- CleanPacific %>% filter(teamname == 'Arizona Coyotes')

#Separate teams into "Good" and "Bad"
#Good teams are either first or second in their division
#Bad teams are either last or second to last in their division
#Neutral teams are those who are not in the top 2 or bottom 2 of their divisions

BadTweets <- rbind(Sens, Sabres, Canes, Flyers, Hawks, Avalanche, Oilers, Coyotes)
GoodTweets <- rbind(Kings, GK, Lightning, Leafs, BlueJackets, Caps, Blues, Preds)
NeutralTweets <- rbind(Devils, Jets, Habs, Bruins, RedWings, Islanders, Pens, Stars, Wild, Canucks,
                       Sharks, Flames, Ducks, Rangers, Panthers)

#Save Bad teams and Good teams as RData
save(BadTweets, file = 'badtweet.RData')
save(GoodTweets, file = 'goodtweet.RData')
save(NeutralTweets, file = 'neutraltweet.RData')

#adding stopwords (common on twitter, search terms, teamnames/cities etc)
addedwords <- c("tweet", "via", "use", "see", "used", "retweet", "follow", 
                '#VegasBorn', '#GoJetsGo', '#TMLtalk', '#NHLBruins', '#Sens', 
                '#NJDevils', '#FlaPanthers', '#Preds', '#GoStars', '#GoBolts', 
                '#SJSharks', '#CofRed', '#GoAvsGo', '#CBJ', '#AllTogetherNowSTL', 
                '#Sabres', '#NYR', '#Yotes', '#LetsGoDucks', '#GoHabsGo', '#Redvolution', 
                '#LetsGoFlyers', '#GoKingsGo', '#mnwild', '#Canucks', '#LGRW', '#Isles', 
                '#LetsGoOilers', '#LetsGoPens', '#ALLCAPS', '#Blackhawks', 'vegas', 'golden',
                'knights', 'jets', 'winnipeg', 'toronto', 'maple', 'leafs', 'san', 'jose', 
                'sharks', 'ottawa', 'senators', 'boston', 'bruins', 'jersey', 'devils', 
                'florida', 'panthers', 'nashville', 'predators', 'dallas', 'stars', 'tampa', 
                'bay', 'lightning', 'canadiens', 'montreal', 'habs', 'calgary', 'flames', 'avalanche', 
                'colorado', 'columbus', 'blue', 'jackets', 'stl', 'buffalo', 'rangers','coyotes', 
                'ducks', 'anaheim', 'arizona', 'red', 'wings', 'islanders', 'york', 'oilers', 
                'edmonton', 'detroit', 'vancouver', 'minnesota', 'wild', 'kings', 'los', 'angeles', 
                'flyers', 'philadelphia', 'penguins', 'pittsburgh', 'capitals', 'caps', 'washington', 
                'carolina','hurricanes', 'canes', 'hawks', 'chicago', 'blues', 'stlblues', 'lakings', 
                '1', '2', '3', '4', '5', '6', '7', '8', '9', '0')

#Removes all non-ASCII and punctuation
addedwords <- sapply(addedwords,function(row) iconv(row, "latin1", "ASCII", sub=""))
#Remove punctuation from all stop words
addedwords <- gsub("[[:punct:]]", "", addedwords)
#Make all stop words lowercase 
addedwords <- tolower(addedwords)

#Use with Corpus for wordcloud
#Removes tm package stopwords we want to keep in analysis
#Adds stopwords used 
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")), addedwords)
myStopwords<-myStopwords[!grepl("down", myStopwords)]
myStopwords<- myStopwords[!grepl("up", myStopwords)]
myStopwords<- myStopwords[!grepl("below", myStopwords)]
myStopwords<- myStopwords[!grepl("behind", myStopwords)]
myStopwords<- myStopwords[!grepl("ahead", myStopwords)]
myStopwords<- gsub("[[:punct:]]", "", myStopwords)

#Use with tidytext to create "custom_stop_words" frame
#This is used for sentiment analysis
data(stop_words)
custom_stop_words <- bind_rows(data_frame(word = addedwords, lexicon = c(rep("custom", 119))), stop_words)

custom_stop_words<- custom_stop_words[!grepl("down", custom_stop_words$word),]
custom_stop_words<- custom_stop_words[!grepl("up", custom_stop_words$word),]
custom_stop_words<- custom_stop_words[!grepl("below", custom_stop_words$word),]
custom_stop_words<- custom_stop_words[!grepl("behind", custom_stop_words$word),]
custom_stop_words<- custom_stop_words[!grepl("ahead", custom_stop_words$word),]
custom_stop_words$word <- gsub("[[:punct:]]", "", custom_stop_words$word)


#customizing sentiments from afinn skeleton, adjusting some values
customsentiments <- get_sentiments("afinn")
customsentiments[991, 2] <- 2
customsentiments[992, 2] <- 2
customsentiments[993, 2] <- 2
customsentiments[1687, 2] <- -3
customsentiments[2013, 2] <- 1
customsentiments[419, 1] <- 'lit'
customsentiments[419, 2] <- 3
customsentiments[270, 2] <- -4
customsentiments[271, 2] <- -4
customsentiments[2428, 2] <- 5
customsentiments[2429, 2] <- 5
customsentiments[2430, 2] <- 5
customsentiments[2439, 2] <- 5

#Adding new sentiment words with corresponding sentiment values 
word <- as.character(c('goal', 'rematch', 'powerplay', 'victory', 'lead', 'leading', 'scores', 
          'trail', 'trails', 'trailed', 'trailing', 'shot', 'shots', 'victorious', 
          'wrecked', 'wrecking', 'interference', 'interfere', 'interfering', 'offsides', 
          'first', 'last', 'bottom', 'score', 'down', 'below', 'up', 'ahead', 'behind', 
          'lose', 'top'))

score <- as.numeric(c(4, 2, 3, 5, 3, 3, 2, -3, -3, -3, -3, 1, 1, 5, -3, -3, -1, -1, -1, -1, 4, -4, 
           -4, 2, -2, -2, 3, 3, -3, -4, 4))


newvalent <- data.frame(word, score, stringsAsFactors = FALSE)

customsentiments <- rbind.data.frame(customsentiments, newvalent)


#save sentiment design for use later
save(custom_stop_words, myStopwords, customsentiments, file = "sentimentdesign.RData")

#Scoring by Team
#The following is done for each of the 31 teams, but will not be commented.

#Lightning

#Create data fram with each row being an individual tweet
Lightning1 <- data_frame(line=1:500, text = Lightning$text)
#Separate Tweets into individual words, and remove stopwords
Lightning2 <-  Lightning1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
#Create a new data frame which assigns sentiment value to each word
Lightningafinn <- Lightning2 %>% 
  inner_join(customsentiments)
Lightningafinn$latitude <- 27.94278
Lightningafinn$longitude <- -82.45194
Lightningafinn$team <- 'Tampa Bay Lightning'
#Create a variable of the average sentiment score
Lightning$score <- mean(Lightningafinn$score)
#Create a variable specifying the length (in characters) of each individual tweet
Lightning$length <-nchar(Lightning$text)


###REPEAT FOR ALL TEAMS BELOW (skip to line 914)

#Leafs

Leafs1 <- data_frame(line=1:500, text = Leafs$text)
Leafs2 <-  Leafs1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Leafsafinn <- Leafs2 %>% 
  inner_join(customsentiments)
Leafsafinn$latitude <- 43.64333
Leafsafinn$longitude <- -79.37917
Leafsafinn$team <- 'Toronto Maple Leafs'
Leafs$score <- mean(Leafsafinn$score)
Leafs$length <-nchar(Leafs$text)

#Habs


Habs1 <- data_frame(line=1:500, text = Habs$text)
Habs2 <-  Habs1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Habsafinn <- Habs2 %>% 
  inner_join(customsentiments)
Habsafinn$latitude <- 45.49611
Habsafinn$longitude <- -73.56944
Habsafinn$team <- 'Montreal Canadiens'
Habs$score <- mean(Habsafinn$score)
Habs$length <-nchar(Habs$text)

#Bruins


Bruins1 <- data_frame(line=1:500, text = Bruins$text)
Bruins2 <-  Bruins1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Bruinsafinn <- Bruins2 %>% 
  inner_join(customsentiments)
Bruinsafinn$latitude <- 42.36630
Bruinsafinn$longitude <- -71.06223
Bruinsafinn$team <- "Boston Bruins"
Bruins$score <- mean(Bruinsafinn$score)
Bruins$length <-nchar(Bruins$text)

#RedWings

RedWings1 <- data_frame(line=1:500, text = RedWings$text)
RedWings2 <-  RedWings1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
RedWingsafinn <- RedWings2 %>% 
  inner_join(customsentiments)
RedWingsafinn$latitude <- 42.32528
RedWingsafinn$longitude <- -83.05139
RedWingsafinn$team <- 'Detroit Red Wings'
RedWings$score <- mean(RedWingsafinn$score)
RedWings$length <-nchar(RedWings$text)


#Sens

Sens1 <- data_frame(line=1:500, text = Sens$text)
Sens2 <-  Sens1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Sensafinn <- Sens2 %>% 
  inner_join(customsentiments)
Sensafinn$latitude <- 45.29694
Sensafinn$longitude <- -75.92722
Sensafinn$team <- "Ottawa Senators"
Sens$score <- mean(Sensafinn$score)
Sens$length <-nchar(Sens$text)

#Panthers

Panthers1 <- data_frame(line=1:500, text = Panthers$text)
Panthers2 <-  Panthers1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Panthersafinn <- Panthers2 %>% 
  inner_join(customsentiments)
Panthersafinn$latitude <- 26.15833
Panthersafinn$longitude <- -80.32556
Panthersafinn$team <- "Florida Panthers"
Panthers$score <- mean(Panthersafinn$score)
Panthers$length <-nchar(Panthers$text)

#Sabres

Sabres1 <- data_frame(line=1:500, text = Sabres$text)
Sabres2 <-  Sabres1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Sabresafinn <- Sabres2 %>% 
  inner_join(customsentiments)
Sabresafinn$latitude <- 42.87500
Sabresafinn$longitude <- -78.87639
Sabresafinn$team <- "Buffalo Sabres"
Sabres$score <- mean(Sabresafinn$score)
Sabres$length <-nchar(Sabres$text)

#Preds

Preds1 <- data_frame(line=1:500, text = Preds$text)
Preds2 <-  Preds1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Predsafinn <- Preds2 %>% 
  inner_join(customsentiments)
Predsafinn$latitude <- 36.15917
Predsafinn$longitude <- -86.77861
Predsafinn$team <- "Nashville Predators"
Preds$score <- mean(Predsafinn$score)
Preds$length <-nchar(Preds$text)

#Jets

Jets1 <- data_frame(line=1:500, text = Jets$text)
Jets2 <-  Jets1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Jetsafinn <- Jets2 %>% 
  inner_join(customsentiments)
Jetsafinn$latitude <- 49.89278
Jetsafinn$longitude <- -97.14361
Jetsafinn$team <- "Winnipeg Jets"
Jets$score <- mean(Jetsafinn$score)
Jets$length <-nchar(Jets$text)

#Blues

Blues1 <- data_frame(line=1:500, text = Blues$text)
Blues2 <-  Blues1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Bluesafinn <- Blues2 %>% 
  inner_join(customsentiments)
Bluesafinn$latitude <- 38.62667
Bluesafinn$longitude <- -90.20250
Bluesafinn$team <- "St. Louis Blues"
Blues$score <- mean(Bluesafinn$score)
Blues$length <-nchar(Blues$text)

#Stars

Stars1 <- data_frame(line=1:500, text = Stars$text)
Stars2 <-  Stars1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Starsafinn <- Stars2 %>% 
  inner_join(customsentiments)
Starsafinn$latitude <- 32.79056
Starsafinn$longitude <- -96.81028
Starsafinn$team <- "Dallas Stars"
Stars$score <- mean(Starsafinn$score)
Stars$length <-nchar(Stars$text)

#Wild

Wild1 <- data_frame(line=1:500, text = Wild$text)
Wild2<-  Wild1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Wildafinn <- Wild2 %>% 
  inner_join(customsentiments)
Wildafinn$latitude <- 44.94472
Wildafinn$longitude <- -93.10111
Wildafinn$team <- "Minnesota Wild"
Wild$score <- mean(Wildafinn$score)
Wild$length <-nchar(Wild$text)

#Hawks

Hawks1 <- data_frame(line=1:500, text = Hawks$text)
Hawks2 <-  Hawks1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Hawksafinn <- Hawks2 %>% 
  inner_join(customsentiments)
Hawksafinn$latitude <-41.88056
Hawksafinn$longitude <- -87.67417
Hawksafinn$team <- "Chicago Blackhawks"
Hawks$score <- mean(Hawksafinn$score)
Hawks$length <-nchar(Hawks$text)

#Avalanche

Avalanche1 <- data_frame(line=1:500, text = Avalanche$text)
Avalanche2 <-  Avalanche1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Avalancheafinn <- Avalanche2 %>% 
  inner_join(customsentiments)
Avalancheafinn$latitude <- 39.74861
Avalancheafinn$longitude <- -105.00750
Avalancheafinn$team <- "Colorado Avalanche"
Avalanche$score <- mean(Avalancheafinn$score)
Avalanche$length <-nchar(Avalanche$text)


#BlueJackets

BlueJackets1 <- data_frame(line=1:500, text = BlueJackets$text)
BlueJackets2 <-  BlueJackets1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
BlueJacketsafinn <- BlueJackets2 %>% 
  inner_join(customsentiments)
BlueJacketsafinn$latitude<- 39.96928
BlueJacketsafinn$longitude<- -83.00611
BlueJacketsafinn$team <- "Columbus Blue Jackets"
BlueJackets$score <- mean(BlueJacketsafinn$score)
BlueJackets$length <-nchar(BlueJackets$text)


#Caps

Caps1 <- data_frame(line=1:500, text = Caps$text)
Caps2 <-  Caps1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Capsafinn <- Caps2 %>% 
  inner_join(customsentiments)
Capsafinn$latitude <- 38.89806
Capsafinn$longitude <- -77.02083
Capsafinn$team <- "Washington Capitals"
Caps$score <- mean(Capsafinn$score)
Caps$length <-nchar(Caps$text)


#Devils

Devils1 <- data_frame(line=1:500, text = Devils$text)
Devils2 <-  Devils1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Devilsafinn <- Devils2 %>% 
  inner_join(customsentiments)
Devilsafinn$latitude <- 40.73361
Devilsafinn$longitude <- -74.17111
Devilsafinn$team <- "New Jersey Devils"
Devils$score <- mean(Devilsafinn$score)
Devils$length <-nchar(Devils$text)


#Islanders

Islanders1 <- data_frame(line=1:500, text = Islanders$text)
Islanders2 <-  Islanders1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Islandersafinn <- Islanders2 %>% 
  inner_join(customsentiments)
Islandersafinn$latitude <- 40.72278
Islandersafinn$longitude <- -73.59056
Islandersafinn$team <- "New York Islanders"
Islanders$score <- mean(Islandersafinn$score)
Islanders$length <-nchar(Islanders$text)


#Rangers

Rangers1 <- data_frame(line=1:500, text = Rangers$text)
Rangers2 <-  Rangers1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Rangersafinn <- Rangers2 %>% 
  inner_join(customsentiments)
Rangersafinn$latitude <- 40.75056
Rangersafinn$longitude <- -73.99361
Rangersafinn$team <- "New York Rangers"
Rangers$score <- mean(Rangersafinn$score)
Rangers$length <-nchar(Rangers$text)


#Pens

Pens1 <- data_frame(line=1:500, text = Pens$text)
Pens2 <-  Pens1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Pensafinn <- Pens2 %>% 
  inner_join(customsentiments)
Pensafinn$latitude <- 40.43944
Pensafinn$longitude <- -79.98917
Pensafinn$team <- "Pittsburgh Penguins"
Pens$score <- mean(Pensafinn$score)
Pens$length <-nchar(Pens$text)


#Flyers

Flyers1 <- data_frame(line=1:500, text = Flyers$text)
Flyers2 <-  Flyers1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Flyersafinn <- Flyers2 %>% 
  inner_join(customsentiments)
Flyersafinn$latitude <- 39.90111
Flyersafinn$longitude <- -75.17194
Flyersafinn$team <- "Philadelphia Flyers"
Flyers$score <- mean(Flyersafinn$score)
Flyers$length <-nchar(Flyers$text)


#Canes

Canes1 <- data_frame(line=1:500, text = Canes$text)
Canes2 <-  Canes1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Canesafinn <- Canes2 %>% 
  inner_join(customsentiments)
Canesafinn$latitude <- 35.80333
Canesafinn$longitude <- -78.72194
Canesafinn$team <- "Carolina Hurricanes"
Canes$score <- mean(Canesafinn$score)
Canes$length <-nchar(Canes$text)


#Kings

Kings1 <- data_frame(line=1:500, text = Kings$text)
Kings2 <-  Kings1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Kingsafinn <- Kings2 %>% 
  inner_join(customsentiments)
Kingsafinn$latitude <- 34.04306
Kingsafinn$longitude <- -117.87667
Kingsafinn$team <- "Los Angeles Kings"
Kings$score <- mean(Kingsafinn$score)
Kings$length <-nchar(Kings$text)


#GK

GK1 <- data_frame(line=1:500, text = GK$text)
GK2 <-  GK1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
GKafinn <- GK2 %>% 
  inner_join(customsentiments)
GKafinn$latitude <- 36.1029
GKafinn$longitude <- -115.1784
GKafinn$team <- "Vegas Golden Knights"
GK$score <- mean(GKafinn$score)
GK$length <-nchar(GK$text)


#Canucks

Canucks1 <- data_frame(line=1:500, text = Canucks$text)
Canucks2 <-  Canucks1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Canucksafinn <- Canucks2 %>% 
  inner_join(customsentiments)
Canucksafinn$latitude <- 49.27778
Canucksafinn$longitude <- -123.10889
Canucksafinn$team <- "Vancouver Canucks"
Canucks$score <- mean(Canucksafinn$score)
Canucks$length <-nchar(Canucks$text)


#Sharks

Sharks1 <- data_frame(line=1:500, text = Sharks$text)
Sharks2 <-  Sharks1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Sharksafinn <- Sharks2 %>% 
  inner_join(customsentiments)
Sharksafinn$latitude <- 37.33278
Sharksafinn$longitude <- -121.90111
Sharksafinn$team <- "San Jose Sharks"
Sharks$score <- mean(Sharksafinn$score)
Sharks$length <-nchar(Sharks$text)


#Flames

Flames1 <- data_frame(line=1:500, text = Flames$text)
Flames2 <-  Flames1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Flamesafinn <- Flames2 %>% 
  inner_join(customsentiments)
Flamesafinn$latitude <- 51.03750
Flamesafinn$longitude <- -114.05194
Flamesafinn$team <- 'Calgary Flames'
Flames$score <- mean(Flamesafinn$score)
Flames$length <-nchar(Flames$text)


#Ducks

Ducks1 <- data_frame(line=1:500, text = Ducks$text)
Ducks2 <-  Ducks1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Ducksafinn <- Ducks2 %>% 
  inner_join(customsentiments)
Ducksafinn$latitude <- 33.80778
Ducksafinn$longitude <- -117.87667
Ducksafinn$team <- 'Anaheim Ducks'
Ducks$score <- mean(Ducksafinn$score)
Ducks$length <-nchar(Ducks$text)


#Oilers

Oilers1 <- data_frame(line=1:500, text = Oilers$text)
Oilers2 <-  Oilers1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Oilersafinn <- Oilers2 %>% 
  inner_join(customsentiments)
Oilersafinn$latitude <- 53.57139
Oilersafinn$longitude <- -113.45611
Oilersafinn$team <- 'Edmonton Oilers'
Oilers$score <- mean(Oilersafinn$score)
Oilers$length <-nchar(Oilers$text)


#Coyotes

Coyotes1 <- data_frame(line=1:500, text = Coyotes$text)
Coyotes2 <-  Coyotes1 %>% unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)
Coyotesafinn <- Coyotes2 %>% 
  inner_join(customsentiments)
Coyotesafinn$latitude <- 33.53194
Coyotesafinn$longitude <- -112.26111
Coyotesafinn$team <- "Arizona Coyotes"
Coyotes$score <- mean(Coyotesafinn$score)
Coyotes$length <- nchar(Coyotes$text)

###CREATION OF TEAM INFO STRUCTURE###

#Teamname 
TeamNames <- c('Tampa Bay Lightning', 'New Jersey Devils', 'Nashville Predators', 
               'Los Angeles Kings', 'Toronto Maple Leafs', 'Columbus Blue Jackets', 
               'Winnipeg Jets', 'Vegas Golden Knights', 'Montreal Canadiens', 'Boston Bruins', 
               'Detroit Red Wings', 'Ottawa Senators', 'New York Islanders',  'Pittsburgh Penguins',
               'St. Louis Blues', 'Dallas Stars',  'Minnesota Wild', 'Vancouver Canucks',  
               'San Jose Sharks', 'Calgary Flames', 'Anaheim Ducks',  'New York Rangers', 
               'Washington Capitals',  'Buffalo Sabres', 'Philadelphia Flyers', 'Colorado Avalanche', 
               'Arizona Coyotes',  'Florida Panthers',  'Chicago Blackhawks', 'Edmonton Oilers', 
               'Carolina Hurricanes')

#Number of twitter followers for official team Twitter account
Followers <- c(518000, 679000, 518000, 1150000, 1740000, 418000, 492000, 261000, 1500000, 
               1340000, 1210000, 551000, 477000, 1690000, 633000, 932000, 649000, 1020000, 
               774000, 584000, 556000, 1380000, 667000, 638000, 1430000, 423000, 329000, 340000, 
               2450000, 781000, 335000)

#Number of tweets from official team Twitter account
#Note: The official team twitter uses the official hashtag in most if not all tweets
NumTweets <- c(76200, 65100, 37300, 62000, 86300, 118000, 36100, 9391, 112000, 77200, 62600, 
               54900, 60200, 80900, 49900, 50100, 55600, 66800, 39600, 69100, 54400, 65900, 
               73300, 77200, 33000, 55800, 53000, 49600, 55900, 88700, 50600)

#Average sentiment score from sentiment analysis
avgSentimentScore <- c(Lightning$score[1], Devils$score[1], Preds$score[1], Kings$score[1],
                       Leafs$score[1], BlueJackets$score[1], Jets$score[1], GK$score[1], 
                       Habs$score[1], Bruins$score[1], RedWings$score[1], Sens$score[1], 
                       Islanders$score[1], Pens$score[1], Blues$score[1], Stars$score[1],
                       Wild$score[1], Canucks$score[1], Sharks$score[1], Flames$score[1],
                       Ducks$score[1], Rangers$score[1], Caps$score[1], Sabres$score[1], 
                       Flyers$score[1], Avalanche$score[1], Coyotes$score[1], Panthers$score[1],
                       Hawks$score[1], Oilers$score[1], Canes$score[1])

#Total sentiment score
totalSentimentScore <- c(sum(Lightningafinn$score), sum(Devilsafinn$score), sum(Predsafinn$score),
                         sum(Kingsafinn$score), sum(Leafsafinn$score), sum(BlueJacketsafinn$score), 
                         sum(Jetsafinn$score), sum(GKafinn$score), sum(Habsafinn$score), 
                         sum(Bruinsafinn$score), sum(RedWingsafinn$score), sum(Sensafinn$score), 
                         sum(Islandersafinn$score), sum(Pensafinn$score), sum(Bluesafinn$score), 
                         sum(Starsafinn$score), sum(Wildafinn$score), sum(Canucksafinn$score), 
                         sum(Sharksafinn$score), sum(Flamesafinn$score), sum(Ducksafinn$score), 
                         sum(Rangersafinn$score), sum(Capsafinn$score), sum(Sabresafinn$score), 
                         sum(Flyersafinn$score), sum(Avalancheafinn$score), sum(Coyotesafinn$score), 
                         sum(Panthersafinn$score), sum(Hawksafinn$score), sum(Oilersafinn$score), 
                         sum(Canesafinn$score))

#Power ranking, as of December 15 2017 from 1-31, (1 being the best), as assigned per team
#by ESPN. Typically seen as a good estimator of current team performance
PowerRanking <- c(1, 9, 3, 6, 7, 5, 4, 11, 24, 16, 28, 29, 10, 18, 2, 15, 12, 22, 
                  14, 20, 19, 13, 8, 30, 21, 25, 31, 27, 17, 26, 23)

#Number of points per team, (Number of Wins X 2 + Number of Overtime Losses)
#Hierarchy of points within division determines division ranking
Points <- c(48, 39, 44, 43, 41, 41, 41, 42, 32, 34, 29, 27, 37, 35, 44, 37, 37, 32, 37, 
            35, 35, 35, 41, 22, 33, 32, 19, 29, 37, 28, 31)

#'G' assigned for top 2 teams in each division, 'B' for bottom two teams in each division,
#'N' assigned for teams not at the top nor bottom..
Classifier <- c('G', 'N', 'G', 'G', 'G', 'G', 'N', 'G', 'N', 'N', 'N', 'B', 'N', 'N', 
                'G', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'G', 'B', 'B', 'B', 'B', 'N', 'B', 
                'B', 'B')

#Average length of Tweets (in characters) using the official hashtag
LengthofTweets <- c(mean(Lightning$length), mean(Devils$length), mean(Preds$length), 
                    mean(Kings$length), mean(Leafs$length), mean(BlueJackets$length), 
                    mean(Jets$length), mean(GK$length), mean(Habs$length), mean(Bruins$length), 
                    mean(RedWings$length), mean(Sens$length), mean(Islanders$length), 
                    mean(Pens$length), mean(Blues$length), mean(Stars$length), mean(Wild$length), 
                    mean(Canucks$length), mean(Sharks$length), mean(Flames$length), 
                    mean(Ducks$length), mean(Rangers$length), mean(Caps$length), mean(Sabres$length), 
                    mean(Flyers$length), mean(Avalanche$length), mean(Coyotes$length), 
                    mean(Panthers$length), mean(Hawks$length), mean(Oilers$length), mean(Canes$length))

#Division in which each team is a member of
Division <- c('Atlantic', 'Metropolitan', 'Central', 'Pacific', 'Atlantic', 'Metropolitan', 
              'Central', 'Pacific', 'Atlantic', 'Atlantic', 'Atlantic', 'Atlantic', 'Metropolitan', 
              'Metropolitan', 'Central', 'Central', 'Central', 'Pacific', 'Pacific', 'Pacific', 
              'Pacific', 'Metropolitan', 'Metropolitan',  'Atlantic', 'Metropolitan', 'Central',
              'Pacific', 'Atlantic', 'Central', 'Pacific', 'Metropolitan')

#Latitude of teams home stadium
Latitude <- c(27.94278, 40.73361, 36.15917, 34.04306, 43.64333, 39.96928, 49.89278, 
              36.1029, 45.49611, 42.36630, 42.32528, 45.29694, 40.72278, 40.43944, 38.62667, 
              32.79056, 44.94472, 49.27778, 37.33278, 51.03750, 33.80778, 40.75056, 38.89806, 
              42.87500, 39.90111, 39.74861, 33.53194, 26.15833, 41.88056, 53.57139, 35.80333)

#Longitude of teams home stadium
Longitude <- c(-82.45194, -74.17111, -86.77861, -117.87667, -79.37917, -83.00611, -97.14361, 
               -115.1784, -73.56944, -71.06223, -83.05139, -75.92722, -73.59056, -79.98917, 
               -90.20250, -96.81028, -93.10111, -123.10889, -121.90111, -114.05194, -117.87667, 
               -73.99361, -77.02083, -78.87639, -75.17194, -105.00750, -112.26111, -80.32556, 
               -87.67417, -113.45611, -78.72194)


#Addition of all previously specified variables into one holistic data frame
#Character vectors converted to numeric 
TeamInfo <- as.data.frame(cbind(TeamNames, Followers, NumTweets, avgSentimentScore, 
                                totalSentimentScore, PowerRanking, Points, Classifier, 
                                LengthofTweets, Division, Latitude, Longitude))
TeamInfo$Latitude <- as.numeric(as.character(TeamInfo$Latitude))
TeamInfo$Longitude <- as.numeric(as.character(TeamInfo$Longitude))
TeamInfo$PowerRanking <- as.numeric(as.character(TeamInfo$PowerRanking))
TeamInfo$avgSentimentScore <- as.numeric(as.character(TeamInfo$avgSentimentScore))
TeamInfo$Followers <-as.numeric(as.character(TeamInfo$Followers))
TeamInfo$totalSentimentScore <- as.numeric(as.character(TeamInfo$totalSentimentScore))
TeamInfo$LengthofTweets <- as.numeric(as.character(TeamInfo$LengthofTweets))
TeamInfo$NumTweets <- as.numeric(as.character(TeamInfo$NumTweets))
TeamInfo$Points <- as.numeric(as.character(TeamInfo$Points))

#Save TeamInfo data frame
save(TeamInfo, file='teaminfo.RData')

#Saving individual clean text file
save(Lightning1, Devils1, Kings1, Preds1, Leafs1, BlueJackets1, Jets1, 
     GK1, Habs1, Bruins1, RedWings1, Sens1, Islanders1, Pens1, Blues1, 
     Stars1, Wild1, Canucks1, Sharks1, Flames1, Ducks1, Rangers1, Caps1, 
     Sabres1, Flyers1, Avalanche1, Coyotes1, Panthers1, Hawks1, Oilers1, 
     Canes1, addedwords, customsentiments, myStopwords, file= 'WordCloudsbyTeam/teamtext.RData')

#Saving sentiment score analysis for each team.
save(Lightningafinn, Devilsafinn, Kingsafinn, Predsafinn, Leafsafinn, 
     BlueJacketsafinn, Jetsafinn, GKafinn, Habsafinn, Bruinsafinn, RedWingsafinn, 
     Sensafinn, Islandersafinn, Pensafinn, Bluesafinn, Starsafinn, Wildafinn, 
     Canucksafinn, Sharksafinn, Flamesafinn, Ducksafinn, Rangersafinn, Capsafinn, 
     Sabresafinn, Flyersafinn, Avalancheafinn, Coyotesafinn, Panthersafinn, Hawksafinn, 
     Oilersafinn, Canesafinn, file= 'teamsentiments.RData')

rm(list=ls())

load('alltweets.RData')
load('badtweet.RData')
load('goodtweet.RData')
load('neutraltweet.RData')
load('teaminfo.RData')
load('teamsentiments.RData')
load('sentimentdesign.RData')
