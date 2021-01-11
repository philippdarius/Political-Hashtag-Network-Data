
setwd("Enter_your_working_directory")
install.packages("RCurl")

install.packages(c("devtools", "rjson", "bit64", "httr"))

y
#RESTART R session!

## install devtools package if it's not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

## install dev version of rtweet from github -> This is the person who should get the probs
devtools::install_github("mkearney/rtweet")

## load rtweet package
install.packages("rtweet")
library(rtweet)


## when your twitter development account has been activated you receive your key-code to paste here..
library(twitteR)
consumer_key <- "xxxxxx" # paste your Twitter consumer key
consumer_secret <- "xxxxxx" # paste your consumer secret code
access_token <- "xxxxxx" # paste your access_token # avoids the browser authentication dance
access_secret <- "xxxxxx" # paste your access secret # avoids the browser authentication dance
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

1

# Collect hashtag (max. 18k, most recent tweets using the hashtag within the last 10-14 days)
AfD_tweets <- searchTwitter("#AfD", n = 18000, lang="de")


df_AfD <- twListToDF(AfD_tweets)


save(df_AfD, file = "AfD_tweets.Rda") # choose filename


#####       CREATING A NETWORK OF RETWEETS AND MENTIONS
# code taken from https://www.r-bloggers.com/playing-with-twitter-data/

# PACKAGES
#install.packages('twitteR', 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm')
#install.packages("dplyr")
library(dplyr)
#install.packages("network")
library(network)
#install.packages("sna")
library(sna)


#lapply(c('twitteR', 'dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm'),
#library, character.only = TRUE)


d <- # use the data you just collected!


# now create edgelists of retweet network and mentioning network to graph and analyse (or export to Gephi)
# in Gephi large networks (>2000 nodes) are much easier to explore and graph

###########################################
#####      RETWEET NETWORK        ########

# Split into retweets and original tweets
sp = split(d, d$isRetweet)
orig = sp[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

# Adjust retweets to create an edgelist for network
el = as.data.frame(cbind(sender = tolower(rt$sender), 
                         receiver = tolower(rt$screenName)))
el = count(el, sender, receiver) 
rtnet = network(el, matrix.type = 'edgelist', directed = TRUE, 
                ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs = rtnet %v% 'vertex.names'
vlabs[degree(rtnet, cmode = 'outdegree') == 0] = NA



df_el <- el

names(df_el)[1] <- "Target"
names(df_el)[2] <- "Source"

# create Weight column as a copy of n
df_el$Weight <- df_el$n


write.csv(df_el, file = "FlattenTheCurve_edgelist_rtnet_060420.csv", row.names = FALSE)



#col3 was defiend earleir
col3 = RColorBrewer::brewer.pal(3, 'Paired') 

#plot network
par(mar = c(0, 0, 3, 0))

plot(rtnet, label = vlabs, label.pos = 5, label.cex = .8, 
     vertex.cex = log(degree(rtnet)) + .5, vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#CSU Retweet Network')

plot(rtnet, label = vlabs, label.pos = 5, label.cex = 0, 
     vertex.cex = log(degree(rtnet)) + .5, vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#CSU Retweet Network w/o labels')




###########################################
#####    MENTIONING NETWORK          ######

# Extract who is mentioned in each tweet. 
# Someone has probably written a function to do this, but it's a fun regex problem.

mentioned = 
  lapply(orig$text, function(tx) {
    matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
    sapply(seq_along(matches), function(i) 
      substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
  })

# Make an edge from the tweeter to the mentioned, for each mention
mentionEL = 
  lapply(seq_along(orig$text), function(i) {
    # If the tweet didn't have a mention, don't make edges
    if(mentioned[[i]] == '')  
      return(NULL)
    # Otherwise, loop over each person mentioned, make an edge, and rbind them
    lapply(mentioned[[i]], function(m)
      c(sender = orig$screenName[i], receiver = m)) %>%
      do.call(rbind, .) %>% as.data.frame()
  }) %>% 
  do.call(rbind, .) %>%
  count(tolower(sender), tolower(receiver))

# Make the network
mentionNet = network(mentionEL, matrix.type = 'edgelist', directed = TRUE, 
                     ignore.eval = FALSE, names.eval = 'num')

# plot the network  
plot(mentionNet, displaylabels = TRUE, label.pos = 5, label.cex = .8, 
     vertex.cex = degree(mentionNet, cmode = 'indegree'), vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#CSU Mention Network')

#  PROBLEM two or three nodes are too big for the graph
plot(mentionNet, displaylabels = TRUE, label.pos = 5, label.cex = 0, 
     vertex.cex = degree(mentionNet, cmode = 'indegree')/5, vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#CSU Mention Network')


#size for in and outdegree -> does it really change the graph? (it should!)
plot(mentionNet, displaylabels = TRUE, label.pos = 5, label.cex = 0.5, 
     vertex.cex = degree(mentionNet, cmode = 'indegree')/10, vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#CSU Mention Network (Indegree)')

plot(mentionNet, displaylabels = TRUE, label.pos = 5, label.cex = 0.5, 
     vertex.cex = degree(mentionNet, cmode = 'outdegree')/10, vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#CSU Mention Network (Outdegree)')

save(mentionNet, file = "CSU_mention_network.Rda")


#############################################################################
###### Descriptive Analysis -> Who are the mos mentioned people (in a table)
# a few users have been mentioned a lot -> who is it? 
#count(mentionEL$`tolower(receiver)`)

class(mentionEL)
attach(mentionEL)
count(n,"tbl")



mention_counts <- mentionEL$`tolower(receiver)` %>%
  count(mentionEL$`tolower(receiver)`, sort = TRUE) ########### does not work !!! ahh!!!

# without names
degree_mention <- degree(mentionNet,"indegree")
hist(degree_mention)


#Centrality Scores
network.degree(mentionNet)
