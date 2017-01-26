# Author : Elise Tancoigne - elise.tancoigne@unige.ch
# License: CC-by 4.0 (https://creativecommons.org/licenses/by/4.0/)
# January 2017

# This R script allows to 
  # - retrieve Twitter users accounts on the topic of citizen science
  # - add supplementary qualitative variables to them (gender, speciality...)
  # - make some basic descriptive statistics on these variables
  # - build the network of followers/followees links within the dataset.
# See http://citizensciences.net/2017/01/26/4-things-twitter…-citizen-science/ for the resulting analysis of the data.


##### PREAMBLE: METHODOLOGICAL LESSONS WE LEARNT FROM WORKING WITH TWITTER 
## Lesson #1: access to Twitter data is not straightforward
  # - if you like to protect your privacy, you will probably hate giving your phone number 
    # to get the access to Twitter’s API.  
  # - your dataset have to be small, or rate limitations will dramatically increase 
    # your download rate (or you’ll have to buy a direct access through Twitter Firehose)
## Lesson #2: network analysis is not always the best way to make sense of the data
  # - If you use only one keyword or really similar keywords for retrieval, 
    # you will probably end up with a highly interconnected network of accounts
  # - the network does not speak for itself. You will have to dig into the biographical data, 
    # make categories, identify the nodes before you can tell anything on it.


##################################################
##### SET THE ENVIRONMENT

# install the required packages
library(rtweet) # to get data from Twitter
# OR the most recent:
devtools::install_github("mkearney/rtweet")
library(rgexf) # to export the network and open it in Gephi

# save (or load) your working environment
setwd(dir = "~/path/to/myworkingdirectory/")
load("myenvironment.RData")
# save.image("myenvironment.RData")


##################################################
###### CREATE CONNECTION WITH TWITTER API

vignette("tokens") # provides documentation for creating connections

# go on https://apps.twitter.com/app/12124871/settings
consumerKey <- "the_consumer_key_provided_by_twitter"
consumerSecret <- "the_consumer_secret_provided_by_twitter"
token <- create_token(app = "my_application_name", consumerKey, consumerSecret)


##################################################
####### SEARCH USERS
### exact match not supported, nor concatenation (see documentation)
### see also the thread on https://github.com/mkearney/rtweet/issues/29

# get users and their data
# (check from time to time that you did not reach your download limit) 
# (wait the necessary time if the answer is yes)
rate_limit(token, query="users/search")

users <- search_users("citsci", n=10000, verbose = T)
users <- rbind(users, search_users("citizenscience", n=10000, verbose = T))
users <- rbind(users, search_users("citizensciences", n=10000, verbose = T))
users <- rbind(users, search_users("citizen science", n=10000, verbose = T))
users <- rbind(users, search_users("citizen sciences", n=10000, verbose = T))
# > 600 users. There are duplicates (e.g. people using "citsci" and "citizenscience" at the same time)
# and false positives (e.g. "citizen of the world, science addict")
# select only exact matches:
users <- users[grep("citsci|citizenscience|citizensciences|citizen science|citizen sciences", ignore.case = T, paste(users$screen_name, users$name, users$description)),]
# !!! don't forget to precise "ignore.case=T"
# 470 users, with duplicates
users <- unique(users) 
# 446 unique users
users <- users[, -c(19,20)] # delete url and descr_url (encoded as a list)


##################################################
# CODE THE RETRIEVED ACCOUNTS AND MAKE SOME STATS
## data are coded in Open Office calc with the following variables (see blog post): 
# type 1 ("individual" or "organization"), 
# type 2 (e.g. "scientist", "CS project"...), 
# field (e.g. "DIY", "conservation"), 
# gender ("male" or "female" or "unknown")
write.csv(users, "../twitter/446nodes_to_code.csv", row.names = F)
## !!! be careful - ids should be imported as characters in open office - otherwise they are rounded with scientifi notation

codes <- read.csv("~/Documents/THEMES_Citizen_Sciences/twitter/446nodes_to_code.csv", numerals = "no.loss", stringsAsFactors = F)
# % of individual vs. organizations
round(prop.table(table(codes$type1))*100, 0)

# individuals' professions (%)
round(prop.table(table(codes$type2[codes$type1=="individual"]))*100,0)
# specialities of scientists or science-related professions (%)
round(prop.table(table(codes$field[codes$type2=="scientist"|codes$type2=="science"]))*100,0)
# specialities of outreach professions (%)
round(prop.table(table(codes$field[codes$type2=="outreach"]))*100,0)

# subtypes of organizations (%)
round(prop.table(table(codes$type2[codes$type1=="organization"]))*100,0)
# specialities of citizen science projects (%)
round(prop.table(table(codes$field[codes$type2=="CS_project"]))*100)

# % of men/women
prop.table(table(codes$gender[codes$type1=="individual"]))*100
# % of men/women among scientists
prop.table(table(codes$gender[codes$type2=="scientist"]))*100
# % of men/women across professions
round(prop.table(table(codes[codes$type1=="individual", c("gender", "type2")]),2)*100,0)
# % of men/women across specilities
round(prop.table(table(codes[codes$type1=="individual", c("gender", "field")]),2)*100,0)


# build a dataframe of users' data + their codes
codes <- merge(users, codes[,c("user_id", "type1", "type2", "field", "gender")], by="user_id")


##################################################
# BUILD THE LIST OF FOLLOWERS/FOLLOWEES LINKS WITHIN THESE 446 USERS
# If A --> B, the link can be traced through A's followees or through B's followers
# so we'll only get the followers for each account 

data <- users

# the function get_followrs is limited to 75000 responses
max(data$followers_count) # max in our dataset < 75000 so no need to worry about limits in responses
options(timeout=1200) # we'll wait up to 20 min if the website does not respond

# create an empty dataframe for the retrieved links of followers
links <- data.frame(source="", target="", stringsAsFactors = F)

# record starting time
starting_time = Sys.time();cat(paste("Starting at", starting_time, sep=" "))

# for each user
for(i in 1:dim(data)[1])  {
  # display where we are in the loop
  cat(paste("User #", i, " ---> Start\n", sep=""))  
  
  # build followers links
  if(data$followers_count[i]!=0){
    
    followersID <- get_followers(data$screen_name[i])$ids
    # the function returns a vector of length=75000
    # need to get rid of NA
    followersID <- followersID[which(!is.na(followersID))]
    # add the new links (source = followers, target = selected nodes)
    links <- rbind(links,
                   data.frame(source=followersID, target=data$user_id[i])) 
    # (you can also choose to keep only those in the dataset.)
  }
  rm(followersID)
  
  # display where we are in the loop
  cat(paste("User #", i, " ---> Done\n", sep=""))
  
  # handle twitter rate limits
  limit <- rate_limit(token, query="followers/ids") 
  # if the limit is reached
  if(limit$remaining==0){
    # display a message
    cat(paste("Waiting", round(limit$reset, 2), "mins for rate limitation\n", sep=" "))
    # wait for the necessary time
    Sys.sleep(limit$reset*60+1)
  }
  rm(limit)
  
  # restart the loop for the next user  
} ; rm(i, start)

# at the end, print total execution time
cat(paste("Time elapsed:", Sys.time()-starting_time, sep=" ")) ; rm(starting_time) 

# errors may occur for a few accounts; re-launch the loop after them (#119, #165)

##################################################
# BUILD THE GEXF TO VISUALIZE THE NETWORK IN GEPHI

# build the data frame of nodes, with nodes attributes
data <- codes
nodes <- data.frame(id=data$user_id, label=data$name, stringsAsFactors = F)
nodes$label <- gsub("&", "and", nodes$label) # "For some reason, write.gexf fails to properly handle & symbols", see http://gopalakrishna.palem.in/iGraphExport.html
nodes.att <- subset(data, select=c(description, followers_count, friends_count, statuses_count, url, location, lang, screen_name, type1, type2, field, gender))
nodes.att$description <- gsub("&", "and", nodes.att$description) 
nodes.att$location <- gsub("&", "and", nodes.att$location)

# add dates to the nodes, so that the network can by visuallized dynamically
node.Dynamic <- data.frame(start=as.Date(data$created_at, format="%Y-%m-%d"), 
                           end=as.Date("2016.10.31", format="%Y.%m.%d"))
                           # end should be your download date

# select only the edges that connect our 446 users
# (i.e. remove links$source that are not in users$user_id)
mut_links <- links[links$source %in% users$user_id,]
mut_links <- mut_links[mut_links$target %in% users$user_id,]

# write the file.
x1 <- write.gexf(output = "446nodes_CS.gexf", nodes = nodes, edges = mut_links, 
                 nodesAtt = nodes.att, nodeDynamic = node.Dynamic, tFormat="Date",
                 keepFactors=F, defaultedgetype = "directed")

# open it in Gephi (see http://gephi.org).
