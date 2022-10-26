# london council election exercise script stub ---------------


library(tidyverse)
library(here)
library(modelr)

df <- read.csv(here("exercises", "ws06-tableData", "election exercise", "london_council_election_2014_ward.csv"),fileEncoding = "UTF-8-BOM")
df$number_votes <- as.numeric(df$number_votes)


# create the position for each candidate in alphabetical order per ward and party
df <- df %>%
  group_by(Ward_code, party) %>%
  arrange(candidate) %>%
  mutate(position_within = 1:n()) %>%
  arrange(as.numeric(number_votes)) %>%
  mutate(votingRankInParty = 1:n()) %>%
  ungroup()%>%
  filter(votingRankInParty<4)

# create the position for each candidate in alphabetical order per ward independent of party
df <- df %>%
  group_by(Ward_code) %>%
  arrange(candidate) %>%
  mutate(position_overall = 1:n()) %>% 
  ungroup()


#starting letter of candidate
df$startingLetterCandidate <- substr(df$candidate,1,1)

df <- df %>% 
  group_by(Ward_code, party) %>%
  dplyr::summarise(partyVotesInWard=sum(as.numeric(number_votes))) %>%
  ungroup() %>%
  merge(df)

# create countable numbers based on elected flag
db <- df %>%
  filter(party %in% c("CON", "LAB", "LD")) %>%
  group_by(Borough_name, party, position_within) %>%
  summarise(elected = sum(ifelse(elected_flag == "Yes", 1, 0)))

df<- df %>%
  group_by(Ward_code, party) %>%
  summarize(VotesForParty=sum(number_votes)) %>% 
  right_join(df) %>% 
  ungroup() %>%
  filter(party %in% c("CON", "LAB", "LD")) %>%
  mutate(differenceFromExpected=number_votes/VotesForParty-1/Number.of.councillors.in.ward)
  

# create a data grid with all combinations of borough and party, in case some parties were not present in the borough
gr <- df %>%
  filter(party %in% c("CON", "LAB", "LD")) %>%
  data_grid(Borough_code, party,position_within=c(1,2,3))

ggplot(db)+geom_col(aes(x=c(party),y = elected, fill=factor(-position_within)), position = "dodge")+coord_flip()+facet_wrap(~Borough_name)

summaryBiasByBorough <-df %>% 
  group_by(Borough_name,position_within,party) %>%
  summarize(meanBias=sum(number_votes)/sum(VotesForParty)-1/3)


