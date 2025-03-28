# london council election exercise script stub ---------------
library(tidyverse)
library(here)
library(modelr)
library(numbers)

df <- read.csv(here("exercises", "ws05-tableData", "election exercise", "london_council_election_2014_ward.csv"),fileEncoding = "UTF-8-BOM")
df$number_votes <- as.numeric(df$number_votes)


# create the position for each candidate in alphabetical order per ward and per party - remove candidates with a rank higher than three
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

# create countable numbers based on elected flag
db <- df %>%
  filter(party %in% c("CON", "LAB", "LD")) %>%
  group_by(Borough_name, party, position_within) %>%
  summarise(elected = sum(ifelse(elected_flag == "Yes", 1, 0)))

#create the councillors/candidates in ward per party
df<- df %>%
  select(Ward_code,party) %>%
  group_by(Ward_code,party) %>%
  summarize(numOfPartyCouncillorsOnBallot=n()) %>%
  merge(df)
  
# create vote per party per ward, limit parties to CON/LAB/LD and create bias
df<- df %>%
  group_by(Ward_code, party) %>%
  summarize(VotesForParty=sum(number_votes)) %>% 
  right_join(df) %>% 
  ungroup() %>%
  filter(party %in% c("CON", "LAB", "LD")) %>%
  mutate(potentialBias=number_votes/VotesForParty-1/numOfPartyCouncillorsOnBallot)
  

# create a data grid with all combinations of borough and party, in case some parties were not present in the borough at all or with fewer than three candidates
wardRowsCols <- df %>% 
  select(Borough_name,Ward_name,Borough_code,Ward_code) %>% 
  arrange(Borough_name,Ward_name) %>% 
  select(-c(Borough_name,Ward_name,)) %>% 
  unique() %>% 
  mutate(WardNum=1:n(),
      WardCol= WardNum %% 30,
      WardRow= div(WardNum, 30)+1)

#limit data to three main parties and only the first three position_within per party
wardgrid <- df %>%
  arrange(Borough_name,Ward_name) %>%
  filter(party %in% c("CON", "LAB", "LD")) %>%
  data_grid(Ward_code, party,position_within=c(1,2,3)) %>%
  left_join(wardRowsCols)


boroughgrid <- df %>%
  arrange(Borough_name,Ward_name) %>%
  filter(party %in% c("CON", "LAB", "LD")) %>%
  data_grid(Borough_code, party,position_within=c(1,2,3))


db %>% 
  mutate(position_within=as.factor(position_within)) %>% 
  mutate(position_within=factor(position_within, levels=rev(levels(position_within)))) %>%
  ggplot(aes(x=party,y = elected))+geom_col(aes(fill=position_within), position = "dodge")+coord_flip()+scale_fill_brewer()+facet_wrap(~Borough_name) +theme_bw()+ scale_fill_manual(values = c("orange","yellow", "blue")) 

summaryBiasByWard <-df %>% 
  right_join(wardgrid) 

summaryBiasByWard <-df %>% 
  select(Borough_code,Borough_name,Ward_code,Ward_name) %>%
  unique() %>%
  right_join(summaryBiasByWard, multiple = "all")

# Try a plot below for summaryBiasByWard e.g. with geom_tile() that shows all data. 
# The code above provides you with the a way to show all wards by creating a row column position for each ward.
# To avoid overplotting you will need to use facet_grid() e.g. by faceting by position_within and party.  
# In order to see the small differences in the bias you need to figure out a good colour scheme, which also takes care of missing (na) values by making them less prominent. 

ggplot(summaryBiasByWard,aes(x= WardCol,y= WardRow,fill=potentialBias))+
  geom_tile()+
  facet_grid(cols = vars(position_within),rows = vars(party))+
  scale_fill_gradient2(midpoint = 0,
                     low = 'green2',
                     mid = 'yellow',
                     high = 'red3',
                     na.value = 'black')



  



