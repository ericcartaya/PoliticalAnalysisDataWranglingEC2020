library(tidyverse)
library(tidytext)
library(ggplot2)
library(httr)
library(XML)
library(lubridate)
library(pracma)

yeartodate <- function(year)# A function to get the accurate number of days since 1970 to convert into the date format.
{
  day1970 <- ((year-1970)*365) + floor((year-1970)/4)# The math for the conversion.
  day1970 <- day1970 + 320# I chose November 15th as that is the last possible day after the presidential elections taking everything into account, but I got the date wrong so November 16th will have to suffice.
}

partyPresidentTable <- read.table("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/42MVDX/MFU99O", sep="\t", header=TRUE)# Using the MIT Election Lab's data (which is held by Harvard for some reason) I got the data for every popular vote for every presidential election since 1976.

partyPresidentTable <- partyPresidentTable %>% mutate(date = as_date(yeartodate(year))) %>% select(-year) %>% group_by(date, party) %>% summarise(candidatevotes = sum(candidatevotes), totalvotes = sum(totalvotes))# Changes the dataset to give the national popular vote by year and by party, and to have an actual date associated with the elections.
partyNames <- partyPresidentTable %>% group_by(party) %>% select(party) %>% distinct()# Gets a list of the political parties. This one was more for me than anything else.
consolidatedPresidentTableDem <- partyPresidentTable %>% filter(party == "democrat") %>% rename(demvotes = candidatevotes) %>% select(-party)# Gets the number of votes that the Dems got in each election.
consolidatedPresidentTableRep <- partyPresidentTable %>% filter(party == "republican") %>% rename(repvotes = candidatevotes) %>% select(-party)# Same but for the GOP.

consolidatedPresidentTable <- full_join(consolidatedPresidentTableDem, consolidatedPresidentTableRep, by = "date") %>% select(-totalvotes.x) %>% rename(totalvotes = totalvotes.y) %>% mutate(thirdpartyvotes = totalvotes - demvotes - repvotes)# Combines the Dems and GOP so that we can use the total votes cast to see the number of votes that went to third party and independent candidates. Gather was also used so that the votes were all in one row.

ggplot(data = consolidatedPresidentTable, aes(x = date)) + geom_line(aes(y = repvotes, color = "darkred")) + geom_line(aes(y = thirdpartyvotes, color = "green")) + geom_line(aes(y = demvotes, color = "steelblue")) + labs(title ="Votes Each Election", x = "Year", y = "Votes")# Printed a graph with the election resuslts for the combine 3rd parties. Note the spikes being Ross Perot in 1992 and basically anyone in the 2016 election.

ggplot(data = partyPresidentTable, aes(x = date, y = candidatevotes, fill = party)) + geom_line(aes(x = date, y = candidatevotes, fill = party)) + scale_y_continuous(name="Votes", limits=c(100000, 70000000)) + labs(title ="Votes Each Election", x = "Year", y = "Votes")







writ <- GET(url = "https://news.gallup.com/poll/15370/party-affiliation.aspx")# This was to scrape data from the Gallup Polling website, and yes this was the best I could do for a second dataset from a second website.
writXML <- htmlParse(content(writ, as = "text"))# All of this was to get the scraped table into a workable dataset.
writTable <- getNodeSet(writXML, '//*[contains(concat( " ", @class, " " ), concat( " ", "figure-table", " " ))]')
rowGetter <-sapply(writTable, xmlValue)

gallupPol <- rowGetter[1]# All of this is to get the correct table and to remove all of the strings that are unusable.
gallupPol <- str_split(gallupPol, "\\r\\n")
gallup <- as.data.frame(gallupPol)# Converts the data into a dataframe.
gallup <- gallup %>% rename(x = c..In.politics..as.of.today..do.you.consider.yourself.a.Republican..a.Democrat.or.an.independent....) %>% filter(x != "%") %>% filter(x != " ") %>% filter(x != "Republicans") %>% filter(x != "Democrats") %>% filter(x != "Independents") %>% filter(x != "Trend since 2004") %>% filter(x != "In politics, as of today, do you consider yourself a Republican, a Democrat or an independent?") %>% filter(x != "") %>% filter(x != "Gallup")

gallupDem <- gallup[seq(4, nrow(gallup), 4), ] %>% as.data.frame()# Gets the correct percentages for each of the parties. 
gallupInd <- gallup[seq(3, nrow(gallup), 4), ] %>% as.data.frame()
gallupRep <- gallup[seq(2, nrow(gallup), 4), ] %>% as.data.frame()
gallupDat <- gallup[seq(1, nrow(gallup), 4), ] %>% as.data.frame()# This one gets the dates.

gallupDem <- gallupDem %>% rename('Democrats %' = '.')# This corrects the names.
gallupInd <- gallupInd %>% rename('Independents %' = '.')
gallupRep <- gallupRep %>% rename('Republicans %' = '.')
gallupDat <- gallupDat %>% rename(Date = '.')

gallup <- bind_cols(gallupDat, gallupRep)# Binds the tables together.
gallup <- bind_cols(gallup, gallupInd)
gallup <- bind_cols(gallup, gallupDem)

gallup <- gallup %>% mutate(`Republicans %` = as.numeric(as.character(`Republicans %`))) %>% mutate(`Independents %` = as.numeric(as.character(`Independents %`))) %>% mutate(`Democrats %` = as.numeric(as.character(`Democrats %`))) %>% mutate(Date = as_date(substr(gallup$Date, 0, str_locate(gallup$Date, "-")[,1]-1))) %>% gather("party", "percent", `Republicans %`, `Independents %`, `Democrats %`)# Fixes the dates and the parties so that all the percentages so that they are in one party.

ggplot(data = gallup, aes(x = Date, y = percent, color = party)) + geom_line(aes(x = Date, y = percent, color = party)) + scale_y_continuous(name="Percentage", limits=c(0, 50)) + labs(title ="Percentage of Population", x = "Year", y = "Percentage")# Shows tha popularity of these groups since 2007

gallup <- gallup %>% mutate(parInd = 0) %>% mutate(parRep = 0) %>% mutate(parDem = 0)

for (i in 1:356)
{
  gallup[[5]][i] = 1
}
for (i in 357:712)
{
  gallup[[4]][i] = 1
}
for (i in 713:1068)
{
  gallup[[6]][i] = 1
}

pm <- glm(percent~Date+parInd+parRep+parDem, data = gallup)
summary(pm)


