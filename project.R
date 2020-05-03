library(tidyverse)
library(tidytext)
library(ggplot2)
library(httr)
library(XML)
library(lubridate)

partyPresidentTable <- read.table("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/42MVDX/MFU99O", sep="\t", header=TRUE)

partyPresidentTable <- partyPresidentTable %>% group_by(year, party) %>% summarise(candidatevotes = sum(candidatevotes), totalvotes = sum(totalvotes))
partyNames <- partyPresidentTable %>% group_by(party) %>% select(party) %>% distinct()
consolidatedPresidentTableDem <- partyPresidentTable %>% filter(party == "democrat") %>% rename(demvotes = candidatevotes) %>% select(-party)
consolidatedPresidentTableRep <- partyPresidentTable %>% filter(party == "republican") %>% rename(repvotes = candidatevotes) %>% select(-party)

consolidatedPresidentTableRep$year <- consolidatedPresidentTableDem$year

consolidatedPresidentTable <- full_join(consolidatedPresidentTableDem, consolidatedPresidentTableRep, by = "year") %>% select(-totalvotes.x) %>% rename(totalvotes = totalvotes.y) %>% mutate(thirdpartyvotes = totalvotes - demvotes - repvotes)

ggplot(data = consolidatedPresidentTable, aes(x = year)) + geom_line(aes(y = repvotes, color = "darkred")) + geom_line(aes(y = thirdpartyvotes, color = "green")) + geom_line(aes(y = demvotes, color = "steelblue")) + scale_x_continuous(name="Year", breaks = c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) + scale_y_continuous(name="Votes", limits=c(100000, 70000000)) + labs(title ="Votes Each Election", x = "Year", y = "Votes")

#ggplot(data = partyTable, aes(x = year, y = candidatevotes, fill = party)) + geom_line(aes(x = year, y = candidatevotes, fill = party)) + scale_x_continuous(name="Year", breaks = c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) + scale_y_continuous(name="Votes", limits=c(100000, 70000000)) + labs(title ="Votes Each Election", x = "Year", y = "Votes")

#partyAlignmentTable <- read.csv("https://assets.pewresearch.org/wp-content/uploads/sites/5/2016/09/09-13-16-Party-ID-Detailed-Tables.csv")

monthFixer <- function(vector)
{
  vector <- as.vector(match(vector, month.abb))
}

writ <- GET(url = "https://news.gallup.com/poll/15370/party-affiliation.aspx")
writXML <- htmlParse(content(writ, as = "text"))
writTable <- getNodeSet(writXML, '//*[contains(concat( " ", @class, " " ), concat( " ", "figure-table", " " ))]')
rowGetter <-sapply(writTable, xmlValue)

gallupPol <- rowGetter[1]
gallupPol <- str_split(gallupPol, "\\r\\n")
gallupPol <- gallupPol[gallupPol != " "]
gallupPol <- gallupPol[gallupPol != "%"]
gallupPol <- gallupPol[gallupPol != "Republicans"]
gallupPol <- gallupPol[gallupPol != "Democrats"]
gallupPol <- gallupPol[gallupPol != "Independents"]

gallup <- as.data.frame(gallupPol)
gallup <- gallup %>% rename(x = c..In.politics..as.of.today..do.you.consider.yourself.a.Republican..a.Democrat.or.an.independent....) %>% filter(x != "%") %>% filter(x != " ") %>% filter(x != "Republicans") %>% filter(x != "Democrats") %>% filter(x != "Independents") %>% filter(x != "Trend since 2004") %>% filter(x != "In politics, as of today, do you consider yourself a Republican, a Democrat or an independent?") %>% filter(x != "") %>% filter(x != "Gallup")

gallupDem <- gallup[seq(4, nrow(gallup), 4), ] %>% as.data.frame()
gallupInd <- gallup[seq(3, nrow(gallup), 4), ] %>% as.data.frame()
gallupRep <- gallup[seq(2, nrow(gallup), 4), ] %>% as.data.frame()
gallupDat <- gallup[seq(1, nrow(gallup), 4), ] %>% as.data.frame()

gallupDem <- gallupDem %>% rename('Democrats %' = '.')
gallupInd <- gallupInd %>% rename('Independents %' = '.')
gallupRep <- gallupRep %>% rename('Republicans %' = '.')
gallupDat <- gallupDat %>% rename(Date = '.')

gallup <- bind_cols(gallupDat, gallupRep)
gallup <- bind_cols(gallup, gallupInd)
gallup <- bind_cols(gallup, gallupDem)

gallup <- gallup %>% mutate(`Republicans %` = as.numeric(as.character(`Republicans %`))) %>% mutate(`Independents %` = as.numeric(as.character(`Independents %`))) %>% mutate(`Democrats %` = as.numeric(as.character(`Democrats %`))) %>% mutate(year = as.numeric(substr(gallup$Date, 0, 4))) %>% mutate(month = substr(gallup$Date, 6, 8))

gallup <- gallup %>% mutate(monNum = monthFixer(gallup$month)) %>% select(-Date, -month) %>% mutate(monthsfrom1976 = 12*(year-1976) + monNum) %>% gather("party", "percent", `Republicans %`, `Independents %`, `Democrats %`)

#gallup <- gallup %>% group_by(Date) %>% summarise(`Republicans %` = mean(`Republicans %`))

ggplot(data = gallup, aes(x = monthsfrom1976, y = percent, color = party)) + geom_line(aes(x = monthsfrom1976, y = percent, color = party)) + scale_x_continuous(name="Year", breaks = c(336, 360, 384, 408, 432, 456, 480, 504, 528, 552)) + scale_y_continuous(name="Percentage", limits=c(0, 50)) + labs(title ="Percentage of Population", x = "Year", y = "Percentage")

#gallup <- gallup %>% mutate(Date2 = as.Date(gallup$Date))



