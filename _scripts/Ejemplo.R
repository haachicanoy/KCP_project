library(networkD3)
library(dplyr)
library(xlsx)

## Remove scientific notation
options(scipen=999)

## Read data in
a <- read.xlsx2("ALL_Origincountry-Tocountry_circosfinal_2016_7_7_avannualwgenebank_share.xlsx",1)
str(a)
a[,1] <- as.character(a[,1])
a[,2] <- as.character(a[,2])
a[,3] <- as.character(a[,3])
a[,4] <- as.numeric(a[,4])

# ## We are interested in international collaboration, 
# ## so let's take out all the self-contributions
# a %>%
#   filter(Origin!=Genebank_country & 
#            Genebank_country != Recipient) -> a

GeneBanks <- unique(a[,2])
countryRegions <- read.xlsx2('Countries_gb_data_regions_2016_6_13_sankey_share.xlsx', 1)
grep2 <- Vectorize(FUN = grep, vectorize.args = 'pattern')
table(unlist(lapply(grep2(pattern = paste('^', a[,1] %>% unique %>% sort, '$', sep = ''), x = countryRegions$Country), FUN = length)))

aux  <- grep(pattern = '\\(', x = countryRegions$Country, fixed = FALSE)
mtch <- grep2(pattern = paste('^', a[,1] %>% unique %>% sort, '$', sep = ''), x = countryRegions$Country)
for(u in aux){
  aux <- asas
}


match()

## Now do one sankey per genebank
for (i in  1:length(GeneBanks)){
  a %>% filter(Genebank_country == GeneBanks[i]) -> b
  # Step 0, make 3 levels
  b[,1] <- paste(b[,1]," >",sep="")
  b[,3] <- paste("> ",b[,3],sep="")
  
  ## or just 2 levels? To Genebank and back to see how ti works:
  # b[,2] <- paste(b[,2],"GB",sep="")
  
  ## Step 1, get SOURCE -> Genebank relationships
  b %>%
    group_by(Origin,Genebank_country) %>%
    summarize(Val=sum(Average.no.samples.per.year)) -> Source2GB
  
  names(Source2GB)[1:2] <- c("FROM","TO") 
  
  ## Step 2, get GB -> Sink
  b %>%
    group_by(Genebank_country,Recipient) %>%
    summarize(Val=sum(Average.no.samples.per.year)) -> GB2Sink
  
  names(GB2Sink)[1:2] <- c("FROM","TO") 
  
  
  ## Combine
  Boff <- bind_rows(Source2GB,GB2Sink)
  
  # ## And now see what the biggest players are:
  # Boff %>% filter(Val>100000) -> Boff
  
  
  ## get nodes and edges:
  source("https://gist.githubusercontent.com/mexindian/a77102065c75c69c22216f43cc3761be/raw/08b53d06a7caa4a7bee4f93d5879443223f385e6/easyModeNodeEdge.R")
  nodesEdges <- easyMode(Boff,0)
  nodes <- nodesEdges[[1]]
  edges <- nodesEdges[[2]]
  
  edges$thingie <- sub(' ', '', nodes[edges$from + 1, 'name'])
  
  ## this bug cost me about 4 hours to find.
  edges <-as.data.frame(edges)
  
  # Save JSON file
  library(jsonlite)
  json <- list(nodes=data.frame(name=nodes$name), # id=nodes$id
              links=edges)
  
  sink('sankey_draft.json') # redirect console output to a file
  toJSON(json, pretty=FALSE)
  sink()
  
  # Create graph
  c <- sankeyNetwork(Links = edges, Nodes = nodes, 
                     Source = 'from',Target = 'to', Value = 'value', NodeID = 'name',
                     LinkGroup = 'thingie', fontSize = 15)
  saveNetwork(c,paste("sankeyOneGB.",GeneBanks[i],".html",sep=""),selfcontained = T)
}
