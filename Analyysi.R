library(tidyverse)
library(reshape2)
library(urltools)
library(tidyjson)
library(tidytext)
library(GGally)
library(ggpubr)
library(gridExtra)
library(corrr)


#setwd("C:/Users/pauli_000/OneDrive - Oulun yliopisto/gradu/csv")
setwd("/Users/patez/OneDrive - Oulun yliopisto/gradu/csv") 

# The palette with grey:
cbPalette <- c("#CC1111", "#0000AA", "#56B4E9", "#009E73", "#E69F00", "#0072B2", "#D55E00", "#CC79A7")

#load data files
dataHighest <- readRDS("highest/dataHighest.Rda")

dataUuvuksissa <- readRDS("uuvuksissa/dataUuvuksissa.Rda")
parsedJSONuu <- readRDS("uuvuksissa/parsedJSONuu.Rda")

dataCrawled <- readRDS("dataCrawled.Rda")
parsedJSON <- readRDS("parsedJSON.Rda")

data1 <- readRDS("data.Rda")

#classifying domains
data1 <- data1 %>%
  mutate(Type = ifelse(Domain %in% c( "www.suomalainen.com",
                                "www.akateeminen.com",
                                "www.adlibris.com",
                                "kirja.elisa.fi",
                                "otava.fi",
                                "www.wsoy.fi",
                                "www.prisma.fi",
                                "cdon.fi",
                                "www.nextory.fi",
                                "www.karkkainen.com",
                                "www.bookbeat.fi",
                                "www.booky.fi",
                                "www.rosebud.fi",
                                "www.finlandiakirja.fi",
                                "www.minervakustannus.fi",
                                "www.antikvaari.fi",
                                "www.sarjakuvakauppa.fi")
                                ,"kauppa", 
                ifelse(Domain %in% c( "www.is.fi",
                                "www.hs.fi",
                                "www.aamulehti.fi",
                                "yle.fi",
                                "www.iltalehti.fi",
                                "www.ts.fi",
                                "www.apu.fi",
                                "www.satakunnankansa.fi",
                                "www.kansanuutiset.fi",
                                "www.ksml.fi",
                                "www.maaseuduntulevaisuus.fi",
                                "www.ess.fi",
                                "areena.yle.fi",
                                "www.mtvuutiset.fi",
                                "suomenkuvalehti.fi",
                                "www.menaiset.fi")
                                ,"perinteinen media",
                ifelse(Domain %in% c(grep("blogspot.com",Domain, value=TRUE),
                                "www.facebook.com",
                                "twitter.com",
                                "www.goodreads.com",
                                "www.kirsinkirjanurkka.fi",
                                "kirsinbookclub.com")
                                ,"sosiaalinen media",
                ifelse(Domain %in% c("ekirjasto.kirjastot.fi",
                                "www.kirjasampo.fi",
                                "piki.verkkokirjasto.fi",
                                "www.helmet.fi",
                                "www.ellibs.com",
                                "www.keskikirjastot.fi",
                                "kuopio.finna.fi")
                                ,"kirjasto",
                                "muut")))))

#semrush vs google rank

viz <-  ggplot(data=data1, aes(x = Rank, y = SEMrushRank/1000000)) +
        geom_point() +
        geom_smooth() +
        labs(x="Google Rank", y="SEMrush Rank")

data1 %>% select(Rank, SEMrushRank) %>% correlate  
cor.test(data1$Rank, data1$SEMrushRank, alternative = "g", method = "spearman")
#0,07
viz

#no robot and rank
data2 <- data1 %>%
  filter(robot_content =="robots.txt not found") %>%
  count(Rank)


viz2 <- ggplot(data = data2, aes(x = Rank, y = n)) + 
  geom_point() + 
  scale_y_continuous(breaks=c(0,5,10,15)) +
  geom_smooth(se = TRUE) +
  labs(x="Google Rank", y="robots.txt puuttuu")
viz2

correlate(data2)
cor.test(data2$Rank, data2$n, method = "spearman")
#-0,09
#0,29

#sitemap and rank

data3 <- data1 %>%
  filter(str_detect(robot_content, "Sitemap:")) %>%
  count(Rank)
  
correlate(data3)
# -0,534 sitemap found pearson
# 0,496 no sitemap pearson

# -0,55 spearman
# 0,51 spearman
cor.test(data3$Rank,data3$n,method = "spearman")

viz3 <- ggplot(data = data3, aes(x = Rank, y = n)) + 
  geom_point() + 
  scale_y_continuous(breaks=c(0,5,10,15)) +
  geom_smooth(se = TRUE) +
  labs(x="Google Rank", y="Sitemap puuttuu")
viz3


basic_stats(data3, n, Rank)

#finnan urli
data4 <- data1 %>%
  filter(str_detect(Url, "finna"))

viz4 <- ggplot(data = data4, aes(x = Rank)) + 
  geom_histogram(bins = 5, aes(fill=Rank)) +
  labs(x="Google Rank", y="Finna")
viz4


#order domains by rank, include only 10 first results (1 page)

data5 <- data1 %>% 
  filter(Rank <= 10)

data5 <- within(data5, 
                   Domain <- factor(Domain, 
                                      levels=names(sort(table(Domain), 
                                                        decreasing=FALSE))))

data5 <- data5[data5$Domain %in% names(which(table(data5$Domain) > 1)), ]

viz5 <- ggplot(data = data5, aes(x = Domain)) +
  scale_fill_manual(values = cbPalette) +
  geom_bar(aes(fill=Type)) +
  geom_text(stat='count', aes(label=..count..), vjust = +0.3, hjust = -0.2) +
  coord_flip() +
  expand_limits(y=c(0,18)) +
  labs(y="M??r?", x="Sivusto", fill="Tyyppi") #huom coord flipin vaikutus

viz5

#order by cumulative sum of weighted rank where WeightedFreq = 1/Rank*Freq

data6 <- data1 %>%
  mutate(Freq = table(data1$Domain)[data1$Domain]) %>%
    mutate(WeightedFreq = 1/Rank*Freq)
     
sum <- data6 %>%
  group_by(Domain) %>%
    summarise(Csum = sum(WeightedFreq))

data6 <- merge(data6,sum,by="Domain",all.y=TRUE)
data6 <- data6 %>% distinct(Domain, .keep_all = TRUE)
data6 <- arrange(data6, -Csum)


viz6 <- ggplot(data6, aes(x = reorder(Domain,Csum), y = as.numeric(Freq))) +
  scale_fill_manual(values = cbPalette) +
  geom_col(aes(fill=Type)) +
  geom_text(aes(label = Freq), vjust = +0.3, hjust = -0.2) +
  coord_flip() +
  labs(y="M??r?", x="Sivusto", fill="Tyyppi")

viz6

#barchart with metadata encondings by amount of rows, use melt to gather format columns
#no conclusive results.. yet. rdfa is verbose and amount of metadata varies a lot.


mdat = melt(dataHighest, id.vars = c("Domain", "Rank"),
            measure.vars = c("jsonld","microdata","microformat","opengraph","rdfa"))

viz7 <- ggplot(mdat, aes(reorder(Domain, -Rank), value, fill = variable)) +
  geom_col() +
  scale_fill_manual(values = cbPalette) +
  coord_flip() +
  ylim(c(0, 3000))

viz7

viz8 <- ggplot(dataHighest, aes(reorder(Domain, -Rank), rdfa)) +
  geom_col() +
  scale_fill_manual(values = cbPalette) +
  coord_flip() 

viz8


bs(dataCrawled)
bs(dataUuvuksissa)
bs(dataHighest)

names(parsedJSON[5]) == "rdfa" 


# parse json and find out about encodings

parsedJSON <- parseJSON(dataCrawled)
dataCrawled <- addEncodings(dataCrawled, parsedJSON)

#parsedJSONuu <- parseJSON(dataUuvuksissa)
#dataCrawled <- addEncodings(dataUuvuksissa, parsedJSONuu)

parsedJSONHigh <- parseJSON(dataHighest)

dataCrawled <- dataCrawled %>%
  filter(!Search == "tuntematon sotilas") %>%
  filter(!Search == "finlandia kirja") %>%
  filter(!Search == "hyva kirja") %>%
  filter(!Search == "lasten kirja") %>%
  filter(!Search == "kirja")
#filter(Rank <= 50)
#nameCounted <- tf_idf %>% bind_all_formats("nameCombined", "Search")
#nameAndStringCounted <- tf_idf %>% bind_all_formats("nameAndString", "Search")

stringCounted <- dataCrawled %>%
  mutate(Search = substr(Search,2, (str_length(Search) - 4))) %>%
  mergeFrom(parsedJSON) %>%
  combineStructure %>%
  cleanData %>%
  bind_all_formats("string", "Search")
  

search = "bolla kirja"
searches <- distinct(stringCounted$microdata, Search) %>% slice (1:6) %>% pull(Search)
#ordercol = "Rweighted_tf_idf"
ordercol = "tf_idf"
plot_list <- stringCounted %>%
  lmap(make_format_chart_by, search, ordercol)

ggarrange(plotlist = plot_list, ncol = 3, nrow = 2) %>%
  grid.arrange(top = paste("Haku:", search))


make_tf_idf_chart(stringCounted$microdata, "Search", ordercol, searches)



### correlations from...
combined <- dataCrawled %>%
  mutate(Search = substr(Search,2, (str_length(Search) - 4))) %>%
  mergeFrom(parsedJSON) %>%
  combineStructure #%>%
  #bind_all_formats("nameCombined", "Rank")

x = combined$json_ld$nameCombined

names <- unique(x[x!=""])
names <- names

res <- list()
cors <- data.frame("name"=character(0), "cor"=numeric(0), "cortest"=I(list()))
for(i in names) {
  res[[i]] <- combined$json_ld %>% 
    filter(nameCombined == i) %>%
    count(Rank)
  cors <- cors %>% add_row(name = i, cor = cor(res[[i]]$Rank,res[[i]]$n)) %>%
    cortest = unlist(cor.test(res[[i]]$Rank,res[[i]]$n))
  #cors <- cors %>% unlist(correlate(res[[i]]))
}

res <- combined$opengraph %>% 
  select(nameCombined,Rank) %>%
  aggregate(by = list(unique.values = (.)$nameCombined), FUN = length )

merged <- merge(combined$opengraph,res, by.x = "nameCombined", by.y = "unique.values")


correlate(res)

ggplot(res, aes(Rank, n)) + 
  geom_point() + 
  scale_y_continuous(breaks=c(0,5,10,15)) +
  geom_smooth(se = TRUE) +
  labs(x="Google Rank", y="ominaisuus")



combined$opengraph %>% 
  select(n, Rweighted_tf_idf, tf_idf, nameCombined, Search, Domain, Rank) %>%
  arrange(desc(Rweighted_tf_idf),Rank) %>%
  slice(1:100)

 

cors <- combined$opengraph %>% 
  select(n, tf, idf, tf_idf, Rweighted_tf_idf, nameCombined, string,Search, Domain, Rank)
  
cors %>% 
  select(n, tf_idf, Rweighted_tf_idf, Rank) %>%
  correlate
  


cor.test(cors$Rank, cors$Rweighted_tf_idf)

ggplot(ogs, aes(Rank, n)) + 
  geom_point() + 
  scale_y_continuous(breaks=c(0,5,10,15)) +
  geom_smooth(se = TRUE) +
  labs(x="Google Rank", y="og-kenttä")




## teee tämä
make_format_chart_by <- function(df_lst, doc, col) {
  ret <- df_lst[[1]] %>%
    filter(Search == doc) %>%
    arrange(desc(get(col))) %>%
    mutate(words = factor(words, levels = rev(unique(words)))) %>% 
    slice(1:10) %>%
    ungroup %>%
    mutate(words = paste(str_trunc(string, 70), "\n", Domain, ":" , str_trunc(nameCombined, 70))) %>% 
    ggplot(aes(reorder(words, get(col)), get(col), fill = Search)) +
    geom_col(show.legend = FALSE, width = 0.7, position = position_dodge(0.9)) +
    labs(x = NULL, y = col, title = names(df_lst)) +
    theme(#aspect.ratio = 16/9,
          axis.text.y = element_text(size = 8),
          plot.title = element_text(face = "plain", size = 11)) +
    coord_flip()
  list(ret)
}


make_tf_idf_chart <- function(df, docs, col, searches) {
  df %>%
    filter(Search %in% searches) %>%
    arrange(desc(get(col))) %>%
    mutate(words = factor(words, levels = rev(unique(words)))) %>% 
    group_by(get(docs)) %>% 
    slice(1:10) %>% 
    ungroup() %>% ###next line contains what the labels show.
    mutate(words = paste(str_trunc(string, 70), "\n", Domain, ":" , str_trunc(nameCombined, 70))) %>% 
    ggplot(aes(reorder(words,get(col)), get(col), fill = !!docs)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = col) +
    facet_wrap(paste0("~", get('docs')), ncol = 3, scales = "free") +
    theme( 
          axis.text.y = element_text(size = 8),
          plot.title = element_text(face = "plain", size = 11)) +
    coord_flip()
}

bind_all_formats <- function (df_list, words, docs ){
  
  df_list <- countTF(df_list, words, docs)
  new_list <- list()
  for (i in 1:length(df_list)) {
    new_list[[i]] <- df_list[[i]] %>%
      bind_tf_idf(!!words, !!docs, n) %>%
      mutate(Rweighted_tf_idf = 1/Rank * tf_idf)
  }
  names(new_list) <- names(df_list)
  new_list
}


       
combineStructure <- function (df_list) {
  new_list <- list()
  val <- data.frame()
  for (i in 1:length(df_list)) {
    val <- df_list[[i]] %>%
      #mutate(nameCombined = names(df_list)[i]) %>%
      unite(nameCombined, starts_with("name"),  na.rm = TRUE, sep = "/") %>%
      unite(nameAndString, string, nameCombined, remove = FALSE, sep = "\n")#%>%
      #unite(nameCombined, nameCombined, starts_with("array"), na.rm = TRUE)
    new_list[[i]] <- val
  }
  names(new_list) <- names(df_list)
  new_list
}

countTF <- function (df_list, words,docs) {
  new_list <- list()
  df1 <- data.frame()
  for (i in 1:length(df_list)) {
    df1 <- df_list[[i]] %>%
      select(!!docs,!!words) %>%
      group_by_all() %>% tally() %>% ungroup()
    new_list[[i]] <- merge(df_list[[i]], df1)
    new_list[[i]] <- distinct(new_list[[i]], words = get(words), .keep_all = TRUE)
  }
  names(new_list) <- names(df_list)
  new_list
}

cleanData <- function(df_list) {
  new_list <- list()
  df1 <- data.frame()
  
  for (i in 1:length(df_list)) {
    new_list[[i]] <- df_list[[i]] %>%
      mutate(string = str_remove(string, "list\\(`@value` =")) %>%
      mutate(string = str_remove(string, "list\\(`@id` =")) %>%
      mutate(string = str_remove(string, "\"")) %>%
      mutate(string = str_remove(string, "\\n")) %>%
      mutate(string = str_remove(string, "^/")) %>%
      mutate(string = str_remove(string, "\"\\)")) %>%
      filter(!string =="NA") %>%
      filter(!string == "<NA>") %>%
      filter(!string == "") %>%
      filter(!(str_detect(nameCombined, "vocab#") & names(df_list[i]) == "rdf_a")) %>%
      filter(!(str_detect(nameCombined, "@id") & names(df_list[i]) == "rdf_a")) %>%
      filter(!str_detect(nameCombined, "datePublished")) %>%
      filter(!str_detect(nameCombined, "dateCreated")) %>%
      filter(!str_detect(nameCombined, "dateModified")) %>%
      filter(!str_detect(nameCombined, "modified_time")) %>%
      filter(!str_detect(nameCombined, "release_date")) %>%
      filter(!str_detect(nameCombined, "updated_time")) %>%
      filter(!str_detect(nameCombined, "published_time")) %>%
      filter(!str_detect(nameCombined, "image")) %>%
      filter(!str_detect(nameCombined, "height")) %>%
      filter(!str_detect(nameCombined, "width"))
      
  }
  names(new_list) <- names(df_list)
  new_list
}
  
 
  
# parse metadata formats to dataframes named og, jsonld, rdf
# append arrays and objects is a recursive helper function to sort out all nested data

append_arrays_and_objects <- function (tbl) {
  res1 <- tbl
  res2 <- tbl
  
  objs <- tbl %>%
    filter(is_json_object(.)) %>% gather_object %>%
    append_values_string
 
  arr <- tbl %>% 
    filter(is_json_array(.)) %>% gather_array %>%
    append_values_string
  
  if (nrow(objs) > 0) res1 <- merge(append_arrays_and_objects(objs),tbl, all=TRUE)
  if (nrow(arr) > 0) res2 <- merge(append_arrays_and_objects(arr),tbl, all=TRUE)
  
  print(objs) #for debugging
  print(arr)  #for debugging
  
  merge(res1,res2, all=TRUE)
} 

parseJSON <- function (df) {
  #parse all 5 formats and return list of dataframes
  
  md <- df$JSON %>%
    enter_object("microdata") %>% append_arrays_and_objects 
  
  mf <- df$JSON %>%
    enter_object("microformat") %>% append_arrays_and_objects 
  
  og <- df$JSON %>%
    enter_object("opengraph") %>% append_arrays_and_objects
  
  jsonld <- df$JSON %>%
    enter_object("json-ld") %>% append_arrays_and_objects 
  
  rdfa <- df$JSON %>%
    enter_object("rdfa") %>% append_arrays_and_objects 
  
  #rename document.id columns to id (just for consistency)
  jsonld <- jsonld %>% rename(id = document.id)
  md <- md %>% rename(id = document.id)
  mf <- mf %>% rename(id = document.id)
  og <- og %>% rename(id = document.id)
  rdfa <- rdfa %>% rename(id = document.id)
  
  list(md=md,mf=mf,og=og,jsonld=jsonld,rdfa=rdfa)
}


#detect encodings and calculate length for each encoding by rows of data they contain
addEncodings <- function (df, metaList) {
  
  df <- df %>%
    mutate(jsonld = 0, microdata = 0, microformat = 0, opengraph = 0, rdfa = 0)
  
  ids1 <- as.data.frame(table(metaList$jsonld$id))
  ids2 <- as.data.frame(table(metaList$md$id))
  ids3 <- as.data.frame(table(metaList$mf$id))
  ids4 <- as.data.frame(table(metaList$og$id))
  ids5 <- as.data.frame(table(metaList$rdfa$id))
  
  
  df$jsonld <- ids1$Freq[match(df$id, ids1$Var1)]
  df$microdata <- ids2$Freq[match(df$id, ids2$Var1)]
  df$microformat <- ids3$Freq[match(df$id, ids3$Var1)]
  df$opengraph <- ids4$Freq[match(df$id, ids4$Var1)]
  df$rdfa <- ids5$Freq[match(df$id, ids5$Var1)]

  df[is.na(df)] = 0
  
  df
}

bs <- function (df) {
  
  df1 <- basic_stats(df, df$jsonld, "jsonld")
  df1 <- rbind(df1, basic_stats(df, df$microdata, "microdata"))
  df1 <- rbind(df1, basic_stats(df, df$microformat, "microformat"))
  df1 <- rbind(df1, basic_stats(df, df$opengraph, "opengraph"))
  df1 <- rbind(df1, basic_stats(df, df$rdfa, "rdfa"))
  
  df1
}

mergeFrom <- function(src_df, dst_lst) {
  new_lst <- list()
  new_lst$json_ld <- merge(src_df, dst_lst$jsonld, by = "id")
  new_lst$microdata <- merge(src_df, dst_lst$md, by = "id")
  new_lst$microformat <- merge(src_df, dst_lst$mf, by = "id")
  new_lst$opengraph <- merge(src_df, dst_lst$og, by = "id")
  new_lst$rdf_a <- merge(src_df, dst_lst$rdfa, by = "id")
  
  new_lst
}

## some helper functions for basic stats
se <- function(x) sd(x) / sqrt(length(x))

basic_stats <- function(df,col,col2) {
  df %>%
    summarise(
              mean = mean(col), 
              median = median(col), 
              variance = var(col), 
              standard_deviation = sd(col), 
              standard_error = se(col)) 
}

