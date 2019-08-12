getwd()


setwd("C:/Users/soong/Desktop/U.R.P/DATA")
setwd("C:/Users/soong/Desktop")
ex <- fread("ex1.csv", encoding = "UTF-8") %>% as_tibble

library("data.table")
library(tidyr)
library(tibble)


library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisatio
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('ggrepel') # visualisation
library('RColorBrewer') # visualisation
library('data.table') # data manipulation
library('dplyr') # data manipulation
library('readr') # data input
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('stringr') # string manipulation
library('purrr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('forecast') # time series analysis
library('prophet') # time series analysis

library(stringr)

require(rgdal)
require(sf)



animal <- fread("animal_final.csv") %>% as_tibble
animal <- fread("before_woohyun_embedding.csv") %>% as_tibble

smote <- fread("smote_final.csv") %>% as_tibble()



# 전처리해야될 변수들 확인하기

animal$redlistCategory %>% table

animal %>% select(3,4,10,16) %>% View



### Step 1)  : 필요없는 단어들 제거해주기 
library(tm)




corpus <- Corpus(VectorSource(animal$rationale))



corpus[["69331"]][["content"]]

for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("<em>", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("<span", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("span>", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("</em", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("em>", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("sheader", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("font-style", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("italic", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("span style", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("class=", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("<br/>", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("</", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("style=", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("sheader5", " ", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("  ", " ", corpus[[i]]) }
corpus <- tm_map (corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("  ", " ", corpus[[i]]) }

corpus2 <- corpus


### 두 단어로 되어있는 redlistCategory 레이블(Y)들을  한단어로 이어주자
#animal$redlistCategory %>% table

# 각 레이블들이 몇번 나오는지 세보기
#rationale_df$text %>% str_detect("Critically Endangered") %>% sum(na.rm = TRUE)
#rationale_df$text %>% str_detect("Data Deficient") %>% sum(na.rm = TRUE)
#rationale_df$text %>% str_detect("Extinct in the Wild") %>% sum(na.rm = TRUE)
#rationale_df$text %>% str_detect("Lower Risk least concern") %>% sum(na.rm = TRUE)
#rationale_df$text %>% str_detect("Lower Risk near threatened") %>% sum(na.rm = TRUE)
#rationale_df$text %>% str_detect("Near Threatened") %>% sum(na.rm = TRUE)
#rationale_df$text %>% str_detect("Least Concern") %>% sum(na.rm = TRUE)

# 레이블들 한 단어로 붙여주기

for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("Critically Endangered", "Critically_Endangered", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("Data Deficient", "Data_Deficient", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("Extinct in the Wild", "Extinct_in_the_Wild", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("Least Concern", "Least_Concern", corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub("Near Threatened", "Near_Threatened", corpus[[i]]) }



corpus3 <- corpus
corpus[["69331"]][["content"]]

corpus <- tm_map(corpus, content_transformer(stringi::stri_trans_tolower))



corpus[["69331"]][["content"]]

# 2글자 이하 단어 지워주기
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub('\\b\\w{1,2}\\b', '', corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub('  ', ' ', corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub('  ', ' ', corpus[[i]]) }

# 20글자 이상 단어 지워주기
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub('\\b\\w{19,100}\\b', '', corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub('  ', ' ', corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub('  ', ' ', corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub('  ', ' ', corpus[[i]]) }



#첫번쨰 글자와 마지막 글자 지우기
library(stringi)

for(i in 1 : length(corpus)) {corpus[[i]] <-  gsub('.{1}$', '', corpus[[i]]) }
for(i in 1 : length(corpus)) {corpus[[i]] <-  stri_sub(corpus[[i]], 2) }




corpus[["2"]][["content"]]


rationale_df <- data.frame(text = sapply(corpus, identity), stringsAsFactors = F)
write.csv(rationale_df, "rationale_token7.csv", row.names = F)



### TF-IDF로 단어 가중치 구하기 ( word2vec으로 살아남은 단어들만 가지고 해야됨)

rationale_vectors <- fread("rationale_vectors5.csv", encoding = "UTF-8") %>% as_tibble
names(rationale_vectors) <- c("word", "vector")

rationale_vectors
for(i in 1 : nrow(rationale_vectors)) {rationale_vectors[i,2] <-  gsub('\\[', '', rationale_vectors[i,2]) }
for(i in 1 : nrow(rationale_vectors)) {rationale_vectors[i,2] <-  gsub('\\]', '', rationale_vectors[i,2]) }
for(i in 1 : nrow(rationale_vectors)) {rationale_vectors[i,2] <-  gsub('.{2}$', '', rationale_vectors[i,2]) }

for(i in 1 : nrow(rationale_vectors)) {rationale_vectors[i,2] <-  gsub('  ', ' ', rationale_vectors[i,2]) }
for(i in 1 : nrow(rationale_vectors)) {rationale_vectors[i,2] <-  gsub('  ', ' ', rationale_vectors[i,2]) }
for(i in 1 : nrow(rationale_vectors)) {rationale_vectors[i,2] <-  gsub('  ', ' ', rationale_vectors[i,2]) }


rationale_vectors <- rationale_vectors %>% separate(vector, c("v1","v2","v3"), " ")

rationale_vectors$v1 <- as.numeric(rationale_vectors$v1)
rationale_vectors$v2 <- as.numeric(rationale_vectors$v2)
rationale_vectors$v3 <- as.numeric(rationale_vectors$v3)

survived_words <- rationale_vectors$word %>% as.matrix %>% as.character



library(tidytext)
#rationale_df <- read_csv("rationale_token8.csv")


rationale_df <- rationale_df %>% as_tibble
rationale_df$index <- seq.int(nrow(rationale_df)) 
rationale_df <- rationale_df %>% select(2, 1)
rationale_df


# 몇몇 인덱스는 아예 rationale 칸이 비어있다. 이새끼들은 뺴버려야도
blank_index <- rationale_df %>% filter(text =="") %>% select(index) %>% as.matrix %>% as.character %>% as.integer



rationale_table <- rationale_df %>% unnest_tokens(word, text) %>% count(index, word) # 이게 문제다
rationale_table$index %>% unique %>% length



rationale_table

rationale_table <- rationale_table %>% filter(word %in% survived_words)
rationale_table$index %>% unique %>% length


rationale_table <- rationale_table %>% bind_tf_idf(word, index, n)

rationale_table <- rationale_table %>% arrange(index)
rationale_table <- rationale_table %>% select(-total)

rationale_table <- rationale_table %>% group_by(index) %>% arrange(desc(tf_idf)) %>% arrange(index)

rationale_table <- rationale_table %>% select(-tf, -idf, -n)
rationale_table$index %>% unique %>% length

# 이제 벡터값과 단어의 tf_idf 값을 가중합 한다.

rationale <- left_join(rationale_table, rationale_vectors)
#rm(rationale_table, rationale_vectors, rationale_df)
#write.csv(rationale,"rationale_final.csv", row.names = F)

rationale2 <-
  rationale %>% mutate(V1 = tf_idf*v1,
                     V2 = tf_idf*v2,
                     V3 = tf_idf*v3) %>% 
  select(-tf_idf,-v1,-v2,-v3)


rationale <-
  rationale2 %>% select(-word) %>% group_by(index) %>% summarise(V1 = sum(V1, na.rm = T),
                                                               V2 = sum(V2, na.rm = T),
                                                               V3 = sum(V3, na.rm = T))

rationale$index %>% unique %>% length



animal <- fread("animal_final.csv", encoding ="UTF-8") %>% as_tibble
animal$index <- seq.int(nrow(animal)) 
animal <- animal %>% select(23, everything())

selected_index <- animal$index[-blank_index]
animal <- animal %>% filter(index %in% selected_index)
animal$index <- NULL
animal$index <- seq.int(nrow(animal)) 
animal <- animal %>% select(23, everything())

animal
rationale 

animal_sg <- left_join(animal,rationale) %>% select(-index)
animal_sg <- 
  animal_sg %>% select(assessmentId, internalTaxonId, scientificName, redlistCategory,
                     redlistCriteria, yearPublished, assessmentDate, criteriaVersion, language,
                     rationale,V1,V2,V3, everything()) 

write.csv(animal_sg, "animal_rationale추가한거.csv", row.names = F)






####################################################################################################
##### systems 전처리 #####


animal <- fread("animal.csv") %>% as_tibble
animal$systems %>% is.na %>% sum
animal$systems %>% unique
animal$systems %>% table


# 병신같은 레이블에 대한 조사
animal %>% filter(systems == "Freshwater (=Inland waters)|Marine|Marine")  # 1개 -> 지워시발
animal %>% filter(systems == "Marine|Marine")# 8개
animal %>% filter(systems == "Marine")

# One hot encoding
animal %<>% 
  mutate(Freshwater = ifelse(str_detect(systems, "Freshwater"), 1, 0)) %>% 
  mutate(Marine = ifelse(str_detect(systems, "Marine"), 1, 0)) %>% 
  mutate(Terrestrial = ifelse(str_detect(systems, "Terrestrial"), 1, 0)) %>% 
  select(-systems)


animal_sg <-animal %>% select(1,2,3,4,
                  11,12,13,
                  25,26,27)

names(animal_sg)[5:7] <- c("rationale_v1", "rationale_v2", "rationale_v3" )

write.csv(animal_sg, "animal_rationale&systems_변수추가.csv", row.names = F)













################################################################################################
# 최종 통합코드 



# Libraries -----------------------------------------
library(data.table)
library(tidyverse)
library(magrittr)
library(tm)
library(slam)
library(topicmodels)

# Read File ----------------------------------------
animal <- fread("animal_final.csv") %>% as_tibble

############################################################# Data Wrangling #######################################################################

colnames(animal)

# internalTaxonId ----------------------------------
length(unique(animal$internalTaxonId))
animal$internalTaxonId <- NULL

# scientificName -----------------------------------
animal %<>% 
  separate(scientificName, into=c("Genus", "Species"), sep=" ") #Separate into two columns

# redlistCriteria ----------------------------------
animal$redlistCriteria <- NULL

# yearPublished -----------------------------------
animal$yearPublished <- NULL

# assessmentDate -----------------------------------
animal$assessmentDate <- NULL

# criteriaVersion ----------------------------------
animal$criteriaVersion <- NULL

# language -----------------------------------------
animal$language <- NULL

# rationale ---------------------------------------
rationale <- fread("animal_rationale&systems_변수추가.csv", encoding = "UTF-8") %>% as_tibble
rationale <- rationale %>% select(1, 5,6,7)

animal <- 
  left_join(animal, rationale, by="assessmentId")

animal$rationale <- NULL

# habitat ------------------------------------------
'아직 안되었음'
# population ---------------------------------------
animal$population <- NULL

# populationTrend ---------------------------------
table(animal$populationTrend)
animal$populationTrend[which(animal$populationTrend=="")] <- "Unknown"
table(animal$populationTrend)

# range --------------------------------------------
Sys.setlocale(category = 'LC_ALL',locale = 'us')
library(tm)
range <- animal$range
range <- Corpus(VectorSource(range))
range <- tm_map(range, content_transformer(tolower))
range <- tm_map(range, removePunctuation)
range <- tm_map(range, removeWords, stopwords("english"))

repvec <- rep(0,length(range))

for ( i in 1:length(range)){
  repvec[i] <- as.character(range[[i]]$content )
}

range <- repvec

country_list <- fread('country_list.csv')
country_name <- country_list$name
country_name[42]
country_name[42]<-"Cote d'Ivoire"
country_name[205]
country_name[205]<-'Sao Tome and Principe'
country_name[185]
country_name[185]<-'Reunion'
country_name <- tolower(country_name)

america_states <- fread('america_states.csv')
america_name <- america_states$name
america_name <- tolower(america_name)

country_name[country_name %in% america_name] # remove two names in countries 
country_name<-country_name[-which(country_name %in% america_name == T)]

for ( i in 1:length(country_name)){
  country_name[i]<-str_replace_all(country_name[i],'\\[.*]','')
  country_name[i]<-str_trim(country_name[i],'right')
}

country_name <- unique(country_name)

range <- str_split(range,' ')

temp_vec <- vector(length(range),mode = 'list')

for ( i in 1:length(repvec)){
  temp_vec[[i]]<- unique(range[[i]][range[[i]] %in% country_name])
  if ( sum(range[[i]] %in% america_name) > 0 ) { temp_vec[[i]] <- unique(c(temp_vec[[i]] , 'united states'))  }
}

country_df <- as.data.frame(matrix(0, nrow(animal), length(country_name)+1 ))
colnames(country_df) <- c("assessmentId", country_name)

#as.numeric(country_name %in% bb[[11]])
to_vec <- function(dat){
  as.numeric(country_name %in% dat )
  
}

temp <- lapply(temp_vec, to_vec)

temp_unlist <- unlist(temp)

temp_mat <- matrix(temp_unlist,length(temp),length(country_name),byrow = T)

country_df[,2:ncol(country_df)] <- temp_mat

animal <- cbind(animal, country_df[,-1])

animal$range <- NULL

# useTrade -------------------------------------
animal %<>% 
  mutate(NotUsed=ifelse(str_detect(useTrade, "\\b(no|No|unlikey|Unlikely|non|Non)\\w.{0,35}(use|utilize|trade|information)"), 1, 0),
         Used=ifelse(str_detect(useTrade, "(commercial|consume)"), 1, 0),
         Collect=ifelse(str_detect(useTrade, "collect"), 1, 0),
         byCatch=ifelse(str_detect(useTrade, "bycatch"), 1, 0)) %>% 
  mutate(NotUsed=ifelse(Used==1|Collect==1|byCatch==1, 0, 1)) %>% 
  select(-useTrade)

# systems ---------------------------------------
animal %<>% 
  mutate(Freshwater = ifelse(str_detect(systems, "Freshwater"), 1, 0)) %>% 
  mutate(Marine = ifelse(str_detect(systems, "Marine"), 1, 0)) %>% 
  mutate(Terrestrial = ifelse(str_detect(systems, "Terrestrial"), 1, 0)) %>% 
  select(-systems)

# conservationActions -----------------------------
conserve <- animal$conservationActions 

yeswords <- c("within a protected area","this protection","wildlife reserve",
              "protected areas","protected")

nowords <- c("no conservation","no species-specific conservation measures",
             "no known","research is required","no species specific conservation measures",
             "none known","not known","further research","no species","information is needed on",
             "no information")

# binary encoding
new_conserve <- data.frame(ConservationActions = rep(NA,length(conserve)),
                           category = animal$redlistCategory)

for (i in 1:length(yeswords)){
  index.y <- grep(yeswords[i], conserve, fixed = TRUE)
  new_conserve[index.y,1] <- 1
}

for (i in 1:length(nowords)){
  index.n <- grep(nowords[i], conserve, fixed = TRUE)
  new_conserve[index.n,1] <- 0
}

animal <-
  cbind(animal, ConservationActions=new_conserve[,1])

animal$conservationActions <- NULL

# realm ----------------------------------
realm <- animal$realm

# Check Distribution
realm.table <- strsplit(realm, fixed=T, split="|") %>% unlist()
(table <- table(realm.table))

# Binary encoding
realm.type <- names(table)

new.realm <- matrix(NA, nrow(animal), length(realm.type))
colnames(new.realm) <- realm.type

for(i in 1:8) {
  index <- grep(realm.type[i], realm, fixed=TRUE)
  new.realm[index,i] <- 1 
  new.realm[-index,i] <- 0
}

animal <-
  cbind(animal, new.realm)

animal$realm <- NULL

# yearLastSeen ----------------------------
animal$yearLastSeen <- NULL

# possiblyExtinct -------------------------
animal$possiblyExtinct <- NULL
animal$possiblyExtinctInTheWild <- NULL

# scopes ----------------------------------
animal %<>% 
  mutate(Global = ifelse(str_detect(scopes, "Global"), 1, 0)) %>% 
  mutate(Europe = ifelse(str_detect(scopes, "Europe"), 1, 0)) %>% 
  mutate(Mediterranean = ifelse(str_detect(scopes, "Mediterranean"), 1, 0)) %>% 
  mutate(Gulf_of_Mexico = ifelse(str_detect(scopes, "Gulf of Mexico"), 1, 0)) %>% 
  mutate(Pan_Africa = ifelse(str_detect(scopes, "Pan-Africa"), 1, 0)) %>% 
  mutate(Eastern_Africa = ifelse(str_detect(scopes, "Eastern Africa"), 1, 0)) %>% 
  mutate(Central_Africa = ifelse(str_detect(scopes, "Central Africa"), 1, 0)) %>% 
  mutate(Northeastern_Africa = ifelse(str_detect(scopes, "Northeastern Africa"), 1, 0)) %>% 
  mutate(S_Africa_FW = ifelse(str_detect(scopes, "S. Africa FW"), 1, 0)) %>% 
  mutate(Northern_Africa = ifelse(str_detect(scopes, "Northern Africa"), 1, 0)) %>% 
  mutate(Western_Africa = ifelse(str_detect(scopes, "Western Africa"), 1, 0)) %>% 
  mutate(Persian_Gulf = ifelse(str_detect(scopes, "Persian Gulf"), 1, 0)) %>% 
  mutate(Arabian_Sea = ifelse(str_detect(scopes, "Arabian Sea"), 1, 0)) %>% 
  mutate(Europe = ifelse(str_detect(scopes, "Europe"), 1, 0)) %>% 
  select(-scopes)

# Delete all 0 columns -------------------------
animal <- animal[, colSums(animal != 0, na.rm = TRUE) > 0]

# Unify Red List Categories ------------------------
table(animal$redlistCategory)

animal$redlistCategory[which(animal$redlistCategory=="Lower Risk/least concern")] <- "Least Concern"
animal$redlistCategory[which(animal$redlistCategory=="Lower Risk/near threatened")] <- "Near Threatened"

# Delete Unknown -----------------------------------
animal <- animal %>% filter(redlistCategory != "Unknown")



#write.csv(animal, "animal_전처리완료.csv", row.names = F)




##############################################################################################
###################################### S M O T E  ############################################
##############################################################################################

#install.packages("DMwR")
library(data.table)
library(DMwR)
library(caret)
library(tidyr)
library(dplyr)
library(xgboost)
library(dummies)
library(ggplot2)
animal <- fread("animal_octoparse.csv")

setwd("C:/Users/JIWON/Desktop/URP")

## checking the class distribution of data set
animal <- fread("animal_octoparse.csv")
table(animal$redlistCategory)
prop.table(table(animal$redlistCategory))

## make train/test 
DD <- animal %>% filter(redlistCategory == "Data Deficient")
animal <- animal %>% filter(redlistCategory != "Data Deficient")

set.seed(2019)
id <- createDataPartition(animal$redlistCategory, p = 0.7, list = F)
prop.table(table(animal$redlistCategory))

## whole parameter tuning
category <- unique(animal$redlistCategory)
major_group <- "Least Concern"
minor_group <- setdiff(category,major_group)

over <- seq(1000,10000,length = 5)
smote_tune <- as.data.frame(matrix(NA,30,3))
colnames(smote_tune) <- c('over','class','rate')
k = 1

for (j in 1:length(minor_group)){
  for (i in 1:length(over)){ 
    
    train <- animal[id,]
    valid <- animal[-id,]
    train <- train %>% mutate_if(is.character,as.factor)
    valid <- valid %>% mutate_if(is.character,as.factor)
    
    major <- ifelse(train$redlistCategory == minor_group[j], 1, 0)
    train$redlistCategory <- major %>% as.factor() #need to be factor to smote
    major_v <- ifelse(valid$redlistCategory == minor_group[j], 1, 0)
    valid$redlistCategory <- major_v
    
    newanimal <- SMOTE(redlistCategory ~ ., train, perc.over = over[i])
    
    phy <- as.data.frame(dummy(newanimal$phylum))
    cla <- as.data.frame(dummy(newanimal$class))
    pop <- as.data.frame(dummy(newanimal$populationTrend))
    thr <- as.data.frame(dummy(newanimal$threats))
    use <- as.data.frame(dummy(newanimal$useTrade))
    con <- as.data.frame(dummy(newanimal$conservationAction))
    
    newanimal <- cbind(phy,cla,pop,thr,use,con,newanimal)
    newanimal <- select(newanimal, -phylum,-class,-populationTrend,-threats,-useTrade,-conservationAction)
    newanimal$redlistCategory <- as.numeric(newanimal$redlistCategory) - 1 # make numeric again
    
    x.train <- xgb.DMatrix(as.matrix(newanimal %>% select(-redlistCategory)),
                           label = newanimal$redlistCategory)
    
    xgb_model <- xgb.train(objective = "binary:logistic", data = x.train, nrounds = 100)
    
    phy2 <- as.data.frame(dummy(valid$phylum))
    cla2 <- as.data.frame(dummy(valid$class))
    pop2 <- as.data.frame(dummy(valid$populationTrend))
    thr2 <- as.data.frame(dummy(valid$threats))
    use2 <- as.data.frame(dummy(valid$useTrade))
    con2 <- as.data.frame(dummy(valid$conservationAction))
    
    valid <- cbind(phy2,cla2,pop2,thr2,use2,con2,valid)
    valid <- select(valid, -phylum,-class,-populationTrend,-threats,-useTrade,-conservationAction)
    # valid$redlistCategory is already numeric
    
    x.valid <- xgb.DMatrix(as.matrix(valid %>% select(-redlistCategory)),
                           label = valid$redlistCategory)
    
    xgb_val_preds <- predict(xgb_model, newdata = x.valid)
    xgb_val_out <- as.numeric(xgb_val_preds > 0.5) %>% as.factor()
    redlistCategory <- valid$redlistCategory %>% as.factor() # make factor to get confusion matrix
    
    xgb_val_conf <- confusionMatrix(xgb_val_out, redlistCategory, mode = "sens_spec")
    rate <- xgb_val_conf$table[2,2]/(xgb_val_conf$table[2,1]+xgb_val_conf$table[2,2])
    
    smote_tune[k,1] <- over[i]
    smote_tune[k,2] <- minor_group[j]
    smote_tune[k,3] <- rate
    k <- k+1
    print(i)
    
  }
}


smote_tune

## delecate parameter tuning ex)EW

over <- seq(10000,40000,length = 5) # change to specific parameter range
k = 26 # change to specific spot

for (i in 1:length(over)){ 
  
  train <- animal[id,]
  valid <- animal[-id,]
  train <- train %>% mutate_if(is.character,as.factor)
  valid <- valid %>% mutate_if(is.character, as.factor)
  
  major <- ifelse(train$redlistCategory == "EW", 1, 0)
  train$redlistCategory <- major %>% as.factor()
  major_v <- ifelse(valid$redlistCategory == "EW", 1, 0)
  valid$redlistCategory <- major_v
  
  newanimal <- SMOTE(redlistCategory ~ ., train, perc.over = over[i])
  
  phy <- as.data.frame(dummy(newanimal$phylum))
  cla <- as.data.frame(dummy(newanimal$class))
  pop <- as.data.frame(dummy(newanimal$populationTrend))
  thr <- as.data.frame(dummy(newanimal$threats))
  use <- as.data.frame(dummy(newanimal$useTrade))
  con <- as.data.frame(dummy(newanimal$conservationAction))
  
  newanimal <- cbind(phy,cla,pop,thr,use,con,newanimal)
  newanimal <- select(newanimal, -phylum,-class,-populationTrend,-threats,-useTrade,-conservationAction)
  newanimal$redlistCategory <- as.numeric(newanimal$redlistCategory) - 1 
  
  x.train <- xgb.DMatrix(as.matrix(newanimal %>% select(-redlistCategory)),
                         label = newanimal$redlistCategory)
  
  xgb_model <- xgb.train(objective = "binary:logistic", data = x.train, nrounds = 100)
  
  
  phy2 <- as.data.frame(dummy(valid$phylum))
  cla2 <- as.data.frame(dummy(valid$class))
  pop2 <- as.data.frame(dummy(valid$populationTrend))
  thr2 <- as.data.frame(dummy(valid$threats))
  use2 <- as.data.frame(dummy(valid$useTrade))
  con2 <- as.data.frame(dummy(valid$conservationAction))
  
  valid <- cbind(phy2,cla2,pop2,thr2,use2,con2,valid)
  valid <- select(valid, -phylum,-class,-populationTrend,-threats,-useTrade,-conservationAction)
  
  x.valid <- xgb.DMatrix(as.matrix(valid %>% select(-redlistCategory)),
                         label = valid$redlistCategory)
  
  xgb_val_preds <- predict(xgb_model, newdata = x.valid)
  xgb_val_out <- as.numeric(xgb_val_preds > 0.5) %>% as.factor()
  redlistCategory <- valid$redlistCategory %>% as.factor()
  xgb_val_conf <- confusionMatrix(xgb_val_out, redlistCategory, mode = "sens_spec")
  rate <- xgb_val_conf$table[2,2]/(xgb_val_conf$table[2,1]+xgb_val_conf$table[2,2])
  
  smote_tune[k,1] <- over[i]
  smote_tune[k,2] <- "EW"
  smote_tune[k,3] <- rate
  k <- k+1
  print(i)
}


### check with different validation

best_smote <- c(400,250,400,550,550,1000)
saved_animal <- NULL

for (b in 1:6){
  
  animal <- fread("animal_octoparse.csv")
  animal <- animal %>% mutate_if(is.character,as.factor)
  binary <- ifelse(animal$redlistCategory == minor_group[b], minor_group[b], "delete")
  animal$redlistCategory <- binary %>% as.factor()
  
  newanimal <- SMOTE(redlistCategory ~ ., animal, perc.over = best_smote[b])
  save <- newanimal %>% filter(redlistCategory != "delete")
  saved_animal <- rbind(saved_animal, save)
}

set.seed(2011)
id <- createDataPartition(animal$redlistCategory, p = 0.7, list = F)
animal <- fread("animal_octoparse.csv")
train <- saved_animal
train$redlistCategory <- as.character() %>% as.factor() # erase "delete" category
valid <- animal[-id,]
train <- train %>% mutate_if(is.character, as.factor)
valid <- valid %>% mutate_if(is.character,as.factor)

phy <- as.data.frame(dummy(train$phylum))
cla <- as.data.frame(dummy(train$class))
pop <- as.data.frame(dummy(train$populationTrend))
thr <- as.data.frame(dummy(train$threats))
use <- as.data.frame(dummy(train$useTrade))
con <- as.data.frame(dummy(train$conservationAction))

train <- cbind(phy,cla,pop,thr,use,con,train)
train <- select(train, -phylum,-class,-populationTrend,-threats,-useTrade,-conservationAction)
train$redlistCategory <- as.numeric(train$redlistCategory) - 1 
x.train <- xgb.DMatrix(as.matrix(train %>% select(-redlistCategory)),label = train$redlistCategory)

xgb_model <- xgb.train(booster = "gbtree", 
                       num_class = 7,
                       objective ="multi:softprob", 
                       data = x.train, 
                       nrounds = 100)

phy2 <- as.data.frame(dummy(valid$phylum))
cla2 <- as.data.frame(dummy(valid$class))
pop2 <- as.data.frame(dummy(valid$populationTrend))
thr2 <- as.data.frame(dummy(valid$threats))
use2 <- as.data.frame(dummy(valid$useTrade))
con2 <- as.data.frame(dummy(valid$conservationAction))
valid <- cbind(phy2,cla2,pop2,thr2,use2,con2,valid)
valid <- select(valid, -phylum,-class,-populationTrend,-threats,-useTrade,-conservationAction)
valid$redlistCategory <- as.numeric(valid$redlistCategory) - 1 
x.valid <- xgb.DMatrix(as.matrix(valid %>% select(-redlistCategory)), label = valid$redlistCategory)

xgb_val_preds <- predict(xgb_model, newdata = x.valid)
xgb_val_out <- matrix(xgb_val_preds, nrow = 7, ncol = length(xgb_val_preds) / 7) %>% 
  t() %>%
  data.frame() %>%
  mutate(max = max.col(., ties.method = "last"), label = valid$redlistCategory + 1) 
xgb_val_conf <- confusionMatrix(factor(xgb_val_out$max), factor(xgb_val_out$label), mode = "everything")

F1 <- xgb_val_conf$byClass[,7]
total_F1 <- 7/(1/F1[1]+1/F1[2]+1/F1[3]+1/F1[4]+1/F1[5]+1/F1[6]+1/F1[7])
total_F1


### get final whole smote

best_smote <- c(400,250,400,550,550,1000)
saved_animal <- NULL

for (b in 1:6){
  
  animal <- fread("animal_octoparse.csv")
  animal <- animal %>% mutate_if(is.character,as.factor)
  binary <- ifelse(animal$redlistCategory == minor_group[b], minor_group[b], "delete")
  animal$redlistCategory <- binary %>% as.factor()
  
  newanimal <- SMOTE(redlistCategory ~ ., animal, perc.over = best_smote[b])
  save <- newanimal %>% filter(redlistCategory != "delete")
  saved_animal <- rbind(saved_animal, save)
}

animal <- fread("animal_octoparse.csv")
animal <- animal %>% filter(redlistCategory != "Data Deficient")
#prop.table(table(animal$redlistCategory))
#table(animal$redlistCategory)
final_animal <- rbind(saved_animal,filter(animal, redlistCategory == "Least Concern"))
#prop.table(table(final_animal$redlistCategory))
#table(final_animal$redlistCategory)





##############################################################################################
#################################### M O D E L I N G #########################################
##############################################################################################

animal <- fread("animal_전처리완료.csv", encoding = "UTF-8") %>% as_tibble
animal %>% str
### SVM --------------------------------------------------------------------------------------------------------------
library("e1071")

animal_svm <- animal %>% select(-1,-2,-4);rm(animal)
animal_svm$redlistCategory <-  as.factor(animal_svm$redlistCategory)
animal_svm$populationTrend <- as.factor(animal_svm$populationTrend)

# Encoding 
animal_svm$redlistCategory %>% unique  ; animal_svm$redlistCategory %>% unique %>% as.numeric()
animal_svm$populationTrend %>% unique  ; animal_svm$populationTrend %>% unique %>% as.numeric()


animal_svm$redlistCategory <- animal_svm$redlistCategory %>% as.numeric %>% as.factor 
animal_svm$populationTrend <- animal_svm$populationTrend %>% as.numeric %>% as.factor 
animal_svm[is.na(animal_svm)] <- c(-1) # NA 값 -1로 대체


#write.csv(animal_svm, "animal_lightgbm.csv", row.names = F)


# Holdout 데이터 분리
holdout <- animal_svm %>% filter(redlistCategory == 2)
animal_svm <- animal_svm %>% filter(redlistCategory != 2)




# train & test split
library(caTools) 


set.seed(123) 
split = sample.split(animal_svm$redlistCategory, SplitRatio = 0.8) 




train = subset(animal_svm, split == TRUE) 
test = subset(animal_svm, split == FALSE) 
table(train$redlistCategory)/nrow(train)*100 ; table(test$redlistCategory)/nrow(test)*100



# 10-fold cross validation

svm_cv_model <- tune.svm(redlistCategory ~ ., 
                             data = train, 
                             gamma = 10^(-3:-1), 
                             cost = 10^(-1:1) )
summary(svm_cv_model)





# Final fitting 
svm_final_model <- svm(formula = redlistCategory ~ ., 
                  data = train, 
                  type = 'C-classification', 
                  kernel = 'linear') 
y_pred = predict(svm_final_model, newdata = test %>% select(-redlistCategory)) 

table(test$redlistCategory, y_pred)


### LightGBM ------------------------------------------------------------------------------------------

library(data.table)
library(Matrix)
library(dplyr)
install.packages("MLmertrics")
install.packages("lightgbm")
library(MLmetrics)
library(lightgbm)
set.seed(257)










####################################################################################################################################################################################
####################################################################################################################################################################################
################################################################################################################################################################################################################################################
########################################################################################################################################################################################################
#### 해석모델 (Soongyu Part)------------------------------------------------------------------------------------

library(data.table)
library(DMwR)
library(caret)
library(tidyr)
library(dplyr)
library(xgboost)
library(dummies)
library(ggplot2)
library(foreign)
library("nnet")

setwd("C:/Users/soong/Desktop/U.R.P/DATA")

# 데이터 불러오기
#animal <- fread("animal_after_embedding.csv") #smote <- fread("smote_final.csv") %>% as_tibble()
#animal2 <- fread("animal_after_embedding.csv") #smote <- fread("smote_final.csv") %>% as_tibble()
animal <- fread("animal_smote.csv") %>% as_tibble


# Extinct                       : Ex 멸종
# Critically Endangered         : CE 매우 위험
# Endangered                    : En 위험

# Vulnerable                    : V  취약 (기준)
# Near Threatened               : NT  살짝 위기
# Least Concern                 : LC  안전




### Ordinal Logistic Regression -------------------------------------------------------------------------
library(data.table)
library(DMwR)
library(caret)
library(tidyr)
library(dplyr)
library(xgboost)
library(dummies)
library(ggplot2)
library(foreign)
library("nnet")
library(VGAM)
library(tibble)
setwd("C:/Users/soong/Desktop/U.R.P/DATA")

# 데이터 불러오기
#animal <- fread("animal_after_embedding.csv") %>% as_tibble #smote <- fread("smote_final.csv") %>% as_tibble()
animal <- fread("animal_smote.csv") %>% as_tibble

# Holdout 데이터 분리
holdout <- animal %>% filter(redlistCategory == "DD")
animal <- 
  animal %>% 
  filter(redlistCategory != "DD") 

# 해석이 불가능한 변수들 제거
animal <- animal[,-c(28,29,30,31,32)]


# Ordinal Logistic 을 적합하기 위해, 반응변수들을 숫자로 변환 (숫자가 작을수록 멸종위험 높음)
animal[animal == "Ex"] <- 1
animal[animal == "CE"] <- 2
animal[animal == "En"] <- 3
animal[animal == "V"] <- 4
animal[animal == "NT"] <- 5
animal[animal == "LC"] <- 6

# 팩터처리
colnames(animal)[3] <- "y"

animal$y <- as.ordered(animal$y)
animal$phylum <- as.factor(animal$phylum)
animal$class <- as.factor(animal$class)
animal$populationTrend <- as.factor(animal$populationTrend)
animal$threats <- as.factor(animal$threats)
animal$useTrade <- as.factor(animal$useTrade)
animal$conservationAction <- as.factor(animal$conservationAction)



# 팩터변수들 숫자 인코딩   : 해야되나 ? ? ?? ? 
#animal <- animal %>% mutate_at(c(1,2,4,5,6,7), as.numeric)
#animal <- animal %>% mutate_at(c(1,2,4,5,6,7), as.factor)



# 팩터변수들 table 확인
lapply(animal[, c("phylum", "class", "y", "populationTrend",
                  "threats", "useTrade", "conservationAction")], table)

animal$y %>% table


## 비례오즈가정 테스트
#1) Chi-square Test      -> Not best
model_parallel <- vglm(y ~ . , 
                       data = animal,
                       Hess = TRUE, 
                       family = cumulative(link = "logit", parallel = TRUE))

model_nonparallel <- vglm(redlistCategory ~ . , 
                          data = animal,
                          Hess = TRUE, 
                          family = cumulative(link = "logit", parallel = FALSE))


1 - pchisq(deviance(model_parallel) - deviance(model_nonparallel),
           df = ((5 + 5*26)-(5 + 26))) # H0 : 비례오즈이다


#2) Graphical Test    -> Recommend
library(Hmisc)
'
방식 : 각 Logistic Regression(이항모델) 에서다
predicted 된 로짓을 그린다. (단 하나의 x만 가지고)
이 로짓이 같으면 , 비례오즈가정이 어느정도 성립하는것.

Harrel 의 논문 참조하기

'

sf <- function(y) {
  c('y>=1' = qlogis(mean(y >= 1)),
    'y>=2' = qlogis(mean(y >= 2)),
    'y>=3' = qlogis(mean(y >= 3)),
    'y>=4' = qlogis(mean(y >= 4)),
    'y>=5' = qlogis(mean(y >= 5)),
    'y>=6' = qlogis(mean(y >= 6)))
}


s1 <- with(animal, summary(as.numeric(y) ~ 
                            phylum + class + populationTrend +
                            threats + useTrade + conservationAction,
                           fun = sf))

compare1 <- data.frame(cbind((s1[,3]-s1[,4]), (s1[,4]-s1[,5]), (s1[,5]-s1[,6]), (s1[,6]-s1[,7]) ))
colnames(compare1) <- c("difference1", "difference2", "difference3", "difference4")

# phylum 변수에 대한 binary logistic Regression 적합해보자
phylum2 <- glm(I(as.numeric(y) >= 2) ~ phylum,
    family = binomial(link = "logit"), data = animal)
phylum2$coefficients %>% exp

phylum3 <- glm(I(as.numeric(y) >= 3) ~ phylum,
              family = binomial(link = "logit"), data = animal)
phylum3$coefficients %>% exp

phylum4 <- glm(I(as.numeric(y) >= 4) ~ phylum,
               family = binomial(link = "logit"), data = animal)
phylum4$coefficients %>% exp


binary_logistic_model <- glm(I(as.numeric(y) <= 3) ~ ., 
                             family = binomial(link = "logit"),
                             data = animal)
summary(binary_logistic_model)


s2 <- with(animal, summary(as.numeric(y) ~
                            Freshwater + Marine + Terrestrial + MarineNeritic + Unknown + 
                            Wetlandsinland + Forest + Grassland + Shrubland + ArtificialAquaticMarine,
                           fun = sf))
compare2 <- data.frame(cbind((s2[,3]-s2[,4]), (s2[,4]-s2[,5]), (s2[,5]-s2[,6]), (s2[,6]-s2[,7]) ))
colnames(compare2) <- c("difference1", "difference2", "difference3", "difference4")


s3 <- with(animal, summary(as.numeric(y) ~                              
                            Rockyareas + ArtificialTerrestrial + Savanna + Desert + CavesandSubterraneanHabitatsnonaquatic +
                            MarineDeepBenthic + MarineIntertidal + MarineOceanic + MarineCoastalSupratidal, 
                            fun = sf))
compare3 <- data.frame(cbind((s3[,3]-s3[,4]), (s3[,4]-s3[,5]), (s3[,5]-s3[,6]), (s3[,6]-s3[,7]) ))
colnames(compare3) <- c("difference1", "difference2", "difference3", "difference4")

library(pander)
rbind(compare1,compare2,compare3) 

plot(s1,
     which = 1:6,
     pch = 1:6,
     xlab = 'logit',
     main = 'Interpretation Plot',
     xlim = range(-5:5))

plot(s2,
     which = 1:6,
     pch = 1:6,
     xlab = 'logit',
     main = 'Interpretation Plot',
     xlim = range(-5:5))

plot(s3,
     which = 1:6,
     pch = 1:6,
     xlab = 'logit',
     main = 'Interpretation Plot',
     xlim = range(-5:5))

par(mfrow = c(1,1))
### 최종 모형 적합

library(MASS)

ordinal_logistic_model2 <- vglm(y ~ . , 
                          data = animal,
                          Hess = TRUE, 
                          family = cumulative(link = "logit", parallel = TRUE))

summary(ordinal_logistic_model2)


### Logistic Regression (Binary) ---------------------------------------------------------------------------------------------
logistic_model <- glm(I(y <= 4) ~ . , 
                    data = animal,
                    family = binomial(link='logit'))



summary(logistic_model)






### GAM --------------------------------------------------------------------------------------------------------
'
설명변수 유형에 따라 여러가지 GAM 플랏을 그려보자

'
library(gam)
animal %>% str

## 1) Full model  : 풀모델로하든 부분모델로하든 각 변수의 플랏은 똑같음  -> 풀모델 할 필요가 없ㅇ

gam_full <- mgcv::gam(I(y <= 4) ~ . , 
                         data = animal[,c(1,2,3,4,5,6,7)],
                         family = binomial)


summary(gam_full)

par(mfrow = c(3,2))
plot(gam_full, se = T)



plot(gam_full$additive.predictors, se = TRUE)


# Visualization with ggplot
library(voxel)
gam_full <- mgcv::gam(I(y < 4) ~  phylum + class , 
                      data = animal,
                      family = binomial)


plotGAM(gamFit = gam_full,
        smooth.cov = 'phylum',
        groupCovs = 'class',
        orderedAsFactor = FALSE,
        rawOrFitted = 'raw',
        plotCI = TRUE)


gam <- mgcv::gam(y ~ s(x) + group, data=data)

plot1 <- plotGAM(gamFit = gam, smooth.cov = "x", groupCovs = NULL,
                 rawOrFitted = "raw", plotCI=TRUE, orderedAsFactor = FALSE)
gam <- mgcv::gam(y ~ s(x) + group + s(x, by=group), data=data)
plot2 <- plotGAM(gamFit = gam, smooth.cov = "x", groupCovs = "group",
                 rawOrFitted = "raw", orderedAsFactor = FALSE)
plot1

plot2
















## 2) Phylum, Class   : 해당 종의 카테고리에 관한 해석 모델

#logistic_category_gam1 <- gam(I(y <= 2) ~ phylum+ class,
                     data = animal,
                     family = binomial)

logistic_category_gam2 <- gam(I(y <= 3) ~ phylum+ class,
                     data = animal,
                     family = binomial)

#logistic_category_gam3 <- gam(I(y <= 6) ~ phylum + class,
                     data = animal,
                     family = binomial)

par(mfrow = c(3,3))

plot(logistic_category_gam1, se = T)
plot(logistic_category_gam2, se = T)
plot(logistic_category_gam3, se = T)



## 3) Threats, Use Trade, Conservation Action
logistic_factor_gam1 <- gam(I(y <= 2) ~ threats + useTrade + conservationAction,
                              data = animal,
                              family = binomial)
logistic_factor_gam2 <- gam(I(y <= 4) ~ threats + useTrade + conservationAction,
                              data = animal,
                              family = binomial)
logistic_factor_gam3 <- gam(I(y <= 6) ~ threats + useTrade + conservationAction,
                              data = animal,
                              family = binomial)


par(mfrow = c(1,3))
plot(logistic_factor_gam1, se = T)
plot(logistic_factor_gam2, se = T)
plot(logistic_factor_gam3, se = T)


## 4) Systems: 체계에 관한 모델 -Freshwater/Marine/Terrestrial/MarineNeritic/Unknown 

logistic_systems_gam1 <- gam(I(y <= 2) ~ Freshwater + Marine + Terrestrial + MarineNeritic + Unknown,
                            data = animal,
                            family = binomial)
logistic_systems_gam2 <- gam(I(y <= 4) ~ Freshwater + Marine + Terrestrial + MarineNeritic + Unknown,
                             data = animal,
                             family = binomial)
logistic_systems_gam3 <- gam(I(y <= 6) ~ Freshwater + Marine + Terrestrial + MarineNeritic + Unknown,
                             data = animal,
                             family = binomial)


par(mfrow = c(2,3))
plot(logistic_systems_gam1)
plot(logistic_systems_gam2)
plot(logistic_systems_gam3)


## 5) Habitat : 서식지에 관한 모델 
                 # -"Wetlandsinland / Forest /Grassland  /Shrubland   /ArtificialAquaticMarine   /Rockyareas /ArtificialTerrestrial /Savanna /Desert    /
#CavesandSubterraneanHabitatsnonaquatic/MarineDeepBenthic   / MarineIntertidal  /MarineOceanic    / MarineCoastalSupratidal  "

logistic_habitat_gam1 <- gam(I(y <= 2) ~ Wetlandsinland + Forest + Grassland + Shrubland +
                               ArtificialAquaticMarine + Rockyareas + ArtificialTerrestrial + 
                               Savanna + Desert + CavesandSubterraneanHabitatsnonaquatic + 
                               MarineDeepBenthic + MarineIntertidal + MarineOceanic + 
                               MarineCoastalSupratidal + Blank,
                             data = animal,
                             family = binomial)

logistic_habitat_gam1 <- gam(I(y <= 4) ~ Wetlandsinland + Forest + Grassland + Shrubland +
                               ArtificialAquaticMarine + Rockyareas + ArtificialTerrestrial + 
                               Savanna + Desert + CavesandSubterraneanHabitatsnonaquatic + 
                               MarineDeepBenthic + MarineIntertidal + MarineOceanic + 
                               MarineCoastalSupratidal + Blank,
                             data = animal,
                             family = binomial)

logistic_habitat_gam1 <- gam(I(y <= 6) ~ Wetlandsinland + Forest + Grassland + Shrubland +
                               ArtificialAquaticMarine + Rockyareas + ArtificialTerrestrial + 
                               Savanna + Desert + CavesandSubterraneanHabitatsnonaquatic + 
                               MarineDeepBenthic + s(MarineIntertidal, df = 4) + s(MarineOceanic, df = 4) + 
                               s(MarineCoastalSupratidal) + Blank,
                             data = animal,
                             family = binomial)


par(mfrow = c(3,3))
plot(logistic_habitat_gam1)
plot(logistic_habitat_gam2)
plot(logistic_habitat_gam3)









###  Multinomial Logistic Regression ------------------------------------------------------------------------------------------



##  변수들팩터처리
animal$phylum <- as.factor(animal$phylum)
animal$class <- as.factor(animal$class)
#animal$redlistCategory <- as.factor(animal$redlistCategory)
animal$populationTrend <- as.factor(animal$populationTrend)
animal$threats <- as.factor(animal$threats)
animal$useTrade <- as.factor(animal$useTrade)
animal$conservationAction <- as.factor(animal$conservationAction)





glm_practice <- multinom(redlistCategory ~ ., data = animal[,c(1,2,3,4,5,6,7,8,9)])
summary(glm_practice)


# 
animal$redlistCategory <- relevel(animal$redlistCategory, ref = "Ex")

glm_model1 <- multinom(redlistCategory ~ ., data = animal)
summary(glm_model1)
exp(summary(glm_model1)$coefficients)
summary(glm_model1)$coefficients %>% exp()


library(AER)
library(car)

coeftest(glm_model1)  #왈드검정
anova(glm_model1, test = "Chisq") #이탈도 검정 
lrtest(glm_model1) # 가능도비 검정
vif(glm_model1) # 다중공선성 검정  -> 전부다 nan 뜸 
 






### Naive Bayesian Classifier ------------------------------------------------------------------------------------------
library(e1071)
?naiveBayes 

animal_NBC <- animal_glm


# train & test split
library(caTools) 
set.seed(777) 
split = sample.split(animal_NBC$redlistCategory, SplitRatio = 0.8) 
train = subset(animal_NBC, split == TRUE) 
test = subset(animal_NBC, split == FALSE) 



animal_NBC %>% str
#Fitting the Naive Bayes model
Naive_Bayes_Model = naiveBayes(redlistCategory ~ populationTrend + Freshwater, 
                               data = train)
#What does the model say? Print the model summary
Naive_Bayes_Model 






#Prediction on the dataset
NB_Predictions <- predict(Naive_Bayes_Model, test)
#Confusion matrix to check accuracy
table(NB_Predictions,test$redlistCategory)



