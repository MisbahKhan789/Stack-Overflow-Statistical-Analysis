#######################################################################################################################################
# The following script is answering:
#
#RQ7-3: Are there any that tend to attract users with particularly high or low reputation in questions 
#######################################################################################################################################

#STEP 1- LOADING DATA
# Replace PATH
tagsSO <- read.csv("/PATH/tags.csv")
questionsRepH <- read.csv("/PATH/questionsReputationH.csv")
questionsRepL <- read.csv("/PATH/questionsReputationL.csv")

#STEP 2- EDITING COLUMN HEADERS
names(questionsRepH) <- c("user_id", "user_name", "user_reputation", "question_id", "answer_count", "comment_count", "score", "view_count","tags")
View(head(questionsRepH, n=100))
names(questionsRepL) <- c("user_id", "user_name", "user_reputation", "question_id", "answer_count", "comment_count", "score", "view_count","tags")
View(head(questionsRepL, n=100))

#-------- Normalize tags in Questions ----------#

# required libraries 
library (data.table)
library (stringr)
library (splitstackshape)

#2. Find the end of each tag by locating ">" and 
# split the tags column by using cSplit fun to 5 distinct columns:
# tags_1, tags_2, tags_3, tags_4, tags_5
questionsRepH <- cSplit(questionsRepH, "tags", '>')

#3. Remove all '<' from the 5 sub-columns 
questionsRepH$tags_1 <- str_replace_all(questionsRepH$tags_1,"<","")
questionsRepH$tags_2 <- str_replace_all(questionsRepH$tags_2,"<","")
questionsRepH$tags_3 <- str_replace_all(questionsRepH$tags_3,"<","")
questionsRepH$tags_4 <- str_replace_all(questionsRepH$tags_4,"<","")
questionsRepH$tags_5 <- str_replace_all(questionsRepH$tags_5,"<","")

#------- End of Normalizing Tags in questionsRepH ------#

#get top 25% higher reputation#
queTopHighRep=head(questionsRepH,850164)
View(queTopHighRep)

#get lower 25% user reputation#
queLowRep <- questionsRepH[order(questionsRepH$user_reputation),] 
queTopLowRep=head(queLowRep,850164)
View(queTopLowRep)


#------- Counting the number of each distinct tag in queTopHighRep------#
result1 <- as.data.frame(table(factor(queTopHighRep$tags_1,levels=unique(tagsSO$tagname))))
colnames(result1) <- c("name","Count")
View(head(result1, n=100))

result2 <- as.data.frame(table(factor(queTopHighRep$tags_2,levels=unique(tagsSO$tagname))))
colnames(result2) <- c("name","Count")
View(head(result2, n=100))

result3 <- as.data.frame(table(factor(queTopHighRep$tags_3,levels=unique(tagsSO$tagname))))
colnames(result3) <- c("name","Count")
View(head(result3, n=100))

result4 <- as.data.frame(table(factor(queTopHighRep$tags_4,levels=unique(tagsSO$tagname))))
colnames(result4) <- c("name","Count")
View(head(result4, n=100))

result5 <- as.data.frame(table(factor(queTopHighRep$tags_5,levels=unique(tagsSO$tagname))))
colnames(result5) <- c("name","Count")
View(head(result5, n=100))

#sum all the counts per tag
total <- data.frame(result1$Count+result2$Count+result3$Count+result4$Count+result5$Count) 

result <- data.frame(tagsSO$tagname, total)
names(result) <- c("tags", "tcount")
# queTopHighRepTags is a sorted datafram has all the tags along with their counts in queTopHighRep
queTopHighRepTags <- data.frame(result)
View(queTopHighRepTags)
#order tags in decending order based on their count#
queTopHighRepTags <- queTopHighRepTags[order(-queTopHighRepTags$tcount),] 
#Select Top 10 tags used by high user reputation#
queTopHighRepTags=head(queTopHighRepTags,10)
#plot barplot for top 10 tags used by users with high reputation#
barplot(queTopHighRepTags$tcount, main="Top 10  tags in questions used by users with high reputation")
axis(1,at=mids,labels=queTopHighRepTags$tags)
View(queTopHighRepTags)

#------ End Top 10 tags in questions used by users with high reputation  ----#



#------- Counting the number of each distinct tag in queTopLowRep------#
result1 <- as.data.frame(table(factor(queTopLowRep$tags_1,levels=unique(tagsSO$tagname))))
colnames(result1) <- c("name","Count")
View(head(result1, n=100))

result2 <- as.data.frame(table(factor(queTopLowRep$tags_2,levels=unique(tagsSO$tagname))))
colnames(result2) <- c("name","Count")
View(head(result2, n=100))

result3 <- as.data.frame(table(factor(queTopLowRep$tags_3,levels=unique(tagsSO$tagname))))
colnames(result3) <- c("name","Count")
View(head(result3, n=100))

result4 <- as.data.frame(table(factor(queTopLowRep$tags_4,levels=unique(tagsSO$tagname))))
colnames(result4) <- c("name","Count")
View(head(result4, n=100))

result5 <- as.data.frame(table(factor(queTopLowRep$tags_5,levels=unique(tagsSO$tagname))))
colnames(result5) <- c("name","Count")
View(head(result5, n=100))

#sum all the counts per tag
total <- data.frame(result1$Count+result2$Count+result3$Count+result4$Count+result5$Count) 

result <- data.frame(tagsSO$tagname, total)
names(result) <- c("tags", "tcount")
# queTopLowRepTags is a sorted datafram has all the tags along with their counts in queTopLowRep
queTopLowRepTags <- data.frame(result)
View(queTopLowRepTags)
#order tags in decending order based on their count#
queTopLowRepTags <- queTopLowRepTags[order(-queTopLowRepTags$tcount),] 
#Select Top 10 tags used by lower user reputation#
queTopLowRepTags=head(queTopLowRepTags,10)
#plot barplot for top 10 tags used by users with lower reputation#
barplot(queTopLowRepTags$tcount, main="Top 10  tags in questions used by users with low reputation")
axis(1,at=mids,labels=queTopLowRepTags$tags)
View(queTopLowRepTags)

#------ End Top 10 tags in questions used by users with low reputation  ----#

