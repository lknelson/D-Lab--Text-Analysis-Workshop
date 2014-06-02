#!/usr/bin/Rscript

library(tm)

setwd("/home/laura/Desktop/My Documents/DigitalHumanities/ceos_textanalysis/ceo_sentiment/text_diff")

a <- Corpus(DirSource("/home/laura/Desktop/My Documents/DigitalHumanities/ceos_textanalysis/ceo_sentiment/text_diff/diff_nyt")) #the directory should have two text files in it, the files you want to compare
summary(a)

a <- tm_map(a, tolower) # convert all text to lower case
a <- tm_map(a, removePunctuation) 
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removeWords, stopwords("english"))

library(SnowballC) # also needed for stemming function
a <- tm_map(a, stemDocument, language = "english") # converts terms to tokens
a.dtm <- TermDocumentMatrix(a) #puts tokens into a term document matrix
inspect(a.dtm[1:2,1:2]) # have a quick look at the term document matrix
findFreqTerms(a.dtm, lowfreq=100) # have a look at common words
nrow(a.dtm) #gives you the number of unique words
a.dtm.sp <- removeSparseTerms(a.dtm, sparse=0.50) #remove sparse terms, the sparse number should be higher with a large number of documents, smaller with small number of documents, always less than 1
nrow(a.dtm.sp) #compare the number of unique words after removing sparse terms
a.dtm.sp.df <- as.data.frame(inspect(a.dtm.sp)) # convert document term matrix to data frame
nrow(a.dtm.sp.df) # check to see how many words are left
a.dtm.sp.df <- t(a.dtm.sp.df) #transpose matrix
rowTotals <- apply(a.dtm.sp.df, 1, sum) #create column with row totals, total number of words per document
head(rowTotals)
prop <- a.dtm.sp.df/rowTotals #change frequencies to proportions
prop <- t(prop)
head(prop)
data <- as.data.frame(prop)
data$diff <- data$men_nyt.txt - data$women_nyt.txt #creates new column (diff) which is the difference between columns 1 and 2, need to change the names to fit your file names
sorted <- data[with(data,order(data$diff)),] #sorts data by column diff
row.names(sorted)[1:40] #prints first row names, the top 40 words define your second file
row.names(sorted)[2741:2782] #prints last row names, change the numbers to be the last 40 rows, the top 40 words definine your first file

#red <- sapply(sorted, function(x) `*`(x[1:40], 1000))
first_rows <- cbind(row.names(sorted)[1:40], sorted[1:40,3]) #prints last row names
last_rows <- cbind(row.names(sorted)[3455:3494], sorted[3455:3494,3]) #prints last row names, change the numbers to fit your matrix

write.csv(first_rows, file = "diffprop_women_terms.csv") #writes csv file with the first rows and weights
write.csv(last_rows, file = "diffprop_men_terms.csv") #dito with the last rows





