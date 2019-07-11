library(okcupiddata)
data("profiles")

#Initial exploration
str(profiles)
View(profiles)


# Splitting out swear words -----------------------------------------------

#Start to build a dataframe of "Profiles" with columns for each swear word
Cleaned_profiles <- data.frame(profiles) #created a dataframe

length(grep("fuck",Cleaned_profiles$essay0)) #count of word 'fuck' in entire 'essay' column (146)

#Here are the swear words we are identifying
mystring1<-"fuck" 
mystring2 <- "shit"
mystring3 <- "cunt"
mystring4 <- "bitch"

#Function to count each swear word
countOccurr = function(text,motif) {
  res = gregexpr(motif,text,fixed=T)[[1]]
  ifelse(res[1] == -1, 0, length(res))
}

#Creating the extra columns for each swear word
Cleaned_profiles = cbind(Cleaned_profiles, count_of_Fuck = vapply(Cleaned_profiles$essay0, countOccurr, 1, motif=mystring1))
Cleaned_profiles = cbind(Cleaned_profiles, count_of_Shit = vapply(Cleaned_profiles$essay0, countOccurr, 1, motif=mystring2))
Cleaned_profiles = cbind(Cleaned_profiles, count_of_Cunt = vapply(Cleaned_profiles$essay0, countOccurr, 1, motif=mystring3))
Cleaned_profiles = cbind(Cleaned_profiles, count_of_Bitch = vapply(Cleaned_profiles$essay0, countOccurr, 1, motif=mystring4))


unique(Cleaned_profiles$count_of_F) #This shows us all the categorical answers available
unique(Cleaned_profiles$count_of_Shit)
unique(Cleaned_profiles$count_of_Cunt)
unique(Cleaned_profiles$count_of_Bitch)

Cleaned_profiles$Total_swear_words <- Cleaned_profiles$count_of_F + Cleaned_profiles$count_of_Shit + Cleaned_profiles$count_of_Cunt + Cleaned_profiles$count_of_Bitch

unique(Cleaned_profiles$Total_swear_words)

View(Cleaned_profiles)


# Plotting ----------------------------------------------------------------
sum(Cleaned_profiles$Total_swear_words,na.rm = TRUE) 
#There are 371 instances of the use of the words "fuck", "shit", "cunt" and/or "bitch" in the dataset

plot(Cleaned_profiles$Total_swear_words ~ Cleaned_profiles$age)
#From the plot we can see that there is a higher concentration of swear words in younger people. 
#There are only 3 instances of swear words being 3+ times; we can consider these (very rude) outliers. 
