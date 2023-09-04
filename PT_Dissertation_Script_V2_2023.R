############################## PT Dissertation Project 

############# Part 1: Cleaning

##Prep environment
## Clear
rm(list = ls())

install.packages("naniar")
install.packages("data.table")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("plyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("psych")
install.packages("aod")
install.packages("caret")

## Load packages
library(naniar)
library(data.table)
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)
library(psych)
library(aod)
library(caret)
install.packages("beepr")
library(beepr)
beep (sound = 1, expr = NULL)
library(readr)

setwd("C:/Users/phoeb/OneDrive - University of Bristol/MSc Epidemiology/Unit 8 Dissertation/Data/realdata")
setwd("C:/Users/xv19729/OneDrive - University of Bristol/MSc Epidemiology/Unit 8 Dissertation/Data/realdata")

## Removed cats, German cases, and cases with missing breed information in Excel 

##### Standardise breeds in Excel against female dataset
#Read in reviewed and corrected breeds
males <- read_csv("Males.csv")
View(males)

## Preview data
str(males)

## Make copy
males2 <- males
males <- as_tibble(males2)

## Simplify column names
names(males)[names(males) == "CaseBreed"] <- "Breed"
names(males)[names(males) == "CaseAnimalAgeYears"] <- "AgeYears"
names(males)[names(males) == "CaseAnimalAgeMonths"] <- "AgeMonths"
names(males)[names(males) == "CaseSex"] <- "SexNeuterStatus"
names(males)[names(males) == "CaseHistologicalDiagnosis"] <- "HistDiagnosis"

## Check new variable names
names(males)

## Delete perfectly duplicate rows
males<-unique(males)

## Checking 'unique' worked- 176736 had 3 duplicates, now 1 remaining
males[males$CaseNumber == '176736',]

## Pull out all rows with duplicate case numbers
maleCNdups <-males[duplicated(males$CaseNumber)|duplicated(males$CaseNumber, fromLast=TRUE),]
table(maleCNdups$CaseNumber)

## Export, keep rows with more information. import back in. Assign different case numbers to different individuals with same CN.
view(maleCNdups)
getwd()
install.packages("writexl")
library(writexl)
write_xlsx(maleCNdups, "CNdups.xlsx")

## Read back in 
library(readxl)
CNnondups <- read_csv("CNdups.csv")
View(CNnondups)

## Check it has worked by checking for duplicate case numbers again
dupcheck <- CNnondups[duplicated(CNnondups$CaseNumber)|duplicated(CNnondups$CaseNumber, fromLast=TRUE),]
write_xlsx(dupcheck, "CNdups2.xlsx")

## Read in final version and add non-duplicates back to total dataset
CNnondupsfinal <- read_csv("CNdups2complete.csv")
str(males)
str(CNnondupsfinal)
CNnondupsfinal$CaseNumber <- as.numeric(CNnondupsfinal$CaseNumber)
Males <- setdiff(males, CNnondups)
males <- rbind(Males,CNnondupsfinal)
rm(Males)

## Assign age categories to males
males$Age <- males$AgeYears
males$Age[males$AgeYears <3] <- "<3"
males$Age[males$AgeYears >=3 & males$AgeYears < 6] <- "3 to <6"
males$Age[males$AgeYears >=6 & males$AgeYears < 9] <- "6 to <9"
males$Age[males$AgeYears >=9 & males$AgeYears < 12] <- "9 to <12"
males$Age[males$AgeYears >=12] <- ">12"

##Assign NA to missing age
males$AgeYears[males$AgeYears==0] <- "NA" 
males$AgeMonths[males$AgeMonths==0] <- "NA"
males$Age[males$AgeYears=='NA' & males$AgeMonths=='NA'] <- "NA" 

table(males$Age) 

## Check that older ages are feesible- 1=77, 1=87, 2=89
agecheck <- males %>% filter(Age=='>12')
agecheck2 <- males %>% filter(Age== '<3')
table(agecheck$AgeYears)
table(agecheck2$AgeYears)

##Assign NA to above ages
males$AgeYears[males$AgeYears==77] <- "NA"
males$AgeYears[males$AgeYears==87] <- "NA"
males$AgeYears[males$AgeYears==89] <- "NA"
agecheck3 <- males %>% filter(Age=='>12')
table(agecheck3$AgeYears)

## Check how many cases have missing age (=1770) i.e., check NA assignment has worked
AgeNAs<-males %>% filter(AgeYears == 'NA' & AgeMonths == 'NA')
view(AgeNAs)
rm(AgeNAs)

##Importing the female dataset- for now have deleted columns 'Casebreed', 'final case breed' and 'mammary Y/N'so matches males
females <- read_csv("AllFemales_working.csv")
view(females)

## Final check of females
write_xlsx(females, "femalescheck.xlsx")
females<- read_csv("femalescheck.csv")

##Make copy
females2<-females
females <- as_tibble(females2)

##Preview data
str(females)

##Simplify column names for females as per males
names(females)[names(females) == "#"] <- "CaseNumber"
names(females)[names(females) == "CondolidateCaseBreed"] <- "Breed"
names(females)[names(females) == "CaseAnimalAgeYears"] <- "AgeYears"
names(females)[names(females) == "CaseAnimalAgeMonths"] <- "AgeMonths"
names(females)[names(females) == "CaseSex"] <- "SexNeuterStatus"
names(females)[names(females) == "CaseHistologicalDiagnosis"] <- "HistDiagnosis"

names(females)

##Fix classes (compare to males)
females$AgeYears <- as.numeric(females$AgeYears)
females$AgeMonths <- as.numeric(females$AgeMonths)
females$CaseNumber <- as.numeric(females$CaseNumber)
str(males)
str(females)

##Assign ages to females
females$Age <- females$AgeYears
females$Age[females$AgeYears <3] <- "<3"
females$Age[females$AgeYears >=3 & females$AgeYears < 6] <- "3 to <6"
females$Age[females$AgeYears >=6 & females$AgeYears < 9] <- "6 to <9"
females$Age[females$AgeYears >=9 & females$AgeYears < 12] <- "9 to <12"
females$Age[females$AgeYears >=12] <- ">12"

##Assign NA to missing age
females$AgeYears[females$AgeYears==0] <- "NA" 
females$AgeMonths[females$AgeMonths==0] <- "NA"
females$AgeYears[females$AgeYears== 'NA'] <- "NA" 
females$AgeMonths[females$AgeMonths== 'NA'] <- "NA" 
females$Age[females$AgeYears== 'NA' & females$AgeMonths=='NA'] <- "NA"
females$Age[!(females$Age %in% c('<3', '3 to <6', '6 to <9', '9 to <12', '>12'))] <- "NA"
table(females$Age) 

## Unique check on females
females<- unique(females)

##Combine males and females
totalset <- rbind(males, females)
str(totalset)

## Check ages have categorised correctly
totalset$AgeYears <- as.numeric(as.character(totalset$AgeYears))
totalset$AgeMonths <- as.numeric(as.character(totalset$AgeMonths))
sapply(totalset, class)
table(totalset$Age) 

##Check no cases are missing sex
NAsex_subset <- grep("NA", totalset$SexNeuterStatus, value = TRUE)
table(NAsex_subset)

## Final breed check- export for review
write_xlsx(totalset, "breedcheck.xlsx")

##Import checked breeds
totalset<- read_csv("breedschecked.csv")

## Assigning 'Other purebreds'
breeds <- as.data.frame(table(totalset$"Breed"))
breeds$CPercent <- (breeds$Freq/sum(breeds$Freq))*100
colnames(breeds) <- c("Breed", "Number", "Percentage")
breeds
breeds_under_0.001 <- filter(breeds, breeds$Percentage <0.001)
breeds_0.001 <- as.character(breeds_under_0.001$Breed)
totalset$Breed[totalset$Breed %in% breeds_0.001] <- "Other Purebred"
##Check
breeds <- as.data.frame(table(totalset$"Breed"))
breeds

##Re-categorising missing neuter status as 'unknown'
totalset<- totalset %>% mutate(SexNeuterStatus=recode(SexNeuterStatus, 'Male'='Male unknown', 'Female'='Female unknown'))
table(totalset$SexNeuterStatus)

## Export for review of histological diagnosis- exclude any with missing data for this variable
library(writexl)
write_xlsx(totalset, "finalcheck.xlsx")
totalset<- read_csv("finalcheck.csv")

#Categorise purebreds
breed1 <- as.data.frame(table(totalset$BreedStatus))
breed1 <- breed1$Var1
totalset$BreedStatus[(!(totalset$BreedStatus %in% breed1))] <- "Purebreed"
table(totalset$BreedStatus) 
rm(breed1)

## Clean up environment window
rm(agecheck, agecheck2, agecheck3, CNnondups, CNnondupsfinal, maleCNdups, dupcheck)

###################################################################################
#################### Part 2: Classifying tumours

totalset$HistDiagnosis [1:10]

install.packages("fuzzyjoin")
library (fuzzyjoin)
install.packages('fuzzywuzzyR')
library (fuzzywuzzyR)
library(dplyr)
library(stringr)


######################### Part 2a. Exact matching ####################################

## Reading in the malignant epithelial categorisations
malignant_epithelial <- read_csv("EpithelialM.csv")

## Finding matches for malignant epithelial tumours
malig_epithelial_matches <- totalset %>% regex_inner_join(malignant_epithelial, by = c(HistDiagnosis = "epithelialm_regex"))

##Display stats
table(malig_epithelial_matches$EpithelialM)


## Repeat with benign epithelial tumours
benign_epithelial <- read_csv("EpithelialB.csv")
ben_epithelial_matches <- totalset %>% regex_inner_join(benign_epithelial, by = c(HistDiagnosis = "epithelialb_regex"))
table(ben_epithelial_matches$EpithelialB)

## Repeat with spindle cell tumours
malignant_spindle <- read_csv("SpindlecellM.csv")
malig_spindle_matches <- totalset %>% regex_inner_join(malignant_spindle, by = c(HistDiagnosis = "spindlem_regex"))
table(malig_spindle_matches$SpindlecellM)
benign_spindle <- read_csv("SpindlecellB.csv")
ben_spindle_matches <- totalset %>% regex_inner_join(benign_spindle, by = c(HistDiagnosis = "spindleb_regex"))
table(ben_spindle_matches$SpindlecellB)

## Repeat for round cell tumours
malignant_round <- read_csv("RoundcellM.csv")
malig_round_matches <- totalset %>% regex_inner_join(malignant_round, by = c(HistDiagnosis = "roundcell_regex"))
table(malig_round_matches$Roundcell)
benign_round <- read_csv("RoundcellB.csv")
ben_round_matches <- totalset %>% regex_inner_join(benign_round, by = c(HistDiagnosis = "roundb_regex"))
table(ben_round_matches$RoundcellB)

## Repeat for mixed tumours
malignant_mixed <- read_csv("MixedM.csv")
malig_mixed_matches <- totalset %>% regex_inner_join(malignant_mixed, by = c(HistDiagnosis = "mixedm_regex"))
table(malig_mixed_matches$MixedM)
benign_mixed <- read_csv("MixedB.csv")
ben_mixed_matches <- totalset %>% regex_inner_join(benign_mixed, by = c(HistDiagnosis = "mixedb_regex"))
table(ben_mixed_matches$MixedB)

## Combine the prior 6 dataframes- adjust column names so able to be combined
names(malig_epithelial_matches)[names(malig_epithelial_matches) == "EpithelialM"] <- "Assigned diagnosis"
names(malig_epithelial_matches)[names(malig_epithelial_matches) == "epithelialm_regex"] <- "Regex"
names(malig_spindle_matches)[names(malig_spindle_matches) == "SpindlecellM"] <- "Assigned diagnosis"
names(malig_spindle_matches)[names(malig_spindle_matches) == "spindlem_regex"] <- "Regex"
names(malig_round_matches)[names(malig_round_matches) == "Roundcell"] <- "Assigned diagnosis"
names(malig_round_matches)[names(malig_round_matches) == "roundcell_regex"] <- "Regex"
names(ben_spindle_matches)[names(ben_spindle_matches) == "SpindlecellB"] <- "Assigned diagnosis"
names(ben_spindle_matches)[names(ben_spindle_matches) == "spindleb_regex"] <- "Regex"
names(ben_round_matches)[names(ben_round_matches) == "RoundcellB"] <- "Assigned diagnosis"
names(ben_round_matches)[names(ben_round_matches) == "roundb_regex"] <- "Regex"
names(ben_epithelial_matches)[names(ben_epithelial_matches) == "EpithelialB"] <- "Assigned diagnosis"
names(ben_epithelial_matches)[names(ben_epithelial_matches) == "epithelialb_regex"] <- "Regex"
names(malig_mixed_matches)[names(malig_mixed_matches) == "MixedM"] <- "Assigned diagnosis"
names(malig_mixed_matches)[names(malig_mixed_matches) == "mixedm_regex"] <- "Regex"
names(ben_mixed_matches)[names(ben_mixed_matches) == "MixedB"] <- "Assigned diagnosis"
names(ben_mixed_matches)[names(ben_mixed_matches) == "mixedb_regex"] <- "Regex"

matched_malig <- rbind(malig_epithelial_matches, malig_round_matches, malig_spindle_matches, malig_mixed_matches)
matched_ben <- rbind(ben_spindle_matches, ben_round_matches, ben_epithelial_matches, ben_mixed_matches)
matched_total <- rbind(matched_malig, matched_ben)

## Extract cases from the total set that are not captured by any of these categories, if possible, manually assign to a category
matched_total_short<- matched_total[-c(9,10,11)]
unmatched_records <- setdiff(totalset, matched_total_short)

## Create new dataframe containing all non-tumour diagnoses
non_tumours1<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Woven"))
non_tumours2<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Within normal"))
non_tumours3<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Vaginitis"))
non_tumours4<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Vacuolar"))
non_tumours5<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Uveitis"))
non_tumours6<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Stomatitis"))
non_tumours7<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Rhinitis"))
non_tumours8<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Otitis"))
non_tumours9<-unmatched_records %>% filter(str_detect(HistDiagnosis, "No significant lesions"))
non_tumours10<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Colitis"))
non_tumours11<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Cloudy swelling"))
non_tumours12<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Cellulitis"))
non_tumours13<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Gastritis"))
non_tumours14<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Hepatitis"))
non_tumours15<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Haemorrhage"))
non_tumours16<-unmatched_records %>% filter(str_detect(HistDiagnosis, "Dermatitis"))
non_tumours17<-unmatched_records %>% filter(str_detect(HistDiagnosis, "No histologic diagnosis"))
non_tumours18<-unmatched_records %>% filter(str_detect(HistDiagnosis, "No diagnostic lesions"))

non_tumour<- rbind(non_tumours1, non_tumours2, non_tumours3, non_tumours4, non_tumours5, non_tumours6, non_tumours7, non_tumours8, non_tumours9, non_tumours10, non_tumours11, non_tumours12, non_tumours13, non_tumours14, non_tumours15, non_tumours16, non_tumours17, non_tumours18)
rm(non_tumours1, non_tumours2, non_tumours3, non_tumours4, non_tumours5, non_tumours6, non_tumours7, non_tumours8, non_tumours9, non_tumours10, non_tumours11, non_tumours12, non_tumours13, non_tumours14, non_tumours15, non_tumours16, non_tumours17, non_tumours18)
unmatched_records<- setdiff(unmatched_records, non_tumour)



#####################Part 2b. Fuzzy matching to pick up mispellings etc #############

#First simplify strings in HistDiagnosis column
install.packages("stringr")
library("stringr")
library(stringdist)
library(fuzzyjoin)
library(dplyr)
library(readr)

## Remove regex columns
malig_epithelial_matches<- malig_epithelial_matches[-c(10)]
malig_mixed_matches<- malig_mixed_matches[-c(10)]
malig_round_matches<- malig_round_matches[-c(10)]
malig_spindle_matches<- malig_spindle_matches[-c(10)]
ben_epithelial_matches<- ben_epithelial_matches[-c(10)]
ben_mixed_matches<- ben_mixed_matches[-c(10)]
ben_round_matches<- ben_round_matches[-c(10)]
ben_spindle_matches<- ben_spindle_matches[-c(10)]


######################### MALIGNANT EPITHELIAL#########################################

## Load in match terms
fuzzyMEpithelial<- read_csv("fuzzyMEpithelial.csv")

## Initiate loop

beep(for(i in 1:length(fuzzyMEpithelial$HistDiagnosis)) {fuzzy_M_epithelial=stringdist_join(fuzzyMEpithelial,unmatched_records, by = "HistDiagnosis", mode = "left",
                             ignore_case = FALSE, method = "jw", p=.15, max_dist=4,
                             distance_col = "dist") %>%group_by(HistDiagnosis.x) %>%top_n(1,-dist)

fuzzy_M_epithelial$dist=1-fuzzy_M_epithelial$dist   

#Keep only those with a dist value of over 0.90 (below this, matches seem to be inaccurate)
fuzzyMEpi<-filter(fuzzy_M_epithelial, dist>=0.90)

#Add to malignant epithelial matches (malig_epithelial): format malig_epithelial and new fuzzy matches
fuzzyMEpi<- fuzzyMEpi[-c(11)]
names(fuzzyMEpi)[names(fuzzyMEpi) == "HistDiagnosis.y"] <- "HistDiagnosis"
names(fuzzyMEpi)[names(fuzzyMEpi) == "HistDiagnosis.x"] <- "Assigned diagnosis"
fuzzyMEpi<-fuzzyMEpi %>% relocate(Tclassification, .after=Age)
fuzzyMEpi<-fuzzyMEpi %>% relocate("Assigned diagnosis", .after=Age)
malig_epithelial_matches<-rbind(malig_epithelial_matches,fuzzyMEpi)

#Remove from unmatched records
Zmalig_epithelial_matches<- malig_epithelial_matches[-c(9,10)]
unmatched_records<- setdiff(unmatched_records, Zmalig_epithelial_matches)})


rm(fuzzyMEpi,fuzzy_M_epithelial,fuzzyMEpithelial, Zmalig_epithelial_matches)


########################### BENIGN EPITHELIAL######################################

fuzzyBEpithelial<- read_csv("fuzzyBEpithelial.csv")

## Initiate loop

beep(for(i in 1:length(fuzzyBEpithelial$HistDiagnosis)) {fuzzy_B_epithelial=stringdist_join(fuzzyBEpithelial,unmatched_records, by = "HistDiagnosis", mode = "left",
                                   ignore_case = FALSE, method = "jw", p=.15, max_dist=4,
                                   distance_col = "dist") %>%group_by(HistDiagnosis.x) %>%top_n(1,-dist)

fuzzy_B_epithelial$dist=1-fuzzy_B_epithelial$dist   

fuzzyBEpi<-filter(fuzzy_B_epithelial, dist>=0.90)
if (nrow(fuzzyBEpi) ==0) break # Stop the loop if there are no more matches

fuzzyBEpi<- fuzzyBEpi[-c(11)]
fuzzyBEpi<-fuzzyBEpi %>% relocate(HistDiagnosis.x, .after=Age)
names(fuzzyBEpi)[names(fuzzyBEpi) == "HistDiagnosis.y"] <- "HistDiagnosis"
names(fuzzyBEpi)[names(fuzzyBEpi) == "HistDiagnosis.x"] <- "Assigned diagnosis"
ben_epithelial_matches<-rbind(ben_epithelial_matches,fuzzyBEpi)

Zben_epithelial_matches<- ben_epithelial_matches[-c(9,10)]
unmatched_records<- setdiff(unmatched_records, Zben_epithelial_matches)})

### End loop

rm(fuzzyBEpi,fuzzy_B_epithelial,fuzzyBEpithelial, Zben_epithelial_matches)



############################## MALIGNANT SPINDLE ####################################

fuzzyMSpindle<- read_csv("fuzzyMSpindle.csv")

beep(for(i in 1:length(fuzzyMSpindle$HistDiagnosis)) {fuzzy_M_spindle=stringdist_join(fuzzyMSpindle,unmatched_records, by = "HistDiagnosis", mode = "left",
                                   ignore_case = FALSE, method = "jw", p=.15, max_dist=4,
                                   distance_col = "dist") %>%group_by(HistDiagnosis.x) %>%top_n(1,-dist)

fuzzy_M_spindle$dist=1-fuzzy_M_spindle$dist   

fuzzyMSpi<-filter(fuzzy_M_spindle, dist>=0.90)
if (nrow(fuzzyMSpi) ==0) break # Stop the loop if there are no more matches

fuzzyMSpi<- fuzzyMSpi[-c(11)]
fuzzyMSpi<-fuzzyMSpi %>% relocate(HistDiagnosis.x, .after=Age)
names(fuzzyMSpi)[names(fuzzyMSpi) == "HistDiagnosis.y"] <- "HistDiagnosis"
names(fuzzyMSpi)[names(fuzzyMSpi) == "HistDiagnosis.x"] <- "Assigned diagnosis"
malig_spindle_matches<-rbind(malig_spindle_matches,fuzzyMSpi)

Zmalig_spindle_matches<- malig_spindle_matches[-c(9,10)]
unmatched_records<- setdiff(unmatched_records, Zmalig_spindle_matches)})

### End loop

rm(fuzzyMSpi,fuzzy_M_spindle,fuzzyMSpindle, Zmalig_spindle_matches)


########################### BENIGN SPINDLE #########################################

fuzzyBSpindle<- read_csv("fuzzyBSpindle.csv")

beep(for (i in 1:length(fuzzyBSpindle$HistDiagnosis)) {fuzzy_B_spindle=stringdist_join(fuzzyBSpindle,unmatched_records, by = "HistDiagnosis", mode = "left",
                                   ignore_case = TRUE, method = "jw", p=.15, max_dist=4,
                                   distance_col = "dist") %>%group_by(HistDiagnosis.x) %>%top_n(1,-dist)

fuzzy_B_spindle$dist=1-fuzzy_B_spindle$dist   

fuzzyBSpi<-filter(fuzzy_B_spindle, dist>=0.90)
if (nrow(fuzzy_B_spindle) ==0) break  # Stop the loop if there are no more matches

fuzzyBSpi<- fuzzyBSpi[-c(11)]
fuzzyBSpi<-fuzzyBSpi %>% relocate(HistDiagnosis.x, .after=Age)
names(fuzzyBSpi)[names(fuzzyBSpi) == "HistDiagnosis.y"] <- "HistDiagnosis"
names(fuzzyBSpi)[names(fuzzyBSpi) == "HistDiagnosis.x"] <- "Assigned diagnosis"
ben_spindle_matches<-rbind(ben_spindle_matches,fuzzyBSpi)

Zben_spindle_matches<- ben_spindle_matches[-c(9,10)]
unmatched_records<- setdiff(unmatched_records, Zben_spindle_matches)})

### End loop

rm(fuzzyBSpi,fuzzy_B_spindle,fuzzyBSpindle, Zben_spindle_matches)


############################### MALIGNANT ROUND ####################################

fuzzyMRound<- read_csv("fuzzyMRound.csv")

beep(for (i in 1:length(fuzzyMRound$HistDiagnosis)) {fuzzy_M_round=stringdist_join(fuzzyMRound,unmatched_records, by = "HistDiagnosis", mode = "left",
                                ignore_case = FALSE, method = "jw", p=.15, max_dist=4,
                                distance_col = "dist") %>%group_by(HistDiagnosis.x) %>%top_n(1,-dist)

fuzzy_M_round$dist=1-fuzzy_M_round$dist   

fuzzyMRnd<-filter(fuzzy_M_round, dist>=0.90)
if (nrow(fuzzy_M_round) ==0)break  # Stop the loop if there are no more matches

fuzzyMRnd<- fuzzyMRnd[-c(11)]
fuzzyMRnd<-fuzzyMRnd %>% relocate(HistDiagnosis.x, .after=Age)
names(fuzzyMRnd)[names(fuzzyMRnd) == "HistDiagnosis.y"] <- "HistDiagnosis"
names(fuzzyMRnd)[names(fuzzyMRnd) == "HistDiagnosis.x"] <- "Assigned diagnosis"
malig_round_matches<-rbind(malig_round_matches,fuzzyMRnd)

Zmalig_round_matches<- malig_round_matches[-c(9,10)]
unmatched_records<- setdiff(unmatched_records, Zmalig_round_matches)})

### End loop

rm(fuzzyMRnd,fuzzy_M_round,fuzzyMRound, Zmalig_round_matches)


################################# BENIGN ROUND #######################################

fuzzyBRound<- read_csv("fuzzyBRound.csv")

beep(for (i in 1:length(fuzzyBRound$HistDiagnosis)) {fuzzy_B_round=stringdist_join(fuzzyBRound,unmatched_records, by = "HistDiagnosis", mode = "left",
                                ignore_case = FALSE, method = "jw", p=.15, max_dist=4,
                                distance_col = "dist") %>%group_by(HistDiagnosis.x) %>%top_n(1,-dist)

fuzzy_B_round$dist=1-fuzzy_B_round$dist   

fuzzyBRnd<-filter(fuzzy_B_round, dist>=0.90)
if (nrow(fuzzy_B_round) ==0) break  # Stop the loop if there are no more matches

fuzzyBRnd<- fuzzyBRnd[-c(11)]
fuzzyBRnd<-fuzzyBRnd %>% relocate(HistDiagnosis.x, .after=Age)
names(fuzzyBRnd)[names(fuzzyBRnd) == "HistDiagnosis.y"] <- "HistDiagnosis"
names(fuzzyBRnd)[names(fuzzyBRnd) == "HistDiagnosis.x"] <- "Assigned diagnosis"
ben_round_matches<-rbind(ben_round_matches,fuzzyBRnd)

Zben_round_matches<- ben_round_matches[-c(9,10)]
unmatched_records<- setdiff(unmatched_records, Zben_round_matches)})

### End loop

rm(fuzzyBRnd,fuzzy_B_round,fuzzyBRound, Zben_round_matches)


################################ BENIGN MIXED #########################################
fuzzyMixedB<- read_csv("fuzzyMixedB.csv")
names(fuzzyMixedB)[names(fuzzyMixedB) == "MixedB"] <- "HistDiagnosis"

beep(for (i in 1:length(fuzzyMixedB$HistDiagnosis)) {fuzzy_B_mixed=stringdist_join(fuzzyMixedB,unmatched_records, by = "HistDiagnosis", mode = "left",
                              ignore_case = FALSE, method = "jw", p=.15, max_dist=4,
                              distance_col = "dist") %>%group_by(HistDiagnosis.x) %>%top_n(1,-dist)

fuzzy_B_mixed$dist=1-fuzzy_B_mixed$dist   

fuzzyBMixed<-filter(fuzzy_B_mixed, dist>=0.90)
if (nrow(fuzzy_B_mixed) ==0) break  # Stop the loop if there are no more matches


fuzzyBMixed<- fuzzyBMixed[-c(11)]
fuzzyBMixed<-fuzzyBMixed %>% relocate(HistDiagnosis.x, .after=Age)
names(fuzzyBMixed)[names(fuzzyBMixed) == "HistDiagnosis.y"] <- "HistDiagnosis"
names(fuzzyBMixed)[names(fuzzyBMixed) == "HistDiagnosis.x"] <- "Assigned diagnosis"
ben_mixed_matches<-rbind(ben_mixed_matches,fuzzyBMixed)

Zben_mixed_matches<- ben_mixed_matches[-c(9,10)]
unmatched_records<- setdiff(unmatched_records, Zben_mixed_matches)})

### End loop

rm(fuzzyMixedB,fuzzy_B_mixed,fuzzyBMixed, Zben_mixed_matches)


############################## MALIGNANT MIXED #######################################

fuzzyMixedM<- read_csv("fuzzyMixedM.csv")

beep(for (i in 1:length(fuzzyMixedM$HistDiagnosis)) {fuzzy_M_mixed=stringdist_join(fuzzyMixedM,unmatched_records, by = "HistDiagnosis", mode = "left",
                              ignore_case = FALSE, method = "jw", p=.15, max_dist=4,
                              distance_col = "dist") %>%group_by(HistDiagnosis.x) %>%top_n(1,-dist)

fuzzy_M_mixed$dist=1-fuzzy_M_mixed$dist   

fuzzyMMixed<-filter(fuzzy_M_mixed, dist>=0.90)
if (nrow(fuzzy_M_mixed) ==0) break  # Stop the loop if there are no more matches

fuzzyMMixed<- fuzzyMMixed[-c(11)]
fuzzyMMixed<-fuzzyMMixed %>% relocate(HistDiagnosis.x, .after=Age)
names(fuzzyMMixed)[names(fuzzyMMixed) == "HistDiagnosis.y"] <- "HistDiagnosis"
names(fuzzyMMixed)[names(fuzzyMMixed) == "HistDiagnosis.x"] <- "Assigned diagnosis"
malig_mixed_matches<-rbind(malig_mixed_matches,fuzzyMMixed)

Zmalig_mixed_matches<- malig_mixed_matches[-c(9,10)]
unmatched_records<- setdiff(unmatched_records, Zmalig_mixed_matches)})

### End loop

rm(fuzzyMixedM,fuzzy_M_mixed,fuzzyMMixed, Zmalig_mixed_matches)


### Identify tumour types that represent <1% of all diagnoses
fewesttumours<- as.data.frame(table(matched_total$"Assigned diagnosis"))
fewesttumours$CPercent <- (fewesttumours$Freq/sum(fewesttumours$Freq))*100
colnames(fewesttumours) <- c("TumourType", "Number", "Percentage")
fewesttumours
tumours_under_0.01 <- filter(fewesttumours, fewesttumours$Percentage <0.01)
tumours_0.01 <- as.character(tumours_under_0.01$TumourType)

# Change assigned diagnosis to Mixed/Other if under 1%
matched_total$Tclassification[matched_total$Tclassification %in% tumours_0.01] <- "Mixed/Other"


######################################################################################
### Part 2c. Add and clean malignant mammary tumours (from Matt's mammary dataset)
mammarytumours<- read_csv("Malignantmammarys.csv")
view(mammarytumours)
mammarytumours$AgeYears <- as.numeric(mammarytumours$AgeYears)
mammarytumours$AgeMonths <- as.numeric(mammarytumours$AgeMonths)
mammarytumours$CaseNumber <- as.numeric(mammarytumours$CaseNumber)
mammarytumours$`Assigned diagnosis` <- as.character(mammarytumours$`Assigned diagnosis`)

mammarytumours$Age <- mammarytumours$AgeYears
mammarytumours$Age[mammarytumours$AgeYears <3] <- "<3"
mammarytumours$Age[mammarytumours$AgeYears >=3 & mammarytumours$AgeYears < 6] <- "3 to <6"
mammarytumours$Age[mammarytumours$AgeYears >=6 & mammarytumours$AgeYears < 9] <- "6 to <9"
mammarytumours$Age[mammarytumours$AgeYears >=9 & mammarytumours$AgeYears < 12] <- "9 to <12"
mammarytumours$Age[mammarytumours$AgeYears >=12] <- ">12"

##Assign NA to missing age
mammarytumours$AgeYears[mammarytumours$AgeYears==0] <- "NA" 
mammarytumours$AgeMonths[mammarytumours$AgeMonths==0] <- "NA"
mammarytumours$Age[mammarytumours$AgeYears=='NA' & mammarytumours$AgeMonths=='NA'] <- "NA" 
mammarytumours$Age[!(mammarytumours$Age %in% c('<3', '3 to <6', '6 to <9', '9 to <12', '>12'))] <- "NA"
table(mammarytumours$Age)
mammarytumours<- mammarytumours %>% mutate(SexNeuterStatus=recode(SexNeuterStatus, 'Female'='Female Unknown'))
table(mammarytumours$SexNeuterStatus)
mammarytumours<-mammarytumours %>% relocate(Age, .after=HistDiagnosis)
mammarytumours<-mammarytumours %>% relocate("Assigned diagnosis", .after=Age)

# Add all mixed malignant mammary tumours to Tclassification "Mixed/Other (Malignant)
mammarytumours$Tclassification[mammarytumours$Tclassification == "Mixed"] <- "Mixed/Other (Malignant)"

## Assign breed status
breed2 <- as.data.frame(table(mammarytumours$BreedStatus))
breed2 <- breed2$Var1
mammarytumours$BreedStatus[(!(mammarytumours$BreedStatus %in% breed2))] <- "Purebreed"
table(mammarytumours$BreedStatus)

## Add to matched total 
matched_total<- matched_total[-c(10)]
matched_total<- rbind(mammarytumours, matched_total)


######## Part 2d. Assign final unspecified (sarcoma,carcinoma, round cell tumours)- edit!

## Couldn't do this until the end as otherwise you get duplicates where one case 
#e.g., anal sac gland carcinoma has been classified as both 'ASGC' and 'Carcinoma (Unspecified)'

## Pull out the unmatched carcinomas
unmatched_records2<-unmatched_records %>% filter(!grepl('carcinoma', HistDiagnosis))
unmatched_carcinomas<- setdiff(unmatched_records, unmatched_records2)
rm(unmatched_records)
table(matched_total$Tclassification)
table(matched_total$`Assigned diagnosis`)
unmatched_carcinomas$'Assigned diagnosis'  <- rep("Carcinoma (Unspecified)", nrow(unmatched_carcinomas))
unmatched_carcinomas$'Tclassification'  <- rep("Malignant epithelial", nrow(unmatched_carcinomas))
unmatched_carcinomas

# Add to matched total
matched_total<- rbind(matched_total, unmatched_carcinomas)
rm(unmatched_carcinomas)

## Pull out unmatched adenomas
unmatched_records3<-unmatched_records2 %>% filter(!grepl('adenoma', HistDiagnosis))
unmatched_adenomas<- setdiff(unmatched_records2, unmatched_records3)
rm(unmatched_records2)
unmatched_adenomas$'Assigned diagnosis'  <- rep("Adenoma (Unspecified)", nrow(unmatched_adenomas))
unmatched_adenomas$'Tclassification'  <- rep("Benign epithelial", nrow(unmatched_adenomas))
unmatched_adenomas
matched_total<- rbind(matched_total, unmatched_adenomas)
rm(unmatched_adenomas)

## carcinoma with capital letter
unmatched_records4<-unmatched_records3 %>% filter(!grepl('Carcinoma', HistDiagnosis))
unmatched_carcinomas<- setdiff(unmatched_records3, unmatched_records4)
rm(unmatched_records3)
unmatched_carcinomas$'Assigned diagnosis'  <- rep("Carcinoma (Unspecified)", nrow(unmatched_carcinomas))
unmatched_carcinomas$'Tclassification'  <- rep("Malignant epithelial", nrow(unmatched_carcinomas))
unmatched_carcinomas
matched_total<- rbind(matched_total, unmatched_carcinomas)
rm(unmatched_carcinomas)

## Adenoma with capital
unmatched_records5<-unmatched_records4 %>% filter(!grepl('Adenoma', HistDiagnosis))
unmatched_adenomas<- setdiff(unmatched_records4, unmatched_records5)
rm(unmatched_records4)
unmatched_adenomas$'Assigned diagnosis'  <- rep("Adenoma (Unspecified)", nrow(unmatched_adenomas))
unmatched_adenomas$'Tclassification'  <- rep("Benign epithelial", nrow(unmatched_adenomas))
unmatched_adenomas
matched_total<- rbind(matched_total, unmatched_adenomas)
rm(unmatched_adenomas)

## Sarcomas
unmatched_records6<-unmatched_records5 %>% filter(!grepl('Sarcoma', HistDiagnosis))
unmatched_sarcomas<- setdiff(unmatched_records5, unmatched_records6)
rm(unmatched_records5)
unmatched_sarcomas$'Assigned diagnosis'  <- rep("Sarcoma (Unspecified)", nrow(unmatched_sarcomas))
unmatched_sarcomas$'Tclassification'  <- rep("Malignant spindle", nrow(unmatched_sarcomas))
unmatched_sarcomas
matched_total<- rbind(matched_total, unmatched_sarcomas)
rm(unmatched_sarcomas)

## Round cells
unmatched_records7<-unmatched_records6 %>% filter(!grepl('Round cell', HistDiagnosis))
unmatched_round<- setdiff(unmatched_records6, unmatched_records7)
rm(unmatched_records6)
unmatched_round$'Assigned diagnosis'  <- rep("Round cell (Unspecified)", nrow(unmatched_round))
unmatched_round$'Tclassification'  <- rep("Malignant round", nrow(unmatched_round))
unmatched_round
matched_total<- rbind(matched_total, unmatched_round)
rm(unmatched_round)

unmatched_records8<-unmatched_records7 %>% filter(!grepl('round cell', HistDiagnosis))
unmatched_round<- setdiff(unmatched_records7, unmatched_records8)
rm(unmatched_records7)
unmatched_round$'Assigned diagnosis'  <- rep("Round cell (Unspecified)", nrow(unmatched_round))
unmatched_round$'Tclassification'  <- rep("Malignant round", nrow(unmatched_round))
unmatched_round
matched_total<- rbind(matched_total, unmatched_round)
rm(unmatched_round)

## unmatched records now consist of cases with a non-tumour diagnosis (or tumours 
## with no histological information i.e., 'tumour'/'poorly differentiated tumour')


######################################################################################
########################### Part 3: Descriptive statistics

# Write out final set
install.packages("data.table")
library(data.table)
library(readr)
library(writexl)
write_xlsx(matched_total, "totalmatched.xlsx")
setwd("C:/Users/phoeb/OneDrive - University of Bristol/MSc Epidemiology/Unit 8 Dissertation/Data/realdata")
alltumours<- read_csv("totalmatched.csv")

## Trim columns
colnames(alltumours)
all_trim <- alltumours[,c(1,2,3,4,5,6,8,9,10)]
names(all_trim)[names(all_trim) == "Assigned diagnosis"] <- "HistologicalDiagnosis"
names(all_trim)[names(all_trim) == "Tclassification"] <- "TumourClassification"
names(all_trim)
table(all_trim$TumourClassification)
str(all_trim)

## Check NAs
table(all_trim$CaseNumber == "")
table(all_trim$CaseNumber == " ")
table(all_trim$SexNeuterStatus)
list_sex <- c("Female entire","Female neutered","Male entire","Male neutered","Male Unknown","Female Unknown")
all_trim$SexNeuterStatus[(!(all_trim$SexNeuterStatus %in% list_sex))] <- "NA"
table(all_trim$SexNeuterStatus)
all_trim$SexNeuterStatus[((all_trim$SexNeuterStatus == "NA"))] <- "Unrecorded"
table(all_trim$SexNeuterStatus)
breed <- as.data.frame(table(all_trim$Breed))
breed_list <- breed$Var1
all_trim$Breed[(!(all_trim$Breed %in% breed_list))] <- "Breed not recorded"
table(all_trim$Breed)
Bstatus_list <- c("Purebreed", "Crossbreed")
all_trim$BreedStatus[(!(all_trim$BreedStatus %in% Bstatus_list))] <- "NA"
table(all_trim$BreedStatus)
all_trim$BreedStatus[((all_trim$BreedStatus == "NA"))] <- "Unrecorded"
table(all_trim$BreedStatus)
tumours <- as.data.frame(table(all_trim$TumourClassification))
tumours <- tumours$Var1
table(all_trim$TumourClassification)
all_trim$TumourClassification[(!(all_trim$TumourClassification %in% tumours))] <- "NA"
all_trim$TumourClassification[((all_trim$TumourClassification == "NA"))] <- "Unrecorded"
table(all_trim$TumourClassification)
all_trim$TumourClassification[all_trim$TumourClassification == "Mixed/other (Malignant)"] <- "Mixed/Other (Malignant)"
table(all_trim$TumourClassification)


## Split the data into tumour type
all_trim_split <- split(all_trim, all_trim$TumourClassification)

## Get overall stats for: overall age categories, overall breeds, overall sex/neuter status
overallage<-as.data.frame(table(alltumours$Age))
overallage$CPercent <- (overallage$Freq/sum(overallage$Freq))*100
write.table(overallage, file = "Overall_Age_Stats.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
Overallage2_stats <- as.data.frame(summary(all_trim))

overallSNStatus<-as.data.frame(table(alltumours$SexNeuterStatus))
overallSNStatus$CPercent <- (overallSNStatus$Freq/sum(overallSNStatus$Freq))*100
write.table(overallSNStatus, file = "Overall_SNS_Stats.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

overallbreed<-as.data.frame(table(alltumours$Breed))
overallbreed$CPercent <- (overallbreed$Freq/sum(overallbreed$Freq))*100
colnames(overallbreed) <- c("breed", "CNumber", "CPercentage")
overallbreed$CPercentage <- as.numeric(overallbreed$CPercentage)
overallbreed$CPercentage <- format(round(overallbreed$CPercentage, 1), nsmall = 1)
write.table(overallbreed, file = "Overall_Breed_Stats.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

## Get numeric stats for raw age per tumour category
library(writexl)

Mepithelial_stats <- as.data.frame(summary(all_trim_split[[4]]))
write_xlsx(Mepithelial_stats, "MEpiAge.xlsx")

Mround_stats <- as.data.frame(summary(all_trim_split[[5]]))
write_xlsx(Mround_stats, "MRndAge.xlsx")

Mspindle_stats <- as.data.frame(summary(all_trim_split[[6]]))
write_xlsx(Mspindle_stats, "MSpnAge.xlsx")

MMixed_stats <- as.data.frame(summary(all_trim_split[[8]]))
write_xlsx(MMixed_stats, "MMixAge.xlsx")


## Stats for age categories

ME_age <- as.data.frame(table(all_trim_split[[4]]$Age))
ME_age$CPercent <- (ME_age$Freq/sum(ME_age$Freq))*100
colnames(ME_age) <- c("Age category", "Number", "Percentage")
ME_age$CPercentage <- as.numeric(ME_age$CPercentage)
ME_age$CPercentage <- format(round(ME_age$CPercentage, 1), nsmall = 1)
ME_age
write_xlsx(ME_age, "ME_age.xlsx")

MR_age <- as.data.frame(table(all_trim_split[[5]]$Age))
MR_age$CPercent <- (MR_age$Freq/sum(MR_age$Freq))*100
colnames(MR_age) <- c("Age category", "Number", "Percentage")
MR_age$CPercentage <- as.numeric(MR_age$CPercentage)
MR_age$CPercentage <- format(round(MR_age$CPercentage, 1), nsmall = 1)
MR_age
write_xlsx(MR_age, "MR_age.xlsx")

MS_age <- as.data.frame(table(all_trim_split[[6]]$Age))
MS_age$CPercent <- (MS_age$Freq/sum(MS_age$Freq))*100
colnames(MS_age) <- c("Age category", "Number", "Percentage")
MS_age$CPercentage <- as.numeric(MS_age$CPercentage)
MS_age$CPercentage <- format(round(MS_age$CPercentage, 1), nsmall = 1)
MS_age
write_xlsx(MS_age, "MS_age.xlsx")

MM_age <- as.data.frame(table(all_trim_split[[8]]$Age))
MM_age$CPercent <- (MM_age$Freq/sum(MM_age$Freq))*100
colnames(MM_aged) <- c("Age category", "Number", "Percentage")
MM_age$CPercentage <- as.numeric(MM_age$CPercentage)
MM_age$CPercentage <- format(round(MM_age$CPercentage, 1), nsmall = 1)
MM_age
write_xlsx(MM_age, "MM_age.xlsx")

##Breed

##breed desc stats
breeds <- as.data.frame(table(all_trim$"Breed"))
breeds$CPercent <- (breeds$Freq/sum(breeds$Freq))*100
colnames(breeds) <- c("Breed", "Number", "Percentage")
breeds
write_xlsx(breeds, "breedtable.xlsx")
breedtable<- read_csv("breedtable.csv")
library(sjPlot)
tab_df(breedtable,
       title = "Descriptive statistics: Breed, age, and sex/neuter status", 
       file = "DSTABLE.doc")
breedtable$Percentage <- round(breedtable$Percentage, 2)
str(breedtable$Percentage)
breedtable$Percentage<- as.numeric(breedtable$Percentage)


## Stats for sex/neuter status

ME_SNS <- as.data.frame(table(all_trim_split[[4]]$SexNeuterStatus))
ME_SNS$CPercent <- (ME_SNS$Freq/sum(ME_SNS$Freq))*100
colnames(ME_SNS) <- c("SNS", "Number", "Percentage")
ME_SNS$CPercentage <- as.numeric(ME_SNS$CPercentage)
ME_SNS$CPercentage <- format(round(ME_SNS$CPercentage, 1), nsmall = 1)
ME_SNS
write_xlsx(ME_SNS, "ME_SNS.xlsx")

MR_SNS <- as.data.frame(table(all_trim_split[[5]]$SexNeuterStatus))
MR_SNS$CPercent <- (MR_SNS$Freq/sum(MR_SNS$Freq))*100
colnames(MR_SNS) <- c("SNS", "Number", "Percentage")
MR_SNS$CPercentage <- as.numeric(MR_SNS$CPercentage)
MR_SNS$CPercentage <- format(round(MR_SNS$CPercentage, 1), nsmall = 1)
MR_SNS
write_xlsx(MR_SNS, "MR_SNS.xlsx")

MS_SNS <- as.data.frame(table(all_trim_split[[6]]$SexNeuterStatus))
MS_SNS$CPercent <- (MS_SNS$Freq/sum(MS_SNS$Freq))*100
colnames(MS_SNS) <- c("SNS", "Number", "Percentage")
MS_SNS$CPercentage <- as.numeric(MS_SNS$CPercentage)
MS_SNS$CPercentage <- format(round(MS_SNS$CPercentage, 1), nsmall = 1)
MS_SNS
write_xlsx(MS_SNS, "MS_SNS.xlsx")

MM_SNS <- as.data.frame(table(all_trim_split[[8]]$SexNeuterStatus))
MM_SNS$CPercent <- (MM_SNS$Freq/sum(MM_SNS$Freq))*100
colnames(MM_SNS) <- c("SNS", "Number", "Percentage")
MM_SNS$CPercentage <- as.numeric(MM_SNS$CPercentage)
MM_SNS$CPercentage <- format(round(MM_SNS$CPercentage, 1), nsmall = 1)
MM_SNS
write_xlsx(MM_SNS, "MM_SNS.xlsx")


## Stats for histological diagnosis

ME_HD <- as.data.frame(table(all_trim_split[[4]]$HistologicalDiagnosis))
ME_HD$CPercent <- (ME_HD$Freq/sum(ME_HD$Freq))*100
colnames(ME_HD) <- c("HD", "Number", "Percentage")
ME_HD$CPercentage <- as.numeric(ME_HD$CPercentage)
ME_HD$CPercentage <- format(round(ME_HD$CPercentage, 1), nsmall = 1)
ME_HD
write_xlsx(ME_HD, "ME_HD.xlsx")

MR_HD <- as.data.frame(table(all_trim_split[[5]]$HistologicalDiagnosis))
MR_HD$CPercent <- (MR_HD$Freq/sum(MR_HD$Freq))*100
colnames(MR_HD) <- c("HD", "Number", "Percentage")
MR_HD$CPercentage <- as.numeric(MR_HD$CPercentage)
MR_HD$CPercentage <- format(round(MR_HD$CPercentage, 1), nsmall = 1)
MR_HD
write_xlsx(MR_HD, "MR_HD.xlsx")

MS_HD <- as.data.frame(table(all_trim_split[[6]]$HistologicalDiagnosis))
MS_HD$CPercent <- (MS_HD$Freq/sum(MS_HD$Freq))*100
colnames(MS_HD) <- c("HD", "Number", "Percentage")
MS_HD$CPercentage <- as.numeric(MS_HD$CPercentage)
MS_HD$CPercentage <- format(round(MS_HD$CPercentage, 1), nsmall = 1)
MS_HD
write_xlsx(MS_HD, "MS_HD.xlsx")

MM_HD <- as.data.frame(table(all_trim_split[[8]]$HistologicalDiagnosis))
MM_HD$CPercent <- (MM_HD$Freq/sum(MM_HD$Freq))*100
colnames(MM_HD) <- c("HD", "Number", "Percentage")
MM_HD$CPercentage <- as.numeric(MM_HD$CPercentage)
MM_HD$CPercentage <- format(round(MM_HD$CPercentage, 1), nsmall = 1)
MM_HD
write_xlsx(MM_HD, "MM_HD.xlsx")


##goldens tumour stats
GRS<-filter(all_trim2, Breed == "Golden Retriever")
GRdiagnoses <- GRS %>%
  count(HistologicalDiagnosis) %>% 
  mutate(percent = n/sum(n) * 100)

##cocker tumour stats
cockers<-filter(all_trim2, Breed == "Cocker Spaniel (Unspecified/Other)")
cockerdiagnoses <- cockers %>%
  count(HistologicalDiagnosis) %>% 
  mutate(percent = n/sum(n) * 100)

## golden age stats
golden_age_stats <- GRS %>%
  summarize(
    median_age = median(AgeYears, na.rm = TRUE),
    iqr_age = IQR(AgeYears, na.rm = TRUE))
golden_age_stats2 <- GRS %>%
  summarize(
    median_age = median(AgeMonths, na.rm = TRUE),
    iqr_age = IQR(AgeMonths, na.rm = TRUE))

## cocker age stats
cocker_age_stats <- cockers %>%
  summarize(
    median_age = median(AgeYears, na.rm = TRUE),
    iqr_age = IQR(AgeYears, na.rm = TRUE))
cocker_age_stats2 <- cockers %>%
  summarize(
    median_age = median(AgeMonths, na.rm = TRUE),
    iqr_age = IQR(AgeMonths, na.rm = TRUE))

## cocker sns stats
cockersns <- cockers %>%
  count(SexNeuterStatus) %>% 
  mutate(percent = n/sum(n) * 100)
##golden sns stats
goldensns <- GRS %>%
  count(SexNeuterStatus) %>% 
  mutate(percent = n/sum(n) * 100)

## table for sns cockers
tab_df(cockersns,
       title = "Cocker Spaniel Sex Neuter Status Statistics", 
       file = "SNSCockersStats.doc")
## table for sns goldens
tab_df(goldensns,
       title = "Golden Retriever Sex Neuter Status Statistics", 
       file = "SNSGRSStats.doc")


## Descriptive stats tables for hist diagnosis, all breeds
install.packages("sjPlot")
library(sjPlot)
tab_df(malignant_HD_stats,
       title = "Descriptive statistics: Malignant Histotypes", 
       file = "malignantDS.doc")


desc <- c(AGE_results, SNS_results, BREED)
lapply(desc, as.data.frame )
desc <- list(purebred, breed, KC, mass, age, sex, dachs, spaniel, chond, skull)
desc_stats <- rbindlist(desc, use.names = F)
desc_stats <- desc_stats[, c(1,6,7)]


######################################################################################
#################### Univariable Logistic Regression
setwd("C:/Users/phoeb/OneDrive - University of Bristol/MSc Epidemiology/Unit 8 Dissertation/Data/realdata")
install.packages("tidymodels")
install.packages("nnet")
library(writexl)
library(readr)
require(nnet)
library(tidyverse)
library(tidymodels)
library(dplyr)
all_trim<- read_csv("all_trim_cleaned.csv")
all_trim_final<- read_csv("all_trim_final.csv")
all_trim<- read_csv("all_trim.csv")
colnames(all_trim)
all_trim<-all_trim[-c(1)]

### breed model
#make factor variables
allmalignants$Breed <- factor(allmalignants$Breed)
allmalignants$SexNeuter <- factor(allmalignants$SexNeuter)
allmalignants$TumourClassification <- factor(allmalignants$TumourClassification)
allmalignants$Age <- factor(allmalignants$Age)
colnames(allmalignants)
is.factor(allmalignants$Breed)
allmalignants<- allmalignants[-c(2)]

#set base as crossbreed
table(allmalignants$Breed)
is.factor(allmalignants$Breed)
allmalignants$Breed <- relevel(allmalignants$Breed, "Crossbreed")

#set base as male entire
table(allmalignants$SexNeuter)
is.factor(allmalignants$SexNeuter)
allmalignants$SexNeuter <- relevel(allmalignants$SexNeuter, "Male entire")

#set base as <3
table(allmalignants$Age)
is.factor(allmalignants$Age)
allmalignants$Age <- relevel(allmalignants$Age, "<3")

#set base as malignant round
table(allmalignants$TumourClassification)
is.factor(allmalignants$TumourClassification)
allmalignants$TumourClassification <- relevel(allmalignants$TumourClassification, "Malignant round")

multinom_model2 <- multinom(TumourClassification ~ Breed, data = allmalignants)
multinom_model<-as.data.frame(tidy(multinom_model2, exponentiate = TRUE, conf.int = TRUE) |> mutate_if(is.numeric, round, 4))|> select(-std.error, -statistic)

null_breed_model <- multinom(TumourClassificationF ~ 1, data=allmalignants)
breed_anova <- anova(null_breed_model, multinom_model2)
breed_pvalue <- breed_anova$"Pr(Chi)"[2]

# format for publication
names(multinom_model)[names(multinom_model) == "y.level"] <- "Tumour Type"
names(multinom_model)[names(multinom_model) == "term"] <- "Breed"
multinom_model$Breed[((multinom_model$Breed == "(Intercept)"))] <- "Crossbreed (base)"
write.table(multinom_model, "BREEDMODEL.txt", row.names = F, col.names = T, quote = F, sep = '\t')
write_xlsx(multinom_model, "BREEDMODEL.xlsx")
tab_df(multinom_model,
       title = "Univariate model: Odds of diagnosis per tumour type", 
       file = "UNIV_Breed.doc")


#Sex Neuter Status model
SNS_model2 <- multinom(TumourClassificationF ~ SexNeuterF, data = all_trim, family = multinomial)
SNS_results<-as.data.frame(tidy(SNS_model2, exponentiate = TRUE, conf.int = TRUE) |> mutate_if(is.numeric, round, 4))|> select(-std.error, -statistic)

## Assess goodness of fit:
null_SNS_model <- multinom(TumourClassificationF ~ 1, data=all_trim)
SNS_anova <- anova(null_SNS_model, SNS_model2)
SNS_pvalue <- SNS_anova$"Pr(Chi)"[2]

# format for publication
names(SNS_results)[names(SNS_results) == "y.level"] <- "Tumour Type"
names(SNS_results)[names(SNS_results) == "term"] <- "SexNeuterStatus"
SNS_results$SexNeuterStatus[((SNS_results$SexNeuterStatus == "(Intercept)"))] <- "Male entire (base)"
write.table(SNS_results, "SEXNEUTERSTATUSMODEL.txt", row.names = F, col.names = T, quote = F, sep = '\t')



## Age model
AGE_model2 <- multinom(TumourClassificationF ~ AgeF, data = all_trim, family = multinomial)
AGE_results<-as.data.frame(tidy(AGE_model2, exponentiate = TRUE, conf.int = TRUE) |> mutate_if(is.numeric, round, 4))|> select(-std.error, -statistic)

null_age_model <- multinom(TumourClassificationF ~ 1, data=all_trim)
age_anova <- anova(null_age_model, AGE_model2)
age_pvalue <- age_anova$"Pr(Chi)"[2]

names(AGE_results)[names(AGE_results) == "y.level"] <- "Tumour Type"
names(AGE_results)[names(AGE_results) == "term"] <- "AgeCategory"
AGE_results$AgeCategory[((AGE_results$AgeCategory == "(Intercept)"))] <- "<3 (base)"
write.table(AGE_results, "AGEMODEL.txt", row.names = F, col.names = T, quote = F, sep = '\t')



########################################################################################################
###################################### MULTIVARIABLE LOGISTIC REGRESSION (just malignant tumours)
install.packages("nnet")
install.packages("tidyverse")
install.packages("writexl")
install.packages("broom")
library(tidyverse)
library(nnet)
library(broom)
library(dplyr)
library(writexl)

# check class
class(all_trim$Breed)


# all breeds
allmalignants<- read.csv("allmalignants.csv")
allmalignants$BreedF <- factor(allmalignants$Breed)
allmalignants$SexNeuterF <- factor(allmalignants$SexNeuterStatus)
allmalignants$TumourClassificationF <- factor(allmalignants$TumourClassification)
allmalignants$AgeF <- factor(allmalignants$Age)
colnames(allmalignants)
is.factor(allmalignants$BreedF)
allmalig_f<- allmalignants[-c(1,2,3,4)]

#set base as crossbreed
table(allmalig_f$BreedF)
is.factor(allmalig_f$BreedF)
allmalig_f$BreedF <- relevel(allmalig_f$BreedF, "Crossbreed")

#set base as male entire
table(allmalig_f$SexNeuterF)
is.factor(allmalig_f$SexNeuterF)
allmalig_f$SexNeuterF <- relevel(allmalig_f$SexNeuterF, "Male entire")

#set base as <3
table(allmalig_f$AgeF)
is.factor(allmalig_f$AgeF)
allmalig_f$AgeF <- relevel(allmalig_f$AgeF, "<3")

#set base as malignant round
table(allmalig_f$TumourClassificationF)
is.factor(allmalig_f$TumourClassificationF)
allmalig_f$TumourClassificationF <- relevel(allmalig_f$TumourClassificationF, "Malignant round")

allbreed_multi <- multinom(cbind(TumourClassificationF) ~ BreedF + AgeF + SexNeuterF, data = allmalig_f)
allbreedmulti_model<-as.data.frame(tidy(allbreed_multi, exponentiate = TRUE, conf.int = TRUE) |> mutate_if(is.numeric, round, 4))|> select(-std.error, -statistic)
names(allbreedmulti_model)[names(allbreedmulti_model) == "y.level"] <- TumourType
write_xlsx(allbreedmulti_model, "NEWALLBREED_MULTI_RESULTS.xlsx")

## get area under curve
install.packages("pROC")
library(pROC)
probs <- predict(allbreed_multi, type = "probs")
y_true <- allmalig_f$TumourClassificationF
classes <- levels(y_true)
colnames(probs) <- classes
auc <- multiclass.roc(y_true, probs, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)$auc
print(auc)

## LR test
install.packages("lmtest")
library(lmtest)
model_full <- lm(TumourClassification ~ BreedF + AgeF + SexNeuterF, data = allmalig_f) %>%
  tbl_regression() %>%
  add_global_p()
model_full<- as.data.frame(model_full)

model_reduced <- lm(TumourClassification ~ BreedF + AgeF, data = allmalig_f)
lrtest(model_full, model_reduced)

## results table for all breeds
install.packages("sjPlot")
library(sjPlot)
tab_df(allbreedmulti_model,
       title = "Odds of diagnosis per tumour type amongst all breeds", 
       file = "ALLBREEDRESULTSTABLE.doc")

install.packages(ggplot2)
library(ggplot2)

## read in golden data
goldendata <- read.csv("goldendata.csv")

## Get box plot
ggplot(goldendata, aes(x = Tumour.type..cell.of.origin., y = Odds.Ratio)) +
  geom_boxplot() +
  labs(
    title = "Odds Ratios by tumour type (cell of origin) in Golden Retrievers", size=0.5,
    x = "Tumour type (cell of origin)",
    y = "Odds Ratio")+
  geom_errorbar(aes(ymin = Lower.CI, ymax = Upper.CI))


## read in cocker data
cockerdata <- read.csv("cockerdata.csv")

# Create a box plot
ggplot(cockerdata, aes(x = Tumour.type..cell.of.origin., y = Odds.Ratio)) +
  geom_boxplot() +
  labs(
    title = "Odds Ratios by tumour type (cell of origin) in Cocker Spaniels", size=0.5,
    x = "Tumour type (cell of origin)",
    y = "Odds Ratio")+
  geom_errorbar(aes(ymin = Lower.CI, ymax = Upper.CI))


