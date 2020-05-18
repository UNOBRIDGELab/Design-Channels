# ZEPHYR PR Analysis

# Run below Query for Zephyr's pull request data for repo_id:25158
#select pull_request_files.pull_request_id, sum(pr_file_additions) as loc_additions, sum(pr_file_deletions) as loc_deletions,
#count(pr_file_path) as num_files_changed, pr_src_state, pr_src_title, CASE WHEN pr_merged_at IS NULL THEN 'Rejected' ELSE 'Merged' END AS merged_status 
#from augur_data.pull_requests inner join augur_data.pull_request_files on pull_requests.pull_request_id = pull_request_files.pull_request_id 
#where repo_id = 25158 and pr_src_state != 'open'
#group by pull_request_files.pull_request_id, pr_merged_at, pr_src_state, pr_src_title
#order by loc_additions desc


################################# DATA IMPORT AND CLEANUP ##########################################################


# Import Zephyr's pull request data
library(readxl)
Raw_Data <- read.csv("~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/data.csv")
View(Raw_Data) 

# Structure of the data
str(Raw_Data)

# Converting dependent variable (y) to factor for logistic regression
Raw_Data$merged_status <- as.factor(Raw_Data$merged_status)

# Removing unnecessary fields for regression model
Raw_Data$pr_src_title <- NULL
Raw_Data$pr_src_state <- NULL

# Checking nas
colSums(is.na(Raw_Data))
# Removing nas
Raw_Data <- na.omit(Raw_Data)

# All cleaned data
Zephyr_all <- Raw_Data


############################################# BOXPLOTS #######################################################################


# Checking Outliers  
boxplot(Zephyr_all$num_files_changed~Zephyr_all$merged_status,data=Zephyr_all,main="Number of files changed - Outliers", xlab="Merged Status", ylab="Number of files changed")
boxplot(Zephyr_all$loc_deletions~Zephyr_all$merged_status,data=Zephyr_all, main="Lined of Code Deleted - Outliers", xlab="Merged Status", ylab="Lines of Code Deleted")
boxplot(Zephyr_all$loc_additions~Zephyr_all$merged_status,data=Zephyr_all, main="Lined of Code Added - Outliers", xlab="Merged Status", ylab="Lined of Code Added")

################################ LOGISTIC REGRESSION ON DATA WITH OUTLIERS ###################################################

# Descriptive statistics
psych::describeBy(Zephyr_all, Zephyr_all$merged_status)

# Model with all variables
model_1 <- glm(Zephyr_all$merged_status ~ Zephyr_all$loc_additions + Zephyr_all$loc_deletions + Zephyr_all$num_files_changed, family = binomial)
summary(model_1)

# Odds ratio
exp(cbind(Odds_Ratios=coef(model_1), confint(model_1)))

############################################## Num Of Files OUTLIERS ##########################################################


# finding outliers for num of files changed - FIELD 1
outliers_num_of_files_changed <- boxplot(Zephyr_all$num_files_changed, plot=FALSE)$out

# now find which rows the outliers are in
Zephyr_all[which(Zephyr_all$num_files_changed %in% outliers_num_of_files_changed),]

# now add these rows in a new outlier object
Zephyr_all_outliers_nfc <- Zephyr_all[which(Zephyr_all$num_files_changed %in% outliers_num_of_files_changed),]

# Extract outliers for num of files changed
write.csv(Zephyr_all_outliers_nfc, "~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/nfc_outliers.csv")

# now remove those rows from the main data
Zephyr_all <- Zephyr_all[-which(Zephyr_all$num_files_changed %in% outliers_num_of_files_changed),]

Zephyr_all_excluding_NFC_Outliers <- Zephyr_all

# Extract data minus outliers for num of files changed
write.csv(Zephyr_all_excluding_NFC_Outliers, "~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/data_without_nfc_outliers.csv")


############################ LOGISTIC REGRESSION WITH DATA EXCLUDING ONLY Num Of Files OUTLIERS ##############################

# Descriptive Statistics
psych::describeBy(Zephyr_all_excluding_NFC_Outliers, Zephyr_all_excluding_NFC_Outliers$merged_status)

# Model with all variables excluding NFC Outliers
model_2 <- glm(Zephyr_all_excluding_NFC_Outliers$merged_status ~ Zephyr_all_excluding_NFC_Outliers$loc_additions + Zephyr_all_excluding_NFC_Outliers$loc_deletions + Zephyr_all_excluding_NFC_Outliers$num_files_changed, family = binomial)
summary(model_2)

# Odds ratio
exp(cbind(Odds_Ratios=coef(model_2), confint(model_2)))


############################### LOGISTIC REGRESSION ON DATA WITH Num Of Files OUTLIERS #######################################

# Descriptive statistics
psych::describeBy(Zephyr_all_outliers_nfc, Zephyr_all_outliers_nfc$merged_status)

# Model with all variables
model_3 <- glm(Zephyr_all_outliers_nfc$merged_status ~ Zephyr_all_outliers_nfc$loc_additions + Zephyr_all_outliers_nfc$loc_deletions + Zephyr_all_outliers_nfc$num_files_changed, family = binomial)
summary(model_3)

# Odds Ratio
exp(cbind(Odds_Ratios=coef(model_3), confint(model_3)))


# Model with individual variable
model_i <- glm(Zephyr_all_outliers_nfc$merged_status ~ Zephyr_all_outliers_nfc$loc_additions, family = binomial)
summary(model_i)


#################################################### Lines of Code Deleted OUTLIERS ############################################

# finding outliers for lines of code deleted - FIELD 2
outliers_loc_deleted <- boxplot(Zephyr_all$loc_deletions, plot=FALSE)$out

# now find which rows the outliers are in
Zephyr_all[which(Zephyr_all$loc_deletions %in% outliers_loc_deleted),]

# now add these rows in a new outlier object
Zephyr_all_outliers_lod <- Zephyr_all[which(Zephyr_all$loc_deletions %in% outliers_loc_deleted),]

# Extract outliers for lines of code deleted
write.csv(Zephyr_all_outliers_lod, "~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/lod_outliers.csv")

# now remove those rows from main data
Zephyr_all <- Zephyr_all[-which(Zephyr_all$loc_deletions %in% outliers_loc_deleted),]

Zephyr_all_excluding_LOD_Outliers <- Zephyr_all

# Extract data without outliers for num of files changed
write.csv(Zephyr_all_excluding_LOD_Outliers, "~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/data_without_lod_outliers.csv")


#############################  LOGISTIC REGRESSION WITH DATA EXCLUDING ONLY Lines of code Deleted OUTLIERS ###########################

# Descriptive Statistics
psych::describeBy(Zephyr_all_excluding_LOD_Outliers, Zephyr_all_excluding_LOD_Outliers$merged_status)

# Model with all variables excluding NFC Outliers
model_4 <- glm(Zephyr_all_excluding_LOD_Outliers$merged_status ~ Zephyr_all_excluding_LOD_Outliers$loc_additions + Zephyr_all_excluding_LOD_Outliers$loc_deletions + Zephyr_all_excluding_LOD_Outliers$num_files_changed, family = binomial)
summary(model_4)

# Odds ratio
exp(cbind(Odds_Ratios=coef(model_4), confint(model_4)))


############################### LOGISTIC REGRESSION ON DATA WITH lines of code deleted OUTLIERS ##############################

# Descriptive statistics
psych::describeBy(Zephyr_all_outliers_lod, Zephyr_all_outliers_lod$merged_status)

# Model with all variables
model_5 <- glm(Zephyr_all_outliers_lod$merged_status ~ Zephyr_all_outliers_lod$loc_additions + Zephyr_all_outliers_lod$loc_deletions + Zephyr_all_outliers_lod$num_files_changed, family = binomial)
summary(model_5)

# Odds Ratio
exp(cbind(Odds_Ratios=coef(model_5), confint(model_5)))


############################################ Lines of Code Added OUTLIERS ######################################################


# finding outliers for lines of code added - FIELD 3
outliers_loc_added <- boxplot(Zephyr_all$loc_additions, plot=FALSE)$out

# now find which rows the outliers are in
Zephyr_all[which(Zephyr_all$loc_additions %in% outliers_loc_added),]

# now add these rows in a new outlier object
Zephyr_all_outliers_loa <- Zephyr_all[which(Zephyr_all$loc_additions %in% outliers_loc_added),]

# Extract outliers for num of files changed
write.csv(Zephyr_all_outliers_loa, "~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/loa_outliers.csv")

# now remove those rows  from main data
Zephyr_all <- Zephyr_all[-which(Zephyr_all$loc_additions %in% outliers_loc_added),]

Zephyr_all_excluding_LOA_Outliers <- Zephyr_all

# Extract data minus outliers for num of files changed
write.csv(Zephyr_all_excluding_LOA_Outliers, "~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/data_without_loa_outliers.csv")


#############################  LOGISTIC REGRESSION WITH DATA EXCLUDING ONLY Lines of code Added OUTLIERS ###########################

# Descriptive Statistics
psych::describeBy(Zephyr_all_excluding_LOA_Outliers, Zephyr_all_excluding_LOA_Outliers$merged_status)

# Model with all variables excluding NFC Outliers
model_6 <- glm(Zephyr_all_excluding_LOA_Outliers$merged_status ~ Zephyr_all_excluding_LOA_Outliers$loc_additions + Zephyr_all_excluding_LOA_Outliers$loc_deletions + Zephyr_all_excluding_LOA_Outliers$num_files_changed, family = binomial)
summary(model_6)

# Odds ratio
exp(cbind(Odds_Ratios=coef(model_6), confint(model_6)))


############################### LOGISTIC REGRESSION ON DATA WITH lines of code added OUTLIERS #####################################

# Descriptive Statistics
psych::describeBy(Zephyr_all_outliers_loa, Zephyr_all_outliers_loa$merged_status)

# Model with all variables
model_7 <- glm(Zephyr_all_outliers_loa$merged_status ~ Zephyr_all_outliers_loa$loc_additions + Zephyr_all_outliers_loa$loc_deletions + Zephyr_all_outliers_loa$num_files_changed, family = binomial)
summary(model_7)

# Odds Ratio
exp(cbind(Odds_Ratios=coef(model_7), confint(model_7)))


################################## RECAP ######################################################################################


# Data including outliers
view(Raw_Data)

# Data excluding only Num Files Changed Outliers
view(Zephyr_all_excluding_NFC_Outliers)

# Data excluding only Lines of code deleted Outliers
view(Zephyr_all_excluding_LOD_Outliers)

# Data excluding only Lines of code added Outliers
view(Zephyr_all_excluding_LOA_Outliers)

# Data without any outliers
view(Zephyr_all)

# Data with num of files added outliers
view(Zephyr_all_outliers_nfc)

# Data with lines of code deleted outliers
view(Zephyr_all_outliers_lod)

# Data with lines of code added outliers
view(Zephyr_all_outliers_loa)

##################################### ALL OUTLIERS COMBINED #############################################################

# Import Zephyr's all outliers data
library(readxl)
outliers <- read.csv("~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/outliers_combined.csv")

# Descriptive Statistics
psych::describeBy(outliers, outliers$merged_status)

# Model with all variables
model_o<- glm(outliers$merged_status ~ outliers$loc_additions + outliers$loc_deletions + outliers$num_files_changed, family = binomial)
summary(model_o)



############################## LOGISTIC REGRESSION ON DATA WITHOUT ANY OUTLIERS I.E. NORMAL RANGE ####################################


# removing outlier data from full dataset
library(dplyr)
Zephyr_normal <- anti_join(Zephyr_all, outliers, by='pull_request_id')

# Extract file without any outliers
write.csv(Zephyr_normal, "~/Documents/GA Work/Prof Matt/FINAL VERSION/ZEPHYR/DATA/data_without_outliers.csv")

# Descriptive Statistics
psych::describeBy(Zephyr_normal, Zephyr_normal$merged_status)

# Model with all variables
model_normal <- glm(Zephyr_normal$merged_status ~ Zephyr_normal$loc_additions + Zephyr_normal$loc_deletions + Zephyr_normal$num_files_changed, family = binomial)
summary(model_normal)
