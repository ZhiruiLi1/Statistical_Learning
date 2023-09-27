# Load data 
college_df <- read.csv("/Users/alice/Dropbox/2020/college_scorecard.csv")
names(college_df)

# Can drop some variables
college_df <- college_df %>% select(-c(UNITID, OPEID, CITY, STABBR, ACCREDAGENCY,
                         INSTURL, NPCURL, SCH_DEG, CCUGPROF, CCSIZSET))

# Convert to numeric for most variables
college_df[,16:ncol(college_df)] <- apply(college_df[,16:ncol(college_df)], 2, 
                                         as.numeric)
college_df[,2:15] <- apply(college_df[,2:15], 2, as.factor)
head(college_df)

# Outcome of interest
hist(college_df$MD_EARN_WNE_P10) # transformation may be helpful!
hist(log(college_df$MD_EARN_WNE_P10))

# Missing data - there is a lot!
college_df <- college_df[complete.cases(college_df$MD_EARN_WNE_P10),]
dim(college_df)
apply(college_df, 2, function(x) sum(complete.cases(x))/nrow(college_df))

# These variables have been coded separately for private and public
college_df$NPT <- ifelse(is.na(college_df$NPT4_PRIV), 
                         college_df$NPT4_PUB,
                         college_df$NPT4_PRIV)
college_df$NUM <- ifelse(is.na(college_df$NUM4_PRIV), 
                         college_df$NUM4_PUB,
                         college_df$NUM4_PRIV)

# Start to narrow down variables - start with tuition ones
library(GGally)
ggpairs(college_df[,c("MD_EARN_WNE_P10",
                      "NPT", 
                      "COSTT4_A", 
                      "TUITIONFEE_IN", 
                      "TUITIONFEE_OUT", 
                      "INEXPFTE")])
# Can keep NPT and INEXPFTE - much lower percentage missing than others 

# Some variables to drop
college_df <- college_df %>% select(-c(NPT4_PRIV, NPT4_PUB, 
                                       COSTT4_A, TUITIONFEE_IN, 
                                       TUITIONFEE_OUT, NUM4_PUB,
                                       NUM4_PRIV, MN_EARN_WNE_P10))        

names(college_df)
ggpairs(college_df[,c(17:24, 77)]) 
# maybe keep SATAVG and/or ACTWRMID but sacrifice data


