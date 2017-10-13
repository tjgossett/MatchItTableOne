library(MatchIt)
library(tableone)

# https://www.rdocumentation.org/packages/MatchIt/versions/1.0-1/topics/matchit
# tableone Vignettes https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html


csvdata<-read.csv("C:\\R Local Repository\\RD_MatchitTable1\\Market Propsensity ScoreCSV.csv")

## Data Source:: Made up Sales Data
# Name - Sales Person's Name
# Sales - Total Sales in Dollars
#TrainingLevel - Percentage of the training they have completed
#TenureYears - Number of Years the Sales Person has been working
#Salary - Sales person current Salary
#FullyTrainedGT85 - If TraviningLevel is greater than 85% then Sales person is considered Fully Trained




#Set the row names of the dataframe as the names of the sales people
row.names(csvdata)<-csvdata$Name


### Create a comparison table before the data is matched

## CreateTableOne Use ANOVA analysis for continuous variables and chisq.test() for categorical variables
### The Null Hypothesis for ANOVA is that the mean is the same for all groups. (If P is very small, Null hypothesis must be rejected )
### The Alternative Hypothesis is that the mean is NOT the same for all groups.

table1 <- CreateTableOne(vars=c('TenureYears','AwardsWon','Sales'),data=csvdata,strata='FullyTrainedGT85')
table1 <- print(table1, printToggle = FALSE, noSpaces = TRUE,smd=TRUE)
print(table1)



### Use MatchIt to get the control groups based on FullyTrainedGT85 
### Use Tenure and AwardsWon as the matching parameters

MatchItOutPutModel<- matchit( FullyTrainedGT85 ~ (TenureYears+AwardsWon), data=csvdata,method="nearest", distance="logit",replace=TRUE,ratio=5,caliber=.1,calclosest=TRUE)
SummaryMatchItOutPutModel<-summary(MatchItOutPutModel)
Matches<-MatchItOutPutModel$match.matrix
#print(Matches)

MatchedDataFrame<- match.data(MatchItOutPutModel)


## Create another table1 to compare means of the TenureYears and Awardswon 
## Use FullyTrainedGT85 to create two groups

table1 <- CreateTableOne(vars=c('TenureYears','AwardsWon','Sales'),data=MatchedDataFrame,strata='FullyTrainedGT85')
table1 <- print(table1, printToggle = FALSE, noSpaces = TRUE,smd=TRUE)
print(table1)






