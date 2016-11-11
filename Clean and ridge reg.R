library(devtools)
install_github("vqv/ggbiplot")
library(ggplot2)
library(ggbiplot)
library(gridExtra)
library(corrplot)
library(plyr)
library(dplyr)
library(caret)
library(mice)
library(VIM)
library(doParallel)

#working directory
path <- "C:/Users/Francois chevallier/Desktop/BigMart"

#set working directory
setwd(path)

#Load Datasets
train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

#check dimesions, variable types and brief summary
dim(train)
dim(test)
str(train)
summary(train)

#Cleaning Data

#Merge the train and the test dataset to operate the cleaning operations
#To match the dim, let's add 0 into test's missing column

test$Item_Outlet_Sales <-  0
combined <- rbind(train, test)

#Simplify/Factorize the Item_Fat_Content dimension 
levels(combined$Item_Fat_Content)
#Convert LF/low fat into Low Fat and reg into Regular
combined$Item_Fat_Content <- revalue(combined$Item_Fat_Content,
                                  c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))


# count fat levels for each Item type
fat <- as.data.frame( setNames(
  aggregate(
    combined$Item_Fat_Content, 
    by=list(Category=combined$Item_Type,
            Category=combined$Item_Fat_Content), 
    FUN= length),
  c("Item_Type", "Item_Fat_Content", "number")
))

fat

#Create a third fat level (None) for products other than food
levels(combined$Item_Fat_Content) <- c(levels(combined$Item_Fat_Content), "None")
combined[ which(combined$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content <- "None"
combined[ which(combined$Item_Type == "Household") ,]$Item_Fat_Content <- "None"
combined[ which(combined$Item_Type == "Others") ,]$Item_Fat_Content <- "None"
combined$Item_Fat_Content <- factor(combined$Item_Fat_Content)

# count fat levels for each Item type
fat <- as.data.frame( setNames(
  aggregate(
    combined$Item_Fat_Content, 
    by=list(Category=combined$Item_Type,
            Category=combined$Item_Fat_Content), 
    FUN= length),
  c("Item_Type", "Item_Fat_Content", "number")
))

fat

#Entries in Outlet_Size are missing, insert Other instead
levels(combined$Outlet_Size)[1] <- "Other"

#Look for missing values
colSums(is.na(combined))

# Weight values are missing
# boxplot of weights/Item type
ggplot(combined, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item_Type") + 
  ylab("Weight") + 
  ggtitle("Item Weight/Item Type")

# boxplot of weights/Outlet Identifier
ggplot(combined, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Weight") + 
  ggtitle("Item Weight/Outlet identifier")

# Two specific outlet didn't provide weights
# assuming that each item identifier actually identifies a unique item,
# hence a unique weight, let's create a dataframe containing the mean
# weights and standard deviations by item identifier
weightsByItem <- as.data.frame( ddply(na.omit(combined), 
                                      ~Item_Identifier, 
                                      summarise, 
                                      mean=mean(Item_Weight), 
                                      sd=sd(Item_Weight)))

# replace values my mean found
combined$Item_Weight <- ifelse(is.na(combined$Item_Weight), 
                            weightsByItem$mean[
                              match(combined$Item_Identifier, weightsByItem$Item_Identifier)], combined$Item_Weight)

#any values still missing?
table(is.na(combined))


# boxplot of weights/Item type
ggplot(combined, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")

# boxplot of weights/Outlet Identifier
ggplot(combined, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet identifier")

# Create a year dimension base a data set year and the establishement dimension
combined$Year <- as.factor(2013 - combined$Outlet_Establishment_Year)

# Drop Outlet_Establishment_Year
combined <- select(combined, -c(Outlet_Establishment_Year))


# plot Item_MRp
ggplot(combined, aes(x=Item_MRP)) + 
  geom_density(color = "blue", adjust=1/5) +
  geom_vline(xintercept = 69, color="red")+
  geom_vline(xintercept = 136, color="red")+
  geom_vline(xintercept = 203, color="red") + 
  ggtitle("Density of Item MRP")

# Create MRP_Levels to match the four Item densities

combined$MRP_Level <- as.factor(
  ifelse(combined$Item_MRP < 69, "Low",
         ifelse(combined$Item_MRP < 136, "Medium",
                ifelse(combined$Item_MRP < 203, "High", "Very_High")))
)

# reorder the dataset such that the response variable Item_Outlet_Sales comes last
combined <- select( combined, c(Item_Identifier,
                          Item_Weight,
                          Item_Fat_Content,
                          Item_Visibility,
                          Item_Type,
                          Item_MRP,
                          Outlet_Identifier,
                          Outlet_Size,
                          Outlet_Location_Type,
                          Outlet_Type,
                          Year,
                          MRP_Level,
                          Item_Outlet_Sales
))

str(combined)

# how often does each Outlet_Identifier appear in the data
aggregate(combined$Outlet_Identifier, by=list(Category=combined$Outlet_Identifier), FUN=length)

# clearly, the two grocery stores, OUT010 and OUT019 have reported far
# less data than the supermarkets, certainly caused by their limited size and products displayable

# As a check let's count the Item IDs:
aggregate(combined$Item_Identifier, by=list(Category=combined$Outlet_Identifier), FUN= length)

#grocery stores have indeed a smaller selection of goods to sell.

# What else can we learn about the different types of shops?

# boxplot of  Sales vs. Outlet identifier
ggplot(combined[1:nrow(train),], aes(Outlet_Identifier, Item_Outlet_Sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet identifier") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet identifier")

# boxplot of  Sales vs. Outlet Type
ggplot(combined[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet Type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet Type")

# Year of service may have an impact on sales 

# boxplot of  Sales vs. Outlet Type
ggplot(combined[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")

# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
  aggregate(
    combined$Outlet_Size, 
    by=list(Category=combined$Outlet_Identifier, 
            Category=combined$Outlet_Type,
            Category=combined$Outlet_Location_Type,
            Category=combined$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))

otherShops

# a Grocery store should be in category Small
# Type 1 supermarkets are ususally classified as Small

levels(combined$Outlet_Size)[1] <- "Small"

combined[ which(combined$Outlet_Identifier == "OUT010") ,]$Outlet_Size <- "Small"
combined[ which(combined$Outlet_Identifier == "OUT017") ,]$Outlet_Size <- "Small"
combined[ which(combined$Outlet_Identifier == "OUT045") ,]$Outlet_Size <- "Small"

# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
  aggregate(
    combined$Outlet_Size, 
    by=list(Category=combined$Outlet_Identifier, 
            Category=combined$Outlet_Type,
            Category=combined$Outlet_Location_Type,
            Category=combined$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))

otherShops

# apply factor to Outlet_Size in order to drop the level "Other"

combined$Outlet_Size <- factor(combined$Outlet_Size)

str(combined)

# boxplot of  Sales vs. Outlet location
ggplot(combined[1:nrow(train),], aes(x = Outlet_Location_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet location") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet location")

# boxplot of  Sales vs. Outlet type
ggplot(combined[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")

# boxplot of  Sales vs. Item type
ggplot(combined[1:nrow(train),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")

# boxplot of  Sales vs. Item type
ggplot(combined[1:nrow(train),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")


#Replace zero in visibility with inputed value

# boxplot of Visibility vs Item type
ggplot(combined, aes(Item_Type, Item_Visibility, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Item Type")

# boxplot of Visibility vs. Outlet Identifier
ggplot(combined, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")


# Save the visibility distributions before inputation

combinedNonZeroVis <- subset(combined, Item_Visibility > 0)

# replace 0 by NA so that mice can work
outletIdentifiers <- levels(combined$Outlet_Identifier)
itemTypes <- levels(combined$Item_Type)
for (outName in outletIdentifiers) {
  for (itemName in itemTypes) {
    combined[ which(combined$Outlet_Identifier == outName &
                   combined$Item_Type == itemName),]$Item_Visibility <-
      ifelse(
        combined[ which(combined$Outlet_Identifier == outName &
                       combined$Item_Type == itemName), ]$Item_Visibility == 0 ,
        NA ,
        combined[ which(combined$Outlet_Identifier == outName &
                       combined$Item_Type == itemName),]$Item_Visibility
      )
  }
}

# let mice impute the missing visibilities
newcombined <- mice(combined,m=1,maxit=1,meth='pmm',seed=0)
# summary of imputations
summary(newcombined)

# comparison of the distribution 
densityplot(newcombined)
stripplot(newcombined, pch = 20, cex = 1.2)

# let's replace the NA values by the imputed ones
combined <- complete(newcombined,1)


# total visibility per shop

shopSum <- as.data.frame(setNames(
  aggregate(combined$Item_Visibility, by=list(Category=combined$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

shopSum

# Normalize all visibilities such that

for (outName in outletIdentifiers) {
  combined[ which(combined$Outlet_Identifier == outName),]$Item_Visibility <-
    combined[ which(combined$Outlet_Identifier == outName),]$Item_Visibility *
    100/shopSum[ which(shopSum$Outlet_Identifier == outName),]$TotVis
}

shopSum <- as.data.frame(setNames(
  aggregate(combined$Item_Visibility, by=list(Category=combined$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

shopSum

# densities of visibilities before and after imputation
ggplot() + 
  geom_density(aes(x=Item_Visibility), colour="red", data=combinedNonZeroVis) + 
  geom_density(aes(x=Item_Visibility), colour="blue", data=combined)



 # Simplify Item_Class
combined$Item_Identifier <- strtrim(combined$Item_Identifier, 3)
combined$Item_Identifier <- factor(combined$Item_Identifier)


# correlation between numerical variables
corMatrix <- cor(combined[1:nrow(train),][sapply(combined[1:nrow(train),], is.numeric)])
corMatrix

# a brief overview of the correlation matrix
corrplot::corrplot(corMatrix, method="number", type="upper")
corrplot::corrplot(corMatrix, method="number", type="upper", order="hclust")

# Item_Outlet_Sales has a strong positive correlation with Item_MRP

#PCA

subData <- as.data.frame(cbind(
  combined[1:nrow(train),]$Item_Visibility, 
  combined[1:nrow(train),]$Item_MRP, 
  combined[1:nrow(train),]$Item_Outlet_Sales))

names(subData) <- c("Item_Visibility",
                    "Item_MRP",
                    "Item_Outlet_Sales")

sub.groupby <- combined[1:nrow(train),]$Outlet_Type

str(subData)

subData.pca <- prcomp(subData,
                      center = TRUE,
                      scale. = TRUE) 

summary(subData.pca)

max(combined$Item_MRP)
min(combined$Item_MRP)

# Handle outliers by dividing Sales by the MRP

combined$Item_Outlet_Sales <- combined$Item_Outlet_Sales/combined$Item_MRP



# Proportion of Supermarkets vs. Grocery stores in the data
prop.table(table(combined$Outlet_Type))

# In a nutshell, a significant difference could be observed between groceries and supermarkets


# let's resurrect the train and test data sets
new_train <- combined[1:nrow(train),]
new_test <- combined[-(1:nrow(train)),]

# and drop the faked Item_Outlet_Sales column in new_test
new_test <- dplyr::select(new_test, -c(Item_Outlet_Sales))

str(new_train)
str(new_test)

# let's save them, so that we don't have to redo the cleaning
# over and over again
write.csv(new_train, file="new_train.csv", row.names=FALSE, quote = FALSE)
write.csv(new_test, file="new_test.csv", row.names=FALSE, quote = FALSE)

# check variable importance with 
# random feature elimination (RFE)
# from caret

# scale Sales to be in interval [0,1]
maxSales <- max(new_train$Item_Outlet_Sales)
new_train$Item_Outlet_Sales <- new_train$Item_Outlet_Sales/maxSales

set.seed(0)

# one-hot encoding of the factor variables
# leave out the intercept column

new_train <- as.data.frame(model.matrix( ~ . + 0, data = new_train))
new_test <- as.data.frame(model.matrix( ~ . + 0, data = new_test))

str(new_train)

# define a vector of Item_Outlet_Sales
# and a dataframe of predictors
sales <- new_train$Item_Outlet_Sales
predictors <- subset(new_train, select=-c(Item_Outlet_Sales))
predictorsTest <- subset(new_test, select=-c(Item_Outlet_Sales))

#ridge reg
X <- as.matrix(predictors)
Y <- as.matrix(sales)
Z <-as.matrix(predictorsTest)
fit <- glmnet(X, Y, family="gaussian", alpha=0, lambda=0.001)
prediction <- predict(fit, X, type="link")
predictions <- predict(fit, Z, type="link")
#rmse <- mean((Y - predictions)^2)
#print(rmse)

new_test$pred<- predictions*maxSales

#sub<-subset(new_test, select=c("pred"))
#write.csv(sub, file="new_test2.csv", row.names=FALSE, quote = FALSE)
#submision <- read.csv("submission.csv")
#submission$Item_Outlet_Sales<-predictions*maxSales
#write.csv(Sa, file="Submi.csv", row.names=FALSE, quote = FALSE)