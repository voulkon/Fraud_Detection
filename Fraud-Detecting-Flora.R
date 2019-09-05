# Data Manipulation
if(!require(tidyverse)) install.packages("tidyverse",
                              repos = "http://cran.us.r-project.org")
# showing multiple plots
if(!require(gridExtra)) install.packages("gridExtra",
                              repos = "http://cran.us.r-project.org")
# Assisting gridExtra
if(!require(grid)) install.packages("grid",
                              repos = "http://cran.us.r-project.org")
# Model training 
if(!require(caret)) install.packages("caret", 
                              repos = "http://cran.us.r-project.org")
# Pairplot
if(!require(GGally)) install.packages("GGally", 
                              repos = "http://cran.us.r-project.org")
# ROC Curve
if(!require(pROC)) install.packages("pROC", 
                              repos = "http://cran.us.r-project.org")
# ROC Curve
if(!require(ROCR)) install.packages("ROCR",
                              repos = "http://cran.us.r-project.org")
#Refactoring
if(!require(radiant.data)) install.packages("radiant.data",
                              repos = "http://cran.us.r-project.org")



# Relative path - assumes data and .rmd file are in the same directory
# Github Repository : https://github.com/voulkon/Fraud_Detection
zipfile <- paste0(getwd(),"/PaySim1M.zip") %>% 
  str_replace_all( "/", "\\\\" )

# if data is not the same dir
# enable the line below and point it to the zip file location
#zipfile <- choose.files(caption = "Please choose zipfile location")

#unzip and read the csv contained in it
df <- unzip(zipfile = zipfile) %>% 
        read.csv(stringsAsFactors = FALSE)#, nrow = 8000) 


#A glance at the structure
df %>% str()

#Bar Plot with count of fraud vs no_fraud cases
df %>% ggplot(aes(x = isFraud, y = (..count..))) +
    geom_bar(fill =  c( "#5b7fb0", "#c2695b" ) , stat = "count")+
    geom_label(stat='count',aes(   label=  paste0(  round(   ((..count..)/sum(..count..)) ,4)*100 ,  "%" ) ) )+ #add the percentage of each type as label
    labs(x = "Fraud or Not", y = "Frequency", title = "Frequency of Fraud", subtitle = "Labels as percent of Total Observations")+
    theme_linedraw()


#A quick glance to the number of dinstinct values of each character column
print("Number of Distinct Values in Character Vectors")
df[ , sapply(df,is.character)] %>% sapply(n_distinct) %>% print()



print("Distinct Prefixes in nameOrig and nameDest columns : ")

df %>% mutate(origin_name_prefix = str_sub(nameOrig,1,1), #create columns containing prefix
              dest_name_prefix = str_sub(nameDest, 1, 1)) %>%
        select( c( origin_name_prefix,dest_name_prefix ) ) %>% #isolate them 
        table() %>% #count of distinct occurences
        print() #show

df <- df %>% mutate(dest_name_prefix = str_sub(nameDest, 1, 1)) %>% #create columns containing prefix
                    select(-c('nameOrig','nameDest','isFlaggedFraud')) %>% #discard columns with old names
                    mutate_if(is.integer, as.factor) %>% #turn integer features into factors
                    mutate_if(is.character,as.factor) %>% #turn character features into factors
                    rename("Old_Balance_of_Origin" = "oldbalanceOrg" , #rename into a readable format
                            "New_Balance_of_Origin" = "newbalanceOrig",
                            "Old_Balance_of_Destination" = "oldbalanceDest",
                            "New_Balance_of_Destination" = "newbalanceDest")

levels(df$isFraud) <- c("No_Fraud", "Fraud" ) #rename into a readable format

#Define colors associated with each group
dest_name_colors <- c("C" = "#6A4491", "M" = "#CC6600")

#Count of prefix in fraud cases

p3 <- df %>% filter(isFraud == "Fraud") %>% 
    ggplot( aes( x = isFraud, fill = dest_name_prefix ) ) +
    geom_bar()+
    scale_fill_manual(values=dest_name_colors) +
    labs(title = "Destination Name Prefix",subtitle = "Fraud Cases", x = "", y = "Frequency" )+
    theme_classic()

p4 <-df %>% filter(isFraud == "No_Fraud") %>% 
    ggplot( aes( x = isFraud, fill = dest_name_prefix ) ) +
    geom_bar()+
    scale_fill_manual(values=dest_name_colors) +
    labs(title = "Destination Name Prefix", subtitle =  "No Fraud Cases", x = "", y = "" )+
    theme_classic()

grid.arrange(p3,p4,ncol = 2)
rm(p3,p4)

#Create a color palette
type_colors <- c(CASH_IN = "#78858f", CASH_OUT = "#CC6600", DEBIT ="#6A4491", PAYMENT = "#FFC233", TRANSFER = "#788f78")

#Count of type in fraud cases
p1 <- df %>% filter(isFraud == "Fraud") %>% 
    ggplot( aes( x = isFraud, fill = type ) ) +
    geom_bar()+
    theme(axis.text.x = element_blank())+
    scale_fill_manual(values=type_colors) +
    labs(title = "Transaction Type", subtitle =  "Fraud Cases", x = "", y = "" ) + 
    coord_flip() + theme_linedraw()+
    theme(axis.text.y = element_blank())


p2 <-df %>% filter(isFraud == "No_Fraud") %>% 
    ggplot( aes( x = isFraud, fill = type ) ) +
    geom_bar()+
    scale_fill_manual(values=type_colors) +
    labs(subtitle =  "No Fraud Cases", x = "", y = "Frequency" ) + coord_flip() + theme_linedraw() +
    theme(axis.text.y = element_blank())

grid.arrange(p1,p2,nrow = 2)

#Store to number of rows before reduction
rows_before <- dim(df)[1]

df <- df %>% filter( (!(type %in% c("CASH_IN", "DEBIT", "PAYMENT") )) , #remove transaction types
                   dest_name_prefix == "C") %>% #remove rows with M prefixes
            select(-dest_name_prefix) #drop the prefixes column

#Refactor the type column
df$type <- (refactor( df$type, unique(df$type) ))

rows_after <- dim(df)[1]
#Difference in rows
diff <- rows_before - rows_after
#As percentage
diff_perc <- round(diff/rows_before * 100, 2)

sprintf( "%i rows removed, equivalent to %s%%  of the data", diff, diff_perc ) %>% print()


p11 <-  df %>% filter(isFraud == "Fraud") %>% 
    ggplot( aes( x = isFraud, fill = type ) ) +
    geom_bar()+
    scale_fill_manual(values=type_colors) +
    labs(title = "Transaction Type", subtitle =  "Fraud Cases", x = "", y = "" ) + 
    coord_flip() + theme_linedraw()+
    theme(axis.text.y = element_blank())  

p12 <-df %>% filter(isFraud == "No_Fraud") %>% 
    ggplot( aes( x = isFraud, fill = type ) ) +
    geom_bar()+
    scale_fill_manual(values=type_colors) +
    labs(subtitle =  "No Fraud Cases", x = "", y = "Frequency" ) + 
    coord_flip() + theme_linedraw()+
    theme(axis.text.y = element_blank())


grid.arrange(p1,p2,top = "Before Data Reduction")
grid.rect(gp = gpar(lwd = 3, col = "black", fill = NA))
grid.arrange(p11,p12,top = "After Data Reduction")
grid.rect(gp = gpar(lwd = 3, col = "black", fill = NA))
rm(p1,p2,p11,p12)

temp <- seq(0, max(as.numeric(df$step)) , 5 )

df %>% sample_n(floor(nrow(df)*.03)) %>% 
    ggplot()+
    geom_jitter(aes( x = as.numeric(step), y = log(amount+1,10)), alpha = .9, col = "#2ff7b5")+
    facet_grid(~isFraud)+
    scale_x_continuous(breaks = temp )+
    labs(x = "Hour", y = "Amount (in a log10 scale)", title = "Transaction Amounts by hour", subtitle = "")+
    theme_minimal()+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=3) )

df %>% 
    ggplot(aes(x = "isFraud", y = log(amount,10) , fill = isFraud)) + 
    geom_violin() + coord_flip() + 
    scale_fill_manual(values=c("#40448f", "#7d1f6d") , name="Mean", labels=c( "No Fraud","Fraud"))+
    labs(x = "", y = "Amount (in a log 10 scale)", title = "Amount of Transactions", subtitle = "Fraud vs Not Fraud Cases" ) +
    stat_summary(fun.y = "mean",geom = "point", size = 2, col =c("#40448f", "#7d1f6d"))+
    theme_classic() 

which(df %>% sapply(is.numeric))[-1] %>% names() -> labs # store names for use in plot labels

p5 <- df %>%    
    ggplot(aes(x = isFraud, y = log(Old_Balance_of_Origin+1,10) , fill = isFraud)) + 
    geom_boxplot(alpha = .3) + coord_flip() + 
    scale_fill_manual(values=c("navy", "darkred"))+
    labs(x = "", y = paste(labs[1], "(in a log 10 scale)"), title = paste("Amount of", labs[1] ), subtitle = "Fraud vs Not Fraud Cases"  ) +
    theme_linedraw()


p6 <- df %>%
    ggplot(aes(x = isFraud, y = log(New_Balance_of_Origin+1,10) , fill = isFraud)) + 
    geom_boxplot(alpha = .3) + coord_flip() + 
    scale_fill_manual(values=c("navy", "darkred"))+
    labs(x = "", y = paste(labs[2], "(in a log 10 scale)"), title = paste("Amount of", labs[2] ) ) +
    theme_linedraw()

grid.arrange(p5,p6)
rm(p5,p6)

p7 <- df %>%  filter(Old_Balance_of_Origin != 0) %>%
    ggplot(aes(x = isFraud, y = log(Old_Balance_of_Origin+1,10) , fill = isFraud)) + 
    geom_boxplot(alpha = .3) + coord_flip() + 
    scale_fill_manual(values=c("navy", "darkred"))+
    labs(x = "", y = paste(labs[1], "(in a log 10 scale)"), title = paste("Amount of", labs[1] ), subtitle = "Fraud vs Not Fraud Cases (zero values omitted)"  )+
    theme_linedraw()


p8 <- df %>% filter(New_Balance_of_Origin != 0) %>%
    ggplot(aes(x = isFraud, y = log(New_Balance_of_Origin+1,10) , fill = isFraud)) + 
    geom_boxplot(alpha = .3) + coord_flip() + 
    scale_fill_manual(values=c("navy", "darkred"))+
    labs(x = "", y = paste(labs[2], "(log 10 scale)"), title = paste("Amount of", labs[2] )  )+
    theme_linedraw()

grid.arrange(p7,p8)
rm(p7,p8)

p9 <- df %>% filter(Old_Balance_of_Destination != 0) %>%
    ggplot(aes(x = isFraud, y = log(Old_Balance_of_Destination+1,10) , fill = isFraud)) + 
    geom_boxplot(alpha = .3) + coord_flip() + 
    scale_fill_manual(values=c("navy", "darkred"))+
    labs(x = "", y = paste(labs[3], "(in a log 10 scale)"), title = paste("Amount of", labs[3] ), subtitle = "Fraud vs Not Fraud Cases (zero values omitted)"  )+
    theme_linedraw()

p10 <- df %>% filter(New_Balance_of_Destination != 0) %>%
    ggplot(aes(x = isFraud, y = log(New_Balance_of_Destination+1,10) , fill = isFraud)) + 
    geom_boxplot(alpha = .3) + coord_flip() + 
    scale_fill_manual(values=c("navy", "darkred"))+
    labs(x = "", y = paste(labs[4], "(in a log 10 scale)"), title = paste("Amount of", labs[4] ) )+
    theme_linedraw()

grid.arrange(p9,p10)
rm(p9,p10)

#Create a new isRound column 
df <- mutate(df, isRound = 
               as.factor(
                 ifelse( test = (df$amount - as.integer(df$amount)) == 0 , 
                         yes = 'round', no = 'float' )))

p11 <- df %>% filter( isFraud == "Fraud" ) %>% ggplot() + 
    geom_bar(aes( x = isFraud , fill = isRound ) ) +
    scale_fill_manual(values = c( "#849468", "#ded718" )) + 
    labs( title = "Round amounts of transactions" , x= "", y = "Count", subtitle = "Fraud Cases" )+
    theme_linedraw()

p12 <- df %>% filter( isFraud == "No_Fraud" ) %>% ggplot() + 
    geom_bar(aes( x = isFraud , fill = isRound ) ) +
    scale_fill_manual(values = c( "#849468", "#f7f300" ))+
    labs( x= "", y = "Count", subtitle = "No_Fraud Cases" ) +
    theme_linedraw()

grid.arrange(p11,p12)
rm(p11,p12)

iszero <- df %>% select(-amount) %>% #exclude the amount that cannot be 0
    select_if(is.numeric) %>% #select the remaining numerics - ie the balances
    mutate_all(R.utils::isZero) %>% #check whether they are zero
    rename_all(~paste0('isZero_', substr(., 1, nchar(.) - 4))) #add an isZero prefix on the names

df <- df %>% cbind(iszero) #merge with original df
rm(iszero)

p13 <- df %>% filter(isFraud == "No_Fraud") %>%
    ggplot( aes( x = isFraud, fill = isZero_Old_Balance_of_Or ) ) + 
    geom_bar(position = "fill") + 
    labs(x = "", y = "", subtitle = "No Fraud") +
    geom_label(stat="count",aes(label= paste0(round(((..count..)/sum(..count..)*100),2),"%") ), position = position_fill(vjust=0.5)) +
    scale_fill_manual(values =  c("#69b0b3", "#d0e872"), #colors
                      labels = c("Not_Zero", "Zero"), #legend labels
                      name = "Is Old Balance of Origin Zero?") + #legend title
    theme_minimal()+ 
    coord_flip()+
    theme(axis.text.y = element_blank())


p14 <- df %>% filter(isFraud == "Fraud") %>%
    ggplot( aes( x = isFraud, fill = isZero_Old_Balance_of_Or ) ) + 
    geom_bar(position = "fill") + 
    labs(x = "", y = "Percentage",subtitle = "Fraud") +
    geom_label(stat="count",aes(label= paste0(round(((..count..)/sum(..count..)*100),2),"%") ), position = position_fill(vjust=0.5)) +
    scale_fill_manual(values =  c("#69b0b3", "#d0e872"), #colors
                      labels = c("Not_Zero", "Zero"), #legend labels
                      name = "Is Old Balance of Origin Zero?")+ #legend title
    theme_minimal()+
    coord_flip()+
    theme(axis.text.y = element_blank())

p15 <- df %>% filter(isFraud == "No_Fraud") %>%
    ggplot( aes( x = isFraud, fill = isZero_New_Balance_of_Or ) ) + 
    geom_bar(position = "fill") + 
    labs(x = "", y = "",subtitle="No Fraud") +
    geom_label(stat="count",aes(label= paste0(round(((..count..)/sum(..count..)*100),2),"%") ), position = position_fill(vjust=0.5)) +
    scale_fill_manual(values =  c("#69b0b3", "#d0e872"), 
                      labels = c("Not_Zero", "Zero"),
                      name = "Is New Balance of Origin Zero?") +
    theme_minimal() +
    coord_flip()+
    theme(axis.text.y = element_blank())

p16 <- df %>% filter(isFraud == "Fraud") %>%
    ggplot( aes( x = isFraud, fill = isZero_New_Balance_of_Or ) ) + 
    geom_bar(position = "fill") + 
    labs(x = "", y = "Percentage",subtitle = "Fraud") + 
    geom_label(stat="count",aes(label= paste0(round(((..count..)/sum(..count..)*100),2),"%") ), position = position_fill(vjust=0.5)) +
    scale_fill_manual(values =  c("#69b0b3", "#d0e872"), 
                      labels = c("Not_Zero", "Zero"),
                      name = "Is New Balance of Origin Zero?")+
    theme_minimal() + coord_flip()+
    theme(axis.text.y = element_blank())

grid.arrange(p13,p14,nrow = 2, top = "Old Balance of Origin")
grid.rect(gp = gpar(lwd = 3, col = "black", fill = NA))
grid.arrange(p15,p16,nrow = 2, top = "New Balance of Origin")
grid.rect(gp = gpar(lwd = 3, col = "black", fill = NA))

rm(p13,p14,p15,p16)

p17 <- df %>% filter(isFraud == "No_Fraud") %>%
    ggplot( aes( x = isFraud, fill = isZero_Old_Balance_of_Destina ) ) + 
    geom_bar(position = "fill") + 
    labs(x = "", y = "Percentage",subtitle = "No Fraud") +
    geom_label(stat="count",aes(label= paste0(round(((..count..)/sum(..count..)*100),2),"%") ), position = position_fill(vjust=0.5)) +
    scale_fill_manual(values =  c("#69b0b3", "#d0e872"), #colors
                      labels = c("Not_Zero", "Zero"), #legend labels
                      name = "Is Old Balance of Destination Zero?") + #legend title
    theme_minimal()+ 
    coord_flip()+
    theme(axis.text.y = element_blank())

p18 <- df %>% filter(isFraud == "Fraud") %>%
    ggplot( aes( x = isFraud, fill = isZero_Old_Balance_of_Destina ) ) + 
    geom_bar(position = "fill") + 
    labs(x = "", y = "Percentage", subtitle = "Fraud") +
    geom_label(stat="count",aes(label= paste0(round(((..count..)/sum(..count..)*100),2),"%") ), position = position_fill(vjust=0.5)) +
    scale_fill_manual(values =  c("#69b0b3", "#d0e872"), #colors
                      labels = c("Not_Zero", "Zero"), #legend labels
                      name = "Is Old Balance of Destination Zero?")+ #legend title
    theme_minimal()+
    coord_flip()+
    theme(axis.text.y = element_blank())

p19 <- df %>% filter(isFraud == "No_Fraud") %>%
    ggplot( aes( x = isFraud, fill = isZero_New_Balance_of_Destina ) ) + 
    geom_bar(position = "fill") + 
    labs(x = "", y = "Percentage",subtitle = "No Fraud") +
    geom_label(stat="count",aes(label= paste0(round(((..count..)/sum(..count..)*100),2),"%") ), position = position_fill(vjust=0.5)) +
    scale_fill_manual(values =  c("#69b0b3", "#d0e872"), 
                      labels = c("Not_Zero", "Zero"),
                      name = "Is New Balance of Destination Zero?") +
    theme_minimal() +
    coord_flip()+
    theme(axis.text.y = element_blank())

p20 <- df %>% filter(isFraud == "Fraud") %>%
    ggplot( aes( x = isFraud, fill = isZero_New_Balance_of_Destina ) ) + 
    geom_bar(position = "fill") + 
    labs(x = "", y = "Percentage", subtitle = "Fraud") + 
    geom_label(stat="count",aes(label= paste0(round(((..count..)/sum(..count..)*100),2),"%") ), position = position_fill(vjust=0.5)) +
    scale_fill_manual(values =  c("#69b0b3", "#d0e872"), 
                      labels = c("Not_Zero", "Zero"),
                      name = "Is New Balance of Destination Zero?")+
    theme_minimal() + coord_flip()+
    theme(axis.text.y = element_blank())


grid.arrange(p17,p18,nrow = 2, top = "Old Balance of Destination") 
grid.rect(gp = gpar(lwd = 3, col = "black", fill = NA))
grid.arrange(p19,p20,nrow = 2, top = "New Balance of Destination") 
grid.rect(gp = gpar(lwd = 3, col = "black", fill = NA))
rm(p17,p18,p19,p20)

#labels for plotting
labs <- c('Amount',"Old/Origin", "New/Origin", "Old/Destination","New/Destination") 

#Create a smaller dataset
df_small <- df %>% sample_n(floor(nrow(df)*.1))

#Pairplot
df_small[ ,sapply(df,is.numeric)]  %>% 
    ggpairs(mapping=ggplot2::aes(colour = df_small$isFraud), title = "Numeric Features",columnLabels = labs, axisLabels = "none")
rm(df_small)

high_cor_feats <- findCorrelation(
                  cor(df[ ,sapply(df,is.numeric)]), #compute corellation table
                  cutoff = .8, verbose = TRUE, # anything above .8 as high_cor
                  names = TRUE, exact = TRUE) #return the names of columns

# Show which features did not make it 
print("High Correlation Features to be Removed")
print(high_cor_feats)


# Drop the highly correlated columns
df <- df %>% select( -high_cor_feats )


#Finding the number of fraud instances
n_minority <- df %>% filter(isFraud == "Fraud") %>% nrow()

#Sample the appropriate number of rows
rand_majority <- df %>% filter(isFraud == "No_Fraud") %>% sample_n( n_minority*6/4)

#Merge into a new dataset and arrange by time so that they get shuffled
df_bal <- df %>% filter(isFraud == "Fraud") %>% rbind( rand_majority ) %>% arrange(step)

#Repeat the frequencies bar plots to contrast with the earlier version of our dataset
p5 <- df %>% ggplot(aes(x = isFraud, y = (..count..))) + 
    geom_bar(fill =  c( "#5b7fb0", "#c2695b" ), stat = "count")+
    geom_label(stat='count',aes(   label=  paste0(  round(   ((..count..)/sum(..count..)) ,4)*100 ,  "%" ) ) )+
    labs(x = "Fraud or Not", y = "Frequency", title = "Frequency of Fraud before Undersampling", subtitle = "Labels as percent of Total Observations")+
    theme_linedraw()

p6 <- df_bal %>% ggplot(aes(x = isFraud, y = (..count..))) + 
    geom_bar(fill =  c( "#5b7fb0", "#c2695b" ), stat = "count")+
    geom_label(stat='count',aes(   label=  paste0(  round(   ((..count..)/sum(..count..)) ,4)*100 ,  "%" ) ) )+
    labs(x = "Fraud or Not", y = "Frequency", title = "Frequency of Fraud after Undersampling ", subtitle = "Labels as percent of Total Observations") + 
    theme_linedraw()

grid.arrange(p5,p6)

#Create row indices for test/train split
indx <- createDataPartition(y = df$isFraud, p = .75, list = FALSE) 
train_set <- df[indx,] 
test_set <- df[-indx,]

#Train our preprocess function parameters
pp <- preProcess( train_set[, sapply(train_set,is.numeric) ] , 
                            method = c("center", "scale"))

#Transform the train_set
train_set <- predict(pp, newdata = train_set)

#Subsequently, transform the test_set
test_set <- predict(pp, newdata = test_set)

print( "Original (Unbalanced) Train Set Structure : " )
train_set %>% str() %>% print()


indx <- createDataPartition(y = df_bal$isFraud, p = .75, list = FALSE) #75% of data as train set 
train_set_b <- df_bal[indx,]
test_set_b <- df_bal[-indx,]

#Train our preprocess function parameters
pp_b <- preProcess( train_set_b[, sapply(train_set_b,is.numeric) ] , 
                            method = c("center", "scale"))

#Transform the train_set
train_set_b <- predict(pp_b, newdata = train_set_b)

#Subsequently, transform the test_set
test_set_b <- predict(pp_b, newdata = test_set_b)

print( "Balanced Train Set Structure : " )

train_set %>% str() %>% print()

#Setting up the training parameters
tr <- trainControl(method = "repeatedcv", #repeated cross validation 
                        number = 9, #9 folds
                        repeats = 3, #3 times
                        classProbs = TRUE, #calculate probabilities for each class
                        summaryFunction = twoClassSummary) #use twoClassSummary to summarize probabilities

#Model Training - Regression Tree
start_time <- Sys.time()

rpart_model_b <- train(isFraud ~ ., #training formula -- 
                                    #we want to predict isFraud by using all available predictors
                    data = train_set_b, #our balanced dataset
                    method = "rpart", # library containing the tree algorithm
                    tuneLength = 10, # number of different default values used to optimize complexity parameter
                    metric = "ROC", # estimate success by the area under the ROC curve
                    trControl = tr, # use cross validation as defined above
                    parms=list(split='information')) 

end_time <- Sys.time()

end_time - start_time


rpart_train_pred_b <- predict(rpart_model_b, train_set_b) # class predictions on train set

confusionMatrix(train_set_b$isFraud, rpart_train_pred_b) # report results of predictions
rm(rpart_train_pred_b)

rpart_test_pred_b <- predict(rpart_model_b, test_set_b) #class predictions on test set
confusionMatrix(test_set_b$isFraud, rpart_test_pred_b) #results report
rm(rpart_test_pred_b)

rpart_probs_b <- predict(rpart_model_b, test_set_b, type = "prob") #probabilities predictions on balanced test set

rpart_ROC_b <- roc(response = test_set_b$isFraud, 
                 predictor = rpart_probs_b$Fraud, 
                 levels = levels(test_set_b$isFraud))

#probabilities predictions on balanced test set
rpart_probs_b_train <- predict(rpart_model_b, train_set_b, type = "prob") 

rpart_ROC_b_train <- roc(response = train_set_b$isFraud, 
                 predictor = rpart_probs_b_train$Fraud, 
                 levels = levels(train_set_b$isFraud))

plot(rpart_ROC_b, col = "#040491", main = "ROC Curve on Balanced Data Set (Tree Train vs Test)" )
plot(rpart_ROC_b_train, col = "green", add = TRUE)
legend(.4,.18, legend = c("Test Set", "Train Set"), col = c("#040491","green"), lty = 1 )
tree_overfit <- round(rpart_ROC_b_train$auc - rpart_ROC_b$auc, 3)
anot <- paste( "Train - Test Area : ", tree_overfit  )
text(labels = anot, x = 0.2, y = .25 )



#Pick a random sample from the unbalanced test set
random_sample <- test_set %>% 
                  sample_n(nrow(test_set)/1.3) #size = 75% of the test set rows

rpart_probs_c <- predict(rpart_model_b, random_sample, type = "prob")

rpart_ROC_c <- roc(response = random_sample$isFraud, 
                 predictor = rpart_probs_c$Fraud, 
                 levels = levels(random_sample$isFraud))

plot(rpart_ROC_c, col = "#63a105", main = "ROC curve on Unbalanced Test Set")

anot <-   paste("Area Under the Curve : " , round(rpart_ROC_c$auc,3))

text(labels = anot , x = 0, y= .2)



plot(rpart_ROC_b, col = "#040491", main = "Balanced vs Unbalanced Test Set (Classification Tree)" )
plot(rpart_ROC_c, col = "#63a105",add = TRUE)
legend(.4,.18, legend = c("Balanced", "Unbalanced"), col = c("#040491","#63a105"), lty = 1 )
tree_unb_perf <- round((rpart_ROC_b$auc - rpart_ROC_c$auc), 4)
anot <- paste( "Balanced - Unbalanced Area : ",  tree_unb_perf )
text(labels = anot, x = 0.2, y = .25 )


#Define Hyperparameters
tune_grid <- expand.grid(.mtry = c((floor(dim(random_sample)[2] * .6)),  # 60% and of predictors
                                   (floor(dim(random_sample)[2] * .4))) , # 40% of predictors
            .ntree = seq(70, 130, by = 30)) # number of trees

print("Tune Grid:")
print(tune_grid)

#Seed for reproducibility
set.seed(123)

#Model Training - Random Forest
start_time <- Sys.time()
rand_for_b <- train(isFraud ~ ., 
                  data = train_set_b, #train on the balanced set
                  method="rf", # use the random forest package
                  metric = "ROC", # elect best model by ROC curve
                  TuneGrid = tune_grid, # use custom grid for hyperparameters
                  trControl=tr) # #use cross validation

end_time <- Sys.time()
end_time - start_time

#plot error versus number of trees
plot(rand_for_b$finalModel, main = "Estimated Error by Number of Trees")

#Variable Importance
imps <- rand_for_b$finalModel$importance

#isolate feature names
nams <- (imps %>% attr("dimnames"))[[1]]

#bind names with MeandecreaseGini values
imps <- imps %>% as.data.frame() %>% 
  cbind(nams) %>% 
  arrange(desc(MeanDecreaseGini)) %>% 
  head(10) #keep top 10

#Plot importances as red dots annotating values in black text 
imps %>%
  ggplot( aes(x = reorder(nams,MeanDecreaseGini), y = MeanDecreaseGini ) ) +
  geom_point(size = 9, col = "#ff583b")+
  geom_vline(xintercept = 1: nrow(imps), col = "darkgray") + 
  geom_text( aes(label = round(MeanDecreaseGini,2), size = 5), show.legend = FALSE , col = "black" ) +
  coord_flip()+
  labs(title = "Variable Importance", x = "", y = "",subtitle = "Top 10 Features" )+
  theme_classic()

#Predictions on train set
rand_for_pred_b_train <- predict(rand_for_b, train_set_b) #class predictions on test set
print( "Train Set Confusion Matrix (Balanced Data) : " )
confusionMatrix(train_set_b$isFraud, rand_for_pred_b_train) #results report

#Predictions on test set
rand_for_pred_b <- predict(rand_for_b, test_set_b) #class predictions on test set
print( "Test Set Confusion Matrix (Balanced Data) : " )
confusionMatrix(test_set_b$isFraud, rand_for_pred_b) #results report

#probabilities predictions on balanced train set
rand_for_probs_b <- predict(rand_for_b, train_set_b, type = "prob") 

#respective ROC curve
rand_for_ROC_b <- roc(response = train_set_b$isFraud, 
                 predictor = rand_for_probs_b$Fraud, 
                 levels = levels(train_set_b$isFraud))

#probabilities predictions on balanced test set
rand_for_probs_b_test <- predict(rand_for_b, test_set_b, type = "prob") 

#respective ROC curve
rand_for_ROC_b_test <- roc(response = test_set_b$isFraud, 
                 predictor = rand_for_probs_b_test$Fraud, 
                 levels = levels(test_set_b$isFraud))

#Plot Train vs Test Performance for forest
plot(rand_for_ROC_b, col = "#040491", main = "ROC Curve on Balanced Data Set (Forest Train vs Test)" )
plot(rand_for_ROC_b_test, col = "#e6874c", add = TRUE)
#Add legend
legend(.4,.18, legend = c("Train Set", "Test Set"), col = c("#040491","#e6874c"), lty = 1 )
#Store for use in the results section
for_overfit <- round((rand_for_ROC_b$auc - rand_for_ROC_b_test$auc), 4)
#Add text
anot <- paste( "Train - Test Area : ", for_overfit  )
text(labels = anot, x = 0.2, y = .25 )

#Confusion Matrix of performance on random sample
rand_for_pred_c <- predict(rand_for_b, random_sample) #class predictions on random sample set
print("Random Sample Confusion Matrix (Unbalanced Data) : ")
confusionMatrix(random_sample$isFraud, rand_for_pred_c) #results report
rm(rand_for_pred_c)

#probabilities predictions on random sample
rand_for_probs_c <- predict(rand_for_b, random_sample, type = "prob") 

rand_for_ROC_c <- roc(response = random_sample$isFraud, 
                 predictor = rand_for_probs_c$Fraud, 
                 levels = levels(random_sample$isFraud))

#Plot Roc curve from balanced data  
plot(rand_for_ROC_c, col = "#627502", main = "Balanced vs Unbalanced (Random Forest)" )
#Plot Roc curve from unbalanced data
plot(rand_for_ROC_b_test, col = "#cf4696",add = TRUE)
#Add legend
legend(.4,.18, legend = c("Unbalanced", "Balanced"), col = c("#627502","#cf4696"), lty = 1 )
#Store for use in results section
for_unb_perf <- round((rand_for_ROC_c$auc - rand_for_ROC_b_test$auc), 4)
#Add text
anot <- paste( "Unbalanced - Balanced Area : ", for_unb_perf )
text(labels = anot, x = 0.2, y = .25 )
rm(rand_for_probs_c)

#ROC curve of forest
plot(rand_for_ROC_c, col = "#89c1c4", main = "Tree vs Forest (Unbalanced Test Set)" )
#Roc curve of tree
plot(rpart_ROC_c, col = "#b65dcf",add = TRUE)
#Show legend
legend(.4,.18, legend = c("Random Forest", "Classification Tree"), col = c( "#89c1c4","#b65dcf"), lty = 1 )
#Display the difference
anot <- paste( "Forest - Tree Area : ", round((rand_for_ROC_c$auc - rpart_ROC_c$auc), 4) )
text(labels = anot, x = 0.08, y = .25 )


#Create a dataframe containing the train-test differences 
temp <- data.frame( model = c("Tree","Forest"), performance = c(tree_overfit,for_overfit)  )

#Plot the results for each model
temp %>%ggplot( aes(x = model, y = performance  ) ) + 
  geom_point(size = 9, col = "#7cd8f7")+
  geom_vline(xintercept = 1:2, col = "darkgray") + 
  geom_text( aes(label =(performance), size = 5), show.legend = FALSE , col = "black" ) +
  coord_flip()+
  labs(title = "Train - Test Difference per Model", x = "", y = "" )+
  theme_classic()
  
#Create a dataframe containing the Balanced - Unbalanced differences 
temp <- data.frame( model = c("Tree","Forest"), performance = c(tree_unb_perf,for_unb_perf)  )

#Plot the Results
temp %>%ggplot( aes(x = model, y = performance  ) ) + 
  geom_point(size = 9, col = "#d591ff")+
  geom_vline(xintercept = 1:2, col = "darkgray") + 
  geom_text( aes(label =(performance), size = 5), show.legend = FALSE , col = "black" ) +
  coord_flip()+
  labs(title = "Balanced - Unbalanced Dataset Difference per Model", x = "", y = "" )+
  theme_classic()
  

