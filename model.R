# Load the necessary packages
my_packages <-c("ordinal","tidyverse","caTools","MASS","brant","Metrics","gtable","skimr")
lapply(my_packages, require, character.only = TRUE)

# Import data 
ordiDf <- read.csv("dataset/Ordinal.csv",stringsAsFactors = TRUE)
# prepare a randomize copy of the data
ordiDf <- ordiDf[sample(nrow(ordiDf)), ]
ordiDf$f2 <- factor(ordiDf$f2,levels = c("Not Applicable",
                                         "Strongly Agree",
                                         "Agree","Neutral",
                                         "Disagree",
                                         "Strongly Disagree"), ordered = TRUE)

ordiDf$f3 <- factor(ordiDf$f3,levels = c("Not Applicable",
                                         "Strongly Agree",
                                         "Agree","Neutral",
                                         "Disagree",
                                         "Strongly Disagree"), ordered = TRUE)

ordiDf$f1 <- factor(ordiDf$f1,levels = c("Not Applicable",
                                         "Strongly Agree",
                                         "Agree","Neutral",
                                         "Disagree",
                                         "Strongly Disagree"), ordered = TRUE)

# Draw 100 samples each of male and female
male_samples <- ordiDf %>%
  filter(gender == "Male") %>%
  sample_n(90, replace = FALSE)

female_samples <- ordiDf %>%
  filter(gender == "Female") %>%
  sample_n(90, replace = FALSE)

# Combine the male and female samples
combined_samples <- rbind(male_samples, female_samples)

# prepare a randomize copy of the data
combined_samples <- combined_samples[sample(nrow(combined_samples)), ]

# Print the combined samples
View(combined_samples)
new_sample <- combined_samples[,-c(1,2)]

new_sample <- dplyr::select(new_sample,gender,C.G.P,stress,f1,f2,f3)

View(new_sample)

#Splitting the data
split = sample.split(new_sample$C.G.P, SplitRatio = 0.8)
train_set = subset(new_sample, split == TRUE)
test_set = subset(new_sample, split == FALSE)

#Write new files for the train and test sets
write.csv(train_set, "train.csv", row.names = FALSE)
write.csv(test_set, "test.csv", row.names = FALSE)

#Ordinal Logistic Regression
#model
model <- clm(C.G.P ~  ., data = train_set, link = "logit")


# Performance Metrics

# Predicting the test values
y_pred_m <- predict(model, newdata = test_set, type = "class")
ctab <- table(y_pred_m$fit, test_set$C.G.P)
# correctly classified predictions
(CCR <- sum(diag(ctab)) / sum(ctab))
# mean prediction error (proportion of mismatched predictions based on the C.G.P in the train set)
 mpe <-mean(as.character(test_set$C.G.P) != as.character(y_pred_m$fit))

#Saving the model
saveRDS(model, file = "./model.rda")
# Save the CCR
saveRDS(CCR, file = "./CCR.rda")
# Save the mpe
saveRDS(mpe, file = "./mpe.rda")


# f1, f2 and f3 predicts 50% correctly
# stress predict 41% correctly
# f1, f2 ,f3 and stress predicts 41% correctly
# f1, f2 ,f3, stress and gender predicts 41% correctly
# f1, f2 ,f3 and gender predicts 47% correctly
# f2 predict 48% correctly
# f1 predicts 41% correctly 
# f3 predicts 47% correctly 


# The best model in this case is model 2


#anova(model1,model2)
#data summary
#my_data |> skim()
#skim(socio)
#skim(my_data)
#p2+ptheme +coord_flip()


model
