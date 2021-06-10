#packages
library(tidyverse)
library(plotly)
library(class)
library(readxl)

#MLB labels 
# FF  = four-seam fastball
# SL = slider
# CH = change up
# FT = two-seam fastball
# SI = sinker
# CU = curveball
# FC = cutter
# KC = knuckle-curve
# FS = split-finger fastball

#UCSD labels
#FB - fourseam fastball
#2S - twoseam fastball
#CV - curveball
#SL - slider
#CH - changeup
#KN - knuckleball 
#CT - cutter

#Model 

# find train, don't run
train1 <- get_payload(start = "2017-04-03", end = "2017-04-20")
train <- train1$pitch

saveRDS(train, "2017_04_03_to_2017_04_20_pitch.rds")

#get train
train <- readRDS("2017_04_03_to_2017_04_20_pitch.rds")

#find test, don't run
test_set <- get_payload(start = "2016-04-27", end = "2016-04-30")
test <- test_set$pitch

saveRDS(test,"2016_04_27_to_2016_04_30_pitch.rds")

# get test
test <- readRDS("2016_04_27_to_2016_04_30_pitch.rds")

#cleaning, only columns shared with ucsd data

train_cleaned <- train %>%
  filter(!pitch_type %in% c("KN", "IN", "PO", "EP", "FO"),
         !is.na(pitch_type)) %>%
  select(start_speed, pfx_x, pfx_z, pitch_type)

test_cleaned <- test %>%
  filter(!pitch_type %in% c("KN", "IN", "PO", "EP", "FO", "SC"),
         !is.na(pitch_type)) %>%
  select(start_speed, pfx_x, pfx_z, pitch_type)

train_cleaned$pfx_x <- -train_cleaned$pfx_x
test_cleaned$pfx_x <- -test_cleaned$pfx_x


colnames(train_cleaned) <- c("start_speed", "h.break" , "v.break", "pitch_type")
colnames(test_cleaned) <- c("start_speed", "h.break" , "v.break", "pitch_type")

#normalize function 
scale_by <- function(x, mean, sd){
  (x - mean) / sd
}

#normalize
scale_values <- train_cleaned %>% 
  select(-pitch_type) %>% 
  lapply(function(x){
    list(mean = mean(x), sd = sd(x))
  })


## prepare the training data
train_pitches <- train_cleaned$pitch_type
train_cleaned_scaled <- colnames(train_cleaned) %>% 
  setdiff("pitch_type") %>%
  lapply(function(column_name){
    scale_by(
      x = train_cleaned[[column_name]],
      mean = scale_values[[column_name]][["mean"]],
      sd = scale_values[[column_name]][["sd"]]
    )
  }) %>% 
  do.call('data.frame',.) %>% 
  setNames(colnames(train_cleaned) %>%  setdiff("pitch_type"))

## prepare the testing data
test_pitches <- test_cleaned$pitch_type
test_cleaned_scaled <- colnames(test_cleaned) %>% 
  setdiff("pitch_type") %>%
  lapply(function(column_name){
    scale_by(
      test_cleaned[[column_name]],
      scale_values[[column_name]][["mean"]],
      scale_values[[column_name]][["sd"]]
    )
  }) %>% 
  do.call('data.frame',.) %>% 
  setNames(colnames(test_cleaned) %>% setdiff("pitch_type"))

## run a KNN
fit_knn <- knn(
  train = train_cleaned_scaled,
  test = test_cleaned_scaled,
  cl = train_pitches,
  k = 7, ## based on last weeks observations
  prob = TRUE
)





## create a table of predicted and observed outcomes for the test set
pred <- data.frame(Pred = fit_knn, Observed = test_pitches)
table("Predicted" = pred$Pred, "Observed" = pred$Observed)

## calculate the misclassification rate
##  Overall
miss_classification_rate <- 1 - sum(pred$Pred == pred$Observed) / nrow(test_cleaned_scaled)
miss_classification_rate

## within pitch type
table("Predicted" = pred$Pred, "Observed" = pred$Observed) %>%
  data.frame %>% 
  group_by(Observed) %>%
  summarise( mcr = 1 - Freq[Observed == Predicted] / sum(Freq))





##UCSD data

excel_sheets("baseball_project_pitching_data_copy.xlsx")
sheets <- readxl::excel_sheets("baseball_project_pitching_data_copy.xlsx")
sheets <- as.list(sheets)
##players 1-5
p1 <- read_excel("baseball_project_pitching_data_copy.xlsx", sheet = sheets[[2]])
p1 <-data.frame(p1)


  
p2 <- read_excel("baseball_project_pitching_data_copy.xlsx", sheet = sheets[[3]])
p2 <-data.frame(p2)
  
p3 <- read_excel("baseball_project_pitching_data_copy.xlsx", sheet = sheets[[4]])
p3 <-data.frame(p3)

p4 <- read_excel("baseball_project_pitching_data_copy.xlsx", sheet = sheets[[5]])
p4 <-data.frame(p4)

p5 <- read_excel("baseball_project_pitching_data_copy.xlsx", sheet = sheets[[6]])
p5 <-data.frame(p5)

##clean
p1_cleaned <- p1 %>% filter(!is.na(VELOCITY), !is.na(H..BREAK), !is.na(V..BREAK)) %>% select(VELOCITY, H..BREAK, V..BREAK,PITCH.TYPE)
p2_cleaned <- p2 %>% filter(!is.na(VELOCITY), !is.na(H..BREAK), !is.na(V..BREAK)) %>% select(VELOCITY, H..BREAK, V..BREAK,PITCH.TYPE)
p3_cleaned <- p3 %>% filter(!is.na(VELOCITY), !is.na(H..BREAK), !is.na(V..BREAK)) %>% select(VELOCITY, H..BREAK, V..BREAK,PITCH.TYPE)
p4_cleaned <- p4 %>% filter(!is.na(VELOCITY), !is.na(H..BREAK), !is.na(V..BREAK)) %>% select(VELOCITY, H..BREAK, V..BREAK,PITCH.TYPE)
p5_cleaned <- p5 %>% filter(!is.na(VELOCITY), !is.na(H..BREAK), !is.na(V..BREAK)) %>% select(VELOCITY, H..BREAK, V..BREAK,PITCH.TYPE)

colnames(p1_cleaned) = c("start_speed", "h.break" , "v.break", "pitch_type")
colnames(p2_cleaned) = c("start_speed", "h.break" , "v.break", "pitch_type")
colnames(p3_cleaned) = c("start_speed", "h.break" , "v.break", "pitch_type")
colnames(p4_cleaned) = c("start_speed", "h.break" , "v.break", "pitch_type")
colnames(p5_cleaned) = c("start_speed", "h.break" , "v.break", "pitch_type")

#change labels to mlb standard

#p1
  player <- p1_cleaned
  pitches <- c(player$pitch_type)
  for (i in 1:length(player$pitch_type)) { 
    if (player$pitch_type[i] == "CV ") { 
      pitches[i] <- "CU"
    } else if (player$pitch_type[i] == "FB ") { 
      pitches[i] <- "FF"
    } else if (player$pitch_type[i] == "CH ") {
      pitches[i] <- "CH"
    }else if (player$pitch_type[i] == "CT ") {
      pitches[i] <- "FC"
    } else if (player$pitch_type[i] == "SL "){
      pitches[i] <- "SL"
    } else if (player$pitch_type[i] == "2FB ") {
      pitches[i] <- "FT"
    }
  } 
  p1_cleaned$pitch_type <- pitches

#p2
  player <- p2_cleaned
  pitches <- c(player$pitch_type)
  for (i in 1:length(player$pitch_type)) { 
    if (player$pitch_type[i] == "CV ") { 
      pitches[i] <- "CU"
    } else if (player$pitch_type[i] == "FB ") { 
      pitches[i] <- "FF"
    } else if (player$pitch_type[i] == "CH ") {
      pitches[i] <- "CH"
    }else if (player$pitch_type[i] == "CT ") {
      pitches[i] <- "FC"
    } else if (player$pitch_type[i] == "SL "){
      pitches[i] <- "SL"
    } else if (player$pitch_type[i] == "2FB ") {
      pitches[i] <- "FT"
    }
  } 
  p2_cleaned$pitch_type <- pitches

#p3
  player <- p3_cleaned
  pitches <- c(player$pitch_type)
  for (i in 1:length(player$pitch_type)) { 
    if (player$pitch_type[i] == "CV ") { 
      pitches[i] <- "CU"
    } else if (player$pitch_type[i] == "FB ") { 
      pitches[i] <- "FF"
    } else if (player$pitch_type[i] == "CH ") {
      pitches[i] <- "CH"
    }else if (player$pitch_type[i] == "CT ") {
      pitches[i] <- "FC"
    } else if (player$pitch_type[i] == "SL "){
      pitches[i] <- "SL"
    } else if (player$pitch_type[i] == "2FB ") {
      pitches[i] <- "FT"
    }
  } 
  p3_cleaned$pitch_type <- pitches
 
 #need to switch sign of pfx_x in training back 
#p4
  player <- p4_cleaned
  pitches <- c(player$pitch_type)
  for (i in 1:length(player$pitch_type)) { 
    if (player$pitch_type[i] == "CV ") { 
      pitches[i] <- "CU"
    } else if (player$pitch_type[i] == "FB ") { 
      pitches[i] <- "FF"
    } else if (player$pitch_type[i] == "CH ") {
      pitches[i] <- "CH"
    }else if (player$pitch_type[i] == "CT ") {
      pitches[i] <- "FC"
    } else if (player$pitch_type[i] == "SL "){
      pitches[i] <- "SL"
    } else if (player$pitch_type[i] == "2FB ") {
      pitches[i] <- "FT"
    }
  } 
  p4_cleaned$pitch_type <- pitches
  
#p5
  player <- p5_cleaned
  pitches <- c(player$pitch_type)
  for (i in 1:length(player$pitch_type)) { 
    if (player$pitch_type[i] == "CV ") { 
      pitches[i] <- "CU"
    } else if (player$pitch_type[i] == "FB ") { 
      pitches[i] <- "FF"
    } else if (player$pitch_type[i] == "CH ") {
      pitches[i] <- "CH"
    }else if (player$pitch_type[i] == "CT ") {
      pitches[i] <- "FC"
    } else if (player$pitch_type[i] == "SL "){
      pitches[i] <- "SL"
    } else if (player$pitch_type[i] == "2FB ") {
      pitches[i] <- "FT"
    }
  } 
  p5_cleaned$pitch_type <- pitches
  
  

#grab pitches and apply scale


#p1
p1_pitches <- p1_cleaned$pitch_type
p1_cleaned_scaled <-colnames(p1_cleaned) %>% 
       setdiff("pitch_type") %>%
       lapply(function(column_name){
             scale_by(
                   p1_cleaned[[column_name]],
                   scale_values[[column_name]][["mean"]],
                   scale_values[[column_name]][["sd"]]
               )
         }) %>% 
       do.call('data.frame',.) %>% 
       setNames(colnames(p1_cleaned) %>% setdiff("pitch_type"))
#p2
p2_pitches <- p2_cleaned$pitch_type
p2_cleaned_scaled <-colnames(p2_cleaned) %>% 
  setdiff("pitch_type") %>%
  lapply(function(column_name){
    scale_by(
      p2_cleaned[[column_name]],
      scale_values[[column_name]][["mean"]],
      scale_values[[column_name]][["sd"]]
    )
  }) %>% 
  do.call('data.frame',.) %>% 
  setNames(colnames(p2_cleaned) %>% setdiff("pitch_type"))

#p3
p3_pitches <- p3_cleaned$pitch_type
p3_cleaned_scaled <-colnames(p3_cleaned) %>% 
  setdiff("pitch_type") %>%
  lapply(function(column_name){
    scale_by(
      p3_cleaned[[column_name]],
      scale_values[[column_name]][["mean"]],
      scale_values[[column_name]][["sd"]]
    )
  }) %>% 
  do.call('data.frame',.) %>% 
  setNames(colnames(p3_cleaned) %>% setdiff("pitch_type"))

#p4
p4_pitches <- p4_cleaned$pitch_type
p4_cleaned_scaled <-colnames(p4_cleaned) %>% 
  setdiff("pitch_type") %>%
  lapply(function(column_name){
    scale_by(
      p4_cleaned[[column_name]],
      scale_values[[column_name]][["mean"]],
      scale_values[[column_name]][["sd"]]
    )
  }) %>% 
  do.call('data.frame',.) %>% 
  setNames(colnames(p4_cleaned) %>% setdiff("pitch_type"))

#p5
p5_pitches <- p5_cleaned$pitch_type
p5_cleaned_scaled <-colnames(p5_cleaned) %>% 
  setdiff("pitch_type") %>%
  lapply(function(column_name){
    scale_by(
      p5_cleaned[[column_name]],
      scale_values[[column_name]][["mean"]],
      scale_values[[column_name]][["sd"]]
    )
  }) %>% 
  do.call('data.frame',.) %>% 
  setNames(colnames(p5_cleaned) %>% setdiff("pitch_type"))

#make players test
fit_knn_1 <- knn(
  train = train_cleaned_scaled,
  test = p1_cleaned_scaled,
  cl = train_pitches,
  k = 7, ## based on optimization
  prob = TRUE
)
fit_knn_2 <- knn(
  train = train_cleaned_scaled,
  test = p2_cleaned_scaled,
  cl = train_pitches,
  k = 7, 
  prob = TRUE
)

fit_knn_3<- knn(
  train = train_cleaned_scaled,
  test = p3_cleaned_scaled,
  cl = train_pitches,
  k = 7, 
  prob = TRUE
)


fit_knn_4<- knn(
  train = train_cleaned_scaled,
  test = p4_cleaned_scaled,
  cl = train_pitches,
  k = 7, 
  prob = TRUE
)

fit_knn_5<- knn(
  train = train_cleaned_scaled,
  test = p5_cleaned_scaled,
  cl = train_pitches,
  k = 7, 
  prob = TRUE
)


#predictions 
pred_1 <- data.frame(Pred = fit_knn_1, Observed = p1_pitches)
pred_2 <- data.frame(Pred = fit_knn_2, Observed = p2_pitches)
pred_3 <- data.frame(Pred = fit_knn_3, Observed = p3_pitches)
pred_4 <- data.frame(Pred = fit_knn_4, Observed = p4_pitches)
pred_5 <- data.frame(Pred = fit_knn_5, Observed = p5_pitches)
  
#table for player 

p1_results <- table("Predicted" = pred_1$Pred, "Observed" = pred_1$Observed)


p2_results <- table("Predicted" = pred_2$Pred, "Observed" = pred_2$Observed)


p3_results <- table("Predicted" = pred_3$Pred, "Observed" = pred_3$Observed)


p4_results <- table("Predicted" = pred_4$Pred, "Observed" = pred_4$Observed)


p5_results <- table("Predicted" = pred_5$Pred, "Observed" = pred_5$Observed)



#Miss_classification
miss_classification_rate_1 <- 1 - sum(pred_1$Pred == pred_1$Observed) / nrow(p1_cleaned_scaled)
miss_classification_rate_1

miss_classification_rate_2 <- 1 - sum(pred_2$Pred == pred_2$Observed) / nrow(p2_cleaned_scaled)
miss_classification_rate_2

miss_classification_rate_3 <- 1 - sum(pred_3$Pred == pred_3$Observed) / nrow(p3_cleaned_scaled)
miss_classification_rate_3

miss_classification_rate_4 <- 1 - sum(pred_4$Pred == pred_4$Observed) / nrow(p4_cleaned_scaled)
miss_classification_rate_4

miss_classification_rate_5 <- 1 - sum(pred_5$Pred == pred_5$Observed) / nrow(p5_cleaned_scaled)
miss_classification_rate_5


# p4 on of the higher misclassification rates

#deviation for player in each feature

#mlb median standards
pitch_type_summary <- train_cleaned %>%
       group_by(pitch_type) %>%
       summarize(across(everything(),
                                               .fns = median))


variation_p4 <- c()
for (i in 1:length(pred_4$Pred)){
  if (pred_4$Pred[i] != pred_4$Observed[i]){
    variation_p4 <- c(variation_p4, i)
  }
}

#misclassified pitches
p4_ch <- c()
p4_cu <- c()
p4_ff <- c()
p4_sl <- c()
for (i in variation_p4) {
  if (pred_4$Pred[i] == "CH"){
     if (pred_4$Observed[i] == "CH") {
      p4_ch <- c(p4_ch, i)
    } else if (pred_4$Observed[i] == "CU") {
      p4_cu <- c(p4_cu, i)
    } else if (pred_4$Observed[i] == "FF") {
      p4_ff <- c(p4_ff, i)
    } else if (pred_4$Observed[i] == "SL") {
      p4_sl <- c(p4_sl, i)
    } 
  } else {
    0
  }

}
#speed for misclassified pitches
p4_cu_y_speed <- c()
for (i in p4_cu){
  p4_cu_y <- c(p4_cu_y, p4_cleaned$start_speed[i])
}
p4_ff_y_speed <- c()
for (i in p4_ff){
  p4_ff_y <- c(p4_ff_y, p4_cleaned$start_speed[i])
}
p4_sl_y_speed <- c()
for (i in p4_sl){
  p4_sl_y <- c(p4_sl_y, p4_cleaned$start_speed[i])
}


#speed plot
plot(c(p4_cu, p4_ff, p4_sl), c(p4_cu_y_speed, p4_ff_y_speed, p4_sl_speed), main="Player 4's Speed Variation to MLB Standards",xlab = "Pitches",
     ylab="Speed" )

points(p4_ff, p4_ff_y_speed, col = "green", pch = "*") 
points(p4_sl, p4_sl_y_speed, col = "blue", pch="*")
points(p4_cu, p4_cu_y_speed, col = "red", pch="*") 
abline(h= pitch_type_summary$start_speed[1])
text(10, 85, "MLB Starting Speed for Changeup")

#h.break for misclassified pitches
p4_cu_y_hbreak <- c()
for (i in p4_cu){
  p4_cu_y_hbreak <- c(p4_cu_y_hbreak, p4_cleaned$h.break[i])
}
p4_ff_y_hbreak <- c()
for (i in p4_ff){
  p4_ff_y_hbreak <- c(p4_ff_y_hbreak, p4_cleaned$h.break[i])
}
p4_sl_y_hbreak <- c()
for (i in p4_sl){
  p4_sl_y_hbreak <- c(p4_sl_y_hbreak, p4_cleaned$h.break[i])
}

plot(c(p4_cu, p4_ff, p4_sl), c(p4_cu_y_hbreak, p4_ff_y_hbreak, p4_sl_y_hbreak), main="Player 4's H. Break Variation to MLB Standards",xlab = "Pitches",
     ylab="H. Break" )

points(p4_ff, p4_ff_y_hbreak, col = "green", pch = "*") 
points(p4_sl, p4_sl_y_hbreak, col = "blue", pch="*")
points(p4_cu, p4_cu_y_hbreak, col = "red", pch="*") 
abline(h= pitch_type_summary$h.break[1])
text(35, -6, "MLB HBreak for Changeup")


#v.break for misclassified pitches
p4_cu_y_vbreak <- c()
for (i in p4_cu){
  p4_cu_y_vbreak <- c(p4_cu_y_vbreak, p4_cleaned$v.break[i])
}
p4_ff_y_vbreak <- c()
for (i in p4_ff){
  p4_ff_y_vbreak <- c(p4_ff_y_vbreak, p4_cleaned$v.break[i])
}
p4_sl_y_vbreak <- c()
for (i in p4_sl){
  p4_sl_y_vbreak <- c(p4_sl_y_vbreak, p4_cleaned$v.break[i])
}

plot(c(p4_cu, p4_ff, p4_sl), c(p4_cu_y_vbreak, p4_ff_y_vbreak, p4_sl_y_vbreak), main="Player 4's V. Break Variation to MLB Standards",xlab = "Pitches",
     ylab="V. Break", ylim = c( 5, 22) )

points(p4_ff, p4_ff_y_vbreak, col = "green", pch = "*") 
points(p4_sl, p4_sl_y_vbreak, col = "blue", pch="*")
points(p4_cu, p4_cu_y_vbreak, col = "red", pch="*") 
abline(h= pitch_type_summary$v.break[1])
text(35, 6.5, "MLB VBreak for Changeup")
