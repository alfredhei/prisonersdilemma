#fangenes dilemma strategi test
library(hash)
library(ggplot2)
library(dplyr)
library(ggcharts)

# strategies <- c("AIIC", "AIID", "RAND", "TFT", "Grim", 
#                 "Pavlov", "TFTT", "TTFT", "Gradual", 
#                 "SM", "HM", "NP", "RP", "SGRIM", "Prober",
#                 "GTFT", "STFT", "HTFT", "CTFT", "RTFT", "ATFT")
# ikke implementert: Gradual og RP
strategies <- c("AIIC", "AIID", "RAND", "TFT", "Grim",
                "Pavlov", "TTFT", "SM", "HM", "NP", "STFT", "Prober", "TFTT", "OPT", "PHL", "TFT2", "Grim2", "AI")
#strategies <- c("AI", "OPT")
# strategies <- c("AIIC", "AIID", "RAND", "TFT", "Grim", 
#                 "Pavlov", "TFTT", "TTFT", "SM", "HM", "NP", "SGRIM", "Prober", "STFT")
think <- function(strategy, logg_1, logg_2) {
  if(length(logg_1) == 0){
    logg_1 <- c()
    logg_2 <- c()
  }
  if(strategy == "AIIC"){
    return("cooporate")
  }
  if(strategy == "AIID"){
    return("defect")
  }
  if(strategy == "RAND"){
    return(sample(c("cooporate", "defect"), 1))
  }
  if(strategy == "TFT"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else{
      return(logg_2[length(logg_2)])
    }
  }
  if(strategy == "TFT2"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if(length(logg_1) == 9){
      return("defect")
    }else{
      return(logg_2[length(logg_2)])
    }
  }
  if(strategy == "PHL"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if (length(logg_1) == 1){
      return("cooporate")
    }else if (length(logg_1) > 8){
      return("defect")
    } else if ('defect' %in% logg_2){
      return("defect")
    }else{
      return("cooporate")
    }
  }
  
  if(strategy == "Grim"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if ('defect' %in% logg_2){
      return("defect")
    }else{
      return("cooporate")
    }
  }
  if(strategy == "Grim2"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if (length(which('defect' == logg_2)) >= 2){
      return("defect")
    }else{
      return("cooporate")
    }
  }
  if(strategy == "Pavlov"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if (beregn_score(logg_1[length(logg_1)], logg_2[length(logg_2)])[1] > 0){
      return(logg_1[length(logg_1)])
    }else{
      if (logg_1[length(logg_1)] == "cooporate"){
        return("defect")
      } else{
        return("cooporate")
      }
    }
  }
  
  if(strategy == "TFTT"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if(length(logg_2) >= 2){
      if (logg_2[length(logg_2)] == logg_2[length(logg_2) - 1] & logg_2[length(logg_2)] == "defect"){
        return("defect")
      } else {
        return("cooporate")
      }
    }else {
      return("cooporate")
    }
  }
  
  if(strategy == "TTFT"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if (length(which(logg_2=="defect")) >= length(which(logg_1=="defect"))){
      return("defect")
    }else{
      return(logg_2[length(logg_2)])
    }
  }
  if(strategy == "Gradual"){
    #litt vell sofistikert for dette
  }
  if(strategy == "SM"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if (length(which(logg_2=="defect")) > length(which(logg_2=="cooporate"))){
      return("defect")
    }else{
      return("cooporate")
    }
  }
  if(strategy == "OPT"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if (length(which(logg_2=="defect")) > length(which(logg_2=="cooporate"))){
      return("defect")
    }else if(length(logg_1) == 9){
      return("defect")
    }else{
      return("cooporate")
    }
  }
  if(strategy == "HM"){
    if (length(logg_1) == 0){
      return("defect")
    }else if (length(which(logg_2=="defect")) > length(which(logg_2=="cooporate"))){
      return("defect")
    }else{
      return("cooporate")
    }
  }
  if(strategy == "NP"){
    if (sample(c(1,2), 1, prob = c(0.9, 0.1)) == 1){
      if (length(logg_1) == 0){
        return("cooporate")
      }else{
        return(logg_2[length(logg_2)])
      }
    }else{
      return("defect")
    }
  }
  if(strategy == "SGRIM"){
    if (!exists("act_revenge_SGRIM")){
      if (length(logg_1) == 0){
        return("cooporate")
      }else if (logg_2[length(logg_2)] == "defect"){
        act_revenge_SGRIM <<- length(logg_2)
        return("defect")
      }else{
        return("cooporate")
      }
    }else{
      if(abs(length(logg_2) - act_revenge_SGRIM) >= 5){
        act_revenge_SGRIM <- 0
        if (logg_2[length(logg_2)] == "defect"){
          act_revenge_SGRIM <<- length(logg_2)
          return("defect")
        }else{
          return("cooporate")
        }
      }else{
        revenge <- c("defect", "defect", "defect", "defect", "cooporate", "cooporate")
        return(revenge[abs(length(revenge) - act_revenge_SGRIM)])
      }
    }
  }
  if(strategy == "Prober"){
    startvec_Prober <- c("defect", "cooporate", "cooporate")
    if (length(logg_2) <= length(startvec_Prober)){
      if (length(logg_2) == 0){
        return(startvec_Prober[1])
      }else{
        return(startvec_Prober[length(logg_2)])
      }
    }else if (logg_2[length(logg_2)] == logg_2[length(logg_2) - 1] & logg_2[length(logg_2)] == "cooporate"){
      return("defect")
    } else{
      if (length(logg_1) == 0){
        return("cooporate")
      }else{
        return(logg_2[length(logg_2)])
      }
    }
  }
  if(strategy == "STFT"){
    if (length(logg_1) == 0){
      return("defect")
    }else{
      return(logg_2[length(logg_2)])
    }
  }
  if(strategy == "NN"){
    logg_12 <- logg_to_nn(logg_1)
    logg_22 <- logg_to_nn(logg_2)
    if(length(logg_12) < 10){
      for (i in 1:(length(logg_1) - 10)) {
        logg_12 <- c(logg_12, 0)
        logg_22 <- c(logg_22, 0)
      }
    }
    
    testset <- data.frame()
    logg_12[length(logg_1)] <- 1
    testset <- rbind(testset, c(1, logg_12, logg_22))
    logg_12[length(logg_1)] <- -1
    testset <- rbind(testset, c(1, logg_12, logg_22))
    
    nn.results <- neuralnet::compute(nn, testset)
    if(which(max(nn.results$net.result) %in% nn.results$net.result) == 1){
      return()
    }
    
    if (length(logg_1) == 0){
      return("defect")
    }else{
      return(logg_2[length(logg_2)])
    }
  }
  if (strategy == "AI"){
    if(length(logg_2) > 2){
        
      steps <- c(2:length(logg_2))
      training <- logg[ , steps]   
      colnames(training)[length(steps)] <- "y"
      maxmindf <- as.data.frame(lapply(training, normalize))
      
      trainset <- maxmindf[1:20000, ]
      testset <- maxmindf[20001:23000, ]
      
      logitMod <- glm(y ~ ., data=trainset, family=binomial(link="logit"))
      predicted <- predict(logitMod, testset, type="response")
      optCutOff <- optimalCutoff(testset$y, predicted)[1] 
      confusionMatrix(testset$y, predicted, threshold = optCutOff)
      
      logg_22 <- logg_to_nn(logg_2)
      logg_22_df <- data.frame()
      logg_22_df <- rbind(normalize(logg_22))
      maxmindf2 <- as.data.frame(logg_22_df)
      colnames(maxmindf2) <- colnames(training)[1:length(logg_2)]
      pred <- predict(logitMod, maxmindf2, type="response")
      # print(pred[[1]])
      # print(optCutOff)
      # print(pred[[1]] >= optCutOff)
      if (pred[[1]] < optCutOff){
        #tror motspiller skal samarbeide
        return("cooporate")
      }else{
        #tror motspiller skal svike
        return("defect")
      }
    }else{
      return("cooporate")
    }
  }
}
normalize <- function(x) {
  # print(min(x))
  # print(max(x))
  if(max(x) == min(x)){
    return(x)
  }else{
    return ((x - min(x)) / (max(x) - min(x)))
  }
}
beregn_score <- function(valg_spiller1, valg_spiller2){
  if(is.vector(valg_spiller1) && length(valg_spiller1) > 1){
    print(players)
    print(valg_spiller1)
  }
  if (valg_spiller1 == valg_spiller2){
      if (valg_spiller1 == "cooporate"){
        #begge samarbeidercooporate
        return(c(3, 3))
      }else{
        #begge sviker Defects
        return(c(1, 1))
      }
  } else{
    if (valg_spiller1 == "cooporate"){
      #spiller 1 samarbeider, 2 sviker
      return(c(0, 5))
    }else{
      #spiller 1 sviker, 2 samarbeider
      return(c(5, 0))
    }
  }
}

logg_to_nn <- function(org_logg){
  logg_nn_spiller1 <- c()
  for (i in 1:length(org_logg)){
    if(org_logg[i] == "cooporate"){
      logg_nn_spiller1 <- c(logg_nn_spiller1, 1)
    } else {
      logg_nn_spiller1 <- c(logg_nn_spiller1, -1)
    }
  }
  return(logg_nn_spiller1)
}

game <- function(players){
  spill_max = 10
  
  logg_spiller1 <- c()
  logg_spiller2 <- c() 
  
  score_spiller1 <- 0
  score_spiller2 <- 0
  
  for (i in 1:spill_max){
    valg_spiller1 <- think(players[1], logg_spiller1, logg_spiller2) #spiller 1
    valg_spiller2 <- think(players[2], logg_spiller2, logg_spiller1) #spiller 2
    
    logg_spiller1 <- c(logg_spiller1, valg_spiller1)
    logg_spiller2 <- c(logg_spiller2, valg_spiller2)
    
    score <- beregn_score(valg_spiller1, valg_spiller2)
    score_spiller1 <- score_spiller1 + score[1]
    score_spiller2 <- score_spiller2 + score[2]
    # print(players)
    # print(score_spiller1)
    # print(score_spiller2)
    if(i == spill_max){
      #logg_nn_spiller1 <- logg_to_nn(logg_spiller1)
      #logg_nn_spiller2 <- logg_to_nn(logg_spiller2)
    }
  }
  
  #logg to nn logg
  
  if (score_spiller1 == score_spiller2){
    vinner <- c("uavgjort", score_spiller1, score_spiller2)
  } else if (score_spiller1 > score_spiller2){
    vinner <- c(players[1], score_spiller1, score_spiller2)
    #logg <<- rbind(logg, c(1, logg_nn_spiller1, logg_nn_spiller2)) 
  } else{
    vinner <- c(players[2], score_spiller1, score_spiller2)
    #logg <<- rbind(logg, c(0, logg_nn_spiller1, logg_nn_spiller2)) 
  }
  return(vinner)
}

select_strategies <- function(){
  return(c(sample(strategies, 2, replace = FALSE)))
}

main_f <- function(){
  tot <<- 5000 #antall spill med 10 runder
  
  score <<- rep(c(0), each=length(strategies)+1)
  #logg <- data.frame()
  rm(act_revenge_SGRIM)
  for (i in 1:tot){
    players <<- select_strategies()
    vinner <<- game(players)
    if (vinner[1] == "uavgjort"){
      score[match(players[1],strategies)] <<- score[match(players[1],strategies)] + as.integer(vinner[2])
      score[match(players[2],strategies)] <<- score[match(players[2],strategies)] + as.integer(vinner[3])
    }else{
      score[match(players[1],strategies)] <<- score[match(players[1],strategies)] + as.integer(vinner[2])
      score[match(players[2],strategies)] <<- score[match(players[2],strategies)] + as.integer(vinner[3])
    }
  }
  dat <- data.frame(Strategies = c(strategies, "uavgjort"), Scores = score)
  return(dat)
}
dat <- main_f()

chart <- dat[1:18,] %>%
  bar_chart(Strategies, Scores)

chart +
  geom_text(aes(label = Scores, hjust = 1.2), color = "white") + 
  ylab("Wins") 

c <- dat[1:18,]$Scores/((2*10^4)/7)
chart2 <- dat[1:18,] %>%
  bar_chart(Strategies, c)

chart2 +
  geom_text(aes(label = c, hjust = 1.2), color = "white") + 
  ylab("Wins") 

