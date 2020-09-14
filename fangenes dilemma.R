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
                "Pavlov", "TFTT", "TTFT", "SM", "HM", "NP", "SGRIM", "Prober", "STFT")
think <- function(strategy, logg_1, logg_2) {
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
  if(strategy == "Grim"){
    if (length(logg_1) == 0){
      return("cooporate")
    }else if ('defect' %in% logg_2){
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
    }else if (logg_2[length(logg_2)] == logg_2[length(logg_2) - 1] & logg_2[length(logg_2)] == "defect"){
      return("defect")
    } else {
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
      if((length(logg_2) - act_revenge_SGRIM) >= 5){
        rm(act_revenge_SGRIM)
        return(think("SGRIM", logg_1, logg_2))
      }else{
        revenge <- c("defect", "defect", "defect", "defect", "cooporate", "cooporate")
        return(revenge[(length(logg_2) - act_revenge_SGRIM)])
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
  
}

beregn_score <- function(valg_spiller1, valg_spiller2){
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
game <- function(players){
  spill_max = 10
  
  logg_spiller1 <- c()
  logg_spiller2 <- c() 
  
  score_spiller1 <- 0
  score_spiller2 <- 0
  
  for (i in 1:spill_max){
    valg_spiller1 <- think(players[1], logg_spiller1, logg_spiller2) #spiller 1
    valg_spiller2 <- think(players[2], logg_spiller2, logg_spiller1) #spiller 2
    
    append(logg_spiller1, valg_spiller1)
    append(logg_spiller2, valg_spiller2)
    
    score <- beregn_score(valg_spiller1, valg_spiller2)
    score_spiller1 <- score_spiller1 + score[1]
    score_spiller2 <- score_spiller2 + score[2]
    # print(players)
    # print(score_spiller1)
    # print(score_spiller2)
  }
  
  if (score_spiller1 == score_spiller2){
    vinner <- "uavgjort"
  } else if (score_spiller1 > score_spiller2){
    vinner <- players[1]
  } else{
    vinner <- players[2]
  }
  return(vinner)
}

select_strategies <- function(){
  return(c(sample(strategies, 2, replace = TRUE)))
}

main_f <- function(){
  tot <- 2*10^5 #antall spill med 10 runder
  
  score <<- rep(c(0), each=length(strategies)+1)
  
  for (i in 1:tot){
    players <- select_strategies()
    vinner <<- game(players)
    if (vinner == "uavgjort"){
      score[length(score)] <<-  score[length(score)] + 1
    }else{
      score[match(vinner,strategies)] <<-score[match(vinner,strategies)] + 1
    }
  }
  dat <- data.frame(Strategies = c(strategies, "uavgjort"), Scores = score)
  return(dat)
}
dat <- main_f()

chart <- dat[1:14,] %>%
  bar_chart(Strategies, Scores)

chart +
  geom_text(aes(label = Scores, hjust = 1.2), color = "white") + 
  ylab("Wins") 

