library(dplyr)
library(tidyverse)

plays_original = read.csv("NFL Play by Play 2009-2018 (v5).csv")

plays_original = plays_original %>%
  mutate(unique_drive = paste(as.character(plays_original$game_id),"_",as.character(plays_original$drive)))

plays_original = plays_original %>%
  mutate(play_differential = score_differential_post - score_differential)

plays_original = plays_original %>%
  mutate(year = substr(plays_original$game_id,0,4))

plays_original = plays_original %>%
  mutate(unique_play = paste(as.character(plays_original$game_id),"_",as.character(plays_original$play_id)))

plays_original = plays_original %>%
  mutate(Year = as.numeric(substr(plays_original$game_date,1,4)))

qb_2014 = read.csv("QB Ratings_2014.csv")
qb_2015 = read.csv("QB Ratings_2015.csv")
qb_2016 = read.csv("QB Ratings_2016.csv")
qb_2017 = read.csv("QB Ratings_2017.csv")
qb_2018 = read.csv("QB Ratings_2018.csv")

qb_top = function(qb) {
  qb_filtered = qb %>%
    filter(Att > 100) %>%
    select(Player,QBR)
  qb_top_10 = top_n(qb_filtered,10,wt=QBR)
  output = qb_top_10 %>%
    select(Player)
  return(output)
}

qb_bottom = function(qb) {
  qb_filtered = qb %>%
    filter(Att > 100) %>%
    select(Player,QBR)
  qb_btm_10 = top_n(qb_filtered,10,wt=-QBR)
  output = qb_btm_10 %>%
    select(Player)
  return(output)
}

qb_top_2014 = qb_top(qb_2014)
qb_btm_2014 = qb_bottom(qb_2014)
qb_top_2015 = qb_top(qb_2015)
qb_btm_2015 = qb_bottom(qb_2015)
qb_top_2016 = qb_top(qb_2016)
qb_btm_2016 = qb_bottom(qb_2016)
qb_top_2017 = qb_top(qb_2017)
qb_btm_2017 = qb_bottom(qb_2017)
qb_top_2018 = qb_top(qb_2018)
qb_btm_2018 = qb_bottom(qb_2018)

format.names = function (name){
  mysplit=strsplit(name," ")
  first=mysplit[[1]][1]
  last=mysplit[[1]][2]
  out<-paste0(substr(first,1,1),".",last)
  return(out)
}

qb_top_2014 = qb_top_2014 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_top_2014 = as.matrix(qb_top_2014 %>% select(QB))
qb_btm_2014 = qb_btm_2014 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_btm_2014 = as.matrix(qb_btm_2014 %>% select(QB))
qb_top_2015 = qb_top_2015 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_top_2015 = as.matrix(qb_top_2015 %>% select(QB))
qb_btm_2015 = qb_btm_2015 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_btm_2015 = as.matrix(qb_btm_2015 %>% select(QB))
qb_top_2016 = qb_top_2016 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_top_2016 = as.matrix(qb_top_2016 %>% select(QB))
qb_btm_2016 = qb_btm_2016 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_btm_2016 = as.matrix(qb_btm_2016 %>% select(QB))
qb_top_2017 = qb_top_2017 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_top_2017 = as.matrix(qb_top_2017 %>% select(QB))
qb_btm_2017 = qb_btm_2017 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_btm_2017 = as.matrix(qb_btm_2017 %>% select(QB))
qb_top_2018 = qb_top_2018 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_top_2018 = as.matrix(qb_top_2018 %>% select(QB))
qb_btm_2018 = qb_btm_2018 %>% rowwise() %>% mutate(QB= format.names(as.character(Player)))
qb_btm_2018 = as.matrix(qb_btm_2018 %>% select(QB))

plays = plays_original %>%
  select(Year,play_id,game_id,yardline_100,half_seconds_remaining,quarter_end,down,goal_to_go,ydstogo,ydsnet,play_type,
         yards_gained,shotgun,qb_dropback, pass_location,run_location,run_gap,field_goal_result,posteam_timeouts_remaining,
         defteam_timeouts_remaining,timeout,timeout_team,score_differential,score_differential_post,no_score_prob,fg_prob,safety_prob,td_prob,
         third_down_converted,third_down_failed,fourth_down_converted,fourth_down_failed,incomplete_pass,interception,
         safety, fumble_lost,rush_attempt,pass_attempt,sack,pass_touchdown,rush_touchdown,passer_player_name,passer_player_id,receiver_player_id,
         rusher_player_id,kicker_player_id,drive)

plays = plays %>%
  mutate(unique_drive = paste(as.character(plays$game_id),"_",as.character(plays$drive)))

plays = plays %>%
  mutate(play_differential = score_differential_post - score_differential)

plays = plays %>%
  filter(play_differential != 1)

plays = plays %>%
  filter(play_differential != 2)

plays = plays %>%
  group_by(unique_drive) %>%
  filter(play_differential != "N/A") %>%
  mutate(drive_differential = -(max(play_differential)+(max(play_differential)!=min(play_differential))*min(play_differential))) %>%
  ungroup

plays$drive_differential = plays$drive_differential - (plays$drive_differential==-6)
plays$drive_differential = plays$drive_differential + (plays$drive_differential==6)

plays = plays %>%
  add_count(unique_drive,passer_player_name)

plays = plays %>%
  mutate(n_upd = ifelse(is.na(passer_player_name),0,n))

plays = plays %>%
  group_by(unique_drive) %>%
  mutate(qb = passer_player_name[n_upd == max(n_upd)][1]) %>%
  select(-n,-n_upd)

plays_top_2014 = subset(plays, plays$qb %in% qb_top_2014) %>%filter(Year == 2014)
plays_top_2015 = subset(plays, plays$qb %in% qb_top_2015) %>%filter(Year == 2015)
plays_top_2016 = subset(plays, plays$qb %in% qb_top_2016) %>%filter(Year == 2016)
plays_top_2017 = subset(plays, plays$qb %in% qb_top_2017) %>%filter(Year == 2017)
plays_top_2018 = subset(plays, plays$qb %in% qb_top_2017) %>%filter(Year == 2018)
plays_top_qbs = rbind(plays_top_2014,plays_top_2015,plays_top_2016,plays_top_2017,plays_top_2018)

plays_btm_2014 = subset(plays, plays$qb %in% qb_btm_2014) %>%filter(Year == 2014)
plays_btm_2015 = subset(plays, plays$qb %in% qb_btm_2015) %>%filter(Year == 2015)
plays_btm_2016 = subset(plays, plays$qb %in% qb_btm_2016) %>%filter(Year == 2016)
plays_btm_2017 = subset(plays, plays$qb %in% qb_btm_2017) %>%filter(Year == 2017)
plays_btm_2018 = subset(plays, plays$qb %in% qb_btm_2017) %>%filter(Year == 2018)
plays_btm_qbs = rbind(plays_btm_2014,plays_btm_2015,plays_btm_2016,plays_btm_2017,plays_btm_2018)

plays_top_qbs = plays_top_qbs %>%
  select(.,-Year,-passer_player_name,-qb)

plays_btm_qbs = plays_btm_qbs %>%
  select(.,-Year,-passer_player_name,-qb)

drive_results_top = plays_top_qbs %>%
  filter((down != 2) & half_seconds_remaining >= 240 &
           (play_type != "no_play")) %>%
  select(unique_drive,yardline_100,down,ydstogo,goal_to_go,play_type,drive_differential)

drive_results_btm = plays_btm_qbs %>%
  filter((down != 2) & half_seconds_remaining >= 240 &
           (play_type != "no_play")) %>%
  select(unique_drive,yardline_100,down,ydstogo,goal_to_go,play_type,drive_differential)

write.csv(drive_results_top, file = "drive_results_top.csv")
write.csv(drive_results_btm, file = "drive_results_btm.csv")

expected_cost = read.csv("splits_w_costs1127.csv")
events = nrow(expected_cost)

update_expected_col <-function(yardline_100,ydstogo,down,cost.col="BASE"){
  for (i in 1:events){
    if (down>=expected_cost[i,'down_lower'] & down<expected_cost[i,'down_upper'] 
        & yardline_100>=expected_cost[i,'yardline_100_lower'] & yardline_100<expected_cost[i,'yardline_100_upper'] &
        ydstogo>=expected_cost[i,"ydstogo_lower"] & ydstogo<expected_cost[i,"ydstogo_upper"]){
      result<-expected_cost[i,cost.col]
      break
    }
  }
  return(result)
}

for (mycol in colnames(expected_cost)){
  if (grepl("lower",mycol)){expected_cost[,mycol]<-replace_na(expected_cost[,mycol],-Inf)}
  else {expected_cost[,mycol]=replace_na(expected_cost[,mycol],Inf)}
}

scoring_cost <-function(fun="BASE",drive_diff){
  if (fun=="BASE"){
    cost_out=drive_diff}
  else if (fun=="DOWN4LATE"){
    cost_out=-1*(drive_diff==-7)
  }
  else if (fun=="ANYSCORE"){
    cost_out=-1*(drive_diff==-7)-1*(drive_diff==-3)
  }
  else if (fun=="DOWN3LATE"){
    cost_out=-.9*(drive_diff==-7)-.5*(drive_diff==-3)
  }
  return(cost_out)
}

cost_functions<-c('BASE','DOWN4LATE','ANYSCORE','DOWN3LATE')

dr_multicost_top<-drive_results_costs_top
for (cost in cost_functions){
  print(cost)
  dr_multicost_top<-dr_multicost_top%>%
    rowwise()%>%
    mutate(Expected.Cost=update_expected_col(yardline_100,ydstogo,down,cost.col=cost))
  dr_multicost_top<- dr_multicost_top%>%
    group_by(unique_drive) %>%
    mutate(outcome_cost=lead(Expected.Cost,1)) %>%
    ungroup()
  dr_multicost_top <-dr_multicost_top %>%
    group_by(unique_drive) %>%
    mutate(outcome_cost2=ifelse(is.na(outcome_cost),scoring_cost(fun=cost,drive_differential),outcome_cost)) %>%
    ungroup()
  dr_multicost_top<-dr_multicost_top %>% select(-outcome_cost,-Expected.Cost)
  names(dr_multicost_top)[length(dr_multicost_top)]<-cost
}
colnames(dr_multicost_top)

dr_multicost_btm<-drive_results_costs_btm
for (cost in cost_functions){
  print(cost)
  dr_multicost_btm<-dr_multicost_btm%>%
    rowwise()%>%
    mutate(Expected.Cost=update_expected_col(yardline_100,ydstogo,down,cost.col=cost))
  dr_multicost_btm<- dr_multicost_btm%>%
    group_by(unique_drive) %>%
    mutate(outcome_cost=lead(Expected.Cost,1)) %>%
    ungroup()
  dr_multicost_btm <-dr_multicost_btm %>%
    group_by(unique_drive) %>%
    mutate(outcome_cost2=ifelse(is.na(outcome_cost),scoring_cost(fun=cost,drive_differential),outcome_cost)) %>%
    ungroup()
  dr_multicost_btm<-dr_multicost_btm %>% select(-outcome_cost,-Expected.Cost)
  names(dr_multicost_btm)[length(dr_multicost_btm)]<-cost
}
colnames(dr_multicost_btm)

dr_multicost_btm = dr_multicost_btm %>%
  filter(play_type != "qb_kneel")

dr_multicost_top = dr_multicost_top %>%
  filter(!(play_type == "field_goal" & down == 3))

write.csv(dr_multicost_top,'drive_results_w_costs_top.csv')
write.csv(dr_multicost_btm,'drive_results_w_costs_btm.csv')

OPT_input_top <- dr_multicost_top %>%
  group_by(unique_drive) %>%
  mutate(outcome_cost=lead(Expected.Cost,1)) %>%
  ungroup()

OPT_input_btm <- drive_results_costs_btm %>%
  group_by(unique_drive) %>%
  mutate(outcome_cost=lead(Expected.Cost,1)) %>%
  ungroup()

View(replace_na(OPT_input_top$outcome_cost,OPT_input_top$drive_differential))

OPT_input_top <-OPT_input_top %>%
  group_by(unique_drive) %>%
  mutate(outcome_cost2=ifelse(is.na(outcome_cost),drive_differential,outcome_cost)) %>%
  ungroup()

OPT_input_btm <-OPT_input_btm %>%
  group_by(unique_drive) %>%
  mutate(outcome_cost2=ifelse(is.na(outcome_cost),drive_differential,outcome_cost)) %>%
  ungroup()

OPT_input_top = OPT_input_top %>%
  select(- drive_differential, - outcome_cost, - Expected.Cost)

OPT_input_btm = OPT_input_btm %>%
  select(- drive_differential, - outcome_cost, - Expected.Cost)

colnames(OPT_input_top[,"outcome_cost2"]) <- "outcome_cost"
colnames(OPT_input_btm[,"outcome_cost2"]) <- "outcome_cost"

OPT_input_top_filt = OPT_input_top %>%
  filter(play_type != "field_goal" &  play_type != "punt" &  play_type != "qb_kneel")

OPT_input_btm_filt = OPT_input_btm %>%
  filter(play_type != "field_goal" &  play_type != "punt" &  play_type != "qb_kneel")

write.csv(OPT_input_top,"OPT_input_top.csv")
write.csv(OPT_input_btm,"OPT_input_btm.csv")
write.csv(drive_results_top, file = "drive_results_top.csv")
write.csv(drive_results_btm, file = "drive_results_btm.csv")
write.csv(OPT_input_top_filt,"OPT_input_top_filt.csv")
write.csv(OPT_input_btm_filt,"OPT_input_btm_filt.csv")
