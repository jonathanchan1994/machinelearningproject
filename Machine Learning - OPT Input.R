library(dplyr)
library(tidyverse)
library(ggplot2)

plays_original = read.csv("NFL Play by Play 2009-2018 (v5).csv")

plays_original = plays_original %>%
  mutate(unique_drive = paste(as.character(plays_original$game_id),"_",as.character(plays_original$drive)))

plays_original = plays_original %>%
  mutate(play_differential = score_differential_post - score_differential)

plays_original = plays_original %>%
  mutate(year = substr(plays_original$game_id,0,4))

plays_original = plays_original %>%
  mutate(unique_play = paste(as.character(plays_original$game_id),"_",as.character(plays_original$play_id)))


plays = plays_original %>%
  select(play_id,game_id,yardline_100,half_seconds_remaining,quarter_end,down,goal_to_go,ydstogo,ydsnet,play_type,
       yards_gained,shotgun,qb_dropback, pass_location,run_location,run_gap,field_goal_result,posteam_timeouts_remaining,
       defteam_timeouts_remaining,timeout,timeout_team,score_differential,score_differential_post,no_score_prob,fg_prob,safety_prob,td_prob,
       third_down_converted,third_down_failed,fourth_down_converted,fourth_down_failed,incomplete_pass,interception,
       safety, fumble_lost,rush_attempt,pass_attempt,sack,pass_touchdown,rush_touchdown,passer_player_id,receiver_player_id,
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

drive_results = plays %>%
  filter(half_seconds_remaining >= 240 &
           (play_type != "no_play") & (!is.na(down))) %>%
  select(unique_drive,yardline_100,down,ydstogo,goal_to_go,pass_location,run_location, play_type,drive_differential)

drive_results_filtered = drive_results %>%
  select(yardline_100,down,ydstogo,goal_to_go,drive_differential,play_type,unique_drive)

glimpse(drive_results)

write.csv(drive_results_filtered, file = "drive_results.csv")

expected_cost = read.csv("splits_w_costs1127.csv")
events = nrow(expected_cost)


write.csv(drive_results,'drive_results1129.csv')

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

dr_multicost<-drive_results_filtered
for (cost in cost_functions){
  print(cost)
  dr_multicost<-dr_multicost%>%
    rowwise()%>%
    mutate(Expected.Cost=update_expected_col(yardline_100,ydstogo,down,cost.col=cost))
  dr_multicost<- dr_multicost%>%
    group_by(unique_drive) %>%
    mutate(outcome_cost=lead(Expected.Cost,1)) %>%
    ungroup()
  dr_multicost <-dr_multicost %>%
    group_by(unique_drive) %>%
    mutate(outcome_cost2=ifelse(is.na(outcome_cost),scoring_cost(fun=cost,drive_differential),outcome_cost)) %>%
    ungroup()
  dr_multicost<-dr_multicost %>% select(-outcome_cost,-Expected.Cost)
  names(dr_multicost)[length(dr_multicost)]<-cost
}
colnames(dr_multicost)

df_multicost_woFG<-dr_multicost%>%filter((play_type!="qb_kneel")&(play_type!="field_goal")&(play_type!="punt"))
dr_multicost_withFG<-dr_multicost%>% 
  mutate(keep=(play_type!="field_goal" | down==4)) %>%
  filter(play_type!="qb_kneel" & keep) %>%
  select(-keep)

write.csv(dr_multicost_withFG,'drive_results_w_FG_1130.csv')
write.csv(df_multicost_woFG,'drive_results_wo_FG_1130.csv')

back<-readPNG('field_backdrop2.png')
g <- rasterGrob(back, interpolate=TRUE)

dr_multicost_withFG %>% ggplot()+
  aes(x=yardline_100,y=BASE)+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  stat_density2d(aes(fill=..level..), geom="polygon") +
  labs(x="Yardline",y="Expected Drive Score")+
  scale_fill_gradient(low="green", high="red")
dr_means_yardline <- dr_multicost_withFG %>% group_by(yardline_100) %>% summarize(score=mean(BASE))
dr_means_yardline %>% ggplot()+
  aes(x=yardline_100,y=-score,colour='yellow',size='qsec')+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  labs(x="Yardline",y="Average Expcted Score")+
  geom_line()
