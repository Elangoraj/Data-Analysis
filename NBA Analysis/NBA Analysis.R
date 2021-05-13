
nba <- read.csv('C:/Data/Exeter/DS_with_AI/GIT/Juhi/nbadata.csv')

summary(nba)

library(data.table)

library(tidyverse)

library(ggplot2)

#Players points table
cwd

plyrsPoint <- nba %>% select(PLAYER_ID,PLAYER_NAME, PTS)

ppn <- data.frame(table(plyrsPoint$PLAYER_NAME, plyrsPoint$PTS))

#going to call columns from this data frame PPN

attach(ppn)


#reshape(df1, direction = "wide", idvar = "NUM", timevar = "Type"). IT will help us to pivot the table the records.

ppn_final <- reshape(ppn, direction = "wide", idvar = "Var1", timevar = "Var2")

#renamed the playersname column
names(ppn_final)[names(ppn_final) == "Var1"] <- "Player_Name"


#Added the shots made, attempted shots by the player
ppn_final$TotalShots = ppn_final$Freq.0 + ppn_final$Freq.2 + ppn_final$Freq.3


#Get the percentage of the player how accurate his shots were

ppn_final$ShotAccuracy = ((ppn_final$Freq.2 + ppn_final$Freq.3)/ppn_final$TotalShots)*100


#Made a new column to plot the graph. It inclueds the format of Player_name_totalshots

ppn_final$Name_TotalShots = with(ppn_final, paste0(Player_Name, "_", TotalShots))

library(ggplot2)

p<-ggplot(data=head(ppn_final[order(-ppn_final$Freq.3),]), aes(x=Name_TotalShots, y=ShotAccuracy)) +
  geom_bar(stat="identity", fill = "#7a0600", color = "black", width = 0.6)
p

#Plot the graph with top 6 players with highest shot accuracy

p<-ggplot(data=head(ppn_final[order(-ppn_final$ShotAccuracy),]), aes(x=Name_TotalShots, y=ShotAccuracy)) +
  geom_bar(stat="identity", las=2)
p
p + coord_flip()

#Plot the graph with top 6 players with highest 3 pointer accuracy
p<-ggplot(data=head(ppn_final[order(-ppn_final$Freq.3),]), aes(x=Name_TotalShots, y=ShotAccuracy)) +
  geom_bar(stat="identity", fill = "#7a0600", color = "black", width = 0.6)
p

#Plot the graph with top 6 players with highest 2 pointer accuracy
p<-ggplot(data=head(ppn_final[order(-ppn_final$Freq.2),]), aes(x=Name_TotalShots, y=ShotAccuracy)) +
  geom_bar(stat="identity", fill = "#E69F00", color = "black", width = 0.6)
p

#Plot the graph with top 6 players with highest misses
p<-ggplot(data=head(ppn_final[order(-ppn_final$Freq.0),]), aes(x=Name_TotalShots, y=ShotAccuracy)) +
  geom_bar(stat="identity", color="black", fill = "#b0bf0b", width = 0.6)
p

#count of how many shots were made in the season 2014-15 

madeShots <- nba %>% select(GAME_ID, FGM) %>% filter(GAME_ID[1:nrow(nba)] & FGM == "1")


#count proper filter

madeShots <- nba %>% select(GAME_ID, FGM) %>% filter(GAME_ID[1:nrow(nba)] & FGM == "1")

#table is summarzing the data
tbfgm <- table(madeShots)

#converting tabe to dta frame
tbfgm2 <- data.frame(rbind(tbfgm))

#resetting the index
tp <- cbind(newColName = rownames(tbfgm2), tbfgm2)
rownames(tp) <- 1:nrow(tp)

#filtering the top 5 after ordering the data
barFGM <- head(tp[order(-tp$X1),])

#bar plot of the count in the season
barplot(barFGM$X1, names.arg = c(barFGM$newColName))


#Number of made and missed in 7 periods in basketball

md_msd_shots <- nba %>% select(PERIOD,SHOT_RESULT)

mmtb <- data.frame(table(md_msd_shots$PERIOD, md_msd_shots$SHOT_RESULT))

mmtb_final <- reshape(mmtb, direction = "wide", idvar = "Var1", timevar = "Var2")


#spliting the minutes from game clock before colon. Before colon take and after colon, replace with empty string in the GAMECLOCK column

q3 = nba %>% select(GAME_CLOCK, SHOT_RESULT, PERIOD)
q3$GAME_CLOCK_MIN <- sub("\\:.*", "", c(q3$GAME_CLOCK))

#coverting the period to minutes


q3 = q3 %>% mutate(OVERALL_MIN = case_when(
  q3$PERIOD == 1 ~ 12 ,
  q3$PERIOD == 2 ~ 12  + 12,
  q3$PERIOD == 3 ~ 12  + 24,
  q3$PERIOD == 4 ~ 12  + 36,
  q3$PERIOD == 5 ~ 60,
  q3$PERIOD == 6 ~ 60,
  q3$PERIOD == 7 ~ 60
))

#transforming something?

q4 <- transform(q3, OVERALL_MIN = as.numeric(OVERALL_MIN), 
                GAME_CLOCK_MIN = as.numeric(GAME_CLOCK_MIN))

#minus

q4$OVERALL_MIN_FINAL = q4$OVERALL_MIN - q4$GAME_CLOCK_MIN


#table to data frame

q5 <- data.frame(table(q4$OVERALL_MIN_FINAL, q4$SHOT_RESULT))

#reshape the data frame don't want reshape code

#q5 <- reshape(q5, direction = "wide", idvar = "Var1", timevar = "Var2")

#changing the name of Shot Result from var2

names(q5)[names(q5) == "Var2"] <- "Shot_Result"

#plottng the graph made and missed shots in this season


ggplot(q5,aes(x = Var1, y = Freq, color = Shot_Result, group = Shot_Result))+
  geom_line()+
  geom_point()+
  xlab("Minutes of the Match for this season")+
  ylab("Number of Shots")

  
#Inferential Number 1 
attach(nba)

home_away <- nba %>% select(LOCATION, PLAYER_NAME, PTS) %>% filter(SHOT_RESULT == "made")

#aggregate by sum(pts) which inclueds 2 & 3 in h and away for each players
home_away2 <- aggregate(PTS ~ ., data = home_away, FUN = sum)

#anova one way test
one.way <- aov(PTS ~ LOCATION, data = home_away2)
summary(one.way)



home_away3 <- aggregate(PTS ~ LOCATION, data = home_away, FUN = sum)


#t test for the same hom -away players

home_away2
 
t.test(PTS ~ LOCATION, data = home_away2)

library(dplyr)
#anova test
#filtering data for period 1 to 4 excluding OTs where we are getting shot numbers attempted by the players in particular periods in a game

period_shotNum <- nba %>% select(PERIOD, PLAYER_NAME, DRIBBLES) %>% filter(PERIOD <= 4)


period_shotNum2 <- aggregate(DRIBBLES ~ ., data = period_shotNum, FUN = sum) 

one.way <- aov(DRIBBLES ~ PERIOD, data = period_shotNum2)
summary(one.way)
plot(one.way, 1)

#Null hypothesis: The period in the game doesn;t affect the number of dribbles.
#Alternate: The period affects the number of dribbles.

#For 95% confidence, the p value > 0.05 and hence we reject alternate hypothesis. 


#when a 
margin <- nba %>% distinct(HOME_TEAM, AWAY_TEAM, W, LOCATION, FINAL_MARGIN)

attach(margin)

margin2 <- margin %>% mutate(WINNING_TEAM = case_when(
  LOCATION == 'H' && W == 'W' ~ HOME_TEAM,
  LOCATION == 'H' && W == 'L' ~ AWAY_TEAM,
  LOCATION == 'A' && W == 'W' ~ AWAY_TEAM,
  LOCATION == 'A' && W == 'L' ~ HOME_TEAM
))

margin3 <- margin2 %>% select(FINAL_MARGIN, WINNING_TEAM) %>% filter(FINAL_MARGIN > 0)

#we got the data for  avg final margin  each team for this seaon (both away and home matches counted)
margin4 <- aggregate(FINAL_MARGIN ~ WINNING_TEAM, data = margin3, FUN = mean)

margin4$FINAL_MARGIN = round(margin4$FINAL_MARGIN, 2)



install.packages("treemap")


library(treemap)

margin4$label <- paste(margin4$WINNING_TEAM, margin4$FINAL_MARGIN, sep = "\n")

p <- treemap(margin4,
             index=c("label"),
             vSize="FINAL_MARGIN",
             type="index",
             palette = "Paired",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
)   


#
library(ggplot2)

ggplot(nba, aes(x=SHOT_DIST, color = SHOT_RESULT, group= SHOT_RESULT)) + geom_density() + xlab("Shot Distance") + ylab("") + theme_light()





gsw <- nba %>% distinct(GAME_ID, DATE, HOME_TEAM, W, FINAL_MARGIN) %>% filter(HOME_TEAM == "GSW" & W == "W" & FINAL_MARGIN > 11)

gsw2 <- as.Date(gsw$DATE)

qsw2$DATE <- as.Date(gsw2$DATE)

as.Date(gsw$DATE, "%m/%d/%Y")

ggplot(data = gsw,aes(x = DATE, y = FINAL_MARGIN))+ geom_point() + geom_line(linetype = dashed)

plot(FINAL_MARGIN ~ DATE, gsw, type = "l")



cle <- nba %>% distinct(GAME_ID, DATE, HOME_TEAM, W, FINAL_MARGIN) %>% filter(HOME_TEAM == "CLE" & W == "W")


ggplot(data = cle,aes(x = DATE, y = FINAL_MARGIN))+ geom_point() + geom_line()

#correct
test <- as.POSIXlt(gsw$DATE, format = "%b %d, %Y")

gsw$DATE_NEW <- sub("\\ .*", "", c(test))

ggplot(data = gsw,aes(x = DATE_NEW, y = FINAL_MARGIN, group = 1))+ geom_point() + geom_line()


mtcars

attach(mtcars)

m = glm(data = mtcars, gear ~ .)

summary(m)


#hypothesis  t-test :

shot_prd = nba %>% select(PLAYER_NAME, PTS, PERIOD) %>% filter(nba$FGM == "1" & nba$PERIOD <= 4)

shot_prd2 <- shot_prd %>% mutate(PERIOD_NEW = case_when(
  PERIOD == 4 ~ 'FINAL_PERIOD',
  PERIOD <= 3 ~ 'EARLY_PERIODS'
))


t.test(PTS ~ PERIOD_NEW, data = shot_prd2)
