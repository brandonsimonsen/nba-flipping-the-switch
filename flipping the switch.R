
# Libraries ---------------------------------------------------------------

library(scales)
library(nbastatR)
library(lubridate)
library(tidyverse)



# Pull Data ---------------------------------------------------------------


  ## Gamelogs
  
  gamelogs <- game_logs(seasons = 2019,result_types = "team")
  
    # note: if you want to reproduce exactly what is shown in my post, you will want to run the line below as well, to filter out any games post-allstar-break
    # gamelogs <- gamelogs %>% filter(date(dateGame) <= '2019-02-14')


  ## Win Probabilities
  
  wp <- win_probability(game_ids = unique(gamelogs$idGame),filter_non_plays = T)




# Data Manipulation -------------------------------------------------------


  ## Melting Home/Away Win Probabilities into a single variable 'winProb'
  
  wp.melt <- rbind(wp %>% 
                     select(idGame, numberEvent, numberPeriod, timeRemaining, slugTeam = slugTeamHome, winProb = pctWinProbHome)
                   ,wp %>% 
                     select(idGame, numberEvent, numberPeriod, timeRemaining, slugTeam = slugTeamAway, winProb = pctWinProbAway)
  )


  ## Selecting first and last play in 4th quarter with a valid winProb value (i.e. making sure to not select NA values) and joining 
  
  wp.4Q.firstAndLast <- inner_join(
    wp.melt %>% 
      filter(numberPeriod == 4 & !is.na(winProb)) %>% 
      group_by(idGame,slugTeam) %>% 
      top_n(-1,numberEvent) %>% 
      ungroup %>% 
      select(idGame,slugTeam,winProbFirst = winProb)
    ,wp.melt %>% 
      filter(numberPeriod == 4 & !is.na(winProb)) %>% 
      group_by(idGame,slugTeam) %>% 
      top_n(1,numberEvent) %>%
      ungroup %>%
      select(idGame,slugTeam,winProbLast = winProb)
    ,by = c("idGame", "slugTeam")
  )


  ## Calculating winProb difference
  
  wp.4Q.firstAndLast$winProbAdded <- wp.4Q.firstAndLast$winProbLast - wp.4Q.firstAndLast$winProbFirst


  ## Summarizing (i.e. taking mean, by team)
  
  wp.means <- wp.4Q.firstAndLast %>% 
    group_by(slugTeam) %>% 
    summarise(meanWPA = mean(winProbAdded))






# Plots -------------------------------------------------------------------


  ## GSW winProb for GSWvPHX game (idGame 0021800822)
  
    ### Plot 1
    
    ggplot(wp %>% filter(idGame == 0021800822)) +
      geom_line(aes(x = timeRemaining, y = pctWinProbAway), color = '#006BB6') +
      scale_x_reverse(limits = c(48,0),breaks = c(48,36,24,12,0),labels = c('Q1','Q2','Q3','Q4','Game End')) +
      scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      geom_point(aes(x = timeRemaining, y = pctWinProbAway), data = wp %>% filter(idGame == 0021800822 & numberPeriod == 4) %>% top_n(-1,numberEvent), color = '#006BB6') +
      geom_text(aes(x = timeRemaining, y = pctWinProbAway,label=scales::percent(pctWinProbAway)), data = wp %>% filter(idGame == 0021800822 & numberPeriod == 4) %>% top_n(-1,numberEvent), size = 3, nudge_x = -1.5) +
      geom_point(aes(x = timeRemaining, y = pctWinProbAway), data = wp %>% filter(idGame == 0021800822 & numberPeriod == 4) %>% top_n(1,numberEvent), color = '#006BB6') +
      geom_text(aes(x = timeRemaining, y = pctWinProbAway,label=scales::percent(pctWinProbAway)), data = wp %>% filter(idGame == 0021800822 & numberPeriod == 4) %>% top_n(1,numberEvent), size = 3, nudge_y = -.025) +
      theme_classic() +
      theme(axis.line = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(legend.title = element_blank()) +
      labs(y = 'Win Probability')
    
    ### Plot 2
    
    ggplot(wp %>% filter(idGame == 0021800822)) +
      geom_line(aes(x = timeRemaining, y = pctWinProbAway), color = '#006BB6') +
      scale_x_reverse(limits = c(48,-12),breaks = c(48,36,24,12,0),labels = c('Q1','Q2','Q3','Q4','Game End')) +
      scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      geom_point(aes(x = timeRemaining, y = pctWinProbAway), data = wp %>% filter(idGame == 0021800822 & numberPeriod == 4) %>% top_n(-1,numberEvent), color = '#006BB6') +
      geom_point(aes(x = timeRemaining, y = pctWinProbAway), data = wp %>% filter(idGame == 0021800822 & numberPeriod == 4) %>% top_n(1,numberEvent), color = '#006BB6') +
      geom_errorbar(aes(x = -6,ymin = min(pctWinProbAway), ymax = max(pctWinProbAway)), data= rbind(wp %>% filter(idGame == 0021800822 & numberPeriod == 4) %>% top_n(-1,numberEvent), wp %>% filter(idGame == 0021800822 & numberPeriod == 4) %>% top_n(1,numberEvent)), width = 1,linetype = 'longdash', alpha = .25) +
      annotate('text',x = -11, y = 0.6468, label = 'winProb added: \n 70.4%', size = 3) +
      theme_classic() +
      theme(axis.line = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(legend.title = element_blank()) +
      labs(y = 'Win Probability')


  ## Switch Flipping Ability
  
    ### Plot
    
    ggplot(data = wp.means) +
      geom_bar(mapping = aes(x = reorder(slugTeam, meanWPA, FUN = mean), y = meanWPA),stat = 'identity', fill = 'lightgray') +
      geom_bar(mapping = aes(x = reorder(slugTeam, meanWPA, FUN = mean), y = meanWPA), data = wp.means %>% filter(slugTeam == 'GSW'),stat = 'identity', fill = '#006BB6') +
      coord_flip() +
      scale_y_continuous(limits = c(-.15,.15),breaks = c(-.15,0,.15)) +
      geom_text(aes(x = reorder(slugTeam, meanWPA, FUN = mean), y = meanWPA, label=scales::percent(meanWPA)), data = wp.means %>% filter(meanWPA >= 0), alpha=1,nudge_y = .02, size = 5) +
      geom_text(aes(x = reorder(slugTeam, meanWPA, FUN = mean), y = meanWPA, label=scales::percent(meanWPA)), data = wp.means %>% filter(meanWPA < 0), alpha=1,nudge_y = -.02, size = 5) +
      theme_classic() +
      theme(axis.line = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
      theme(axis.ticks.y = element_blank())
