# FantasyFootballR
Collection of programs for fantasy football analysis written in R

###### 1) FF_FAAB.R
This script seeks to analyze how FAAB dollars are spent in an ESPN fantasy football league. It takes as an input an ESPN league ID and a date range and outputs a variety of tables to show the most important trends in FAAB spending. Specifically, we look at how each team spend their budget, how money was spent on each position, and which players were the most costly and most beneficial to add during the season. This script is useful in looking at the tendencies of different owners and a league in general and using the knowledge gained to try to do a better job of securing top free agent acquisitions in future fantasy seasons.

###### 2) FF_PotentialPointsLeague.R
This script looks at how many points a fantasy football team could have scored if the owner always started their players who scored the most points each week. It takes as an input an ESPN league ID, roster settings, and number of weeks to look at and outputs a table showing the potential points each team could have scored compared with the actual number of points they did score. This script is most useful in determining the true strength of a roster, as number of points actually scored can be skewed due to owner incompetence or outright tanking. Potential points are sometimes used in dynasty leagues to set the draft order for rookies drafts in order to discourage owners from tanking.

###### 3) FF_PositionalPoints.R
This script shows how the teams in a fantasy league scored points at different positions during a season. It takes as an input an ESPN league ID, roster settings, and the number of weeks to look at and outputs a table showing the points each fantasy team scored at different starting positions. This script is most useful in identifying teams who have strengths and weaknesses at different roster positions so they can be targetted for trades. 
