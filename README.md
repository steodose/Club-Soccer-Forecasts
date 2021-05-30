# Club-Soccer-Forecasts
Repo for my club soccer season simulations

**Based on Mark Taylor's Expected Goals model and Constantinos Chappas's season simulations**

The below documents a relatively simple model for forecasting Premier League results, and is based on work previously done by [Mark Taylor](http://thepowerofgoals.blogspot.com/2016/02/how-to-frame-individual-match-outcome.html) and [Constantinos Chappas](http://rstudio-pubs-static.s3.amazonaws.com/149923_584734fddffe40799cee564c938948d7.html). This was last updated in Matchweek 35 of the 2020-21 Premier League season. I will revisit for the 2021-22 campaign. 

***

Calculating Individual Match Outcomes
------------

The first step in the analysis is to get match data and create arrays with the games played thus far into the season. I'm using data from the fantastic [football-data.co.uk]( http://www.football-data.co.uk website) website. To create odds for each match outcome in the season, I'm using Mark Taylor’s approach by creating match result probabilities [described here](http://thepowerofgoals.blogspot.com.cy/2016/02/how-to-frame-individual-match-outcome.html) using historical goals scored. He uses Expected Goals to create these odds, but I've substituted that with publicly available Actual Goals. This method works better the deeper we are into the season as variance from teams' true finishing ability vs their goal figures evens out of the course of the year. 

As described by Taylor, the method uses 1) the average number of goals scored by home and away teams in the given campaign and 2) The average number of goals scored and conceded by the two teams in question in any match. As an example, let's do this analysis for a single match: Burnley at home vs. Liverpool. And let's say Home teams are scoring 0.25 goals per game more than visitors -- 1.49 compared to 1.24, and the average game has 1.37 expected goals per team.

Note GS = Goals Scored and GC = Goals Conceded in the table below.

| Avg GS by Home Teams | Avg GS by Away Teams | Avg GS/GC by all teams | Avg GS  by Burnley | Avg GC by Burnley | Avg GS by Liverpool| Avg GC by Liverpool |
| --- | --- | --- | --- | --- | --- | --- |
1.49 | 1.24 | 1.37 | 1.09  | 1.91 | 1.92  | 1.16

This means that Burnley have a goals average of about 1.00 and Liverpool 2.43 goals. The math to get there: Burnley are scoring an average of 1.09 goals per match, thus are scoring (1.09/1.37) = 0.79times the rate of average scoring in the competition. Liverpool allow 1.16/1.37 or 0.85 times the rate of conceding in the competition. Burnley are at home and home teams score 1.49/1.37 or 1.09 times the average rate for this year's campaign. Thus, multiplying these rates together, Burnley are expected to score 0.79 * 0.85 * 1.09 * 1.37 = 1.0 goals in this matchup. Likewise we can do the same for Liverpool:

Liverpool score 1.92/1.37 = 1.40

Burnley allow 1.91/1.37 = 1.39 times league average.

Away teams score 1.24/1.37 = 0.91 times league average.

Liverpool are likely to score 1.40 * 1.39 * 0.91 * 1.37 expected goals = 2.43 expected goals.

Goals scoring in soccer is known to follow a Poisson distribution. Thus if we plug these figures into a Poisson model, we get the following match predictions

Burnley Win | Draw | Liverpool Win
--- | --- | ---|
14% | 18% | 64% 

Simulating all season fixtures
------------

The above shows how we get to probabilities for individual match outcomes. Next, we use these outcomes to simulate an entire season...10,000 times. For this model we're using independent Poisson distributions with means `ExpHG` and `ExpAG` to simulate results from. 

For matches that have yet to be played we assume the standard league format where each team plays all other teams home and away. We create all these possible combinations between the league teams in a dataframe, removing the cases where a team plays itself, and looking for combinations which have not already been played. This is where we replicate Taylor's approach to get the expected scoring rates `ExpHG` and `ExpAG`.

From there we run the season simulation 10,000 times and initialize the results table named `df.all`, where for each iteration, the remaining season results are simulated and total points, Goal Difference, and rankings table is constructed based on points and goal difference.

Visualizing the results
------------

Using the `df.all` output dataframe we can create myriad data visualizations to show the range of outcomes for each Premier League team. I use the Tidyverse Googlesheets API to write this dataset to Google Sheets so we can subsequently use Tableau's Web Data Connector functionality to layer on visualizations in Tableau. We can use the CronR addin for RStudio to schedule our R script to run on a defined weekly cadence to ensure we have the latest league results accounted for.

In addition, we  can create a few graphs in R as well. This one was generated as of Matchweek 9![An example from earlier in the season](/Users/Stephan/Documents/Club-Soccer-Forecasts/Expected_Points.png)

Data
------------

The data come from [football-data.co.uk](http://www.football-data.co.uk/), and is refreshed after all the games have been completed for the week, usually on Mondays.

Learn More
------------

You can read more at my blog [Between the Pipes](https://betweenpipes.wordpress.com/)