# Club-Soccer-Forecasts
Repo for my club soccer season simulations

**The below is based on Mark Taylor's Expected Goals model and Constantinos Chappas's season simulations**

The below documents a relatively simple model for determining win probabilities for each Premier League team and is based on work previously done by [Mark Taylor](http://thepowerofgoals.blogspot.com/2016/02/how-to-frame-individual-match-outcome.html) and [Constantinos Chappas](http://rstudio-pubs-static.s3.amazonaws.com/149923_584734fddffe40799cee564c938948d7.html). This was last updated in Matchweek 35 of the 2020-21 Premier League season. I will revisit for the 2021-22 campaign. 

The first step in the analysis is to get match data and create arrays with the games played thus far into the season. I'm using data from from the fantastic [football-data.co.uk]( http://www.football-data.co.uk website) website. To create odds for each match outcome in the season, I'm using Mark Taylor’s approach by creating match result probabilities [described here](http://thepowerofgoals.blogspot.com.cy/2016/02/how-to-frame-individual-match-outcome.html) using historical goals scored. He uses Expected Goals to create these odds, but I've substituted that with publicly available Actual Goals. This method works better the deeper we are into the season as variance from teams' true finishing ability vs their goal figures evens out of the course of the year. 

***

Calculating Individual Match Outcomes
------------

As described by Taylor, the method uses 1) the average number of goals scored by home and away teams in the given campaign and 2) The average number of goals scored and conceded by the two teams in question in any match. As an example, let's do this analysis for a single match: Burnley away vs. Liverpool. And let's say Home teams are scoring 0.25 goals per game more than visitors -- 1.50 compared to 1.25, and the average game has 1.35 expected goals per team.

Note GS = Goals Scored and GC = Goals Conceded in the table below:

Avg GS by Home Teams | Avg GS by Away Teams | Avg GS/GC by all teams | Avg GS  by Burnley | Avg GC by Burnley | Avg GS by Liverpool| Avg GC by Liverpool
--- | --- | --- |--- |--- |--- |--- |--- |--- |--- |---
1.50 | 1.25 | 1.35 | 1.9  | 1.1  | 1.92  | 1.16

This means that Burnley have a goals average of 1.00 and Liverpool 2.43 goals. The math to get there: Burnley are expected to score 0.79 * 0.85 * 1.0 * 1.37 = 1.00 goals based on the aformentioned scoring rates. Liverpool are likely to score 1.40* 1.39 * 0.91 * 1.37 = 2.43 goals.

Goals scoring in soccer is known to follow a Poisson distribution. Thus if we plug these figures into a Poisson model, we get the following match predictions

Burnley Win | Draw | Liverpool Win
--- | --- | ---|
14% | 18% | 64% 

Simulating all season fixtures
------------

The above shows how we get to probabilities for individual match outcomes. Next, we use these outcomes to simulate an entire season...10,000 times. For this model we're using independent Poisson distributions with means `ExpHG` and `ExpAG` to simulate results from. 

For matches that have yet to be played we assume the standard league format where each team plays all other teams home and away. We create all these possible combinations between the league teams in a dataframe, removing the cases where a team plays itself, and looking for combinations which have not already been played. This is where we replicate Taylor's approach to get the expected scoring rates ExpHG and ExpAG.

From there we simulate the season 10,000 times and initialize the results table df.all where for each iteration, the remaining season results are simulated and total points, Goal difference, and rankings table is constructed based on points and goal difference.

Visualizing the results
------------

Using the `df.all` output dataframe we can create myriad data visualizations to show the range of outcomes for each Premier League team. I use the Tidyverse Googlesheets API to write this dataset to Google Sheets so we can subsequently use Tableau's Web Data Connector functionality to layer on visualizations in Tableau. We can use the CronR addin for RStudio to schedule our R script to run on a defined weekly cadence to ensure we have the latest league results accounted for.

In addition, we  can create a few graphs in R as well.

Data
------------

The data comes from [football-data.co.uk](http://www.football-data.co.uk/), and is refreshed after all the games have been completed for the week, usually on Mondays.

Learn More
------------

You can read more at my blog [Between the Pipes](https://betweenpipes.wordpress.com/)
