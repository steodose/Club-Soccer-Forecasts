##### Premier League Vizualizations #####
## By: Stephan Teodosescu


library(tidyverse)
library(teamcolors)
library(reactable)
library(gt) #for 538-themed tables
library(extrafont) #for adding in new fonts
library(ggtext)
library(RCurl)
library(magick)
library(ggimage) #for working with team logos
library(webshot) #saving high quality images of gt tables
library(glue)
library(htmltools)
library(reactablefmtr)
library(scales)
library(lubridate)
library(ggridges)
library(forcats)
library(viridis)
library(dichromat)


##### Load datasets #####

# Simulations output. Note this is a test for now. Replace with real Simulations file in production
simulations <- read_csv("simulations.csv") 

# Already played Results table

df <- read.csv("http://www.football-data.co.uk/mmz4281/2021/E0.csv", 
               stringsAsFactors = FALSE)

##### Data Pre-processing #####

#Fetch current date for updating visualizations
update_date <- max(df$Date) %>% 
    format(format="%B %d")

# Initialize EPL team colors (using this method for 2021-22)

Team <- c('Arsenal', 'Aston Villa', 'Brighton', 'Burnley', 'Chelsea', 'Crystal Palace', 
          'Everton', 'Norwich', 'Leeds', 'Leicester', 'Liverpool', 'Man City', 'Man United', 'Newcastle', 
          'Brentford', 'Southampton', 'Tottenham', 'Wolves','Watford','West Ham')

Primary <- c("#DB0007", "#95BFE5", "#0057B8", "#8ccce5", "#034694","#1b458f", "#274488","#00A650", "#FFCD00",
             "#003090", "#00a398", "#98c5e9", "#da020e", "#000000", "#ee2737", "#d71920","#001c58",
             "#FDB913","#FBEE23", "#60223b")

epl_colors <- tibble(Team, Primary)

# Import the Chivo Font family from Google Fonts (in Font Book on Mac)...only need to do this once
extrafont::font_import(pattern = "Chivo")


##### Ridge plots #####

# 1. Points Plot (WIP)
ggplot(simulations, aes(x = Pts, y = fct_reorder(Team, Pts), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Points", y = "",
         title = "2021-22 Premier League: Thru Matchweek 30",
         subtitle = "Expected points after simulating the remainder of the season 10,000x",
         caption = "Data: football-data.co.uk"
    ) +
    theme_bw() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    scale_fill_manual(values = epl_colors$Primary)

### load font from google fonts
htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=Chivo:400,600,700&display=swap", rel = "stylesheet")

### match 538's table theme for Reactable table below
theme_538 <- function() {
    reactable::reactableTheme(
        searchInputStyle = list(width = "31%", backgroundColor = "#F9F9F9"),
        style = list(
            fontFamily = "Chivo"
        ),
        headerStyle = list(
            "&:hover[aria-sort]" = list(
                background = "hsl(0, 0%, 80%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
                background = "#555",
                color = "#FFF"
            ),
            borderColor = "#333"
        ),
        borderColor = "#CDCDCD"
    )
}


#Define color palette to use in tables
my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")


###### Create Reactable table(s) #####

# 1. Power rankings table (WIP)


# 2. Historical Results (actuals) Reactable table

df_results_table <- df %>% 
    select(Date, Time, HomeTeam, AwayTeam, FTHG, FTAG, B365H, B365D, B365A) %>% 
    mutate(B365H_prob = round((1/B365H)*100,0), B365D_prob = round((1/B365D)*100),0, B365A_prob = round((1/B365A)*100,0)) %>% 
    select(Date, Time, HomeTeam, AwayTeam, FTHG, FTAG, B365H_prob, B365D_prob, B365A_prob)


tbl_results <- reactable(df_results_table,
                 theme = theme_538,
                 ### add column group header
                 columnGroups = list(
                     colGroup(name = "Bet365 Pre-Match Odds (%)", 
                              columns = c("B365H_prob","B365D_prob","B365A_prob"))
                 ),
                 showSortIcon = TRUE,
                 searchable = TRUE,
                 language = reactableLang(
                     searchPlaceholder = "SEARCH FOR A TEAM..."),
                 defaultPageSize = 100,
                 columns = list(
                     `HomeTeam` = colDef(maxWidth = 120,
                                     name = "Home",
                                     align = "left"),
                     `AwayTeam` = colDef(maxWidth = 120,
                                     name = "Away",
                                     align = "left"),
                     `FTHG` = colDef(maxWidth = 80,
                                          name = "Home Goals",
                                          style = color_scales(df_results_table, colors = my_color_pal),
                                          align = "right"),
                    `FTAG` = colDef(maxWidth = 80,
                                     name = "Away Goals",
                                     style = color_scales(df_results_table, colors = my_color_pal),
                                     align = "right"),
                    ### add bars using data_bars 
                    `B365H_prob` = colDef(maxWidth = 400,
                                          align = "right",
                                          name = "B365H",
                                          cell = data_bars(df_results_table, 
                                                           fill_color = "dodgerblue",
                                                           background = "lightgrey",
                                                           fill_opacity = 0.8,
                                                           max_value = 100,
                                                           scales::number_format(accuracy = 0.1))),
                    `B365D_prob` = colDef(maxWidth = 400,
                                          align = "right",
                                          name = "B365D",
                                          cell = data_bars(df_results_table, 
                                                           fill_color = "dodgerblue",
                                                           background = "lightgrey",
                                                           fill_opacity = 0.8,
                                                           max_value = 100,
                                                           scales::number_format(accuracy = 0.1))),
                    `B365A_prob` = colDef(maxWidth = 400,
                                          align = "right",
                                          name = "B365A",
                                          cell = data_bars(df_results_table, 
                                                           fill_color = "dodgerblue",
                                                           background = "lightgrey",
                                                           fill_opacity = 0.8,
                                                           max_value = 100,
                                                           scales::number_format(accuracy = 0.1)))),
                    
                 pagination = TRUE,
                 compact = TRUE, 
                 borderless = FALSE, 
                 striped = FALSE,
                 fullWidth = FALSE, 
                 defaultColDef = colDef(align = "center", minWidth = 120),
) %>% 
    add_title("2021-22 Premier League Fixture Results") %>% 
    add_subtitle(glue("Data as of {update_date}"),font_size = 18) %>% 
    add_source("Data: www.football-data.co.uk", font_size = 12)