##### Premier League Forecasts App #####
## By: Stephan Teodosescu

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyscreenshot)
library(shinyuieditor)
library(gridlayout)

library(tidyverse)
library(teamcolors)
library(worldfootballR)
library(gt) #for 538-themed tables
library(gtExtras)
library(extrafont) #for adding in new fonts for R graphics
library(ggtext)
library(RCurl)
library(curl)
library(magick)
library(ggimage) #for working with team logos
library(webshot) #saving high quality images of gt tables
library(glue)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(ggridges)
library(ggsci) #for positional heatmap
library(sparkline) #for positional barplot in reactable standings table
library(DT)
library(scales)
library(ggrepel)
library(prismatic)
library(svglite)
library(Cairo)
library(ragg)
library(ggchicklet) #for stylized bar charts

options(shiny.usecairo = TRUE)

#options(use.ragg = TRUE)

##### Load datasets #####

# Load Premier League Game Data from football-data.uk.com
df <- read.csv("https://www.football-data.co.uk/mmz4281/2223/E0.csv", 
               stringsAsFactors = FALSE)

# Simulation results output from Sims script (update to output using GitHub actions when get it working)
simulations <- 'https://raw.githubusercontent.com/steodose/Club-Soccer-Forecasts/main/data/simulations.csv' %>% 
    read_csv()

# Expected goals data from FBref (StatsBomb) via worldfootballR
#dat_2022 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2022, tier = "1st") #not working for 2022 for some reason

# Load team mappings
team_mapping <- 'https://raw.githubusercontent.com/steodose/Club-Soccer-Forecasts/main/team_mapping.csv' %>% 
    read_csv()


##### Data Pre-processing #####

# Fetch current date for updating visualizations
update_date <- max(df$Date) %>% 
    format.Date(format ="%b. %d")

iSim <- 10000 #Define iSim from sims script

# Load font from google fonts
htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=Outfit:400,600,700&display=swap", rel = "stylesheet")

# Define 538 table theme for Reactable table(s) below
theme_538 <- function() {
    reactable::reactableTheme(
        searchInputStyle = list(width = "31%", backgroundColor = "#F9F9F9"),
        style = list(
            fontFamily = "Outfit"
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


# Define table theme for gt tables. Comes from Tom Mock's blog post.
gt_theme_538 <- function(data,...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                vars(logo)
            ),
            fn = function(x) {
                web_image(
                    url = x,
                    height = 25
                )
            }
        ) %>%
        # Relabel columns
        cols_label(
            logo = ""
        ) %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Outfit"),
                default_fonts()
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "white", weight = px(2) #used to be transparent rather than white
            ),
            locations = cells_body(
                columns = TRUE,
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        )  %>% 
        tab_options(
            column_labels.background.color = "white",
            table.border.top.width = px(3),
            table.border.top.color = "white", #used to be transparent
            table.border.bottom.color = "white", #used to be transparent
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "white", #used to be transparent
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        ) 
}


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=12, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'white', color = "white")
        )
}


# Define color palette to use in tables
my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")

temppal <- c("#36a1d6", "#76b8de", "#a0bfd9", "#ffffff", "#d88359", "#d65440", "#c62c34")

# Initialize EPL team colors (using this method for 2022-23)
epl_colors <- team_mapping %>% 
    select(team, color_pri) %>% 
    rename(Primary = color_pri)

# Matches played results (actuals)
df_results_table <- df %>% 
    select(Date, Time, HomeTeam, AwayTeam, FTHG, FTAG, B365H, B365D, B365A) %>% 
    mutate(B365H_prob = round((1/B365H)*100,0), B365D_prob = round((1/B365D)*100),0, B365A_prob = round((1/B365A)*100,0)) %>%
    #mutate('logo_home' = paste0('logos/', Home, '.png')) %>% 
    #mutate('logo_away' = paste0('logos/', Away, '.png')) %>%
    select(Date, Time, HomeTeam, AwayTeam, FTHG, FTAG, B365H_prob, B365D_prob, B365A_prob)

# curate for fixtures table
df_fixtures <- df_results_table %>%
    select(-Time) %>%
    str_c("FTHG", "FTAG", sep = "-")


##### Plots for publication in App #####

## 1. Points Plot using ggridges package
points_forecast <- ggplot(simulations, aes(x = Pts, y = fct_reorder(Team, Pts), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Points", y = "",
         title = glue("2022-23 Premier League: Thru matches as of {update_date}."),
         subtitle = "Distribution of expected points after simulating the remainder of the season 10,000x",
         caption = "Data: football-data.co.uk"
    ) +
    theme_bw() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Outfit")) +
    scale_fill_manual(values = epl_colors$Primary)


## 2. Probabilities by position table
league_table <- table(simulations$Team, simulations$Rank)/iSim

league_table2 <- as.data.frame(league_table) %>% # Convert table to data frame
    rename(Team = Var1, Placement = Var2) %>% 
    arrange(Placement, desc(Freq))

league_table2$Freq <- round(league_table2$Freq, 2)

league_table3 <- league_table2 %>% 
    pivot_wider(names_from = Placement,
                values_from = Freq)


## create color palette to be used in bar plot based on input data
chicklets_pal <- c(
    "gray85",
    rep("gray70", length(league_table2$Freq) - 19), 
    "#ff0078"
)


# Create datatable heatmap with DT (not using now)
position_heatmap2 <- datatable(league_table3,
          options = list(paging = FALSE,
                         searching = F,
                         pageLength = 20)) %>%
    formatPercentage(columns = 2:21, 1) %>% 
    formatStyle(names(league_table3), 
                backgroundColor = styleInterval(seq(0, 1, 0.01), heat.colors(102)[102:1]))


# Try gt for the heatmap
league_table3 <- league_table3 %>% 
    mutate(Rank = row_number()) %>% 
    relocate(Rank)

league_table_gt <- left_join(league_table3, team_mapping, by = c("Team" = "team"))

league_table_gt2 <- league_table_gt %>% 
    mutate(Rank = row_number()) %>% 
    relocate(Rank) %>% 
    select(Rank, url_logo_espn, 2:22) %>% 
    rename(logo = url_logo_espn)

# put in rank order
league_table_gt3 <- league_table_gt2[
    with(league_table_gt2, order(-`1`, -`2`, -`3`, -`4`, -`5`, -`6`, -`7`, -`8`, -`9`, -`10`,-`11`, -`12`, -`13`, -`14`, -`15`, -`16`, -`17`, -`18`, -`19`, -`20`)),] %>% 
    mutate(Rank = row_number())

#make gt table
gt_sims <- league_table_gt3 %>% 
    gt() %>% 
    cols_label(Rank = "",
               logo = "",
               Team = "") %>% 
    tab_header(
        title = md("**Premier League Simulations**"), 
        subtitle = paste0("2022-23 Season | Updated ", format(Sys.Date(), format="%B %d, %Y"))
    )  %>% 
    text_transform(
        locations = cells_body(vars(logo)),
        fn = function(x) {
            web_image(url = x, 
                      height = px(30)) 
        }
    ) %>%
    fmt_percent(
        columns = vars(`1`:`20`),
        decimals = 1
    )  %>%
    data_color(
        columns = vars(`1`:`20`),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction  = 1
            ) %>% as.character(),
            domain = c(0, 1), 
            na.color = "#00441BFF"
        )
    ) %>%
    tab_options(
        column_labels.background.color = "white",
        table.border.top.width = px(3),
        table.border.top.color = "white",
        table.border.bottom.color = "white",
        table.border.bottom.width = px(3),
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "white",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 16,
        heading.align = "left"
    )  %>%
    tab_source_note(
        source_note = md("Table: @steodosescu | Data: football-data.co.uk")
    )




## 3. Power rankings Reactable Table 
power_rankings_df <- simulations %>% 
    group_by(Team) %>% 
    summarise(Points = mean(Pts),
              `Goal Differential` = mean(GD),
              `Average Finish` = mean(as.numeric(Rank))) %>% 
    arrange(`Average Finish`) %>% 
    #Not using the below logo mutate for now
    mutate(logo = paste0('https://github.com/steodose/Club-Soccer-Forecasts/Premier-League-Forecasts/raw/master/logos/', Team, '.png')) %>% 
    select(Team, Points, `Goal Differential`, `Average Finish`)

# Create UCL and Relegation columns in the simulations data
league_table3 <- league_table2 %>%
    mutate(UCL = if_else(Placement == 1 | Placement == 2 | Placement == 3 
                         | Placement == 4, Freq, 0)) %>% 
    mutate(Relegation = if_else(Placement == 18 | Placement == 19 | Placement == 20, Freq, 0)) %>% 
    mutate(`Win Premier League` = if_else(Placement == 1, Freq, 0)) %>% 
    group_by(Team) %>% 
    summarise(UCL = sum(UCL),
              Relegation = sum(Relegation),
              `Win Premier League` = sum(`Win Premier League`),
              Placement = list(Freq))

# Left join with Power Rankings DF
power_rankings_df <- left_join(power_rankings_df, league_table3, by = "Team")

# Clean up Team Mappings dataset
team_mapping <- team_mapping %>% 
    select(url_logo_espn, team) %>% 
    rename(Team = team, logo = url_logo_espn)

# Join with team mappings data
power_rankings_df <- left_join(power_rankings_df, team_mapping, by = "Team")

power_rankings_df <- power_rankings_df %>% 
    mutate(`Goal Diff/90` = `Goal Differential`/38,
           Rank = row_number()) %>% 
    relocate(Rank, logo) %>% 
    select(Rank:Points, `Average Finish`, `Goal Differential`, `Goal Diff/90`, UCL:Placement)


## 4.  Current League Table
home <- df_results_table %>% 
    group_by(HomeTeam) %>% 
    mutate(win = if_else(FTHG > FTAG, 1, 0),
           loss = if_else(FTAG > FTHG, 1, 0),
           draw = if_else(FTHG == FTAG, 1, 0)) %>% 
    summarize(
        GS = sum(FTHG, na.rm = TRUE),
        GC = sum(FTAG, na.rm = TRUE),
        Pts = sum((FTHG > FTAG) * 3 + (FTHG== FTAG) * 1, na.rm = TRUE),
        win = sum(win),
        loss = sum(loss),
        draw = sum(draw),
        MP = n()) %>% 
    ungroup()

away <- df_results_table %>% 
    group_by(AwayTeam) %>%
    mutate(win = if_else(FTAG > FTHG, 1, 0),
           loss = if_else(FTHG > FTAG, 1, 0),
           draw = if_else(FTHG == FTAG, 1, 0)) %>%
    summarize(
        GS = sum(FTAG, na.rm = TRUE),
        GC = sum(FTHG, na.rm = TRUE),
        Pts = sum((FTAG > FTHG) * 3 + (FTHG== FTAG) * 1, na.rm = TRUE),
        win = sum(win),
        loss = sum(loss),
        draw = sum(draw),
        MP = n()) %>% 
    ungroup()

results_summary_table <- inner_join(home, away, by = c("HomeTeam" = "AwayTeam")) %>% 
    group_by(HomeTeam) %>% 
    mutate(
        GS = sum(GS.x, GS.y),
        GC = sum(GC.x, GC.y),
        GD = GS-GC,
        Points = sum(Pts.x, Pts.y),
        MP = sum(MP.x + MP.y),
        win_perc = Points/(MP*3),
        win = sum(win.x, win.y),
        draw = sum(draw.x, draw.y),
        loss = sum(loss.x, loss.y)) %>%
    rename("Squad" = "HomeTeam") %>%
    select(Squad, Points, MP, GS:loss)

results_summary_table2 <- results_summary_table %>% 
    left_join(team_mapping, by = c("Squad" = "Team")) %>% 
    relocate(logo) %>% 
    arrange(desc(Points), desc(GD)) %>% 
    ungroup() %>% 
    mutate(Rank = row_number()) %>% 
    relocate(Rank) %>% 
    select(Rank:loss) %>% 
    mutate(list_data = list(c(win,draw,loss)))
    
# create list_data column for table graphic
results_summary_table3 <- results_summary_table2 %>% 
    gather(attr_num, list_data, c(win,draw,loss)) %>%  #could use pivot_longer here
    group_by_at(vars(-attr_num, -list_data)) %>% 
    summarise(list_data = list(list_data)) %>% 
    ungroup()


# Make current standings table using gt package
summary_table_gt <- results_summary_table3 %>%
    gt() %>%
    text_transform(
        locations = cells_body(vars(logo)),
        fn = function(x) {
            web_image(url = x, 
                      height = px(30)) 
        }
    ) %>%
    data_color(columns = 4,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)
               ) %>%
    #gt_theme_538() %>%
    tab_style(
        style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
        locations = cells_body(rows = 4)
    ) %>%
    tab_style(
        style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
        locations = cells_body(rows = 17)
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "red")
        ),
        locations = cells_body(
            columns = vars(GD),
            rows = GD <= 0
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "blue")
        ),
        locations = cells_body(
            columns = vars(GD),
            rows = GD > 0
        )
    ) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**2022-23 Premier League Table**"),
               subtitle = glue("Thru matches played as of {update_date}.")) %>%
    tab_source_note(
        source_note = md("DATA: www.football-data.co.uk")) %>% 
    gt_plt_bar_pct(column = win_perc, scaled = FALSE, fill = "navy", background = "gray") %>% 
    gt_plt_bar_stack(list_data, width = 65,
                     labels = c("  WINS  ", "  DRAWS  ", "  LOSSES  "),
                     palette= c("#ff4343", "#bfbfbf", "#0a1c2b"))
    


## 5a. GS vs GC plot

#Set aspect ratio for logo based plots
asp_ratio <- 1.618

goal_differential_plot <- results_summary_table2 %>% 
    ggplot(aes(x = GS, y = GC)) + 
    geom_image(aes(image = logo), size = 0.055, by = "width", asp = asp_ratio) +
    geom_hline(yintercept = mean(results_summary_table2$GC), color = "red", linetype = "dashed") +
    geom_vline(xintercept =  mean(results_summary_table2$GC), color = "red", linetype = "dashed") +
    theme_custom() +
    labs(x = "Goals Scored",
         y = "Goals Conceded",
         caption = "Data: www.football-data.co.uk | Plot: @steodosescu",
         title = glue("2022-23 Premier League Scoring Profiles"),
         subtitle = glue("Matches thru **{update_date}**.")) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.subtitle = element_markdown()) +
    scale_y_reverse()


## 5b. Goal Differential Bar Plot
results_summary_table4 <- left_join(results_summary_table3, epl_colors, by = c("Squad" = "team"))
    

goal_differential_barplot <- results_summary_table4 %>% 
    ggplot(aes(x = fct_reorder(Squad, -GD), y = GD)) +
    geom_col(aes(fill = Primary, 
            color = after_scale(clr_darken(fill, 0.3))
        ),
        width = 0.4, 
        alpha = .75,
    ) + 
    scale_color_identity(aesthetics =  c("fill"))  +
    geom_image(
        aes(
            image = logo                                  
        ), 
        size = 0.035, 
        by = "width", 
        asp = asp_ratio
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_custom() + 
    theme(axis.text.x = element_blank(), 
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = 'bold', size = 16), 
          plot.title.position = 'plot') + 
    labs(x = "", 
         y = "Goal Differential", 
         caption = "Data: www.football-data.co.uk | Plot: @steodosescu",
         title = glue("2022-23 Premier League Goal Differential"),
         subtitle = glue("Matches thru **{update_date}**.")) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.subtitle = element_markdown())






# _________________________________________________________________________________________________
##### Define UI for application #####
ui <- tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/PL.png"), #Getting the Premier League logo in the browser window
    
    navbarPage(theme = shinytheme("cosmo"), # Navbar theme at the top
               tags$head(tags$style(HTML('.navbar-static-top {background-color: #3d195b;}',
                                         '.navbar-default .navbar-nav>.active>a {background-color: #3d195b;}'))), #EPL purple theme for navbar
               
               title = tags$div(img(src="epl-logo-white.png", height=28), "2022-23 EPL"),
               tabPanel("Power Rankings", icon = icon("sort"), 
                        h1("English Premier League Power Rankings"),
                        glue("Welcome to our EPL projections and probabilities page where you will find each squad’s projected points total, goal differential, average finish, and its probability of Champions League qualification or relegation. These odds are based on 10,000x simulations of the remainder of the current season. The data are refreshed on Mondays after the week's matches have concluded. Last updated {update_date}."),
                        reactableOutput("table_forecasts"),
                        screenshotButton(id = "table_forecasts")
               ),
               
               ### Second tab
               tabPanel("Standings", icon = icon("signal"),
                        sidebarLayout(position = "left",
                                      sidebarPanel(tags$h3("Stats & Standings"),
                                                 
                                                   "Current Premier League standings and stats. Data is refreshed every Monday and accessed via www.football-data.co.uk.\n Please give the app a second to load.",
                                                   
                                                   br(),
                                      ),
                                      mainPanel(
                        h1("Current League Standings"),
                        gt_output("actuals_table"),
                       # plotOutput("xG_plot", width = "100%", height = "500px"),
                        plotOutput("goal_diff_barplot", width = "100%", height = "600px"),
                        plotOutput("goal_diff_plot", width = "100%", height = "600px")
                                      )
                        ),
               ),
               
               ### Third tab
               tabPanel("Fixtures", icon = icon("futbol"),
                        h1("Match Results"),
                        glue("All data courtesy of www.football-data.co.uk. Matches thru: {update_date}"),
                        reactableOutput("table_results")
               ),
               
               ### Fourth tab
               tabPanel("Forecasts",icon = icon("poll"),
                        h1("Premier League Projections"),
                        "The below shows every Premier League team's chances at particular league finishes and distribution of expected points, according to our model.",
                        
                        br(),
                        br(),
                        sidebarPanel(
                            helpText("Select a team to examine its distribution of predicted league classification."),
                            selectizeInput("teamInput", "Team",
                                           choices = unique(league_table2$Team),  
                                           selected="Man City", multiple = FALSE),
                            width = 4),
                        
                        mainPanel(withSpinner(plotOutput("classification_plot", height = "500px")),
                                  gt_output("table_sims"),
                                  #plotOutput("position_plot", width = "100%", height = "500px"),
                                  plotOutput("points_plot", width = "100%", height = "500px"),
                                  width = 8),
               ),
               
               ### About tab
               tabPanel("About", icon = icon("info-circle"),
                        fluidRow(
                            column(8,
                                   includeMarkdown("About.md")
                            )
                        )
               ),
               
               # Footer for web app
               hr(style = "border-color: #cbcbcb;"),
               fluidRow(
                   column(9,
                          p("App created by ", tags$a(href = "https://steodose.github.io/steodosescu.github.io/", 'Stephan Teodosescu', target = '_blank'),", October 2021", HTML("&bull;"),
                            "Find the code on Github:", tags$a(href = "https://github.com/steodose/NHL-Odds", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 100%"),
                          p("Questions? Comments? Reach out on Twitter", tags$a(href = "https://twitter.com/steodosescu", tags$i(class = 'fa fa-twitter', style = 'color:#1DA1F2'), target = '_blank'), style = "font-size: 100%"),
                          p(tags$em("Last updated: September 2021"), style = 'font-size:85%'))),
               windowTitle = "2022-23 Premier League"
    ) #navbarPage bracket
) #END UI function


# _________________________________________________________________________________________________
##### Define SERVER logic #####
server <- function(input, output) {
    
    #options(shiny.useragg = TRUE)
    
    
    # Reactable forecasts table
    output$table_forecasts <- renderReactable({
        reactable(power_rankings_df,
                  theme = theme_538,
                  columnGroups = list(
                      colGroup(name = "Average of 10,000x simulations", 
                               columns = c("Points", "Average Finish", "Goal Differential", "Goal Diff/90")),
                      colGroup(name = "End-of-Season Probabilities", 
                               columns = c("UCL", "Relegation","Win Premier League", "Placement"))
                  ),
                  showSortIcon = TRUE,
                  searchable = TRUE,
                  language = reactableLang(
                      searchPlaceholder = "SEARCH FOR A TEAM..."),
                  defaultPageSize = 100,
                  columns = list(
                      Team = colDef(name = "Team",
                                    minWidth = 120,
                                    align = "left"),
                      Points = colDef(name = "Proj. Points",
                                      align = "right",
                                      style = color_scales(power_rankings_df, colors = my_color_pal),
                                      format =  colFormat(digits = 0)),
                      `Goal Differential` = colDef(name = "Proj. Goal Diff",
                                                   align = "right",
                                                   format =  colFormat(digits = 0)),
                      `Goal Diff/90` = colDef(name = "Proj. GD/90",
                                                   align = "right",
                                              style = list(borderRight = "2px solid #555"),
                                              format =  colFormat(digits = 1)),
                      `Average Finish` = colDef(name = "Proj. Finish",
                                                align = "right",
                                                format =  colFormat(digits = 1)),
                      `UCL` = colDef(name = "UCL (%)",
                                     align = "right",
                                     style = color_scales(power_rankings_df, colors = paletteer::paletteer_d(
                                         palette = "ggsci::amber_material"
                                     )),
                                     format =  colFormat(percent = TRUE)),
                      `Relegation` = colDef(name = "Relegation (%)",
                                            align = "right",
                                            style = color_scales(power_rankings_df, colors = paletteer::paletteer_d(
                                                palette = "ggsci::amber_material"
                                            )),
                                            format =  colFormat(percent = TRUE)),
                      `Win Premier League` = colDef(name = "Win PL (%)",
                                                    align = "right",
                                                    style = color_scales(power_rankings_df, paletteer::paletteer_d(
                                                        palette = "ggsci::amber_material"
                                                    )),
                                                    format =  colFormat(percent = TRUE)),
                      Placement = colDef(cell = function(values) {
                          sparkline(values, type = "bar", barColor = "#BF5700", chartRangeMin = 0, chartRangeMax = 1)
                      }),
                      
                      ### add logos using embed_img()
                      logo = colDef(
                          name = "",
                          maxWidth = 70,
                          align = "center",
                          cell = embed_img(height = "25", width = "28")
                      )),
                  pagination = FALSE,
                  compact = TRUE, 
                  borderless = FALSE, 
                  striped = FALSE,
                  fullWidth = FALSE, 
                  defaultColDef = colDef(align = "center", minWidth = 95)
                  )
    })
    
    
    # Reactable summary of results table
    
    output$table_results <- renderReactable({
        reactable(df_results_table,
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
                  pagination = TRUE,
                  compact = TRUE, 
                  borderless = FALSE, 
                  striped = FALSE,
                  fullWidth = FALSE, 
                  defaultColDef = colDef(align = "center", minWidth = 120),
        )
    })
    
    # Current standings gt table
    output$actuals_table <-
        render_gt(
            expr = summary_table_gt,
            height = px(900),
            width = px(900)
        )
    
    # Goal Differential barplot
    output$goal_diff_barplot <- renderPlot({
        plot(goal_differential_barplot)
    })
    
    # Goals Scored vs Goals Conceded plot
    output$goal_diff_plot <- renderPlot({
        plot(goal_differential_plot)
    })
    
    # ggridges points plot
    output$points_plot <- renderPlot({
        plot(points_forecast)
    })
    
    
    # Heatmap position plot using gt
    output$table_sims <-
        render_gt(
            expr = gt_sims
        )
    
    # Classification bar plot reactive
    d <- reactive({
        league_table2 %>%
            filter(Team == input$teamInput)   
        
    }) 
    
    #Render classification bar plot
    output$classification_plot <- renderPlot({
        
        ggplot(d(), aes(x = Placement, y = Freq, fill = Team)) +
            geom_chicklet(position = "dodge", stat = "identity", alpha = 0.6) +
            geom_text(aes(label = percent(Freq, accuracy = 1L),
                          vjust=-0.25)) +
            scale_fill_manual(values = '#ff0078') +
            labs(
                x = "Predicted League Finish", y = "Chance (%)",
                title = "2022-23 Premier League",
                subtitle = glue("Probabilities of particular placements in the league table. Data as of {update_date}."),
                caption = "Data: www.football-data.co.uk\nGraphic: @steodosescu"
            ) +
            theme(plot.title = element_text(face = "bold"),
                  axis.text.y=element_blank()) +
            theme(plot.subtitle = element_markdown()) +
            theme(legend.position = "none")
        
    })
    
} #END Server function


##### Run the application #####
shinyApp(ui = ui, server = server)