##### Premier League Forecasts App #####
## By: Stephan Teodosescu

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyscreenshot)

library(tidyverse)
library(teamcolors)
library(gt) #for 538-themed tables
library(gtExtras)
library(extrafont) #for adding in new fonts for R graphics
library(ggtext)
library(RCurl)
library(magick)
library(ggimage) #for working with team logos
library(webshot) #saving high quality images of gt tables
library(glue)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(ggridges)
library(ggsci) #for positional heatmap

##### Load datasets #####

# Load 2020-21 Premier League Game Data from football-data.uk.com
df <- read.csv("http://www.football-data.co.uk/mmz4281/2122/E0.csv", 
               stringsAsFactors = FALSE)

# Simulation results output from Sims script (outputted using GitHub actions)
simulations <- 'https://raw.githubusercontent.com/steodose/Club-Soccer-Forecasts/main/simulations.csv' %>% 
    read_csv()

# Load team mappings
team_mapping <- read_csv("team_mapping.csv")


##### Data Pre-processing #####

# Fetch current date for updating visualizations
update_date <- max(df$Date) %>% 
    format(format="%B %d")

iSim <- 10000 #Define iSim from sims script

# Load font from google fonts
htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=Chivo:400,600,700&display=swap", rel = "stylesheet")

# Define 538 table theme for Reactable table(s) below
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
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
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
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        ) 
}

# Define color palette to use in tables
my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")

# Initialize EPL team colors (using this method for 2021-22)
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


##### Plots for publication in App #####

## 1. Points Plot using ggridges package
points_forecast <- ggplot(simulations, aes(x = Pts, y = fct_reorder(Team, Pts), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Points", y = "",
         title = glue("2021-22 Premier League: Thru matches as of {update_date}."),
         subtitle = "Distribution of expected points after simulating the remainder of the season 10,000x",
         caption = "Data: football-data.co.uk"
    ) +
    theme_bw() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    scale_fill_manual(values = epl_colors$Primary)


## 2. Probability by position table
league_table <- table(simulations$Team, simulations$Rank)/iSim

league_table2 <- as.data.frame(league_table) %>% # Convert table to data frame
    rename(Team = Var1, Placement = Var2) %>% 
    arrange(Placement, desc(Freq, Placement))

league_table2$Freq <- round(league_table2$Freq, 2)

position_heatmap <- ggplot(league_table2, aes(x = Placement, y = reorder(Team, Freq, max))) + 
    geom_tile(aes(fill = Freq), colour = "white") + 
    scale_fill_material("deep-orange") +
    labs(x = "", y = "",
         title = glue("2021-22 Premier League Forecasts"),
         subtitle = glue("Chances (%) of particuar league finishes after simulating the remainder of the season 10,000x. Thru matches as of {update_date}."),
         caption = "Data: football-data.co.uk") +
    scale_x_discrete(position = "top") +
    theme(legend.position = "none") +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    geom_text(aes(label = Freq))


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
              `Win Premier League` = sum(`Win Premier League`))

# Left join with Power Rankings DF
power_rankings_df <- left_join(power_rankings_df, league_table3, by = "Team")

# Clean up Team Mappings dataset
team_mapping <- team_mapping %>% 
    select(url_logo_espn, team) %>% 
    rename(Team = team, logo = url_logo_espn)

# Join with team mappings data
power_rankings_df <- left_join(power_rankings_df, team_mapping, by = "Team")

power_rankings_df <- power_rankings_df %>% 
    relocate(logo)


## 4.  Current league table
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
    data_color(columns = 4,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)
               ) %>%
    gt_theme_538() %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**2021-22 Premier League Table**"),
               subtitle = glue("Thru matches played as of {update_date}.")) %>%
    tab_source_note(
        source_note = md("DATA: www.football-data.co.uk")) %>% 
    gt_plt_bar_pct(column = win_perc, scaled = FALSE, fill = "navy", background = "gray") %>% 
    gt_plt_bar_stack(list_data, width = 65,
                     labels = c("  WINS  ", "  DRAWS  ", "  LOSSES  "),
                     palette= c("#ff4343", "#bfbfbf", "#0a1c2b"))
    

# _________________________________________________________________________________________________
##### Define UI for application #####
ui <- tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/PL.png"), #Getting the Premier League logo in the browser window
    
    navbarPage(theme = shinytheme("cosmo"), # Navbar theme at the top
               title = tags$div(img(src="PL_white.png", height=28), "2021-22 EPL"),
               tabPanel("Power Rankings", icon = icon("sort"), 
                        h1("English Premier League Power Rankings"),
                        glue("Welcome to our EPL projections and probabilities page where you will find each squad’s projected points total, goal differential, average finish, and its probability of Champions League qualification or relegation. These odds are based on 10,000x simulations of the remainder of the current season. The data are refreshed on Mondays after the week's matches have concluded. Last updated {update_date}."),
                        reactableOutput("table_forecasts"),
                        screenshotButton(id = "table_forecasts")
               ),
               
               # Second tab
               tabPanel("Standings", icon = icon("signal"),
                        h1("Current League Standings"),
                        gt_output("actuals_table")),
               
               
               # Third tab
               tabPanel("Results", icon = icon("futbol"),
                        h1("Match Results"),
                        glue("All data courtesy of www.football-data.co.uk. Matches thru: {update_date}"),
                        reactableOutput("table_results")
               ),
               
               # Fourth tab
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
                        
                        mainPanel(withSpinner(plotOutput("classification_plot")), 
                                  plotOutput("points_plot", width = "100%"),
                                  plotOutput("position_plot", width = "100%"), 
                                  width = 8),
               ),
               
               # About info tab
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
               windowTitle = "2021-22 Premier League"
    ) #navbarPage bracket
) #END UI function


# _________________________________________________________________________________________________
##### Define SERVER logic required #####
server <- function(input, output) {
    
    # Reactable forecasts table
    output$table_forecasts <- renderReactable({
        reactable(power_rankings_df,
                  theme = theme_538,
                  columnGroups = list(
                      colGroup(name = "Average of 10,000x simulations", 
                               columns = c("Points", "Goal Differential","Average Finish","UCL",
                                           "Relegation","Win Premier League"))
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
                      `Average Finish` = colDef(name = "Proj. Finish",
                                                align = "right",
                                                format =  colFormat(digits = 1)),
                      `UCL` = colDef(name = "UCL (%)",
                                     align = "right",
                                     style = list(borderLeft = "2px solid #555"),
                                     format =  colFormat(digits = 2)),
                      `Relegation` = colDef(name = "Relegation (%)",
                                            align = "right",
                                            format =  colFormat(digits = 2)),
                      `Win Premier League` = colDef(name = "Win PL (%)",
                                                    align = "right",
                                                    format =  colFormat(digits = 2)),
                      
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
                                            name = "PH",
                                            cell = data_bars(df_results_table, 
                                                             fill_color = "dodgerblue",
                                                             background = "lightgrey",
                                                             fill_opacity = 0.8,
                                                             max_value = 100,
                                                             scales::number_format(accuracy = 0.1))),
                      `B365D_prob` = colDef(maxWidth = 400,
                                            align = "right",
                                            name = "PD",
                                            cell = data_bars(df_results_table, 
                                                             fill_color = "dodgerblue",
                                                             background = "lightgrey",
                                                             fill_opacity = 0.8,
                                                             max_value = 100,
                                                             scales::number_format(accuracy = 0.1))),
                      `B365A_prob` = colDef(maxWidth = 400,
                                            align = "right",
                                            name = "PA",
                                            cell = data_bars(df_results_table, 
                                                             fill_color = "dodgerblue",
                                                             background = "lightgrey",
                                                             fill_opacity = 0.8,
                                                             max_value = 100,
                                                             scales::number_format(accuracy = 0.1))),
                      `logo_home` = colDef(cell = function(value) {
                          image <- img(src = sprintf("logos/%s.png", value), height = "24px", alt = value)
                          tagList(
                              div(style = list(display = "inline-block", width = "45px"), image),
                              value
                          )
                      })
                  ),
                  
                  pagination = TRUE,
                  compact = TRUE, 
                  borderless = FALSE, 
                  striped = FALSE,
                  fullWidth = FALSE, 
                  defaultColDef = colDef(align = "center", minWidth = 120),
        ) %>% 
            add_title("2021-22 Premier League Fixtures") %>% 
            add_subtitle(glue("Data as of {update_date}"),font_size = 18) %>% 
            add_source("Data: www.football-data.co.uk", font_size = 12)
    })
    
    # Current standing gt table
    output$actuals_table <-
        render_gt(
            expr = summary_table_gt,
            height = px(700),
            width = px(900)
        )
    
    # ggridges points plot
    output$points_plot <- renderPlot({
        plot(points_forecast)
    })
    
    
    # Heatmap position plot
    output$position_plot <- renderPlot({
        plot(position_heatmap)
    })
    
    # Classification bar plot reactive
    d <- reactive({
        league_table2 %>%
            filter(Team == input$teamInput)   
        
    }) 
    
    #Render classification bar plot
    output$classification_plot <- renderPlot({
        
        ggplot(d(), aes(x = Placement, y = Freq, fill = Team)) +
            geom_bar(position = "dodge", stat = "identity") +
            geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25) +
            labs(
                x = "Predicted League Finish", y = "Chance (%)",
                title = "2021-22 Premier League",
                subtitle = glue("Probabilities of particular placements in the league table. Data as of {update_date}."),
                caption = "Data: www.football-data.co.uk\nGraphic: @steodosescu"
            ) +
            theme(plot.title = element_text(face = "bold")) +
            theme(plot.subtitle = element_markdown()) +
            theme(legend.position = "none")
        
    })
    
    
} #END Server function


##### Run the application #####
shinyApp(ui = ui, server = server)