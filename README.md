# Soccer-Analytics
This repository is where I'll be putting all my practice code and analysis. Sometimes it will be messy. I'm working on commenting out my work better so that other people who look at it won't be so confused....because even I get confused about it sometimes!

Also, I'm not great at using GitHub so this practice for me, as well.

# Pythagorean Formula Work
### Pythagorean Formula Work
library(tidyverse)
library(magrittr)
library(worldfootballR)
library(regista)
library(goalmodel)
library(ggrepel)
library(bbplot)
install.packages("paletteer")
library(paletteer)
library(scales)

# Get match results for this year so far
EFL_Match_Results <- get_match_results(country = "ENG", gender = "M", season_end_year = 2022, 
                                       tier = "4th")
View(EFL_Match_Results)

# select proper variables from dataset
EFL_Table <- EFL_Match_Results %>%
  select(Wk, Date, Home, HomeGoals, Away, AwayGoals)
View(EFL_Table)

# get table 
EFL_L2 <- get_season_team_stats(country = "ENG", gender = "M",
                                                season_end_year = "2022", 
                                                tier = "4th",
                                                stat_type = "league_table_home_away")
View(EFL_L2)

# get the league table
# this data set can be used to compare total goal differential too
EFL_Table <- EFL_L2 %>%
  select(Squad, Rk, GF_Home, GF_Away, GA_Home, GA_Away, Pts_Home, Pts_Away, W_Home, W_Away, L_Home, L_Away, D_Home, D_Away)
View(EFL_Table)
# create Total Points variable
EFL_Table <- EFL_Table %>%
  mutate(Points = Pts_Away + Pts_Home)

# use formattable to create rankings
library(formattable)
formattable(EFL_Table %>%
              select(Squad, Rk, Points) %>%
              arrange(Rk),
            align = c("l", "c", "c"))
install.packages("ztable")
library(ztable)
print(ztable(EFL_Table %>%
         select(Squad, Rk, Points) %>%
         arrange(Rk)))
library(magrittr)
options(ztable.type = "html")
z <- ztable(EFL_Table %>%
              select(Squad, Rk, Points) %>%
              arrange(Rk))
print(z)
library(DT)
options(DT.options = list(pagelength = 10))
datatable(EFL_Table %>%
            select(Squad, Rk, Points) %>%
            arrange(Rk))
install.packages("reactable")
library(reactable)
library(htmltools)
install.packages("hrbrthemes")
library(hrbrthemes)
EFL_Viz <- EFL_Table %>%
  select(Squad, Rk, Points) %>%
  arrange(Rk) 

# EFL table sorted by position
reactable(
  EFL_Viz,
  defaultSortOrder = 'asc',
  defaultSorted = 'Rk',
  showSortIcon = TRUE,
  compact = TRUE,
  pagination = FALSE
)
# function for adding colors to columns
green_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
good_color <- green_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
# generate vector of example numbers between 2 and 32
good_color(seq(0.1, 0.9, length.out = 12))
# display colors
seq(0.1, 0.9, length.out = 12) %>%
  good_color() %>%
  scales::show_col()



# Add style and headers
reactable(
  EFL_Viz,
  defaultSortOrder = 'asc',
  defaultSorted = 'Rk',
  showSortIcon = TRUE,
  compact = TRUE,
  pagination = FALSE,
  defaultColDef = colDef(
    headerStyle = list(
      textAlign = "left",
      fontSize = "11px",
      lineHeight = "14px",
      textTransform = "uppercase",
      color = "#0c0c0c",
      fontWeight = "500",
      borderBottom = "2px solid #e9edf0",
      paddingBottom = "3px",
      verticalAlign = "bottom",
      fontFamily = font_es
    ),
    style = list(
      fontFamily = font_es,
      fontSize = "14px",
      verticalAlign = "center",
      align = "left"
    )
  ),
  # Add coloring to Points column
  columns = list(
    Points = colDef(
      name = "Points",
      style = function(value) {
        value
        normalized <- (value - min(EFL_Viz$Points)) / (max(EFL_Viz$Points) - min(EFL_Viz$Points))
        color <- good_color(normalized)
        list(background = color)
      }
    )
  )
  )

# Additional formatting of the table
tbl <- EFL_Viz %>% 
  reactable(
    pagination = FALSE,
    compact = TRUE,
    borderless = FALSE,
    striped = TRUE,
    fullWidth = FALSE,
    defaultColDef = colDef(
      align = "center",
      minWidth = 100
    ),
    # Add theme for top border
    theme = reactableTheme(
      headerStyle = list(
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%"),
        "&[aria-sort = 'ascending'], &[aria-sort = 'descending']" = list(background = "hsl(0, 0%, 96%)"),
        borderColor = "#555"
      )
    ),
    columns = list(
      Points = colDef(
        name = "Points",
        style = function(value) {
          value
          normalized <- (value - min(EFL_Viz$Points)) / (max(EFL_Viz$Points) - min(EFL_Viz$Points))
          color <- good_color(normalized)
          list(background = color)
        }
      ),
      # Create similar function to color the Rk column
      RK = colDef(
        style = function(value) {
          value
          normalized <- (value - min(EFL_Viz$Rk)) / (max(EFL_Viz$Rk) - min(EFL_Viz$Rk))
          color <- good_color(normalized)
          list(background = color)
        },
        # add border to left of the column
        class = "border-left"
      ),
      # change width of squad column
      Squad = colDef(
        minWidth = 150,
        align = "left"
      )
    )
  )
div(
    class = "Points",
    div(
      class = "title",
      h2("EFL League 2 Table"),
      "As of 11.8.21"
      ),
    tbl
  )
View(EFL_Viz)

# Compare Goal Differential with the actual League Table
# Create new data frame with teams and goals
EFL_GD <- EFL_Table %>%
  select(Squad, GF_Home, GF_Away, GA_Home, GA_Away)
View(EFL_GD)

# Create new column 'GoalsFor', 'GoalsAgainst' and 'Goal_Diff'
EFL_GD <- EFL_GD %>%
  mutate(Goals_For = GF_Home + GF_Away,
         Goals_Against = GA_Away + GA_Home,
         Goal_Diff = Goals_For - Goals_Against)

# Goal Differential table using code to create similar viz

# function for adding colors to columns
green_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
good_color <- green_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
# generate vector of example numbers between 2 and 32
good_color(seq(0.1, 0.9, length.out = 12))
# display colors
seq(0.1, 0.9, length.out = 12) %>%
  good_color() %>%
  scales::show_col()

EFL_GD_Viz <- EFL_GD %>%
  select(Squad, Goal_Diff)

# Add style and headers
reactable(
  EFL_GD_Viz,
  defaultSortOrder = 'asc',
  defaultSorted = 'Goal_Diff',
  showSortIcon = TRUE,
  compact = TRUE,
  pagination = FALSE,
  defaultColDef = colDef(
    headerStyle = list(
      textAlign = "left",
      fontSize = "11px",
      lineHeight = "14px",
      textTransform = "uppercase",
      color = "#0c0c0c",
      fontWeight = "500",
      borderBottom = "2px solid #e9edf0",
      paddingBottom = "3px",
      verticalAlign = "bottom",
      fontFamily = font_es
    ),
    style = list(
      fontFamily = font_es,
      fontSize = "14px",
      verticalAlign = "center",
      align = "left"
    )
  ),
  # Add coloring to Points column
  columns = list(
    Points = colDef(
      name = "Goal_Diff",
      style = function(value) {
        value
        normalized <- (value - min(EFL_GD_Viz$Goal_Diff)) / (max(EFL_GD_Viz$Goal_Diff) - min(EFL_GD_Viz$Goal_Diff))
        color <- good_color(normalized)
        list(background = color)
      }
    )
  )
)

# Additional formatting of the table
tbl <- EFL_GD_Viz %>% 
  reactable(
    pagination = FALSE,
    compact = TRUE,
    borderless = FALSE,
    striped = TRUE,
    fullWidth = FALSE,
    defaultColDef = colDef(
      align = "center",
      minWidth = 150
    ),
    # Add theme for top border
    theme = reactableTheme(
      headerStyle = list(
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%"),
        "&[aria-sort = 'ascending'], &[aria-sort = 'descending']" = list(background = "hsl(0, 0%, 96%)"),
        borderColor = "#555"
      )
    ),
    columns = list(
      Goal_Diff = colDef(
        name = "Goal_Diff",
        style = function(value) {
          value
          normalized <- (value - min(EFL_GD_Viz$Goal_Diff)) / (max(EFL_GD_Viz$Goal_Diff) - min(EFL_GD_Viz$Goal_Diff))
          color <- good_color(normalized)
          list(background = color)
        }
      ),
        # add border to left of the column
        class = "border-left"
    )
    )
  
tbl

# Simple linear formula for Pythagorean Expectation
# PredictPoints = 0.677(GD) + (52.29) * (GP/38)
# Create total matches played in EFL_L2
EFL_L2 <- EFL_L2 %>%
  mutate(Total_Matches = MP_Home + MP_Away)

# Predicted points for Leyton Orient
LeytonOrient <- (0.677 * 13) + (52.29) * (15/38)
LeytonOrient
FGRovers <- (0.677 * 14) + (52.29) * (15/38)
FGRovers
PortVale <- (0.677 * 12) + (52.29) * (15/38)
PortVale
Northampton <- (0.677 * 9) + (52.29) * (15/38)
Northampton
ExeterCity <- (0.677 * 10) + (52.29) * (15/38)
ExeterCity
SwindonTown <- (0.677 * 7) + (52.29) * (15/38)
SwindonTown

# Simple Soccer Stats model
# PtsPerGame = 1.7 * (GS-GA) / (GS + GA) + 1.35

LeytonOrient <- 1.7 * (EFL_GD$Goals_For - EFL_GD$Goals_Against) / (EFL_GD$Goals_For + EFL_GD$Goals_Against) + 1.35
LeytonOrient <- sum(LeytonOrient)
LeytonOrient
FGRovers <- 1.7 * (EFL_GD$Goals_For - EFL_GD$Goals_Against) / (EFL_GD$Goals_For + EFL_GD$Goals_Against) + 1.35
FGRovers <- sum(FGRovers)
FGRovers
LeytonOrient <- 1.7 * (13) / (37) + 1.35 * (15)
LeytonOrient

# Find an underperforming team
Hartlepool <- 1.7 * (-3) / (37) + 1.35 * (15)
Hartlepool
Scunthorpe <- 1.7 * (-18) / (40) + 1.35 * (15)
Scunthorpe
PortVale <- 1.7 * (12) / (42) + 1.35
PortVale
PortVale * 15

# Add PythExp as a column
EFL_GD$Points <- EFL_Table$Points # Add a points column to EFL_GD
EFL_GD <- EFL_GD %>%
  mutate(ExpectedPoints = (1.7) * (EFL_GD$Goal_Diff) / (EFL_GD$Goals_For + EFL_GD$Goals_Against) + (1.35) * (46))

# Penalty Site PythExp
# define the exponents
a <- 1.22777 # exponent of goals for
b <- 1.072388 # exponent for goals for in sum of GF and GA
c <- 1.127248 # exponent for GA
n <- 15 # number of matches played

x <- (EFL_GD$Goals_For^a / (EFL_GD$Goals_For^b + EFL_GD$Goals_Against^c)) * 2.499973 * n
x
EFL_GD$ExpectedPoints <- (EFL_GD$Goals_For^a / (EFL_GD$Goals_For^b + EFL_GD$Goals_Against^c)) * 2.499973 * n
