library(readxl)
library(stringr)
library(tidyr)
library(dplyr)
library(shiny)
library(ggplot2)

file_path <- "adultsmokinghabitsingreatbritain.xlsx"
df0 <- read_excel(file_path, sheet = "Table_1", range = "A9:T45")
colnames(df0) <- gsub("\r\n", "", colnames(df0))

pattern_gender <- "men|women|all persons"
pattern_age <- "\\d{2} to \\d{2}|\\d{2} and over"
name_target = "prop_cigarette_smokers"
df <- as.data.frame(df0) %>%
  pivot_longer(cols = names(df0)[-c(1, 2)], names_to = "group", values_to = name_target) %>%
  rename("weight" = "Weight [note 4]",
         "year0" = "Year [note 5]") %>%
  filter(weight == "Weighted") %>%
  mutate(year = str_extract(year0, "\\d{4}"),
         gender = str_extract(tolower(group), pattern_gender),
         age = str_extract(tolower(group), pattern_age)) %>%
  select(any_of(c("year", "gender", "age", name_target)))

rm(list = setdiff(ls(), "df"))


ui <- fluidPage(
  tabsetPanel(
    tabPanel("", title = "By gender",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "gender1",
                                    label = "Gender",
                                    choices = unique(df$gender),
                                    selected = "all persons",
                                    inline=FALSE),
                 selectInput(inputId = "age1",
                             label = "Age group",
                             choices = unique(df$age),
                             selected = "16 and over"),
                 sliderInput(inputId = "year1",
                             label = "Year",
                             min = 2000, max = 2022,
                             value = c(2005, 2015), step = 1)),
               mainPanel(plotOutput("plot1"))
             ))
    ,
    tabPanel("", title = "By age group",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "gender2",
                             label = "Gender",
                             choices = unique(df$gender),
                             selected = "all persons"),
                 checkboxGroupInput(inputId = "age2",
                                    label = "Age group",
                                    choices = unique(df$age),
                                    selected = "16 and over",
                                    inline=FALSE),
                 sliderInput(inputId = "year2",
                             label = "Year",
                             min = 2000, max = 2022,
                             value = c(2005, 2015), step = 1)),
               mainPanel(plotOutput("plot2"))
             ))
  )
)

server <- function(input, output){
  output$plot1 <- renderPlot({
    df %>%
      filter(year >= input$year1[1], year <= input$year1[2],
             gender %in% input$gender1,
             age == input$age1) %>%
      ggplot() +
      aes(x = year, y = prop_cigarette_smokers,
          group = gender, color=gender) + geom_line() +
      labs(title = "Proportion of Cigarette Smokers Over Years",
           x = "Year",
           y = "Proportion of Cigarette Smokers",
           color = "Gender") +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14))
  })

  output$plot2 <- renderPlot({
    df %>%
      filter(year >= input$year2[1], year <= input$year2[2],
             gender == input$gender2,
             age %in% input$age2) %>%
      ggplot() +
      aes(x = year, y = prop_cigarette_smokers,
          group = age, color=age) + geom_line() +
      labs(title = "Proportion of Cigarette Smokers Over Years",
           x = "Year",
           y = "Proportion of Cigarette Smokers",
           color = "Age group")
  })
}

# Run the app
shinyApp(ui, server)
