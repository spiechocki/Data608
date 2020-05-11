library(ggplot2)
library(dplyr)
library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(personograph)
library(tibble)
library(markdown)
library(maps)
library(DT)

world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)

df <- read.csv('https://raw.githubusercontent.com/spiechocki/Data608/master/FinalProject/Billionaires.csv')

billion <- df[c(1:4, 6:11, 18:19, 23)]

billion$name <- as.character((billion$name))
billion$age_new  <- ifelse(!is.na(billion$age) & billion$age == 0, NA, billion$age)
billion$age_new  <- ifelse(!is.na(billion$age_new) & billion$age_new < 0, -1 * billion$age_new, billion$age_new)

billion$gender <- as.character((billion$gender))
billion$gender <- ifelse(grepl('family|Family', billion$name), 'family', ifelse(grepl('married', billion$gender), 'family', billion$gender))
billion$gender[billion$gender == ""] <- 'unknown'
billion$gender <- as.factor((billion$gender))

billion$typeofwealth <- as.character((billion$typeofwealth))
billion$typeofwealth[billion$typeofwealth == ""] <- 'unknown'
billion$typeofwealth <- as.factor((billion$typeofwealth))

billion$selfmade <- as.character((billion$selfmade))
billion$selfmade[billion$selfmade == ""] <- NA
billion$selfmade <- as.factor((billion$selfmade))

billion$sector <- as.character((billion$sector))
billion$sector[billion$sector == ""] <- NA
billion$sector <- as.factor((billion$sector))

billion$age_cat <- cut(billion$age_new, c(seq(0, 100, by = 10), Inf), include.lowest = TRUE, ordered_result = TRUE)
billion$year <- as.factor(billion$year)

# age
df_age <- billion %>% filter(!is.na(billion$age_cat)) %>%
  group_by(year=factor(year), age_cat) %>%
  tally %>% mutate(pct=(n/sum(n)))

# self-made
df_self <- billion %>% filter(!is.na(billion$selfmade)) %>%
  group_by(year=factor(year), selfmade) %>%
  tally %>% mutate(pct=(n/sum(n)))

# gdp
gdp_percent <- billion %>% filter(complete.cases(.)) %>% group_by(year, citizenship) %>% 
  summarise(gdp1 = mean(gdpcurrentus / 1000000000), tot_net = sum(networthusbillion)) %>%
  group_by(year) %>% summarize(gdp_yr = sum(tot_net) / sum(gdp1))

#ind
categories <- unique(billion$industry)

ui <- fluidPage(
  titlePanel(column(9,div(style = "height:30px; font-size:30px;", 'So You Want to Be a Billionaire?'), offset = 3), windowTitle = "BeABillionaire"),
  fluidRow(tags$br()),
  fluidRow(tags$hr()),
  
  navlistPanel(
   tabPanel("Home",fluidRow(column(7, includeMarkdown("https://raw.githubusercontent.com/spiechocki/Data608/master/FinalProject/About_Billion.md")))),
    tabPanel("Demographics",
             fluidRow(column(5,div(style = "height:20px; font-size:20px;","Are Billionaires Mostly Men or Women?", offset = 4)),
                      column(5,div(style = "height:20px; font-size:20px;","How Old Are Billionaires?"))),
             fluidRow(tags$br()),
             fluidRow(column(5, h5("The percent of billionaires that are females has increased, but billionaires are still overwhelmingly male.")),
                      column(5, h5("Most billionaires are between 50 and 70 years old, but recently the percent of 40-50 year-old billionaires has increased."))),
             fluidRow(tags$br()),
             fluidRow(
               column(5,
                      radioButtons("gen_year", "Choose Year:",
                                   c("1996" = "1996",
                                     "2001" = "2001",
                                     "2014" = "2014"), inline = TRUE))
             ),
             fluidRow(
               column(4,plotOutput('plot_gen',  height=350)), column(5,plotlyOutput('plot_age',  height=400), offset = 1)
             ),
             fluidRow(tags$br()),
             fluidRow(tags$hr())
             ),

    tabPanel("Wealth",
             fluidRow(column(6,div(style = "height:20px; font-size:20px;","Where Do Billionaires Get Their Money?")),
                      column(4,div(style = "height:20px; font-size:20px;","How Wealthy Are Billionaires?", offset = 1))),
             fluidRow(tags$br()),
             fluidRow(column(5, h5("Over time, the percent of billionaires that are self-made has increased, while the percent inheriting their wealth has decreased.")),
                      column(4, h5("The wealth of billionaires has increased greatly from 1996 to 2014. The rich do get richer."), offset = 1)),
             fluidRow(tags$br()),
             fluidRow(
               column(5,plotlyOutput('plot_src', height=400)), column(4,dataTableOutput('plot_gdp'), offset = 1)
             ),
             fluidRow(tags$br()),
             
             fluidRow(tags$hr())
             ),
    tabPanel("Industries",
             fluidRow(column(6,div(style = "height:20px; font-size:20px;","What Industries Are Billionaires In?", offset = 4))),
             fluidRow(tags$br()),
             fluidRow(column(6, h5("The consumer goods sector has the most billionaires in each of the years.  In later years, real estate and technology are giving rise to more of the super wealthy."))),
             fluidRow(
               column(6,
                      radioButtons("ind_year", "Choose Year, then drill down by clicking in a sector:",
                                   c("1996" = "1996",
                                     "2001" = "2001",
                                     "2014" = "2014"), inline = TRUE), offset = 1)
             ),
             fluidRow(column(5,plotlyOutput("industry", height = 500)), column(4,uiOutput("plot.ui") )),  
             # fluidRow(uiOutput("plot.ui")),
          #   fluidRow(column(6,plotlyOutput("sector" , height = 1200))), 
             fluidRow(column(1, uiOutput("back"), offset = 5))
             ),
    tabPanel("Locations",
             fluidRow(column(6,div(style = "height:20px; font-size:20px;","Where Do Billionaires Live?", offset = 4))),
             fluidRow(tags$br()),
             fluidRow(column(7, h5("Billionaires' locations are expanding over time.  More countries can say they are home to a billionaire in 2014 than in 1996. However, more billionaires call the United States home than any other country, by far. "))),
             fluidRow(
               column(6,
                      radioButtons("geo_year", "Choose Year:",
                                   c("1996" = "1996",
                                     "2001" = "2001",
                                     "2014" = "2014"), inline = TRUE))
             ),
             fluidRow(column(8,plotlyOutput("countries", height = 600)))
    ),  widths = c(2, 10), fluid = TRUE
  )
)

axis_titles <- . %>%
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Count of Billionaires")
  )

server <- shinyServer(function(input, output, session) {
  
  selecteddata <- reactive({
    sex_yr <- billion %>% filter(year == input$gen_year) %>% group_by(gender) %>% summarise(count = n() ) %>%
      mutate( prop = round(count / sum(count),2) ) %>% arrange(prop)
  })
  
  selecteddata2 <- reactive({
    setNames(as.list(selecteddata()$prop), as.character(selecteddata()$gender))
  })

  output$plot_gen <- renderPlot({
    personograph(selecteddata2(), colors=list(unknown="gray", male="blue", female="pink", family="green"), fig.title = input$gen_year, icon.style = 9)
  })
  
  output$plot_age <- renderPlotly({
    g1 <- ggplot(df_age, aes(x = age_cat, y = pct, text = paste("Age: ", age_cat, "<br>", scales::percent(pct)))) + 
      geom_col(position = 'dodge', aes(fill = year)) + facet_grid(~year)  +
      scale_y_continuous(labels=scales::percent)  + 
      theme_bw() + theme( strip.background  = element_blank(),
                          strip.text.x = element_text(size = 12),
                          panel.grid.major = element_line(colour = "grey80"),
                          panel.border = element_blank(),
                          panel.grid.minor.x=element_blank(),
                          panel.grid.major.x=element_blank(),
                          axis.ticks = element_blank(),
                          axis.title.y = element_text(size = 12),
                          axis.title.x = element_text(size = 12),
                          axis.text.x = element_text(angle = 90, size = 8),
                          axis.text.y = element_text(size = 10),
                          legend.position = "none") +
      xlab("\n \n Age Category") + ylab("Billionaires (%) \n \n")
    ggplotly(g1, tooltip = "text")
  })
  
  output$plot_src <- renderPlotly({
    s1<- ggplot(df_self, aes(x = selfmade, y = pct, text = paste("",selfmade, "<br>", scales::percent(pct)))) +
      geom_col(position = 'dodge', aes(fill = year)) + facet_grid(~year) +
      scale_y_continuous(labels=scales::percent) +
      theme_bw() + theme( strip.background  = element_blank(),
                          strip.text.x = element_text(size = 12),
                          panel.grid.major = element_line(colour = "grey80"),
                          panel.border = element_blank(),
                          panel.grid.minor.x=element_blank(),
                          panel.grid.major.x=element_blank(),
                          axis.ticks = element_blank(),
                          axis.title.y = element_text(size = 12),
                          axis.title.x = element_text(size = 12),
                          axis.text.x = element_text(size = 8),
                          axis.text.y = element_text(size = 10),
                          legend.position = "none") +
      xlab("\n Money Source") + ylab("Billionaires (%) \n \n")
    ggplotly(s1, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
    output$plot_gdp <- renderDataTable({
      datatable(gdp_percent, rownames = FALSE, 
                caption = htmltools::tags$caption("Total Billionaires' Net Worth as % of Total GDP", style="color:black; font-size:14pt"),
                filter = c("none"),
                width = NULL, height = 200, elementId = NULL, colnames = c("Year", "% of GDP"),
                options = list(dom = 't')) %>% formatPercentage("gdp_yr", 1)
      })

  industry <- reactiveVal()
  sector <- reactiveVal()
  
  observeEvent(event_data("plotly_click", source = "industry"), {
    industry(event_data("plotly_click", source = "industry")$x)
    sector(NULL)
  })
  
  observe({
    req(industry())
    sector(event_data("plotly_click", source = "sector")$x)
    output$plot.ui <- renderUI({
      if(length(unique(secdata()$sector)) <5) (column(6,plotlyOutput("sector" , height = 400))) else 
        if(length(unique(secdata()$sector)) <20) (column(6,plotlyOutput("sector" , height = 600))) else (column(6,plotlyOutput("sector" , height = 1200)))
    })
  })
  
  inddata <- reactive({
    billion %>%
      filter(industry != '0', year == input$ind_year) %>% 
      group_by(industry) %>%
      tally(sort = T) %>%
      ungroup() %>%
      arrange(desc(n))
  })

  output$industry <- renderPlotly({
      plot_ly(inddata(), x = ~industry, y = ~n, source = "industry", type = "bar", marker = list(color = "rgb(0,205,0)",
                                                                                        line = list(color = "rgb(0,205,0)",
                                                                                                    width = 2))) %>%
        axis_titles() %>%
        layout(showlegend = FALSE, xaxis = list(categoryorder = "array", categoryarray = ~n, tickangle = 45)) %>%
        event_register('plotly_click')
  })

  secdata <- reactive({
    billion %>%
      filter(industry %in% industry(), year == input$ind_year) %>%
      group_by(sector) %>%
      tally(sort = T) %>%
      ungroup() %>%
      arrange(n)
  })
  
  output$sector <- renderPlotly({
    if (is.null(industry())) return(NULL)
    
    plot_ly(secdata(), y = ~sector, x = ~n, source = "sector", type = "bar", orientation = 'h', width = .5,
            marker = list(color = "rgb(0,205,0)", line = list(color = "rgb(0,205,0)",                                                                                                        width = 2))) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Count of Billionaires", side ="top"), title = paste0("Drill Down: " ,industry())) %>%
      layout(showlegend = FALSE, yaxis = list(categoryorder = "array", categoryarray = ~n)) %>% config(displayModeBar = FALSE) %>%
      layout(margin=list(t=150, l=100), width = 500) %>%
      event_register('plotly_click')

  })

  # populate back button if category is chosen
  output$back <- renderUI({
    if (!is.null(industry())) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  # clear on back button press
  observeEvent(input$clear, industry(NULL))
  
  
  output$countries <- renderPlotly({
    df_map <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")

    data <- df %>% filter(year == input$geo_year) %>% group_by(countrycode) %>% summarise(bills = n()) %>%  arrange(bills)
    df_map$bills <- data$bills[match(df_map$CODE, data$countrycode)]
    
    l <- list(color = toRGB("grey"), width =1)
    
    g <- list(
      showcountries = TRUE,
      countrycolor = 'grey',
      countrywidth = 0.5,
      showframe = FALSE,
      showcoastlines = TRUE,
      coastlinecolor = 'grey',
      coastlinewidth = 0.5,
      showocean = TRUE,
      oceancolor = "#DDDDDD",
      bgcolor = "#DDDDDD",
      projection = list(type = 'Mercator')
    )
    
    
    fig <- plot_geo(df_map,  width = 1000, height = 500, source = "k")
    fig <- fig %>% add_trace(
      z = ~bills, color = ~bills, color = ~bills, colors = 'Greens',hoverinfo = "text",
      text = ~paste(COUNTRY, bills, sep = "<br />"), locations = ~CODE, marker = list(line = l) 
    )
    fig <- fig %>% colorbar(title = 'Count of Billionaires', xpad = 5, ypad = 5, y=0.8)
    fig <- fig %>% layout(geo = g, margin = list(l=0, r=0, t=0, b=0, pad=0))
    fig$sizingPolicy$padding <- "0"
    fig
  })
  
})

shinyApp(ui = ui, server = server)

