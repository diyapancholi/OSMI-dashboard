#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(bslib)
library(showtext)
library(thematic)
library(shinydashboard)
library(ggplot2)



# Setup the bslib theme object
my_theme <- bs_theme(bootswatch = "darkly",
                     base_font = font_google("Righteous"))

# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")



top_countries = c('united states of america', 'united kingdom', 'canada', 'germany', 'netherlands', 'india', 'australia', 'france', 'ireland', 'spain')

choice_names = c(
                 "Does your employer provide mental health benefits as part of healthcare coverage?",
                 "Would you feel comfortable discussing a mental health issue with your supervisor?",
                 'Do you know the options for mental health care available under your employer provided health coverage?',
                 'Does your employer offer resources to learn more about mental health disorders and options for seeking help?',
                 'Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources provided by your employer?')

choices = c("provide_benefits",
            "comfortable_supervisor",
            'know_options',
            'resources_offered','anonymity')

question_names = setNames(choice_names, choices)

data<- dashboard_data

# Define UI for application that draws a histogram
ui <- dashboardPage( 
  

  

  dashboardHeader(
    title = "Exploring Mental Health in the Workplace",
    titleWidth = 400
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon('home')),
      menuItem("Global", tabName = "global", icon = icon("globe")),
      menuItem("Age/Gender/Race", tabName = "age_gender", icon=icon('user')),
      menuItem("Physical v/s Mental", tabName = "physical_vs_mental", icon = icon("handshake")),
      menuItem("Year Wise Comparison", tabName = "year_wise", icon = icon("calendar-days")),
      menuItem("Data Source", tabName = "data", icon = icon("database"))
      
    )
  ),
  
  
  dashboardBody(
    
    tabItems(
      
      ####Home##########
      
    tabItem(
      tabName = "home",
      
      h2("Home"),
      h4("The purpose of this dashboard is to analyze trends in mental health in the workplace."),
      h4("A multi-year survey was collected online by Open Sourcing Mental Illness, Ltd., a non-profit with a mission to raise awareness and providing resources to support mental wellness in the tech and open source communities. This data was downloaded from the OSMI website, cleaned, and transformed into a dashboard to clearly visualize the data."),
      h4("The survey consisted of around 100 questions about how mental illness impacts professional workplaces. Over the past seven years, the survey has gathered 4,400 responses. The variety of questions and the number of responses made the data difficult to understand on it's own. By visualizing the data, it is easier to find relevant trends about mental health in the workplace."),
      h4('I have taken into account survey data from the last 4 years, 2018-2021.'),
      h2("Author: Diya Pancholi")
      
    ),
    
    
    #global#########
    tabItem(
      tabName = "global",
      fluidPage(
        h2("Global Survey Responses"),
        p("The survey was administered online, so respondands from all across the globe were able to fill it out. In total, there were people over 50 different countries who responded. Many of these countries had less than 10 responses, so only the top ten countries are visualized below."),
        fluidRow(
          column(6,
                 selectizeInput(inputId = "country_q",
                                "Compare results by country:",
                                choices = setNames(choices, choice_names)),
                 plotOutput(outputId = "country_graph")
          ),
          column(6,
                 h2("Inference"),
                 p("In countries like USA, Canada, Ireland mental health is covered as a part of the employee's healthcare coverage."),
                 p("In the same countries, people are much more aware of the options available to them under mental health care provided and probably that's why they are comfortable talking to their supervisors about mental health concerns."),
                 p("Although, employees all over the world regardless of being aware about or being provided with mental healthcare benefits or not, they are not sure if their anonymity in these situations is taken into account or not.")
          )
          
          )
        )),
    
       
  #########age/gender/race#############
  tabItem(
  tabName = "age_gender",
  fluidPage(
    

    h2("Age / Gender / Race Responses"),
    p("The survey collected information about participant's age, gender and race. These questions can also be compared by selecting from the dropdown below."),
    
    fluidRow(
      column(6,
             selectizeInput(inputId = "age_q",
                            "Compare results by age:",
                            choices = setNames(choices, choice_names)),
             plotOutput(outputId = "age_bar_graph"),
             
             h2('Inferences'),
             p('AGE factor: Mostly people in their 30s are the most aware about the options provided for mental healthcare coverage and the resources provided to them. These employees are also more comfortable in talking to their supervisors than other age groups.'),
             p('GENDER factor: Most females are aware about the options and benefits provided for mental health in their healthcare coverage plan. However, employees who identify as non-binary (also, gender fluid, queer, transgender) are more comfortable talking to their supervisors about mental health issues.'),
             p('RACE factor: Asians, caucasians and hispanic people are majorly uncomfortable talking about mental health with their supervisors. Employees from almost all races are unsure about their anonymity status, if they disclose any mental health concerns.')
            
      ),
      column(6,
             selectizeInput(inputId = "gender_q",
                            "Compare results by gender:",
                            choices = setNames(choices, choice_names)),
             plotOutput(outputId = "gender_graph"),
      
      
      
             selectizeInput(inputId = "race_q",
                            "Compare results by Race:",
                            choices = setNames(choices, choice_names)),
             plotOutput(outputId = "race_graph")
      )
    ))
),

## Physical v/s mental #####
tabItem(
  tabName = "physical_vs_mental",
  fluidPage(
    h4("Comfortable with discussing Physical or Mental issues"),
    p("How does the comfort level of employees differ in discussing physical issues or mental health issues with either their potential employers or co-workers"),
    
    fluidRow(
      column(6,
             selectizeInput(inputId = "phy_men",
                            "Compare results for responses on physical v/s mental issues discussions with co-workers",
                            choices = setNames('coworkers_phy_mental','would you feel more comfortable talking to your coworkers about your physical health or your mental health')),
           
             plotOutput(outputId = "phy_men_compare"),
             
             h4("Inference:"),
             p("Clearly employees are more comfortable talking about their physical health issues more than their mental health with their co-workers. Although there are people who feel same level of comfort for discussing both.")
      
             
      ),
      
      column(6,
             selectizeInput(inputId = "phy_men_potential",
                            "Compare results for willingness to bring up physical or mental issues with potential employer",
                            choices = setNames(c('phy_poten_emp','men_poten_emp'), c('would you be willing to bring up a physical health issue with a potential employer in an interview?','would you bring up your mental health with a potential employer in an interview?'))),
             plotOutput(outputId = "phy_men_poten"),
             
             h4("Inference:"),
             p("Although many people are unsure about bringing a physical health concern with a potential employer, there is still a fair distribution among all the three choices"),
             p("Maximum number of people do not want to bring up a mental health issue bothering them with a potential employer!"),
             p("Overall, people would prefer talking about their physical health over mental health in an interview with a potential employer")
             
      
      
      
               
)))),


## year-wise distribution ######

tabItem(
  tabName = "year_wise",
  fluidPage(
    
    
    h2("Year wise comparison of change in attitudes towards mental health issues"),
    p("I have taken survey data from years 2018-2021. Thus, the change in reponses over 4 years regarding mental health issues at workplace can be visualized. "),
    
    fluidRow(
      column(6,
             selectizeInput(inputId = "year_q",
                            "Compare results by year:",
                            choices = setNames(choices, choice_names)),
             plotOutput(outputId = "year_line_graph")
      ),
      
      column(6,
             h2('Inferences:'),
             p('Around 50% of the employers provided mental health benefits as a part of their healthcare in 2018 & 19 which reduced to 40% in 2021'),
             p('Over the years, employees do not feel comfortable in discussing mental health issues with their supervisors'),
             p('There is almost a 10% decrease in employees knowing about the options available for mental healhcare coverage from 2019 to 2020, but slightly increasing in 2021.'),
             p('Positively, over the period of 4 years, employers offer more resources to learn about mental health and seek help if needed. There is also a gradual decrease in unsurity among employees regarding this concern.'),
             p('In 2021, employees feel that their anonymity is protected if they disclose any mental health related issues. Although, most people are unsure about their anonymous status.')
      )
     
    ))
),

## data source ######

  tabItem(
    tabName = "data",
    fluidPage(
    h2("Original source of the data"),
    h4("The basic idea behind creating this dashboard is to spread awareness about mental health issues that are highly stigmatized. Using this dataset, provided by OSMI (open-sourcing mental illness)  helpful insights/visualizations are generated to answer a few questions of interest using the responses of employees to survey questions regarding attitudes towards mental health in workplaces."),
    h4('Open Sourcing Mental Health (OSMI)  is a non-profit, organization dedicated to raising awareness, educating, and providing resources to support mental wellness in the tech communities. Using the survey data that aims to measure attitudes toward mental health in the tech workplace, an insightful dashboard can be created. The goal is to use this dashboard as a medium to spread awareness about the importance of mental health issues, in a workplace setting per se. Using data from 2018 to 2021 will also help lay out various changes in observations and trends for each year. '),
    h4("Survey results were downloaded from the OSMI website. These works are licensed under a Creative Commons Attribution-Share 4.0 International."),
    tags$a(href = "https://osmihelp.org/research", "OSMI Website")
    
  )
)
)
)
)

 #SERVER ################################################


server <- function(input, output) {
  

#global#####
  
  output$text = reactive({question_names[[input$country_q]]})
  
  
  country_order = reactive({
    drop_na(data) %>%
        filter(country %in% top_countries) %>%
        group_by(country, .data[[input$country_q]]) %>%
        summarise(count = n()) %>%
        mutate(rel_perc = count/sum(count)) %>%
        filter(.data[[input$country_q]]=="yes") %>%
        arrange(desc(rel_perc)) %>%
        pull(country)
    
  })
  
  output$country_graph <- renderPlot(
    drop_na(data) %>% 
      filter(country %in% top_countries) %>% 
      filter(!is.na(.data[[input$country_q]])) %>% 
      mutate(country = factor(country, country_order())) %>% 
      ggplot(aes_string(x="country", fill=input$country_q)) + geom_bar(position="fill") + labs(x = 'Country', y = 'Percent', title = question_names[[input$country_q]], fill = 'Responses:') + scale_y_continuous(labels = scales::percent) + theme(plot.title = element_text(size=12,face = 'bold'),axis.text.x = element_text(size=12,angle = 90, vjust = 0.5, hjust = 1,face='bold'))
  )
  
  
  #### age/gender############
  
  output$age_bar_graph <- renderPlot(
    drop_na(data) %>% 
      filter(age > 21, age < 55) %>% 
      ggplot(aes_string(x = 'age', fill = input$age_q)) + geom_bar(position="stack")+  theme(plot.title = element_text(size=12,face = 'bold'), axis.text.x = element_text(size=12,angle = 90, vjust = 0.5, hjust = 1, face = 'bold')) +labs(x = 'Age', y = 'Percent', title = question_names[[input$age_q]], fill = 'Responses:')
  )
  
  
  
  
  age_percent = reactive({
   
      data %>%
        
        group_by(age, .data[[input$age_q]]) %>%
        summarise(count = n()) %>%
        mutate(percent = count/sum(count)) %>%
        filter(.data[[input$age_q]] == "yes")
    
  })
  

  output$gender_graph <- renderPlot(
  
    data %>%
      ggplot(aes_string(x = 'gender', fill = input$gender_q)) + geom_bar(position="fill") + theme(plot.title = element_text(size= 12,face = 'bold'),axis.text.x = element_text(size= 12,angle = 90, vjust = 0.5, hjust = 1, face = 'bold')) +labs(x = 'Gender', y = 'Percent', title = question_names[[input$gender_q]], fill = 'Responses:')
  
    )
  
  output$race_graph <-  renderPlot(
    
    data %>%
      ggplot(aes_string(x = 'race', fill = input$race_q)) + geom_bar(position="fill") + theme(plot.title = element_text(size= 12,face = 'bold'),axis.text.x = element_text(size= 12,angle = 90, vjust = 0.5, hjust = 1, face = 'bold')) +labs(x = 'Race', y = 'Percent', title = question_names[[input$race_q]], fill = 'Responses:')
    
  )
  
  
  #####physical v/s mental ###########
  
  output$phy_men_compare  <- renderPlot(
    drop_na(data)%>%
      ggplot() +
      geom_bar(aes(coworkers_phy_mental)) + labs(x = 'Physical v/s Mental', y = 'Count',title = 'Comfortable with physical health or mental health discussion') + theme(plot.title = element_text(size= 12,face = 'bold'),axis.text.x = element_text(size= 12,angle = 90, vjust = 0.5, hjust = 1, face = 'bold'))
  
  )
  
  output$phy_men_poten  <- renderPlot(
    drop_na(data)%>%
      ggplot() +
      geom_bar(aes(.data[[input$phy_men_potential]])) + labs(x = 'Physical v/s Mental', y = 'Count') + theme(plot.title = element_text(face = 'bold'),axis.text.x = element_text(size= 12,angle = 90, vjust = 0.5, hjust = 1, face = 'bold'))
    
  )
  
  ### year-wise distribution ################
  
  year_order = reactive({
    drop_na(data) %>%
      filter(!is.na(.data[[input$year_q]])) %>% 
      group_by(year, .data[[input$year_q]]) %>%
      summarise(count = n()) %>%
      mutate(year_perc = count/sum(count))
    
  })

 

  
  output$text = reactive({question_names[[input$year_q]]})
  
  output$year_line_graph <- renderPlot(
    year_order() %>%
      
      
      
      ggplot(aes(x=year, y=year_perc,color=.data[[input$year_q]])) + geom_line()+ geom_point() + labs(x = 'Year', y = 'Percent', title = question_names[[input$year_q]], fill = 'Responses:') + scale_y_continuous(labels = scales::percent) + theme(plot.title = element_text(size=12,face = 'bold'), axis.text.x = element_text(size=12,angle = 90, vjust = 0.5, hjust = 1,face='bold'))
  )
    
    
   
  
  
}

  
# Run the application 
shinyApp(ui = ui, server = server)
