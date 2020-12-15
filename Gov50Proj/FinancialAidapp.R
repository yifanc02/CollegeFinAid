#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Loading the libraries:
library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(janitor)
library(gifski)
library(rstanarm)
library(gt)
library(gtsummary)
library(broom.mixed)
library(ggthemes)

# Reading the data that I cleaned 

institutions_percent <- readRDS("institutions_percent.rds")
institutions_amt <- readRDS("institutions_amt.rds")
demographic <- readRDS("demographic.rds")
model_data_1819 <- readRDS("model_data_1819.rds")


# This runs a predictive model on how institutional demographical make-up 
# correlates with what percentage of the student body takes out loans, receives
# institutional aid, and pell grants.

set.seed(9)
model_loans <- stan_glm(data = model_data_1819, 
                        loans ~ pctnum_0_48,
                        refresh = 0)

set.seed(9)
model_instit <- stan_glm(data = model_data_1819, 
                         instit_aid ~ pctnum_0_48,
                         refresh = 0)
set.seed(9)
model_pell <- stan_glm(data = model_data_1819,
                       pell ~ pctnum_0_48,
                       refresh = 0)

# Define UI for application that draws a line plot

ui <- navbarPage(
  
  # This is the title for my Shiny App!
    "Analyzing College Accessibility Through Funding Options",
    
    # Creating first panel to give an introduction of my project. I added
    # a section about introduction, project goals, discussion of my data 
    # sources, and an About Me section with the link to my repo.
    
    tabPanel("About", 
     titlePanel("Analyzing College Accessibility Through Funding Options"),
     column(6,
     fluidPage(theme = shinytheme("flatly"),
     
     h2("Introduction"),
     p("How are students funding their college education? One of the most
     discussed topics today are the rising costs of a college education. Yet,
     the actual price that students pay may be very different from the sticker
     price advertised by the university. While some students take out loans, others
     are able to receive aid through federal, local and states grants, and 
     institutional
     funding."),
     br(),
     
     h2("Project Goals and Highlights"),
     p("As someone who is interested in studying college access, I thought it 
     would be 
     interesting to think about 'access' in terms of affordability. Historically,
     institutions of higher learning were not accessible to students coming from 
     under-represented backgrounds and low-income households. While there has 
     been
     efforts expand college access through practices like holistic admissions
     and minority recruitment programs, are students able to afford the price 
     of a 
     college education after they've been admitted?"),
     p("To investigate this further, I will run a
     visualization to see how financial aid has expanded across different types of 
     institutions in the last two decades. Next,
     you can navigate to the tab 'Student Characteristics' to see how 
     individual factors such as race, 
       gender, and family income may correlate with the average amount 
       of aid that a student receives. 
       Lastly, because I'm interested in investigating how the percentage of 
       low-income
       students at an institution is correlated with that university's student 
       funding options and decisions,
       I will run a model using posterior distribution to
       predict the percentage of students in a given institution who 
       fund their education through pell grants, institutional aid, or 
       loans, taking into account the wealth the institution's student body."),
     br(),
     
     h2("Data Sources"),
     p("All of my data are taken fron the National Center of Educational 
     Statistics. You can access their website to
     familiarize yourself with their survey methodology and data", 
       a("here", href = "https://nces.ed.gov/"),
          ".")),
     br(),
     
     h2("About Me"),
     p("My name is Yifan and I'm a senior studying Sociology. I'm 
     interested in topics related to education access and investigating 
     the factors that affect socioeconomic mobility. 
     You can reach me at yifan_chen@college.harvard.edu. Here is the 
       link to my", 
       a("Github Repo", href = "https://github.com/yifanc02"),
       ".")),
     br(),
     br(),
     br(),
     
     # Added an image about how college costs have risen over time!
     
     column(3,
           imageOutput("collegecost", height = "100%", width = "100%"))
     ),

    # Now, moving on to the institutional level trends. 
    
tabPanel("Institutional Level Trends",
   fluidPage(
       titlePanel("Changes in Financial Aid Percentages and Amounts 2000-2018"),
       sidebarLayout(
           sidebarPanel(
             
                 # Users can select from 4 types of aid. I consider
                 # loans an aid option as well since it's a type of 
                 # funding option. 
                 
           selectInput(inputId = "type", 
                                  label = "Select Type of Aid",
                                  choices = list(
                                    "Federal Aid" = "fed",
                                    "State or Local Aid" = "state_local",
                                    "Institutional Aid" = "institutional",
                                    "Student Loans" = "student_loans")
                                    )),
               
# This creates two panels: One showing percentage change and 
# one showing amount age in financial aid. 

mainPanel(
tabsetPanel(type = "tabs",
     tabPanel("Percentage", 
              h1("How has the percentage 
              of students
                 receiving financial aid 
                 changed from 2000 to 
                 2018?"),
              br(),
              p("The line plot below shows
              the how percentage of first-
              time, full-time 
              undergraduate students 
              receiving financial aid 
              has fluctuated from the 
                years 2000-2018, separated
                by the type of 
                institution."),
              plotOutput("financialaidplot")),
     tabPanel("Amount",
              h1("How has the average amount of aid a given
                 student receive changed from 2000 to 2018?"),
              br(),
              p("The line plot below shows how the amount of aid that an average 
              first-time, full-time undergraduate student 
              receives has fluctuated from the years 2000-2018, separated by the 
              type of institution.
              Note that these dollar amounts have already been adjusted for 
                inflation."),
              plotOutput("financialaid_amt"))
             ))))),
    
# Now, next Panel is on individual characteristics. Only from 2015-2016 data
# set because that's only what's most recent on NCES.

# Users can select the type of characteristics to observe. I used a bar plot
# to show this because it's colorful, and it's obvious to see the height
# corresponding to the amount of aid awarded.

tabPanel("Student Characteristics",
 fluidPage(
   titlePanel("Financial Aid by Student Characteristics 2015-2016"),
   
   sidebarLayout(
     sidebarPanel(p("These bar plots show the amount of financial aid received 
                    by students
                    based on certain characteristics. Start by choosing
                    the individual characteristic you would like to serve 
                    in the drop-down
                    box below:"),
                  selectInput(inputId = "data_type", 
                              label = "Select Student Characteristic",
                              choices = list(
                                "Race" = "race",
                                "Sex" = "sex",
                                
                                # Really want to focus on fam_inc variable
                                # for the model since my question asks
                                # how wealth of student body correlates with
                                # funding options at the universities.
                                
                                "Household Income" = "fam_inc",
                                "Marital Status" = "marital_status",
                                "Housing Arrangement" = "housing",
                                "Age Range" = "age")
                  ),
                  p("Note that the data shown here was collected from 2015-2016 
                  academic year.
                  This is the most recent comprehensive data set available from 
                  NCES showing
                  how aid amount varies depending on certain student 
                    characteristics.")),
     mainPanel(plotOutput("financialaidplot2"))
     ))
 ),
    
# Modeling tab!
tabPanel("Model",
tabsetPanel(type = "tabs",
            
# One tab has just the graphics like the regression tables. Next is a graph
# that creates a correlation between wealth of student body and percentage on
# a certain type of aid. 

tabPanel("Table and Graphics",

br(),

sidebarPanel(
h4("About"),
p("These models observes how the household 
income demographics of an institution correlates
with the percentage of students who take out a certain
form of aid."),

helpText("Choose the type of aid to begin"),

selectInput(inputId = "aid_type",
            label = "Aid Type",
            choices = c("Institutional Aid" = "institution",
                        "Loans" = "loans",
                        "Pell Grant" = "pell"),
            selected = "Insitutional Aid")),

mainPanel(

  # Tab showing the posterior probability tables.
  
tabsetPanel(id = "tabsMain",
            tabPanel("Tables",
                     h2("Posterior 
                        Probability
                        Tables Based on Type 
                        of Aid Awarded"),
                     p("These tables predict how the percentage of 
                       students receiving a particular type of aid
                       changes with every percentage increase of students
                       coming from the family household
                       income bracket of 0-48K a year (low-income 
                       students).
                       Select the type of aid from the panel on the left 
                       to begin."),
                     br(),
                     gt_output("model"),
                     h2("Understanding the Variables:"),
                     p("The 'Intercept' variable
                       assumes a theoretical institution with only students
                       from income levels above 48K."),
                     p("The coefficient for the 'pct_num_0_48' variable 
                     represents the correlation between
                     the percentage of students receiving that form of aid and 
                     the percentage of low-income students
                    at theoretical institution")),
            
            # Table showing the correlation graphs. No modeling here,
            # it's just used to show how actual data can corroborate with 
            # the predictions I ran in my model. 
            
            
            tabPanel("Correlation Graphs",
                     h2("Linear Regression Plots"),
                     p("These graphs show how the
                        percentage of students
                        on a certain type of aid 
                        varies with higher percentages of
                        low-income students at an institution.
                       Select the type of aid from the panel on the left 
                       to begin."),
                     br(),
                     plotOutput("corr_graph"))

            )
)
),
   tabPanel("Interpretation and Discussion",
            h4(strong("Modeling Choice")),
            p("I choose to run a posterior probability prediction using the 
            stan_glm function in R to create a Bayesian generalized linear model 
            for the percentage of students at an institution receiving or 
            awarded some selected form of aid (institutional, loans, and pell 
            grants). I did not include federal or state aid because the 
            microdata I retrieved from from NCES combines the two together, 
            whereas I would only be interested in two forms of aid when they are 
            separated from each other."),
            br(),
            p("My model is quite simple. I did not want to add too many 
            explanatory variables when trying to predict the percentage of s
            tudents receiving aid because my question is mostly focused on how 
            the wealth of an institution’s student body can help predict what 
            percentage of students are on institutional aid, receives pell 
            grants, or takes out loans."),
            br(),
            p("So what am I trying to do by running a posterior probability 
            distribution? I’m using data from what we already know and observed 
            (existing universities) to predict what we don’t know: the overall 
            percentage of students at a university receiving some sort of aid 
            (dependent) variable given that a university has no low-income 
            students (the intercept) and how every increase in the percentage 
            of these low-income students changes that dependent variable."),
            br(),
            br(),
            h4(strong("Interpretations")),
            p(strong("Institutional Aid")),
            p("From the regression table, we can predict that a university with 
            only students who come from households making more than 48K a year 
            has 82% of the student body receiving institutional aid. Then, with 
            every 1% increase of low income students, the percentage of the 
            student body receiving institutional aid decreases by 0.74%."),
            p("The scatter plot graph corroborates with this conclusion. From 
            our observed data points, we can see an obvious negative 
            relationship between the percentage of low income students and the 
            percentage of students getting aid from their university for their 
            education."),
            br(),
            p(strong("Loans")),
            p("Similarly, a richer student body correlates with higher rates of 
              students taking out loans for their education. Our prediction 
              tells us that a university with only students who come from 
              households making more than 48K a year has 67% of the student body 
              on loans. Then, with every 1% increase of low income students, the 
              percentage of the student body taking out loans decreases by 
              0.59%."),
            p("The scatter plot also shows a clear negative correlation between 
            these two percentages. If you observe these data points closely, 
            this correlation is much stronger compared to institutional aid."),
            br(),
            p(strong("Pell Grants")),
            p("The result for pell grants might be the least surprising, since 
            it is common knowledge that pell grants (a form of federal aid) are 
            typically awarded to students from low-income households. Thus, a 
            richer student body correlates with lower rates of students 
            receiving pell grants for their education. Our prediction tells us 
            that a university with only students who come from households making 
            more than 48K a year has 22% of the student body on pell grants. 
            Then, with every 1% increase of low income students, the percentage 
            of the student body on pell grants increases by 0.23%."),
            p("The scatter plot shows a positive correlation between these two 
            percentages."),
            br(),
            br(),
            br()
                         ))
    ))
                                    

# No longer want a gif/graphic. Non-animated plots might work better here
server <- function(input, output) {
  
# This is the first plot shown on the second tab with percentage of aid 
# awarded by year. The NCES dataset reports the average, so it wasn't something
# that I needed to calculate from microdata.

    output$financialaidplot<- renderPlot({
            ggplot(institutions_percent, aes_string(x = "academic_year", 
                                                    y=input$type,
                                                    color = "type")) +
            geom_line() +
            geom_point(size = 1) +
            theme(text = element_text(size = 15)) +
            labs(title = "Percentage of Aid Awarded by Year",
                 y = "Percentage",
                 x = "Academic Year",
                 color = "Type of Institution") +
        theme_stata()
        
    })
    
    
# Second plot, showing amount instead of percentages. Otherwise, it's a similar
# graph to the one above. Might add breaks in here to show off all of the years
# presented in the data.
    
    output$financialaid_amt <- renderPlot({
      ggplot(institutions_amt, aes_string(x = "academic_year", 
                                          y = input$type,
                                          color = "type")) +
        geom_line() +
        geom_point(size = 1) +
        theme(text = element_text(size = 15)) +
        labs(title = "Amount of Aid Awarded by Year",
             y = "Dollar Amount",
             x = "Academic Year",
             color = "Type of Institution") +
        theme_stata()
      
    # Tab on student characteristics. Need to clean up the names and the title 
    # to make things look more professional here. 
      
    })
    output$financialaidplot2<- renderPlot({
      demographic %>%
        filter(data_type == input$data_type) %>%
      ggplot(aes(x = characteristic, y = amount, fill = aid_type)) +
      geom_col(position = "dodge") +
        theme_stata() +
        labs(title = input$data_type,
             y = "Amount",
             x = input$data_type)
    })
    
    output$collegecost <- renderImage({
      # Return a list containing the filename
      list(src = "College_Costs_cnbc.jpg",
           contentType = 'image/jpg',
           width = 500,
           height = 700,
           alt = "This is alternate text"
      )}, deleteFile = FALSE)
    
    # Turning the stan_glm model into a table. I would need to add the 
    # interpretations
    # in future iterations of the project, as well as an equation. 
    
    output$institution_corr <- render_gt({
        model1 <- tbl_regression(model_instit, intercept = TRUE) %>%
        as_gt() %>%
        tab_header(title = "Regression of Student Household Income and 
                   Institutional Aid",
                   subtitle = "Richer Student Body Correlates with More 
                   Institutional Aid") %>%
        tab_source_note("Source: NCES Data")
    
  })
    
    # For the models. Code repeats here sincee I'm creating 3 different tables,
    # if and else if functions to show different tables based on the user's
    # selection choice.
    
    output$model <- render_gt({
      
      if(input$aid_type == "institution") {
        tbl_regression(model_instit, intercept = TRUE) %>%
          as_gt() %>%
          tab_header(title = "Regression of Student Household Income and 
                     Institutional Aid",
                     subtitle = "Richer Student Body Correlates with More 
                     Institutional Aid") %>%
          tab_source_note("Source: NCES Data")
      }
      
      else if(input$aid_type == "loans") {
        tbl_regression(model_loans, intercept = TRUE) %>%
          as_gt() %>%
          tab_header(title = "Regression of Student Household Income and Loan",
                     subtitle = "Richer Student Body Correlates with More 
                     Loans") %>%
          tab_source_note("Source: NCES Data")
      }
      else if(input$aid_type == "pell") {
        tbl_regression(model_pell, intercept = TRUE) %>%
          as_gt() %>%
          tab_header(title = "Regression of Student Household Income and Pell 
                     Grants",
                     subtitle = "Lower Income Student Body Associated with Higher 
                     Percentages
                   of Students on Pell Grants") %>%
          tab_source_note("Source: NCES Data")
      }
    
    })
    
    # For the graphs. Code repeats here sincee I'm creating 3 different graphs,
    # if and else if functions to show different graphs based on the user's
    # selection choice.
    
    output$corr_graph <- renderPlot({
      
      if(input$aid_type == "institution") {
      ggplot(model_data_1819, aes(x = (pctnum_0_30 + pctnum_30_48), 
                                  y = instit_aid)) +
        geom_point(alpha = 0.6, aes(size = totalnum)) +
        geom_smooth(method ='lm', formula = y~x) +
        labs(title = "Low Income Students Receiving Institutional Aid",
             x = "Percentage of Students From Household Incomes Below 48K",
             y = "Percentage of Students Receiving Institutional Aid",
             size = "Undergraduate Population") +
        theme_stata()
      }
      
      else if(input$aid_type == "loans") {
        ggplot(model_data_1819, aes(x = (pctnum_0_30 + pctnum_30_48), 
                                      y = loans)) +
          geom_point(alpha = 0.6, aes(size = totalnum)) +
          geom_smooth(method ='lm', formula = y ~ x) +
          labs(title = "Low Income Students Taking Out Loans",
               x = "Percentage of Students From Household Incomes Below 48K",
               y = "Percentage of Students Taking Out Loans",
               size = "Undergraduate Population") +
          theme_stata()
      }
      
      else if(input$aid_type == "pell") {
        ggplot(model_data_1819, aes(x = (pctnum_0_30 + pctnum_30_48), 
                                   y = pell)) +
        geom_point(alpha = 0.6, aes(size = totalnum)) +
        geom_smooth(method= 'lm', formula = y~x) +
          labs(title = "Low Income Students Receiving Pell Grants",
               x = "Percentage of Students From Household Incomes Below 48K",
               y = "Percentage of Students Receiving Pell Grants",
               size = "Undergraduate Population") +
          theme_stata()
      }
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)





