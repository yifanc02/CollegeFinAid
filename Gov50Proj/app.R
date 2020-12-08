#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(gganimate)
library(janitor)
library(gifski)
library(rstanarm)
library(gt)
library(gtsummary)
library(broom.mixed)

# Goals for the project and general to-do list
# Make the first financial aid plots with different scales
# Specify columns and layout
# Student demographics - filter by year as well (right now it's just 2015-2016 data)
# Organize modeling page (add some graphs as well)


# Reading the data that I cleaned 

institutions_percent <- readRDS("institutions_percent.rds")
institutions_amt <- readRDS("institutions_amt.rds")
demographic <- readRDS("demographic.rds")
model_data_1819 <- readRDS("model_data_1819.rds")
# Trying to create demographics before 
# Trying to create a model

# This runs a predictive model on how institutional demographical make-up 
# correlates with what percentage of the student body takes out loans, receives
# institutional aid, and pell grants.


model_loans <- stan_glm(data = model_data_1819, 
                        loans ~ pctnum_75_110 + pctnum_110,
                        refresh = 0)

model_instit <- stan_glm(data = model_data_1819, 
                         instit_aid ~ 
                        pctnum_75_110 + pctnum_110,
                         refresh = 0)

model_pell <- stan_glm(data = model_data_1819,
                       pell ~ pctnum_0_30 + pctnum_30_48,
                       refresh = 0)

# Define UI for application that draws a line plot
ui <- navbarPage(
  
  # This is the title for my Shiny App!
    "Analyzing College Accessibility Through Funding Options",
    
    # Creating first panel to give an introduction of my project. I 
    tabPanel("About", 
             column(6,
             fluidPage(theme = shinytheme("flatly"),
             titlePanel("About My Project"),
             h2("Introduction"),
             p("How are students funding their college education? One of the most
             discussed topics today are the rising costs of a college education. Yet,
             the actual price that students pay may be very different from the sticker
             price advertised by the university. While some students take out loans, others
             are able to receive aid through federal, local and states grants, and institutional
             funding."),
             h2("Project Goals"),
             p("Under the tab titled Financial Aid by Institution, I will visualize how financial aid has expanded 
               across different types of institutions in the past 20 years. Next,
               you can navigate to the tab Student Characteristics to see how individual factors, like race, 
               gender, and family income may correlate with the average amount of aid that a student receives.
               Lastly, I will run a model that helps 
               predict the percentage of students in a given institution funding 
               their education through pell grants, institutional aid, and loans, 
               respectively, based on the income-level demographic makeup of an 
               institution's student body."),
             h2("Data Sources"),
             p("All of my data are taken fron the National Center of Educational Statistics. You can access their website to
             familiarize yourself with their survey methodology and data", 
               a("here", href = "https://nces.ed.gov/"),
                  ".")),
             h2("About Me"),
             p("My name is Yifan and I'm a senior studying Sociology. I'm broadly interested in topics related to education access and factors
             that affect social mobility. 
             You can reach me at yifan_chen@college.harvard.edu. Here is the link to my", 
               a("Github Repo", href = "https://github.com/yifanc02"),
               ".")),
             column(3,
                   imageOutput("collegecost", height = "100%", width = "100%"))),
    tabPanel("Institutional Level Trends 2000-2018",
             fluidPage(
                 titlePanel("Financial Aid Plots by Institution"),
                 sidebarLayout(
                     sidebarPanel(p("These plots show the historical trends of how college financial has changed on an institutional level
                    for first-time, full-time undergraduate students from the year 2000 to 2018."),
                                  selectInput(inputId = "type", 
                                              label = "Select Type of Aid",
                                              choices = list(
                                                "Federal Aid" = "fed",
                                                "State or Local Aid" = "state_local",
                                                "Institutional Aid" = "institutional",
                                                "Student Loans" = "student_loans")
                                                )),
                     
                     # Need to create new selectInput options
                     mainPanel(
                       tabsetPanel(type = "tabs",
                                           tabPanel("Percentage", plotOutput("financialaidplot"),
                                                    h2("Title"),
                                                    p("Explanation")),
                                           tabPanel("Amount", plotOutput("financialaid_amt"),
                                                    h2("Title"),
                                                    p("Explanation")))
             )))),
    tabPanel("Student Demographics",
             fluidPage(
               titlePanel("Financial Aid by Student Characteristics"),
               sidebarLayout(
                 sidebarPanel(p("These are bar plots showing the amount of financial aid received by students
                                based on individual characteristics using data collected for the 2015-2016 school year"),
                              selectInput(inputId = "data_type", 
                                          label = "Select Student Characteristic",
                                          choices = list(
                                            "Race" = "race",
                                            "Sex" = "sex",
                                            "Household Income" = "fam_inc",
                                            "Marital Status" = "marital_status",
                                            "Housing Arrangement" = "housing",
                                            "Age Range" = "age")
                              )),
                 mainPanel(plotOutput("financialaidplot2"))),
             )),
    tabPanel("Model",
             titlePanel("Predicting Percentage of Aid"),
             column(12,
                    h2("Institutional Aid"),
                    p("This model predicts how the percentage of students receiving 
                institutional aid changes with every percentage increase of students
                from a certain household income level"),
             mainPanel(gt_output("institution_corr"),
                       plotOutput("institution_plot")),
             p("Interpretation: Higher education institutions with a wealthier
               student body (household income above 75K a year) are associated with 
               higher rates of students receiving institutional aid.")),
             
             column(12,
             h2("Loans"),
             p("This model predicts how the percentage of students taking out loans
             changes with every percentage increase of students from a certain 
               household income level"),
             mainPanel(gt_output("loans_corr"),
                       plotOutput("loans_plot")),
             p("Interpretation: Higher education institutions with a wealthier
               student body (household income above 75K a year) are associated with 
               higher rates of students taking out loans to fund their education.")),
             
    # Might need to figure out how columns work.
    
    column(12,
           h2("Pell Grants"),
           p("This model predicts how the percentage of students receiving pell grants
             changes with every percentage increase of students from a certain 
               household income level"),
           mainPanel(gt_output("pell_corr"),
                     plotOutput("pell_plot")),
           p("Interpretation: Higher education institutions with a lower-income
               student body (household income below 48K a year) are associated with 
               higher rates of students receiving pell grants."))),
             
    tabPanel("Discussion",
             titlePanel("Work in Progress"),
             h2("Findings"),
             h2("Conclusion"),
             p("Please check back later!")))

# Define server logic required to draw a histogram

# No longer want a gif/graphic. Non-animated plots might work better here
server <- function(input, output) {
  
  # This is the first plot shown on the second tab with percentage of aid 
  # awarded by year. The NCES dataset reports the average, so it wasn't something
  # that I needed to calculate from microdata.
  
    output$financialaidplot<- renderPlot({
            ggplot(institutions_percent, aes_string(x="academic_year", y=input$type)) +
            geom_line(color = "red") +
            geom_point(size = 1) +
            theme_gray() +
            facet_wrap(~type, nrow = 3, scales = "free_y") +
            theme(panel.spacing = unit(3,"lines"),
                  text = element_text(size = 15)) +
            labs(title = "Percentage of Aid Awarded by Year",
                 y = "Percentage",
                 x = "Academic Year") 
    })
    
    
    # Second plot, showing amount instead of percentages. Otherwise, it's a similar
    # graph to the one above. Might add breaks in here to show off all of the years
    # presented in the data.
    
    output$financialaid_amt <- renderPlot({
      ggplot(institutions_amt, aes_string(x="academic_year", y=input$type)) +
        geom_line(color = "blue") +
        geom_point(size = 1) +
        theme_gray() +
        facet_wrap(~type2, nrow = 3, scales = "free_y") +
        theme(panel.spacing = unit(3,"lines"),
              text = element_text(size = 15)) +
        labs(title = "Amount of Aid Awarded by Year",
             y = "Dollar Amount",
             x = "Academic Year") 
      
    # Tab on student characteristics. Need to clean up the names and the title 
    # to make things look more professional here. 
      
    })
    output$financialaidplot2<- renderPlot({
      demographic %>%
        filter(data_type == input$data_type) %>%
      ggplot(aes(x = characteristic, y = amount, fill = aid_type)) +
      geom_col(position = "dodge") +
        theme_bw() +
        labs(title = input$data_type,
             y = "Amount",
             x = input$data_type)
    })
    
    output$collegecost <- renderImage({
      # Return a list containing the filename
      list(src = "College_Costs_cnbc.jpg",
           contentType = 'image/jpg',
           width = 400,
           height = 600,
           alt = "This is alternate text"
      )}, deleteFile = FALSE)
    
    # Turning the stan_glm model into a table. I would need to add the interpretations
    # in future iterations of the project, as well as an equation. 
    
    output$institution_corr <- render_gt({
        model1 <- tbl_regression(model_instit, intercept = TRUE) %>%
        as_gt() %>%
        tab_header(title = "Regression of Student Household Income and Institutional Aid",
                   subtitle = "Richer Student Body Correlates with More Institutional Aid") %>%
        tab_source_note("Source: NCES Data")
    
  })
    
    # For the table on institutional aid. 
    
    output$institution_plot<- renderPlot({
      ggplot(model_data_1819, aes(x = (pctnum_75_110 + pctnum_110), y = instit_aid, 
                                  size = totalnum, color = totalnum)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method='lm') +
        scale_color_gradient(low="blue", high="red")
    
    })
    
    output$loans_plot<- renderPlot({
      ggplot(model_data_1819, aes(x = (pctnum_75_110 + pctnum_110), y = loans,
                                  size = totalnum, color = totalnum)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method='lm') +
        scale_color_gradient(low="blue", high="red")
      
    })
    
    output$loans_corr <- render_gt({
      tbl_regression(model_loans, intercept = TRUE) %>%
      as_gt() %>%
      tab_header(title = "Regression of Student Household Income and Loan",
                 subtitle = "Richer Student Body Correlates with More Loans") %>%
      tab_source_note("Source: NCES Data")
    
})
    # Creates a posterior probability table that estimates how a given university
    # with only students above the 75K income bracket 
    output$pell_corr <- render_gt({
      tbl_regression(model_pell, intercept = TRUE) %>%
        as_gt() %>%
        tab_header(title = "Regression of Student Household Income and Pell Grants",
                   subtitle = "Lower Income Student Body Associated with Higher Percentages
                   of Students on Pell Grants") %>%
        tab_source_note("Source: NCES Data")
})
    # Creates the plot showing how the percentages on pell grants increases
    # with more lower-income students at an institution. 
    
    output$pell_plot<- renderPlot({
      ggplot(model_data_1819, aes(x = (pctnum_0_30 + pctnum_30_48), y = pell, 
                                  color = totalnum, size = totalnum)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method='lm') +
        scale_color_gradient(low="blue", high="red")
      
})
}


# Run the application 
shinyApp(ui = ui, server = server)

# This was the code that creates the animation, but because it took so long to 
# run I chose not to use it. 

# animate(p, nframes = 75, renderer = gifski_renderer("outfile.gif"))
# output$financialaidplot<- renderImage({
#   outfile <- tempfile(fileext='.gif')
#   plot <- ggplot(allinstitutions3, aes(x=academic_year, y=total_percent_awarded)) +
#     geom_line(color = "red") +  
#     theme_classic() +
#     facet_wrap(~type) +
#     labs(title = "Percentage of Aid Awarded by Year",
#          y = "Percentage",
#          x = "Academic Year") +
#     transition_reveal(academic_year) 
#   animate(plot, nframes = 75, renderer = gifski_renderer("outfile.gif"))
#   list(src = "outfile.gif",   
#        contentType = 'image/gif')
