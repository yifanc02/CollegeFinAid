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
library(gifski)
library(janitor)
library(rstanarm)
library(gt)
library(gtsummary)
library(broom.mixed)

setwd("~/Dropbox/Gov50/unifunds/Gov50Proj")

allinstitutions <- read_excel("PercentageFinancialAid2000-2018.xls", skip = 1) %>%
  select(1:8, 13:16)

colnames(allinstitutions) <- c("academic_year", "enrollment", 
                              "total_number_awarded", "total_percent_awarded",
                              "fed", "state_local", "institutional", 
                              "student_loans", "fed_amt", "state_local_amt", 
                              "institutional_amt",
                              "loan_amt")

allinstitutions <- allinstitutions %>%
  mutate(academic_year = as.numeric(substr(academic_year, 1, 4)))

allinstitutions_percent <- allinstitutions %>%
  select(1:8) %>%
  slice(5:22) %>% 
  drop_na() %>%
  mutate(type = "All")

select_type <- function(row_select, x){
  allinstitutions %>%
    select(1:8) %>%
    mutate(academic_year = as.numeric(substr(academic_year, 1, 4))) %>%
    slice(row_select)
  
}

public <- select_type(24:31) %>%
  mutate(type = "Public")

private4prof <- select_type(78:85) %>%
  mutate(type = "Private For-Profit Colleges")

privatenonprof <- select_type(51:58) %>%
  mutate(type = "Private Nonprofit Colleges")

# Bind them all together

institutions_percent <- bind_rows(list(public, private4prof, privatenonprof)) %>%
  mutate_at(vars(fed:student_loans), as.numeric)


# Now let's work on the amount
allinstitutions_amt <- allinstitutions %>%
  select(1, 9:12) %>%
  slice(5:22) %>% 
  drop_na() %>%
  mutate(type = "All")

select_type2 <- function(row_select, x){
  allinstitutions %>%
    select(1, 9:12) %>%
    slice(row_select)
  
}

public_amt <- select_type2(24:31) %>%
  mutate(type = "Public")

private4prof_amt <- select_type2(78:85) %>%
  mutate(type = "Private For-Profit Colleges")

privatenonprof_amt <- select_type2(51:58) %>%
  mutate(type = "Private Nonprofit Colleges")

institutions_amt <- bind_rows(list(public_amt, private4prof_amt, privatenonprof_amt)) %>%
  mutate_at(vars(fed_amt:loan_amt), as.numeric)

#Now lets turn to personal characteristics

characteristics <- read_excel("Student_Characteristics_Aid15-16.xls", skip = 2)
 
#By sex, race, and family income level

characteristics <- characteristics %>%
slice(c(5:6, 9:15, 18:20, 23:25, 29:34, 43:45)) %>%
  select(1, 2, 8, 14, 20)

colnames(characteristics) <- c("characteristic", 
                               "any_aid", 
                               "grants", 
                               "loans",
                               "work_study")

# Sex, race, age, marital status, income, housing status 15-16, thinking about doing this by year:
# 03-04, 07-08, 11-12 as well

sex <- characteristics %>%
  slice(1:2) %>%
  clean_names() %>%
  select(characteristic, any_aid, grants, loans, work_study) %>%
  mutate(data_type = "sex")
         
race <- characteristics %>%
  slice(3:9) %>%
  clean_names() %>%
  select(characteristic, any_aid, grants, loans, work_study) %>%
  drop_na() %>%
  mutate(data_type = "race")

age <- characteristics %>%
  slice(10:12) %>%
  clean_names() %>%
  select(characteristic, any_aid, grants, loans, work_study) %>%
  mutate(data_type = "age")

marital <- characteristics %>%
  slice(13:15) %>%
  clean_names() %>%
  select(characteristic, any_aid, grants, loans, work_study) %>%
  mutate(data_type = "martial_status")

fam_inc <- characteristics %>%
  slice(16:21) %>%
  clean_names() %>%
  select(characteristic, any_aid, grants, loans, work_study) %>%
  mutate(data_type = "fam_inc")

housing <- characteristics %>%
  slice(22:24) %>%
  clean_names() %>%
  select(characteristic, any_aid, grants, loans, work_study) %>%
  mutate(data_type = "housing")

demographic <- bind_rows(sex, race, fam_inc, marital, housing, age) %>%
  mutate(work_study = ifelse(work_study == "‡", 0, work_study)) %>%
  mutate(characteristic = str_remove_all(characteristic, "\\.")) %>%
  pivot_longer(cols = c(-characteristic, -data_type), names_to = "aid_type", values_to = "amount") %>%
  mutate(amount = as.numeric(amount))
  
# Trying to create a model

model_data <- read_csv("sfa1819.csv")

model_data_1819 <- model_data %>% 
  mutate(totalnum = GIS4N2,
         pctnum_0_30 = GIS4N12/totalnum*100,
         pctnum_30_48 = GIS4N22/totalnum*100,
         pctnum_48_75 = GIS4N32/totalnum*100,
         pctnum_75_110 = GIS4N42/totalnum*100,
         pctnum_110 = GIS4N52/totalnum*100,
         instit_aid = IGRNT_P, 
         loans = UFLOANP,
         pell = UPGRNTP) %>%
  select(totalnum:pell) %>%
  drop_na()

model_loans <- stan_glm(data = model_data_1819, 
                        loans ~ pctnum_75_110 + pctnum_110,
                        refresh = 0)

print(model_loans, digits = 4)
results <- model_loans %>%
  as_tibble()

# Level of institutional aid

model_instit <- stan_glm(data = model_data_1819, 
                         instit_aid ~ 
                        pctnum_75_110 + pctnum_110,
                         refresh = 0)

print(model_instit, digits = 4)

model_table <- tbl_regression(model_instit, intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "Regression of Student Household Income and Institutional Aid",
             subtitle = "Richer Student Body Correlates with More Institutional Aid") %>%
  tab_source_note("Source: NCES Data")

model_table

#Percent of students awarded institutional aid, 
#Percent of undergraduates awarded student loans
#Percent of undergraduate students awarded Pell grants
#Percentage of students in fall cohort who are paying in-district tuition rates
#Percentage of students in fall cohort who are paying out-of-state tuition rates



# Define UI for application that draws a line plot
ui <- navbarPage(
  
  # This is the title for my Shiny App!
    "Analyzing College Accessibility Through Funding Options",
    
    # Creating first panel to give an introudction of my project
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
                     sidebarPanel(p("This is a line plot of showing the percentage of first-time, full-time undergraduate
                                    students who received financial aid for their college education starting from the year 2000"),
                                  selectInput(inputId = "type", 
                                              label = "Select Type of Aid",
                                              choices = list(
                                                "Total Percent Award" = "total_percent_awarded",
                                                "Federal Aid" = "fed",
                                                "State or Local Aid" = "state_local",
                                                "Institutional Aid" = "institutional",
                                                "Student Loans" = "student_loans")
                                                )),
                     mainPanel(plotOutput("financialaidplot"))),
             )),
    tabPanel("Student Demographics",
             fluidPage(
               titlePanel("Financial Aid Plots by Student Characteristics"),
               sidebarLayout(
                 sidebarPanel(p("These are bar plots showing the amount of financial aid received by students
                                based on individual characteristics"),
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
             mainPanel(gt_output("institution_corr")),
             p("Interpretation: [will add interpretation here")),
             
             column(12,
             h2("Loans"),
             p("This model predicts how the percentage of students taking out loans
             changes with every percentage increase of students from a certain 
               household income level"),
             mainPanel(gt_output("loans_corr")),
             p("Interpretation: [will add interpretation here]"))),
             
    tabPanel("Discussion",
             titlePanel("Work in Progress"),
             h2("Findings"),
             h2("Conclusion"),
             p("Please check back later!")))

# Define server logic required to draw a histogram

# No longer want a gif/graphic. Non-animated plots might work better here
server <- function(input, output) {
  
    output$financialaidplot<- renderPlot({
            ggplot(institutions_percent, aes_string(x="academic_year", y=input$type)) +
            geom_line(color = "red") +
            geom_point() +
            theme(panel.spacing = unit(5,"lines")) +
            theme_bw() +
            facet_wrap(~type, nrow = 3) +
            labs(title = "Percentage of Aid Awarded by Year",
                 y = "Percentage",
                 x = "Academic Year") 
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
    
    output$institution_corr <- render_gt({
        model1 <- tbl_regression(model_instit, intercept = TRUE) %>%
        as_gt() %>%
        tab_header(title = "Regression of Student Household Income and Institutional Aid",
                   subtitle = "Richer Student Body Correlates with More Institutional Aid") %>%
        tab_source_note("Source: NCES Data")
    
  })
    
    output$loans_corr <- render_gt({
      tbl_regression(model_loans, intercept = TRUE) %>%
      as_gt() %>%
      tab_header(title = "Regression of Student Household Income and Loan",
                 subtitle = "Richer Student Body Correlates with More Loans") %>%
      tab_source_note("Source: NCES Data")
    
})
}


# Run the application 
shinyApp(ui = ui, server = server)

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
