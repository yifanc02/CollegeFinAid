#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(gganimate)
library(gifski)
library(janitor)

percentaid <- read_excel("PercentageFinancialAid2000-2018.xls", skip = 1)

allinstitutions <- percentaid %>%
    select(1:8) %>%
    slice(5:22) 
colnames(allinstitutions) <- c("academic_year", "enrollment", "total_number_awarded", "total_percent_awarded",
                               "fed", "state_local", "institutional", "student_loans")
    
allinstitutions <- allinstitutions %>% 
  mutate(academic_year = as.numeric(substr(academic_year, 1, 4)))
 
allinstitutions2 <- percentaid %>%
     select(1:8)
colnames(allinstitutions2) <- c("academic_year", "enrollment", "total_number_awarded", "total_percent_awarded",
                               "fed", "state_local", "institutional", "student_loans")

allinstitutions2 <- allinstitutions2 %>%
  drop_na() %>%
    mutate(academic_year = as.numeric(substr(academic_year, 1, 4)))


select_type <- function(row_select, x){
    allinstitutions2 %>%
        slice(row_select)

}

#Create graphs here:
public <- select_type(24:31) %>%
    mutate(type = "Public")

private4prof <- select_type(78:85) %>%
    mutate(type = "Private For-Profit Colleges")

privatenonprof <- select_type(51:58) %>%
    mutate(type = "Private Nonprofit Colleges")

#This will either be facet-wrapped in ggplot or show up on different tabs
allinstitutions3 <- bind_rows(list(public, private4prof, privatenonprof))


#Now lets turn to personal characterisitcs

characteristics <- read_excel("Student_Characteristics_Aid15-16.xls", skip = 2)
 
#By sex, race, and family income level

characteristics <- characteristics %>%
slice(c(5:7, 9:17, 29:37)) 

sex <- characteristics %>%
  slice(1:3) %>%
  clean_names() %>%
  select(selected_student_characteristic, any_aid, grants, loans, work_study) 
  #rename("sex" = selected_student_characteristic) 
         
race <- characteristics %>%
  slice(4:12) %>%
  clean_names() %>%
  select(selected_student_characteristic, any_aid, grants, loans, work_study) %>%
  #rename("race" = selected_student_characteristic) %>%
  slice(2:9) %>%
  drop_na()


fam_inc <- characteristics %>%
  slice(16:21) %>%
  clean_names() %>%
  select(selected_student_characteristic, any_aid, grants, loans, work_study) 
  #rename("family_income" = selected_student_characteristic) 

demographic <- bind_rows(sex, race, fam_inc) %>%
  slice(-1) %>%
  mutate(work_study = ifelse(work_study == "‡", 0, work_study)) %>%
  mutate(selected_student_characteristic = str_remove_all(selected_student_characteristic, "\\.")) %>%
  pivot_longer(cols = c(-selected_student_characteristic), names_to = "aid_type", values_to = "amount")
  
 
#Customize by demographic category
#What are the next steps to this?

# Define UI for application that draws a line plot
ui <- navbarPage(
    "Financial Aid Trends Over Time",
    tabPanel("Percentage of Students Awarded Financial Aid in Year 2000-2018",
             fluidPage(
                 titlePanel("Financial Aid Plots of 2011"),
                 sidebarLayout(
                     sidebarPanel(p("This is a line plot of showing the percentage of first-time, full-time undergraduate
                                    students who received financial aid for their college education.")),
                     mainPanel(imageOutput("financialaidplot"))),
             )),
    tabPanel("______",
             fluidPage(
               titlePanel("_______"),
               sidebarLayout(
                 sidebarPanel(p("______")),
                 mainPanel(imageOutput("financialaidplot"))),
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About My Project"),
             h3("Project Background and Motivations"),
             p("As we think about how to make college more accessible to a wider population, 
               it's also important to think about accessibility in terms of affordability. How are students funding their education?
               A future direction I'd want to take with this project is to identity how students from different socio-economic backgrounds
               are funding their college education. From there, we can identity other other individual factors, like race, gender, type of high school attended, etc
               correlates with their funding options or decisions."),
             h3("Data Sources"),
             p("NCES Data + Some others"),
             h3("About Me"),
             p("My name is Yifan and I'm a senior studying Sociology. I'm broadly interested in topics related to education access and factors
             that affect social mobility. 
             You can reach me at yifan_chen@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$financialaidplot<- renderImage({
        outfile <- tempfile(fileext='.gif')
        plot <- ggplot(allinstitutions3, aes(x=academic_year, y=total_percent_awarded)) +
            geom_line(color = "red") +  
            theme_classic() +
            facet_wrap(~type) +
            labs(title = "Percentage of Aid Awarded by Year",
                 y = "Percentage",
                 x = "Academic Year") +
    
           transition_reveal(academic_year) 
        animate(plot, nframes = 75, renderer = gifski_renderer("outfile.gif"))
        list(src = "outfile.gif",   
        contentType = 'image/gif')
        
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# animate(p, nframes = 75, renderer = gifski_renderer("outfile.gif"))

