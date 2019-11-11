#We import the source code for creating graphs and detailed analysis of our data from this file
source("Source_Code.R",local = TRUE)

#Required libraries are imported
library(shinydashboard)
library(shiny)
library(ggplot2)
library(plotly)
library(corrplot)

#RShiny Dashboard
if (interactive()) {
  header <- dashboardHeader(title = "Educational and Disciplinary Outcomes in Pennsylvania")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("About",tabName = "dashboard_about",icon = icon("th")),
      menuItem("Absenteeism", tabName = "dashboard_a", icon = icon("bar-chart-o"),
               menuSubItem("Ethnicity", tabName = "subitem11"),
               menuSubItem("Gender", tabName = "subitem12")
      ),
      menuItem("Bullying", icon = icon("bar-chart-o"), tabName = "dashboard_b",
               menuSubItem("Ethnicity", tabName = "subitem21"),
               menuSubItem("Gender", tabName = "subitem22")
      ),
      menuItem("In School Suspension", tabName="dashboard_s",icon = icon("bar-chart-o"),
               menuSubItem("Ethnicity", tabName = "subitem31"),
               menuSubItem("Gender", tabName = "subitem32")
               
      ),
      menuItem("School Characteristics",tabName = "dashboard_sc",icon = icon("th")),
      menuItem("Data Dictionary",tabName = "dashboard_dataD",icon = icon("th"))
      
      
    )
  )
  
  body <- dashboardBody(
    tabItems(
      tabItem("dashboard_a",
              div(p("Dashboard tab content"))
      ),
      tabItem("dashboard_b",
              "Widgets tab content"
      ),
      tabItem("dashboard_sc",
              div(h3("School Characteristics HeatMap"))
      ),
      tabItem("dashboard_dataD",
              div(h3("Data Dictionary"))
      ),
      tabItem("dashboard_about",
              div(h3("About"))
      ),
      tabItem("subitem11",
              h3("Absenteeism v/s Ethnicity"),radioButtons(inputId = "radio",label = "Ethnicity",
                                                           c("White and Black" = "a", "White and Asian" = "b",
                                                             "White and Hispanic" = "c","Black and Asian"="d",
                                                             "Black and Hispanic" = "e", "Asian and Hispanic"="f"))
      ),
      tabItem("subitem12",
              h3("Absenteeism v/s Gender")
      ),
      tabItem("subitem21",
              h3("Bullying v/s Ethnicity"),radioButtons(inputId = "radio2",label = "Ethnicity",
                                                        c("White and Black" = "a", "White and Asian" = "b",
                                                          "White and Hispanic" = "c","Black and Asian"="d",
                                                          "Black and Hispanic" = "e", "Asian and Hispanic"="f"))
      ),
      tabItem("subitem22",
              h3("Bullying v/s Gender")
      ),
      tabItem("subitem31",
              h3("In-School Suspension v/s Ethnicity"),radioButtons(inputId = "radio3",label = "Ethnicity",
                                                                    c("White and Black" = "a", "White and Asian" = "b",
                                                                      "White and Hispanic" = "c","Black and Asian"="d",
                                                                      "Black and Hispanic" = "e", "Asian and Hispanic"="f"))
      ),
      tabItem("subitem32",
              h3("In-School Suspension v/s Gender")
      )
      
    ),
    mainPanel(
      
      div(style="width:1200px;",fluidRow(verbatimTextOutput(outputId = "res", placeholder = TRUE))),
      #verbatimTextOutput(outputId = "res"),
      plotlyOutput("distPlot", width = "1000", height = "400")
          
    )
  )
  
  shinyApp(
    ui = dashboardPage(header, sidebar, body),
    server = function(input, output,session) {
    
    #Plotly outputs displayed
      output$distPlot <- renderPlotly(
        if(input$tabs=="subitem11"){
          switch(input$radio,
                 a = my_plot("White", PA_abs_rates$RATE_ABSENT_WH,"purple", "Black", PA_abs_rates$RATE_ABSENT_BL,"yellow","Absenteeism Rates"),
                 b = my_plot("White", PA_abs_rates$RATE_ABSENT_WH,"purple", "Asian", PA_abs_rates$RATE_ABSENT_AS,"green","Absenteeism Rates"),
                 c = my_plot("White", PA_abs_rates$RATE_ABSENT_WH,"purple", "Hispanic", PA_abs_rates$RATE_ABSENT_HI,"blue","Absenteeism Rates"),
                 d = my_plot("Black", PA_abs_rates$RATE_ABSENT_BL,"yellow", "Asian", PA_abs_rates$RATE_ABSENT_AS,"green","Absenteeism Rates"),
                 e = my_plot("Black", PA_abs_rates$RATE_ABSENT_BL,"yellow", "Hispanic", PA_abs_rates$RATE_ABSENT_HI,"blue","Absenteeism Rates"),
                 f = my_plot("Asian", PA_abs_rates$RATE_ABSENT_AS,"green", "Hispanic", PA_abs_rates$RATE_ABSENT_HI,"blue","Absenteeism Rates")
          )
        }
        else if(input$tabs=="subitem21"){
          switch(input$radio2,
                 a = my_plot("White", PA_bully_rates$RATE_BULLY_WH,"purple", "Black", PA_bully_rates$RATE_BULLY_BL,"yellow","Bullying Rates"),
                 b = my_plot("White", PA_bully_rates$RATE_BULLY_WH,"purple", "Asian", PA_bully_rates$RATE_BULLY_AS,"green","Bullying Rates"),
                 c = my_plot("White", PA_bully_rates$RATE_BULLY_WH,"purple", "Hispanic", PA_bully_rates$RATE_BULLY_HI,"blue","Bullying Rates"),
                 d = my_plot("Black", PA_bully_rates$RATE_BULLY_BL,"yellow", "Asian", PA_bully_rates$RATE_BULLY_AS,"green","Bullying Rates"),
                 e = my_plot("Black", PA_abs_rates$RATE_ABSENT_BL,"yellow", "Hispanic", PA_abs_rates$RATE_ABSENT_HI,"blue","Absenteeism Rates"),
                 f = my_plot("Asian", PA_bully_rates$RATE_BULLY_AS,"green", "Hispanic", PA_bully_rates$RATE_BULLY_HI,"blue","Bullying Rates")        
          )
        }
        else if(input$tabs=="subitem31"){
          switch(input$radio3,
                 a = my_plot("White", PA_iss_rates$RATE_ISS_WH,"purple", "Black", PA_iss_rates$RATE_ISS_BL,"yellow","In school Suspension(ISS) Rates"),
                 b = my_plot("White", PA_iss_rates$RATE_ISS_WH,"purple", "Asian", PA_iss_rates$RATE_ISS_AS,"green","In school Suspension(ISS) Rates"),
                 c = my_plot("White", PA_iss_rates$RATE_ISS_WH,"purple", "Hispanic", PA_iss_rates$RATE_ISS_HI,"blue","In school Suspension(ISS) Rates"),
                 d = my_plot("Black", PA_iss_rates$RATE_ISS_BL,"yellow", "Asian", PA_iss_rates$RATE_ISS_AS,"green","In school Suspension(ISS) Rates"),
                 e = my_plot("Black", PA_iss_rates$RATE_ISS_BL,"yellow", "Hispanic", PA_iss_rates$RATE_ISS_HI,"blue","In school Suspension(ISS) Rates"),
                 f = my_plot("Asian", PA_iss_rates$RATE_ISS_AS,"green", "Hispanic", PA_iss_rates$RATE_ISS_HI,"blue","In school Suspension(ISS) Rates")
          )
          
        }
        else if(input$tabs == "dashboard_sc"){
          vals <- unique(scales::rescale(c(corr_matrix)))
          o <- order(vals, decreasing = FALSE)
          cols <- scales::col_numeric("Reds", domain = NULL)(vals)
          colz <- setNames(data.frame(vals[o], cols[o]), NULL)
          plot_ly( x = c("SAL_PER_TEACH","RATE_LEP","STU_TEA_RATIO", "TEA_ABSENT", "EXP_STU", "RATE_ABSENT", "RATE_ISS", "RATE_BULLY"),
                   y = c("SAL_PER_TEACH","RATE_LEP","STU_TEA_RATIO", "TEA_ABSENT", "EXP_STU", "RATE_ABSENT", "RATE_ISS", "RATE_BULLY"),z = corr_matrix, type = "heatmap",colorscale = colz)
        }
        else{
          if(input$tabs == "subitem12"){
            my_plot("Male", PA_abs_rates$RATE_ABSENT_M,"black", "Female", PA_abs_rates$RATE_ABSENT_F,"red","Absenteeism Rates")
          }
          else if(input$tabs == "subitem22"){
            my_plot("Male", PA_bully_rates$RATE_BULLY_M,"black", "Female", PA_bully_rates$RATE_BULLY_F,"red","Bullying Rates")
          }
          else if(input$tabs == "subitem32"){
            my_plot("Male", PA_iss_rates$RATE_ISS_M,"black", "Female", PA_iss_rates$RATE_ISS_F,"red","In School Suspension (ISS) Rates")
          }
        }
      )
      output$res <- renderPrint(
        if(input$tabs=="subitem11"){
          switch(input$radio,
                 a = cat("The rates of Blacks and Whites who are chronically absent are significantly different with a p-value of", 
                         t.test(PA_rates$RATE_ABSENT_BL, PA_rates$RATE_ABSENT_WH)$p.value),
                 b = cat("The rates of Asians and Whites who are chronically absent are significantly different with a p-value of", 
                         t.test(PA_rates$RATE_ABSENT_AS, PA_rates$RATE_ABSENT_WH)$p.value),
                 c = cat("The rates of Hispanics and Whites who are chronically absent are significantly different with a p-value of", 
                         t.test(PA_rates$RATE_ABSENT_HI, PA_rates$RATE_ABSENT_WH)$p.value),
                 d = cat("The rates of Blacks and Asians who are chronically absent are significantly different with a p-value of", 
                         t.test(PA_rates$RATE_ABSENT_BL, PA_rates$RATE_ABSENT_AS)$p.value),
                 e = cat("The rates of Hispanics and Blacks who are chronically absent are significantly different with a p-value of", 
                         t.test(PA_rates$RATE_ABSENT_HI, PA_rates$RATE_ABSENT_BL)$p.value),
                 f = cat("The rates of Hispanics and Asians who are chronically absent are significantly different with a p-value of", 
                         t.test(PA_rates$RATE_ABSENT_HI, PA_rates$RATE_ABSENT_AS)$p.value)
          )
        }
        else if(input$tabs=="subitem21"){
          switch(input$radio2,
                 a = cat("The rates of Blacks and Whites reporting bullying on the basis of race are significantly different with a p-value of", t.test(PA_rates$RATE_BULLY_BL, PA_rates$RATE_BULLY_WH)$p.value),
                 b = cat("The rates of Asians and Whites reporting bullying on the basis of race are not significantly different with a p-value of", t.test(PA_rates$RATE_BULLY_AS, PA_rates$RATE_BULLY_WH)$p.value),
                 c = cat("The rates of Hispanics and Whites reporting bullying on the basis of race are significantly different with a p-value of", t.test(PA_rates$RATE_BULLY_HI, PA_rates$RATE_BULLY_WH)$p.value),
                 d = cat("The rates of Blacks and Asians reporting bullying on the basis of race are significantly different with a p-value of", t.test(PA_rates$RATE_BULLY_BL, PA_rates$RATE_BULLY_AS)$p.value),
                 e = cat("The rates of Hispanics and Blacks reporting bullying on the basis of race are significantly different with a p-value of", t.test(PA_rates$RATE_BULLY_HI, PA_rates$RATE_BULLY_BL)$p.value),
                 f = cat("The rates of Hispanics and Asians reporting bullying on the basis of race are not significantly different with a p-value of", t.test(PA_rates$RATE_BULLY_HI, PA_rates$RATE_BULLY_AS)$p.value)
          )
        }
        else if(input$tabs == "subitem31"){
          switch(input$radio3,
                 a = cat("The rates of Blacks and Whites with in-school suspensions are significantly different with a p-value of", t.test(PA_rates$RATE_ISS_BL, PA_rates$RATE_ISS_WH)$p.value),
                 b = cat("The rates of Asians and Whites with in-school suspensions are significantly different with a p-value of", t.test(PA_rates$RATE_ISS_AS, PA_rates$RATE_ISS_WH)$p.value),
                 c = cat("The rates of Hispanics and Whites with in-school suspensions are significantly different with a p-value of", t.test(PA_rates$RATE_ISS_HI, PA_rates$RATE_ISS_WH)$p.value),
                 d = cat("The rates of Blacks and Asians with in-school suspensions are significantly different with a p-value of", t.test(PA_rates$RATE_ISS_BL, PA_rates$RATE_ISS_AS)$p.value),
                 e = cat("The rates of Hispanics and Blacks with in-school suspensions are significantly different with a p-value of", t.test(PA_rates$RATE_ISS_HI, PA_rates$RATE_ISS_BL)$p.value),
                 f = cat("The rates of Hispanics and Asians with in-school suspensions are significantly different with a p-value of", t.test(PA_rates$RATE_ISS_HI, PA_rates$RATE_ISS_AS)$p.value)
          )
        }
        else if(input$tabs == "dashboard_sc"){
          cat("We observed a positive correlation coefficient of 0.18 between students having limited english proficiency (LEP) and the Absenteeism Rate.",
              "We also observed a positive correlation coefficient of 0.12 betweeen In-School Suspensions and Bullying Rates.",sep = '\n')
        }
        else{
          if(input$tabs == "subitem12"){
            cat("The rates of males and females who are chronically absent are not significantly different with a p-value of", t.test(PA_rates$RATE_ABSENT_M, PA_rates$RATE_ABSENT_F)$p.value)
          }
          else if(input$tabs == "subitem22"){
            cat("The rates of males and females reporting bullying on the basis of sex are significantly different with a p-value of", t.test(PA_rates$RATE_BULLY_M, PA_rates$RATE_BULLY_F)$p.value)
          }
          else if(input$tabs == "dashboard_dataD"){
            cat("RATE_BULLY: For ethnicities, the number of students reported as harassed or bullied on the basis of race, color or national origin divided by the total number of enrolled students. For genders, the number of students reported as harassed or bullied on the basis of sex divided by the total number of enrolled students.", "RATE_ISS: The number of students without disabilities who received one or more in-school suspensions divided by the total number of enrolled students.", "RATE_ABSENT: The number of students absent 15 or more days in the school year divided by the total number of enrolled students.", "RATE_LEP: The number of students who have limited English proficiency divided by the total number of enrolled students.", "SAL_PER_TEACH: Total salary expenditures for all teachers divided by the total FTE of teachers.", "TEA_ABSENT: Number of FTE teachers who were absent more than 10 school days during the school year divided by the number of FTE teachers.", "STU_TEA_RATIO: Total number of enrolled students divided by the number of FTE teachers.", "STU_EXP: Total salary expenditures for all personnel plus total non-salary expenditures divided by the total number of enrolled students.", sep="\n")
          }
          else if(input$tabs == "dashboard_about"){
            cat("About the data: The Civil Rights Data Collection (CRDC) 2015-16 includes data from a survey of public school districts in the United States for school year 2015-16. It is submitted biannually to the US Department of Education Office of Civil Rights. Data includes school characteristics, services, and outcomes, most of which are disaggregated by race, ethnicity, gender, limited English proficiency status, and disability. Each school district submits data from the relevant year and certifies that it is correct. The ","public-use data is suppressed to protect confidentiality. Our data focuses on the Pennsylvania School Disctrict.","\n","A quick look at the population being analysed :","\n",
                
                "No. of White Students :","\t",enroll_sums["SCH_ENR_WH"],"\n",
                
                "No. of Black Students :","\t",enroll_sums["SCH_ENR_BL"],"\n",
                
                "No. of Asian Students :","\t", enroll_sums["SCH_ENR_AS"],"\n",
                
                "No. of Hispanic Students :", "\t",enroll_sums["SCH_ENR_HI"],"\n",
                
                "No. of Male Students :","\t", enroll_sums["TOT_ENR_M"],"\n",
                
                "No. of Female Students :","\t", enroll_sums["TOT_ENR_F"],"\n",
                "We analysed school indicators like Absenteeism, Bullying and In-school Suspension based on ethnicities and school characteristics.","\n","Hypothesis : Our Null Hypothesis is that there is no signifcant difference in the mean of these indicators(rates) between two","\n","ethnic groups.")
          }
          else{
            cat("The rates of males and females with in-school suspensions are significantly different with a p-value of", round(t.test(PA_rates$RATE_ISS_M, PA_rates$RATE_ISS_F)$p.value, 8))
          }
        }
      )
    }
  )
}

