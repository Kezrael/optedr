library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(shinyalert)
library(plotly)
library(tidyverse)
library(hrbrthemes)
library(nleqslv)
library(shinyjs)


# Source dopt ----
source("dopt.R")

ui <- dashboardPage(
  dashboardHeader(title = "Design Calculator"),
  dashboardSidebar(
    sidebarMenu(id="sidebar",
                tags$head(tags$style(HTML(
                  '.myClass {
                font-size: 20px;
                line-height: 50px;
                text-align: left;
                font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                padding: 0 15px;
                overflow: hidden;
                color: white;
                }
                '))),
                tags$script(HTML('
                               $(document).ready(function() {
                               $("header").find("nav").append(\'<div id="pageHeader" class="myClass"></div>\');
                               })
                               '))
                ,
                useShinyjs(),
                menuItem("Presentation", tabName = "presentation", icon = icon("info")),
                menuItem("Optimal design", tabName = "optdes", icon = icon("calculator")),
                menuItem("Custom design", tabName = "custom", icon = icon("clipboard")),
                menuItem("Efficiency of industry designs", tabName = "compare", icon = icon("book")),
                # menuItem("Help", tabName = "help", icon = icon("question")),
                menuItem("Contact", tabName = "contact", icon = icon("envelope"))
    ),
    hr(),
    conditionalPanel(condition="input.sidebar == 'optdes'",
        selectizeInput(
          'variance', 'Variance Structure', choices = c("Homoscedastic", "Heteroscedastic"),
          options = list(
            placeholder = 'Type the desired variance',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectizeInput(
          'criteria', 'Optimality Criteria', choices = c("D-Optimality", "A-Optimality", "Ds-Optimality", "I-Optimality"),
          options = list(
            placeholder = 'Type the desired criteria',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectizeInput(
          'substance', 'Substance', choices = c("Water", "Acetic Acid", "Benzene", "Carbon Dioxide"#, "Methane"
                                                , "Naphtalene"),
          options = list(
            placeholder = 'Optionally choose a substance',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        sliderInput("design_sp", "Design Space",
                    min = -200, max = 400, value = c(1, 100)
        ),
        numericInput('a_par', 'Initial value for parameter a', value = 8.07131, step = 0.01
        ),
        numericInput('b_par', 'Initial value for parameter b', value = 1730.63, step = 0.01
        ),
        numericInput('c_par', 'Initial value for parameter c', value = 233.426, step = 0.01
        ),
        conditionalPanel("input.criteria == 'I-Optimality'",
                         sliderInput("int_region", "Region of Interest",
                                     min = -200, max = 400, value = c(80, 120)),
                         radioButtons("radio_integrate", label = h3("Distribution of choice"),
                                      choices = list("Uniform" = 1, "Triangular" = 2),
                                      selected = 1)
        ),
        conditionalPanel("input.radio_integrate == 2 && input.criteria == 'I-Optimality'",
                         uiOutput("vertexChoice")
        ),
        conditionalPanel("input.criteria == 'Ds-Optimality'",
                         checkboxGroupInput("intPars", "Parameters of Interest:",
                                            c("A" = 1,
                                              "B" = 2,
                                              "C" = 3))
        ),
        conditionalPanel("input.criteria == 'Ds-Optimality' || input.criteria == 'I-Optimality' || input.criteria == 'A-Optimality'",
                         checkboxInput("checkAlgoC", "Show convergence of algorithm?", FALSE)),
        actionButton("calc_opt_design", "Calculate Optimal Design"
        )
    ),
    #sliderInput("int_region", "Region of Interest",
    #            min = 1, max = 374, value = c(1, 100)
    #),
    #tags$p("Only for I-Optimality"
    #),

    conditionalPanel(condition="input.sidebar == 'custom'",
        numericInput('points_add', '1. Points', value = 0, min = 1, max = 374, step = 1
        ),
        conditionalPanel('input.points_add != 0',
                         numericInput('weights_add', '2. Weights', value = 0, min = 0, max = 1, step = 0.01)
        ),
        actionButton("add", "Add point"
        ),
        actionButton("remove", "Remove point"
        ),
        actionButton("both_designs", "Calculate efficiency"
        )
    ),
    conditionalPanel(condition="input.sidebar == 'compare'",
       conditionalPanel(condition="input.criteria == 'D-Optimality' || input.criteria == 'Ds-Optimality'",
         selectizeInput(
           'family', 'Choose a family of designs', choices = c("Equidistant", "Arithmetic", "Geometric"
           ),
           options = list(
             placeholder = 'Type the desired family',
             onInitialize = I('function() { this.setValue(""); }')
           )
         )
       ),
       conditionalPanel(condition="input.criteria == 'A-Optimality' || input.criteria == 'I-Optimality'",
                        selectizeInput(
                          'family2', 'Choose a family of designs', choices = c("Equidistant"#, "Arithmetic", "Geometric"
                          ),
                          options = list(
                            placeholder = 'Type the desired family',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        )
       ),
       sliderInput("ind_points", "Points of the design",
                   min = 3, max = 20, value = 5
       ),
       actionButton("calc_ind_eff", "Calculate Efficiencies"
       )
    )
  ),
  dashboardBody(
    useShinyalert(),
    tabItems(
      tabItem(tabName = "presentation", fluidPage(withMathJax(includeMarkdown("presentacion.Rmd")))),
      tabItem(tabName = "optdes",
              fluidRow(
                valueBox(value = "Optimum Design"#tags$p("Optimum Design", style = "font-size: 75%;")
                         , "For the selected criterion", icon = icon("check"), color = "yellow"),
                valueBoxOutput("op_sensitivity")
              ),
              fluidRow(
                box(title = "Optimum design",
                    solidHeader = T,
                    width = 4,
                    collapsible = T,
                    div(DT::DTOutput("opt_df"), style = "font-size: 120%;")
                ),
                box(title = "Sensitivity Function", solidHeader = T,
                    width = 8, collapsible = T,
                    plotlyOutput("sens_opt")
                )
              ), # row
              uiOutput('conditional_selection')
      ),
      tabItem(tabName = "custom",
              fluidRow(
                valueBoxOutput("new_design"),
                valueBoxOutput("new_sensitivity"),
                valueBoxOutput("efficiency")
              ), # row
              fluidRow(
                box(title = "New design",
                    solidHeader = T,
                    width = 4,
                    collapsible = T,
                    div(DT::DTOutput("custom_des"), style = "font-size: 120%;")
                ),
                box(title = "New sensitivity", solidHeader = T,
                    width = 8, collapsible = T,
                    plotlyOutput("sens_new")
                )
              )
      ),
      tabItem(tabName = "compare",
    # Row --
    fluidRow(
      valueBoxOutput("industry_des"),
      valueBoxOutput("industry_sens"),
      valueBoxOutput("efficiency_ind")
    ),
    # Row --fas
    fluidRow(
      box(title = "Industry design",
          solidHeader = T,
          width = 4,
          collapsible = T,
          div(DT::DTOutput("ind_df"), style = "font-size: 120%;")
      ),
      box(title = "Sensitivity Function", solidHeader = T,
          width = 8, collapsible = T,
          plotlyOutput("sens_ind")
      )
    )
    ),
      # tabItem(tabName = "help", fluidPage(withMathJax(includeMarkdown("help.Rmd")))),
      tabItem(tabName = "contact", fluidPage(withMathJax(includeMarkdown("contact.Rmd"))))
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # make reactive to store points
  ing_df <- shiny::reactiveValues()
  
  # Tm<-86;
  # TM<-218;
  ##Par?metros
  # p1<-8.07763 ;
  # p2<-2603.26 ;
  # p3<-282.769 ;
  # Update Nominal Values and Space Design based on Substance
  observeEvent(input$substance, {
    updateSliderInput(session, "design_sp", value = c(switch (input$substance,
                                                          "Water" = 1,
                                                          "Acetic Acid" = 17,
                                                          "Benzene" = 8,
                                                          "Carbon Dioxide" = -62,
                                                          #"Methane" = -181,
                                                          "Naphtalene" = 86
                                                      ), switch (input$substance,
                                                          "Water" = 100,
                                                          "Acetic Acid" = 118,
                                                          "Benzene" = 80,
                                                          "Carbon Dioxide" = 31,
                                                          #"Methane" = -163,
                                                          "Naphtalene" = 218
                                                      )
                      )
    )
    updateSliderInput(session, "int_region", value = c(switch (input$substance,
                                                          "Water" = 80,
                                                          "Acetic Acid" = 110,
                                                          "Benzene" = 75,
                                                          "Carbon Dioxide" = 20,
                                                          #"Methane" = -170,
                                                          "Naphtalene" = 200
                                                      ), switch (input$substance,
                                                          "Water" = 100,
                                                          "Acetic Acid" = 118,
                                                          "Benzene" = 80,
                                                          "Carbon Dioxide" = 31,
                                                          #"Methane" = -140,
                                                          "Naphtalene" = 218
                                                      )
                      )
    )
    updateNumericInput(session, "a_par", value = switch (input$substance,
                                                         "Water" = 8.07131,
                                                         "Acetic Acid" = 7.5596,
                                                         "Benzene" = 6.87987,
                                                         "Carbon Dioxide" = 7.5322,
                                                         #"Methane" = 6.34159,
                                                         "Naphtalene" = 8.07763
    ))
    updateNumericInput(session, "b_par", value =switch (input$substance,
                                                        "Water" = 1730.63,
                                                        "Acetic Acid" = 1644.05,
                                                        "Benzene" = 1196.76,
                                                        "Carbon Dioxide" = 835.06,
                                                        #"Methane" = 342.22,
                                                        "Naphtalene" = 2603.26
    ))
    updateNumericInput(session, "c_par", value = switch (input$substance,
                                                         "Water" = 233.426,
                                                         "Acetic Acid" = 233.524,
                                                         "Benzene" = 219.161,
                                                         "Carbon Dioxide" = 268.223,
                                                         #"Methane" = 260.221,
                                                         "Naphtalene" = 282.769
    ))
  })
  
  # Update the input dataframe
  observeEvent(input$remove, {
    isolate(ing_df$design<-ing_df$design[-(nrow(ing_df$design)),])
  })
  
  # Remove
  observeEvent(input$add, {
    isolate(ing_df$design[nrow(ing_df$design) + 1,] <- as.list(c(input$points_add,
                                                                 input$weights_add)
    ))
    
    # update choices
    updateNumericInput(session, 'weights_add', '2. Weights', 0)
    updateNumericInput(session, 'points_add', '1. Points', value = 0)
  })
  
  # Render custom design
  output$custom_des <- DT::renderDataTable({
    datatable(arrange(ing_df$design, Point), rownames=F, options = list(pageLength = 5)) %>% formatRound(c(1,2), 3)
  })
  
  # Industrial des
  output$ind_df <- DT::renderDataTable({
    datatable(arrange(ing_df$ind_des, Point), rownames=F, options = list(pageLength = 10)) %>% formatRound(c(1,2), 3)
  })
  
  # Render optimum design
  output$opt_df <- DT::renderDataTable({
    datatable(arrange(ing_df$opt_design, Point), rownames=F, options = list(pageLength = 5)) %>% formatRound(c(1,2), 3)
  })
  
  # Optimum design
  ing_df$opt_design <- data.frame("Point" = numeric(),
                                  "Weight" = numeric())
  
  # Custom design
  ing_df$design <- data.frame("Point" = numeric(),
                              "Weight" = numeric())
  
  # Convergence of the Algorithm
  ing_df$alg_conv <- data.frame("criteria" = numeric(),
                                "step" = numeric())
  
  # Industry design
  ing_df$ind_des <- data.frame("Point" = numeric(),
                               "Weight" = numeric())
  
  
  # Render I-Opt triang center
  output$vertexChoice <- renderUI({
    sliderInput("vertex", "Vertex of the triangule",
                min = input$int_region[[1]], max = input$int_region[[2]], value = (input$int_region[[2]]+input$int_region[[1]])/2)
  })
  
  
  observeEvent(input$calc_opt_design, {
    if(!input$criteria %in% c("D-Optimality", "Ds-Optimality", "A-Optimality", "I-Optimality") |
       !input$variance %in% c("Homoscedastic", "Heteroscedastic")){
      shinyalert("Enter a valid criterion and variance!", type = "error")
    }
    else if(identical(input$criteria, "Ds-Optimality") & length(input$intPars) < 1){
      shinyalert("Select the parameters of interest", type = "warning")
    }
    else if(identical(input$criteria, "Ds-Optimality") & length(input$intPars) > 2){
      shinyalert("You cannot select all three parameters", type = "error")
    }
    else {
      if (identical(input$criteria, "I-Optimality")){
        if(input$radio_integrate == 1)
          B <- IntMatAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$int_region[[1]], input$int_region[[2]], 0, option = "Unif")
        else if(input$radio_integrate == 2)
          B <- IntMatAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$int_region[[1]], input$int_region[[2]], input$vertex, option = "Triang")
        else
          B <- diag(3)
      }
      else
        B <- diag(3)
      if(identical(input$criteria, "D-Optimality")){
        ing_df$opt_design <- doptAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$design_sp[[1]], input$design_sp[[2]])
        line_top <- 3
      } else {
        des <- data.frame(Point = c(input$design_sp[[1]], (input$design_sp[[1]]+input$design_sp[[2]])/2, input$design_sp[[2]]), Weight = c(1/3, 1/3, 1/3))
        outputWF <- WF(input$variance, input$criteria, input$a_par, input$b_par, input$c_par, des, as.numeric(input$intPars), B, min = input$design_sp[[1]], max = input$design_sp[[2]], grid.length = 1000, joinThresh = 9, deleteThresh = 0.01, k = 3, s = length(as.numeric(input$intPars)), 1/2, 0.00001)
        ing_df$opt_design <- outputWF$optdes
        ing_df$conv_plot <- ggplot(data = outputWF$convergence, aes(x=step, y=criteria)) +
          geom_line( color="coral1") +
          #geom_point() +
          theme_ipsum()
        if(identical(input$criteria, "Ds-Optimality"))
          line_top <- length(input$intPars)
        else
          line_top <- crit(input$criteria, dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$opt_design), k = 3, s = length(intPars), intPars = as.numeric(input$intPars), matB = B)
      }
      mat <- dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$opt_design)
      sens_opt <- sens(input$variance, input$criteria, input$a_par, input$b_par, input$c_par, mat, as.numeric(input$intPars), B)
      
      x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
      y_val <- purrr::map_dbl(x_val, sens_opt)
      
      p <- ggplot() + theme_ipsum()
      
      ing_df$sens_opt <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  line_top, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "")
    }
  })
  
  
  
  # Render Graph and Box for Algo convergence if asked for
  
  observeEvent(input$checkAlgoC,{
    if(input$checkAlgoC == TRUE){
      output$conditional_selection <- renderUI({
        div(fluidRow(
          valueBox(value = "Convergence"#tags$p("Optimum Design", style = "font-size: 75%;")
                   , "Value of the Criteria along the steps", icon = icon("check"), color = "navy"),
          valueBoxOutput("conv_box")
        ),
        box(title = "Convergence Plot", solidHeader = T,
            width = 12, collapsible = T,
            plotlyOutput("conv_plot")
        )
        )
      })
    }
    else{
      output$conditional_selection <- NULL
    }
  })
  
  
  
  # Efficiency of the new design
  # Optimum design
  # ing_df$eff <- ""
  
  observeEvent(input$both_designs, {
    criterion <- input$criteria
    compute <- TRUE
    length1 <- nrow(ing_df$design)
    ing_df$design <- ing_df$design %>%
      group_by(Point) %>%
      summarise_all(funs(sum))
    
    if(length1 != nrow(ing_df$design)){
      shinyalert("", "There are repeated points in the design, they have been added together"
                 , type = "warning"
                 , timer = 1500)
    }
    
    if(length(ing_df$design[["Point"]]) < 3)
    {
      shinyalert("The design is singular!", type = "error")
      compute <- FALSE
    }
    else if(0 %in% ing_df$design[["Weight"]])
    {
      compute <- FALSE
      shinyalert("Error", "There are points without weight!", type = "error")
    }
    else if(max(ing_df$design[["Point"]]) > input$design_sp[[2]] || min(ing_df$design[["Point"]]) < input$design_sp[[1]])
    {
      compute <- FALSE
      shinyalert("Error", "There are design points outside the design space!", type = "error")
    }
    else if(!near(1, sum(ing_df$design[["Weight"]])))
    {
      shinyalert("", "The weights don't add up to 1, it'll be corrected"
                 # , do you want us to fix that for you?"
                 , type = "warning"
                 # , showCancelButton = TRUE, showConfirmButton = TRUE, callbackR = mycallback
                 , timer = 1500)
      # if (TRUE) {
      ing_df$design[["Weight"]] <- ing_df$design[["Weight"]]/sum(ing_df$design[["Weight"]])
      # }
      # else {
      #   compute <- FALSE
      # }
    }
    if(isTRUE(compute)) {
      if(!input$criteria %in% c("D-Optimality", "Ds-Optimality", "A-Optimality", "I-Optimality") |
         !input$variance %in% c("Homoscedastic", "Heteroscedastic")){
        shinyalert("Enter a valid criterion and variance!", type = "error")
      }
      else if(identical(input$criteria, "Ds-Optimality") & length(input$intPars) < 1){
        shinyalert("Select the parameters of interest", type = "warning")
      }
      else if(identical(input$criteria, "Ds-Optimality") & length(input$intPars) > 2){
        shinyalert("You cannot select all three parameters", type = "error")
      }
      else {
        if (identical(input$criteria, "I-Optimality")){
          if(input$radio_integrate == 1)
            B <- IntMatAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$int_region[[1]], input$int_region[[2]], 0, option = "Unif")
          else if(input$radio_integrate == 2)
            B <- IntMatAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$int_region[[1]], input$int_region[[2]], input$vertex, option = "Triang")
          else
            B <- diag(3)
        }
        else
          B <- diag(3)
        if(identical(input$criteria, "D-Optimality")){
          ing_df$opt_design <- doptAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$design_sp[[1]], input$design_sp[[2]])
          line_top <- 3
          line_top_opt <- 3
        } else {
          des <- data.frame(Point = c(input$design_sp[[1]], (input$design_sp[[1]]+input$design_sp[[2]])/2, input$design_sp[[2]]), Weight = c(1/3, 1/3, 1/3))
          outputWF <- WF(input$variance, input$criteria, input$a_par, input$b_par, input$c_par, des, as.numeric(input$intPars), B, min = input$design_sp[[1]], max = input$design_sp[[2]], grid.length = 1000, joinThresh = 9, deleteThresh = 0.01, k = 3, s = length(as.numeric(input$intPars)), 1/2, 0.00001)
          ing_df$opt_design <- outputWF$optdes
          ing_df$conv_plot <- ggplot(data = outputWF$convergence, aes(x=step, y=criteria)) +
            geom_line( color="coral1") +
            #geom_point() +
            theme_ipsum()
          if(identical(input$criteria, "Ds-Optimality")){
            line_top <- length(input$intPars)
            line_top_opt <- length(input$intPars)
          }
          else {
            line_top <- crit(input$criteria, dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$design), k = 3, s = length(intPars), intPars = as.numeric(input$intPars), matB = B)
            line_top_opt <- crit(input$criteria, dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$opt_design), k = 3, s = length(intPars), intPars = as.numeric(input$intPars), matB = B)
          }
          
        }
        mat_opt <- dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$opt_design)
        mat <- dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$design)
        ing_df$eff <- eff(input$criteria, mat, mat_opt, k = 3, s = length(input$intPars), intPars = as.numeric(input$intPars), matB = B)
        sens_opt <- sens(input$variance, input$criteria, input$a_par, input$b_par, input$c_par, mat_opt, as.numeric(input$intPars), B)
        sens_new <- sens(input$variance, input$criteria, input$a_par, input$b_par, input$c_par, mat, as.numeric(input$intPars), B)
        
        x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
        
        y_val_opt <- purrr::map_dbl(x_val, sens_opt)
        y_val_new <- purrr::map_dbl(x_val, sens_new)
        
        p <- ggplot() + theme_ipsum()
        
        ing_df$sens_opt <- p + geom_line(mapping = aes(x = x_val, y = y_val_opt), color = "steelblue3") + geom_hline(yintercept =  line_top_opt, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "")
        ing_df$sens_new <- p + geom_line(mapping = aes(x = x_val, y = y_val_new), color = "steelblue3") + geom_hline(yintercept =  line_top, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "")
      }
    }
  })
  
  # Efficiency of the industry designs
  observeEvent(input$calc_ind_eff, {
    calculate <- T
    B <- diag(3)
    if(identical(input$criteria, "D-Optimality")) {
      ing_df$opt_design <- doptAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$design_sp[[1]], input$design_sp[[2]])
      opt_mat <- dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$opt_design)
      line_top <- 3
    }
    else if(input$criteria %in% c("Ds-Optimality", "I-Optimality", "A-Optimality")){
      if (identical(input$criteria, "I-Optimality")){
        if(input$radio_integrate == 1)
          B <- IntMatAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$int_region[[1]], input$int_region[[2]], 0, option = "Unif")
        else if(input$radio_integrate == 2)
          B <- IntMatAntoine(input$variance, input$a_par, input$b_par, input$c_par, input$int_region[[1]], input$int_region[[2]], input$vertex, option = "Triang")
      }
      des <- data.frame(Point = c(input$design_sp[[1]], (input$design_sp[[1]]+input$design_sp[[2]])/2, input$design_sp[[2]]), Weight = c(1/3, 1/3, 1/3))
      outputWF <- WF(input$variance, input$criteria, input$a_par, input$b_par, input$c_par, des, as.numeric(input$intPars), B, min = input$design_sp[[1]], max = input$design_sp[[2]], grid.length = 1000, joinThresh = 9, deleteThresh = 0.01, k = 3, s = length(as.numeric(input$intPars)), 1/2, 0.00001)
      ing_df$opt_design <- outputWF$optdes
      opt_mat <- dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$opt_design)
    }
    else{
      shinyalert("Error", "Enter a valid criterion!", type = "error")
      calculate <- F
    }
    if(input$criteria %in% c("D-Optimality", "Ds-Optimality")){
      if(identical(input$family, "Equidistant")){
        ing_df$ind_des <- uniform_design(input$ind_points, input$design_sp[[1]], input$design_sp[[2]])
      }
      else if(identical(input$family, "Arithmetic")){
        out <- tryCatch(
          {
            arithmetic_design(as.numeric(input$ind_points), as.numeric(input$design_sp[[1]]), as.numeric(input$design_sp[[2]]), opt_mat, input$criteria, input$a_par, input$b_par, input$c_par, k = 3, s = length(input$intPars), intPars = as.numeric(input$intPars), matB = B)
          }, error=function(e) {
            cat(paste("in err handler\n"))
            shinyalert("Error", "The problem was computationally singular", type = "error")
            # Choose a return value in case of error
            return(data.frame("Point" = numeric(),
                              "Weight" = numeric()))
          }, warning=function(w) {
            cat(paste("in warn handler\n"))
            shinyalert("Error", "Error", type = "error")
            # Choose a return value in case of error
            return(data.frame("Point" = numeric(),
                              "Weight" = numeric()))
          }, finally={
            # message("I'm here")
          })
        ing_df$ind_des <- out
      }
      else if(identical(input$family, "Geometric")){
        out <- tryCatch(
          {
            geometric_design(input$variance, as.numeric(input$ind_points), as.numeric(input$design_sp[[1]]), as.numeric(input$design_sp[[2]]), opt_mat, input$criteria, input$a_par, input$b_par, input$c_par, k = 3, s = length(input$intPars), intPars = as.numeric(input$intPars), matB = B)
          }, error=function(e) {
            cat(paste("in err handler\n"))
            shinyalert("Error", "The problem was computationally singular", type = "error")
            # Choose a return value in case of error
            return(data.frame("Point" = numeric(),
                              "Weight" = numeric()))
          }, warning=function(w) {
            cat(paste("in warn handler\n"))
            shinyalert("Error", "Error", type = "error")
            # Choose a return value in case of error
            return(data.frame("Point" = numeric(),
                              "Weight" = numeric()))
          }, finally={
            # message("I'm here")
          })
        
        ing_df$ind_des <- out
      }
      else{
        shinyalert("Error", "Choose a family of designs!", type = "error")
        calculate <- F
      }
    }
    else{
      if(identical(input$family2, "Equidistant")){
        ing_df$ind_des <- uniform_design(input$ind_points, input$design_sp[[1]], input$design_sp[[2]])
      }
      else{
        shinyalert("Error", "Choose a valid family of designs for this criterion!", type = "error")
        calculate <- F
      }
    }
    if(calculate & nrow(ing_df$ind_des)>0){
      if(identical(input$criteria, "Ds-Optimality")){
        line_top <- length(input$intPars)
      }
      else if(input$criteria %in% c("I-Optimality", "A-Optimality")){
        line_top <- crit(input$criteria, dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$opt_design), k = 3, s = length(intPars), intPars = as.numeric(input$intPars), matB = B)
      }
      ind_mat <- dmatrixAntoine(input$variance, input$a_par, input$b_par, input$c_par, ing_df$ind_des)
      ing_df$effind <- eff(input$criteria, ind_mat, opt_mat, k = 3, s = length(input$intPars), intPars = as.numeric(input$intPars), matB = B)
      
      sens_new <- sens(input$variance, input$criteria, input$a_par, input$b_par, input$c_par, ind_mat, as.numeric(input$intPars), B)
      
      p <- ggplot() + theme_ipsum()
      new_des_data <- data.frame("Point" = ing_df$ind_des[["Point"]], "Weight" = ing_df$ind_des[["Weight"]], "Value" = rep(0, length(ing_df$ind_des[["Point"]])))
      
      x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
      y_val <- purrr::map_dbl(x_val, sens_new)
      
      ing_df$sens_ind <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  line_top, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))
    }
    
  })
  
  # Compute Effinciency
  efficiency <- reactive({
    if(is.numeric(ing_df$eff))
      round(ing_df$eff*100, 1)
    else
      ing_df$eff
  })
  
  # Compute Effinciency industry
  efficiency_indus <- reactive({
    if(is.numeric(ing_df$effind))
      round(ing_df$effind*100, 1)
    else
      ing_df$effind
  })
  
  # Plot sens
  ing_df$sens_opt <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()
  
  ing_df$sens_new <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()
  
  ing_df$sens_ind <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()
  
  output$sens_opt <- renderPlotly({
    ggplotly(ing_df$sens_opt)
  })
  
  output$sens_new <- renderPlotly({
    ggplotly(ing_df$sens_new)
  })
  
  output$sens_ind <- renderPlotly({
    ggplotly(ing_df$sens_ind)
  })
  
  # Plot Conv
  ing_df$conv_plot <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()
  
  output$conv_plot <- renderPlotly({
    ggplotly(ing_df$conv_plot)
  })
  
  # value boxes
  output$op_sensitivity <- renderValueBox({
    valueBox(
      paste0("Sensitivity Function"#, output$efficiency
      ), "For the optimum design", icon = icon("bar-chart-o"),
      color = "purple"
    )
  })
  output$new_design <- renderValueBox({
    valueBox(
      paste0("Input Design"
      ), "By the user", icon = icon("clipboard-list"),
      color = "aqua"
    )
  })
  output$new_sensitivity <- renderValueBox({
    valueBox(
      paste0("Sensitivity Function"
      ), "For the input design", icon = icon("bar-chart-o"),
      color = "green"
    )
  })
  output$efficiency <- renderValueBox({
    valueBox(
      paste0("Efficiency ", efficiency(), "%" #input$count
      ), "For the input design", icon = icon("divide"),
      color = "red"
    )
  })
  
  output$efficiency_ind <- renderValueBox({
    valueBox(
      paste0("Efficiency ", efficiency_indus(), "%" #input$count
      ), "For the input design", icon = icon("divide"),
      color = "red"
    )
  })
  
  
  observeEvent(input$criteria, {
    header <- input$criteria
    
    # you can use any other dynamic content you like
    shinyjs::html("pageHeader", header)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
