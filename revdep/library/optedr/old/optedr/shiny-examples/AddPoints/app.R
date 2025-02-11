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
library(markdown)
library(knitr)
library(shinyjs)
library(orthopolynom)


# Source dopt ----
source("dopt.R")

# Auxiliar functions -----
source("auxfuncs.R")


# UI -------
ui <- shinydashboard::dashboardPage(

    # Header ---------
    shinydashboard::dashboardHeader(title = "Design Builder"
                    # , tags$li(class = "dropdown", tags$p("Antoine"))
                    ),

    # Sidebar --------
    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id="sidebar",
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
            shinyjs::useShinyjs(),
            shinydashboard::menuItem("Presentation", tabName = "presentation", icon = icon("info")),
            shinydashboard::menuItem("Set up & Optimal design", tabName = "optdes", icon = icon("calculator")),
            shinydashboard::menuItem("Custom design", tabName = "custom", icon = icon("clipboard"),
                     shinydashboard::menuSubItem("Design Proposal", tabName = "givedes"),
                     shinydashboard::menuSubItem("Restricted design", tabName = "restricteddes")),
            shinydashboard::menuItem("Your design", tabName = "your", icon = icon("clipboard"),
                     shinydashboard::menuSubItem("Build your design", tabName = "builddes"),
                     shinydashboard::menuSubItem("Enhance your design", tabName = "enhacedes")
            )
            # menuItem("Efficiency of industry designs", tabName = "compare", icon = icon("book")),
            # menuItem("Help", tabName = "help", icon = icon("question")),
            # menuItem("Contact", tabName = "contact", icon = icon("envelope"))
        ),
        shiny::hr(),
        shiny::conditionalPanel(condition="input.sidebar == 'optdes'",
            shiny::selectizeInput(
            'model',  shiny::HTML(paste("Inputs:",'<br/>',"Choose a model")), choices = c("Antoine Equation", "Quadratic", "Quadratic Heteroscedastic", "Michaelis-Menten"#, "A-Optimality", "Ds-Optimality", "I-Optimality"
                                                           ),
            options = list(
                placeholder = 'Type the desired model',
                onInitialize = I('function() { this.setValue(""); }')
            )
            ),
            shiny::conditionalPanel("input.model == 'Antoine Equation' || input.model == 'Quadratic'",
                shiny::sliderInput("design_sp", "Design Space",
                        min = -100, max = 374, value = c(1, 100)
                )
            ),
            shiny::conditionalPanel("input.model == 'Michaelis-Menten'",
                             shiny::sliderInput("design_sp_MM", "Design Space: [0, value]",
                                         min = 0.01, max = 100, value = 10
                             )
            ),
            shiny::conditionalPanel("input.model == 'Antoine Equation'",
                             shiny::numericInput('a_par', 'Initial value for parameter a', value = 8.07131, step = 0.01
                             ),
                             shiny::numericInput('b_par', 'Initial value for parameter b', value = 1730.63, step = 0.01
                             ),
                             shiny::numericInput('c_par', 'Initial value for parameter c', value = 233.426, step = 0.01
                             )
            ),
            shiny::conditionalPanel("input.model == 'Michaelis-Menten'",
                             shiny::numericInput('k_par', 'Initial value for parameter K', value = 227.27, step = 0.01
                             ),
                             shiny::numericInput('v_par', 'Initial value for parameter V', value = 43.73, step = 0.01
                             )
            ),
            shiny::conditionalPanel("input.model == 'Quadratic Heteroscedastic'",
                             shiny::sliderInput('order', 'Order of the polynomial', min = -0.99, max = 1, value = 0.5
                             ),
                             shiny::sliderInput("design_sp_CH", "Design Space: [0, value]",
                                         min = 8, max = 20, value = 10
                             )
            ),
            shiny::actionButton("calc_opt_design", "Calculate Optimal Design"
        )),
        # conditionalPanel(condition="input.sidebar == 'compare'",
        #                  selectizeInput(
        #                      'family', HTML(paste("Inputs:",'<br/>',"Choose a family of designs")), choices = c("Equidistant", "Arithmetic", "Geometric"#, "A-Optimality", "Ds-Optimality", "I-Optimality"
        #                      ),
        #                      options = list(
        #                          placeholder = 'Type the desired family',
        #                          onInitialize = I('function() { this.setValue(""); }')
        #                      )
        #                  ),
        #                  sliderInput("ind_points", "Points of the design",
        #                              min = 3, max = 20, value = 5
        #                  ),
        #                  actionButton("calc_ind_eff", "Calculate Efficiencies"
        #                  )
        # ),
        shiny::conditionalPanel(condition="input.sidebar == 'givedes' || input.sidebar =='restricteddes'",
            shiny::sliderInput("delta", shiny::HTML(paste("Inputs:",'<br/>',shiny::withMathJax("Weight of the new point(s), alpha"))),
                        min = 0, max = 0.99, value = 0
            ),
            shiny::uiOutput("eff"),
            shiny::actionButton(inputId = "give_points", "Region of Candidate Points"
            )
        ),
        shiny::conditionalPanel("input.sidebar == 'builddes'",
            shiny::numericInput('points_add',  shiny::HTML(paste("Inputs:",'<br/>',"1. Point")), value = -101, min = -100, max = 374, step = 1
            ),
            shiny::conditionalPanel('input.points_add != -101',
                            shiny::numericInput('weights_add', '2. Weight', value = 0, min = 0, max = 1, step = 0.01)
            ),
            shiny::actionButton("add", "Add point"
            ),
            shiny::actionButton("remove", "Remove last point"
            ),
            shiny::actionButton("remove_all", "Remove all points"
            ),
            shiny::actionButton("both_designs", "Calculate efficiency"
            )
            ),
        shiny::conditionalPanel("input.sidebar == 'restricteddes'",
        shiny::conditionalPanel("output.show_rest == 1",
                 shiny::numericInput('points_add_opt', '1. Point', value = -101, min = -100, max = 374, step = 1
                 ),
                 shiny::conditionalPanel('input.points_add_opt != -101',
                                shiny::numericInput('weights_add_opt', '2. Weight', value = 0, min = 0, max = 1, step = 0.01)
                 ),
                 shiny::actionButton("add_opt", "Add point"
                 ),
                 shiny::actionButton("remove_opt", "Remove last point"
                 ),
                 shiny::actionButton("remove_opt_all", "Remove all points"
                 ),
                 shiny::actionButton("with_opt", "Calculate efficiency"
                 )
             )
        ),
        shiny::conditionalPanel(condition="input.sidebar == 'enhacedes'",
                 shiny::sliderInput("deltaenh",  shiny::HTML(paste("Inputs:",'<br/>',"Weight of the new point(s)")),
                             min = 0, max = 0.99, value = 0
                 ),
                 shiny::uiOutput("effenh"),
                 shiny::actionButton("enhace_points", "Region of Candidate Points"
                 ),
                 shiny::conditionalPanel("output.show_enh == 1",
                     shiny::numericInput('points_add_enh', '1. Point', value = -101, min = -100, max = 374, step = 1
                     ),
                     shiny::conditionalPanel('input.points_add_enh != -101',
                                    shiny::numericInput('weights_add_enh', '2. Weight', value = 0, min = 0, max = 1, step = 0.01)
                     ),
                     shiny::actionButton("add_enh", "Add point"
                     ),
                     shiny::actionButton("remove_enh", "Remove last point"
                     ),
                     shiny::actionButton("remove_enh_all", "Remove all points"
                     ),
                     shiny::actionButton("enhace_design", "Calculate efficiency"
                     )
                 )
        ),
        shiny::conditionalPanel("input.sidebar == 'givedes'",
            #span("Test", style="color:white"),
            shiny::actionButton("stand_design", "Calculate design"
        ))),

    # Body ----------
    shinydashboard::dashboardBody(
        shinyalert::useShinyalert(),
        shinydashboard::tabItems(
            # Presentation
            shinydashboard::tabItem(tabName = "presentation",
                    shiny::fluidPage(shiny::withMathJax(shiny::includeMarkdown("presentacion.Rmd")))),
            # Optimal Design
            shinydashboard::tabItem(tabName = "optdes",
                    # Row --
                    shiny::fluidRow(
                        shinydashboard::valueBox(value = "Optimum Design"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "For the selected criterion", icon = icon("check"), color = "yellow"),
                        shinydashboard::valueBoxOutput("op_sensitivity")
                    ),
                    # Row --
                    shiny::fluidRow(
                        shinydashboard::box(title = "Optimum design",
                            solidHeader = T,
                            width = 4,
                            collapsible = T,
                            div(DT::DTOutput("opt_df"), style = "font-size: 120%;")
                        ),
                        shinydashboard::box(title = "Sensitivity Function", solidHeader = T,
                            width = 8, collapsible = T,
                            plotlyOutput("sens_opt")
                        )
                    )
            ),
            # Custom design
            tabItem(tabName = "givedes",
                    # Row --
                    fluidRow(
                        valueBox(value = "Given Design"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "With crosspoints", icon = icon("clipboard-list"), color = "aqua"),
                        valueBox(value = "Sensitivity Function"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "For the given design", icon = icon("bar-chart-o"), color = "green"),
                        valueBoxOutput("efficiency")
                    ),
                    # Row --
                    fluidRow(
                        box(title = "New design",
                            solidHeader = T,
                            width = 4,
                            collapsible = T,
                            div(DT::DTOutput("givedes"), style = "font-size: 120%;")
                        ),
                        box(title = "New sensitivity", solidHeader = T,
                            width = 8, collapsible = T,
                            plotlyOutput("sens_new")
                        )
                    )
                    # ,
                    # Row --
                    # fluidRow(
                    #     valueBox(value = "Region for the points"#tags$p("Optimum Design", style = "font-size: 75%;")
                    #              , "Points at the extremes", icon = icon("bar-chart-o"), color = "yellow"),
                    #     box(title = "Feasible region 2", solidHeader = T,
                    #         width = 8, collapsible = T,
                    #         plotlyOutput("feas_regions")
                    #     )
                    # )
            ),
            tabItem(tabName = "restricteddes",
                    # Row --
                    fluidRow(
                        valueBox(value = "Design from the Optimum"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "Chosen points", icon = icon("clipboard-list"), color = "aqua"),
                        valueBox(value = "Sensitivity Function"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "For the added design", icon = icon("bar-chart-o"), color = "green"),
                        valueBoxOutput("efficiencyresdes")
                    ),
                    # Row --
                    fluidRow(
                        box(title = "New design",
                            solidHeader = T,
                            width = 4,
                            collapsible = T,
                            div(DT::DTOutput("woptdes"), style = "font-size: 120%;")
                        ),
                        box(title = "New sensitivity", solidHeader = T,
                            width = 8, collapsible = T,
                            plotlyOutput("sens_res")
                        )
                    )
                    # ,
                    # Row --
                    # fluidRow(
                    #     valueBox(value = "Feasible region"#tags$p("Optimum Design", style = "font-size: 75%;")
                    #              , "To add points", icon = icon("bar-chart-o"), color = "yellow"),
                    #     box(title = "Feasible region", solidHeader = T,
                    #         width = 8, collapsible = T,
                    #         plotlyOutput("feas_regions_res")
                    #     )
                    # )
            ),
            tabItem(tabName = "builddes",
                    # Row --
                    fluidRow(
                        valueBox(value = "Full custom design"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "Chosen by the user", icon = icon("clipboard-list"), color = "aqua"),
                        valueBox(value = "Sensitivity Function 3"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "For the built design", icon = icon("bar-chart-o"), color = "green"),
                        valueBoxOutput("efficiencybuildes")
                    ),
                    # Row --
                    fluidRow(
                        box(title = "New design",
                            solidHeader = T,
                            width = 4,
                            collapsible = T,
                            div(DT::DTOutput("buildes"), style = "font-size: 120%;")
                        ),
                        box(title = "New sensitivity", solidHeader = T,
                            width = 8, collapsible = T,
                            plotlyOutput("sens_build")
                        )
                    )
            ),
            tabItem(tabName = "enhacedes",
                    # Row --
                    fluidRow(
                        valueBox(value = "Design from the built design"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "Chosen points", icon = icon("clipboard-list"), color = "aqua"),
                        valueBox(value = "Sensitivity Function"#tags$p("Optimum Design", style = "font-size: 75%;")
                                 , "For the enhanced design", icon = icon("bar-chart-o"), color = "green"),
                        valueBoxOutput("efficiencyenhdes")
                    ),
                    # Row --
                    fluidRow(
                        box(title = "New design",
                            solidHeader = T,
                            width = 4,
                            collapsible = T,
                            div(DT::DTOutput("enhdes"), style = "font-size: 120%;")
                        ),
                        box(title = "New sensitivity", solidHeader = T,
                            width = 8, collapsible = T,
                            plotlyOutput("sens_enh")
                        )
                    )
                    # ,
                    # Row --
                    # fluidRow(
                    #     valueBox(value = "Feasible region"#tags$p("Optimum Design", style = "font-size: 75%;")
                    #              , "To add points to the built design", icon = icon("bar-chart-o"), color = "yellow"),
                    #     box(title = "Feasible region", solidHeader = T,
                    #         width = 8, collapsible = T,
                    #         plotlyOutput("feas_regions_enh")
                    #     )
                    # )
            ),
            # tabItem(tabName = "compare",
            #         fluidRow(
            #             # Row --
            #             fluidRow(
            #                 valueBoxOutput("industry_des"),
            #                 valueBoxOutput("industry_sens"),
            #                 valueBoxOutput("efficiency_ind")
            #             ),
            #             # Row --
            #             fluidRow(
            #                 box(title = "Industry design",
            #                     solidHeader = T,
            #                     width = 4,
            #                     collapsible = T,
            #                     div(DT::DTOutput("ind_df"), style = "font-size: 120%;")
            #                 ),
            #                 box(title = "Sensitivity Function", solidHeader = T,
            #                     width = 8, collapsible = T,
            #                     plotlyOutput("sens_ind")
            #                 )
            #             )
            #         )
            # ),
            # tabItem(tabName = "help", fluidPage(withMathJax(includeMarkdown("help.Rmd")))),
            tabItem(tabName = "contact", fluidPage(withMathJax(includeMarkdown("contact.Rmd"))))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # make reactive to store points
    ing_df <- shiny::reactiveValues()

    # Diseño en blanco que el usuario va construyendo
    ing_df$design <- data.frame("Point" = numeric(),
                                "Weight" = numeric())

    # dataset "standard", diseño añadiendo al óptimo los puntos de cortes con igula peso
    ing_df$design_std <- data.frame("Point" = numeric(),
                                "Weight" = numeric())

    # Punto a añadir al óptimo
    ing_df$points_to_add <- data.frame("Point" = numeric(),
                                      "Weight" = numeric())

    # Data frame con el óptimo valiendo (1-alpha)
    ing_df$weighted_opt <- data.frame("Point" = numeric(),
                                       "Weight" = numeric())

    # Optimum design
    ing_df$opt_design <- data.frame("Point" = numeric(),
                                    "Weight" = numeric())

    # Diseño final al partir del óptimo y añadir puntos
    ing_df$design_w_opt <- data.frame("Point" = numeric(),
                                    "Weight" = numeric())

    # Diseño industrial
    ing_df$ind_des <- data.frame("Point" = numeric(),
                                      "Weight" = numeric())

    # Punto a añadir al construido
    ing_df$points_to_add_build <- data.frame("Point" = numeric(),
                                       "Weight" = numeric())

    # Diseño final al partir del construido valiendo (1-alpha)
    ing_df$weighted_build <- data.frame("Point" = numeric(),
                                     "Weight" = numeric())

    # Diseño final al partir del construido y añadir puntos
    ing_df$enhaced_des <- data.frame("Point" = numeric(),
                                      "Weight" = numeric())

    # Se ha calculado el diseño óptimo?
    ing_df$calculated_design <- F

    # Se ha calculado un diseño propio?
    ing_df$built_design <- F

    # Límites de eficiencia en funcion de delta
    ing_df$eff_limit <- c(0, 1)

    # Límites de eficiencia en funcion de delta para enhace design
    ing_df$eff_limit_enh <- c(0, 1)

    # Regiones factibles añadir óptimo
    ing_df$fac_reg_opt <- vector("numeric")

    # Regiones factibles añadir custom
    ing_df$fac_reg_build <- vector("numeric")


    # Update weighted_opt
    ing_df$weighted_opt <- reactive({
        if(nrow(ing_df$opt_design > 0)){
            data.frame("Point" = ing_df$opt_design[["Point"]], "Weight" = ing_df$opt_design[["Weight"]]*(1-input$delta))
        }
        else{
            data.frame("Point" = numeric(),
                       "Weight" = numeric())
        }
    })

    # Update weighted_build
    weighted_build <- reactive({
        if(nrow(ing_df$design > 0)){
            data.frame("Point" = ing_df$design[["Point"]], "Weight" = ing_df$design[["Weight"]]*(1-input$deltaenh))
        }
        else{
            data.frame("Point" = numeric(),
                       "Weight" = numeric())
        }
    })


    # Update the input dataframe
    observeEvent(input$remove, {
        ing_df$design<-ing_df$design[-(nrow(ing_df$design)),]
        ing_df$fac_reg_build <- vector("numeric")
        ing_df$points_to_add_build <- data.frame("Point" = numeric(), "Weight" = numeric())
    })

    observeEvent(input$remove_all, {
        ing_df$design <- data.frame("Point" = numeric(), "Weight" = numeric())
        ing_df$fac_reg_build <- vector("numeric")
        ing_df$points_to_add_build <- data.frame("Point" = numeric(), "Weight" = numeric())
    })

    observeEvent(input$add, {
        if(input$weights_add <= 0){
            shinyalert("The weight must be greater than 0", type = "error")
        }
        else if((input$model %in% c('Antoine Equation', 'Quadratic') & between(input$points_add, input$design_sp[[1]], input$design_sp[[2]])) ||
                (input$model == 'Quadratic Heteroscedastic' & between(input$points_add, 0, input$design_sp_CH)) ||
                (input$model == 'Michaelis-Menten' & between(input$points_add, 0, input$design_sp_MM))){
            ing_df$design[nrow(ing_df$design) + 1,] <- c(input$points_add,
                                                         input$weights_add
            )

            # update choices
            updateNumericInput(session, 'weights_add', '2. Weights', 0)
            updateNumericInput(session, 'points_add', '1. Points', value = -101)
        }
        else{
            shinyalert("The point is out of the space of the design", type = "error")
        }
    })


    # Update the input dataframe for the design with (1-alpha) opt
    observeEvent(input$remove_opt, {
        ing_df$points_to_add<-ing_df$points_to_add[-(nrow(ing_df$points_to_add)),]
        ing_df$design_w_opt <- bind_rows(ing_df$weighted_opt(), ing_df$points_to_add)
    })

    observeEvent(input$remove_opt_all, {
        ing_df$points_to_add<-data.frame("Point" = numeric(), "Weight" = numeric())
        ing_df$design_w_opt <- bind_rows(ing_df$weighted_opt(), ing_df$points_to_add)
    })

    observeEvent(input$add_opt, {
        if(length(ing_df$fac_reg_opt) == 0){
            shinyalert("Calculate the Region of Candidate Points", type = "error")
        }
        else{
            in_region <- F
            for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                if(between(input$points_add_opt, ing_df$fac_reg_opt[[2*i-1]], ing_df$fac_reg_opt[[2*i]])){
                    in_region <- T
                    break
                }
            }
            if(input$weights_add_opt <= 0){
                shinyalert("The weight must be greater than 0", type = "error")
            }
            else if(in_region){
                ing_df$points_to_add[nrow(ing_df$points_to_add) + 1,] <- c(input$points_add_opt,
                                                             input$weights_add_opt
                )
                ing_df$design_w_opt <- bind_rows(ing_df$weighted_opt(), ing_df$points_to_add)

                # update choices
                updateNumericInput(session, 'weights_add_opt', '2. Weights', 0)
                updateNumericInput(session, 'points_add_opt', '1. Points', value = -101)
            }
            else{
                shinyalert("The point is out of the allowed region", type = "error")
            }
        }
    })

    # Update the input dataframe for enhaced design
    observeEvent(input$remove_enh, {
        ing_df$points_to_add_build<-ing_df$points_to_add_build[-(nrow(ing_df$points_to_add_build)),]
        ing_df$enhaced_des <- bind_rows(weighted_build(), ing_df$points_to_add_build)
    })

    observeEvent(input$remove_enh_all, {
        ing_df$points_to_add_build <- data.frame("Point" = numeric(), "Weight" = numeric())
        ing_df$enhaced_des <- bind_rows(weighted_build(), ing_df$points_to_add_build)
    })

    observeEvent(input$add_enh, {
        if(length(ing_df$fac_reg_build) == 0){
            shinyalert("Calculate the region of candidate points", type = "error")
        }
        else{
            in_region <- F
            for(i in 1:(length(ing_df$fac_reg_build)/2)){
                if(between(input$points_add_enh, ing_df$fac_reg_build[[2*i-1]], ing_df$fac_reg_build[[2*i]])){
                    in_region <- T
                    break
                }
            }
            if(input$weights_add_enh <= 0){
                shinyalert("The weight must be greater than 0", type = "error")
            }
            else if(in_region){
                ing_df$points_to_add_build[nrow(ing_df$points_to_add_build) + 1,] <- c(input$points_add_enh,
                                                                                       input$weights_add_enh
                )
                ing_df$enhaced_des <- bind_rows(weighted_build(), ing_df$points_to_add_build)

                # update choices
                updateNumericInput(session, 'weights_add_enh', '2. Weights', 0)
                updateNumericInput(session, 'points_add_enh', '1. Points', value = -101)
            }
            else{
                shinyalert("The point is out of the allowed region", type = "error")
            }
        }
    })

    # Delta slider
    observeEvent(input$delta, {
        if(ing_df$calculated_design == TRUE){
            if(identical(input$model, "Antoine Equation")){
                sens_opt <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design))
                sens_min <- findminval(sens_opt, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds<- delta_bound(input$delta, 3, sens_min)
                ing_df$eff_limit <- c(ceiling(bounds[1]*100)/100,floor(bounds[2]*100)/100)
            }
            else if(identical(input$model, "Quadratic")){
                sens_opt <- dsensCuad(dmatrixCuad(ing_df$opt_design))
                sens_min <- findminval(sens_opt, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds<- delta_bound(input$delta, 3, sens_min)
                ing_df$eff_limit <- c(ceiling(bounds[1]*100)/100,floor(bounds[2]*100)/100)
            }
            else if(identical(input$model, "Michaelis-Menten")){
                sens_opt <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$opt_design))
                sens_min <- findminval(sens_opt, 0, input$design_sp_MM, 1000)
                bounds<- delta_bound(input$delta, 2, sens_min)
                ing_df$eff_limit <- c(ceiling(bounds[1]*100)/100,floor(bounds[2]*100)/100)
            }
            else if(identical(input$model, "Quadratic Heteroscedastic")){
                sens_opt <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$opt_design))
                sens_min <- findminval(sens_opt, 0, input$design_sp_CH, 1000)
                bounds<- delta_bound(input$delta, 3, sens_min)
                ing_df$eff_limit <- c(ceiling(bounds[1]*100)/100,floor(bounds[2]*100)/100)
            }
        }
    })

    # Delta enh slider
    observeEvent(input$deltaenh, {
        if(ing_df$built_design == FALSE)
        {

        } else if((nrow(ing_df$design) < 3 && !identical(input$model, "Michaelis-Menten")) || nrow(ing_df$design) < 2 || !is.numeric(ing_df$effbuild)){
            shinyalert("Build a regular design and evaluate it!", type = "error")
        }
        else{
            if(identical(input$model, "Antoine Equation")){
                sens_des <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design))
                sens_min <- findminval(sens_des, input$design_sp[[1]], input$design_sp[[2]], 1000)
                sens_max <- findmaxval(sens_des, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds_enh <- delta_bound(input$deltaenh, 3, sens_min, sens_max)#*ing_df$effbuild
                ing_df$eff_limit_enh <- c(ceiling(bounds_enh[1]*100)/100,floor(bounds_enh[2]*100)/100)
            }
            else if(identical(input$model, "Quadratic")){
                sens_des <- dsensCuad(dmatrixCuad(ing_df$design))
                sens_min <- findminval(sens_des, input$design_sp[[1]], input$design_sp[[2]], 1000)
                sens_max <- findmaxval(sens_des, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds_enh <- delta_bound(input$deltaenh, 3, sens_min, sens_max)#*ing_df$effbuild
                ing_df$eff_limit_enh <- c(ceiling(bounds_enh[1]*100)/100,floor(bounds_enh[2]*100)/100)
            }
            else if(identical(input$model, "Michaelis-Menten")){
                sens_des <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$design))
                sens_min <- findminval(sens_des, 0, input$design_sp_MM, 1000)
                sens_max <- findmaxval(sens_des, 0, input$design_sp_MM, 1000)
                bounds_enh <- delta_bound(input$deltaenh, 2, sens_min, sens_max)#*ing_df$effbuild
                ing_df$eff_limit_enh <- c(ceiling(bounds_enh[1]*100)/100,floor(bounds_enh[2]*100)/100)
            }
            else if(identical(input$model, "Quadratic Heteroscedastic")){
                sens_des <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$design))
                sens_min <- findminval(sens_des, 0, input$design_sp_CH, 1000)
                sens_max <- findmaxval(sens_des, 0, input$design_sp_CH, 1000)
                bounds_enh <- delta_bound(input$deltaenh, 3, sens_min, sens_max)#*ing_df$effbuild
                ing_df$eff_limit_enh <- c(ceiling(bounds_enh[1]*100)/100,floor(bounds_enh[2]*100)/100)
            }
            else{
                shinyalert("Enter a valid model!", type = "error")
            }
        }
    }, ignoreInit = TRUE)

    # Render eff slider
    output$eff <- renderUI({
        sliderInput("deff", "Possible efficiency, delta", min=ing_df$eff_limit[1], max=ing_df$eff_limit[2], value = ing_df$eff_limit[1] + ing_df$eff_limit[2]/2 - ing_df$eff_limit[1]/2)
    })

    # Render effenh slider
    output$effenh <- renderUI({
        sliderInput("deffenh", "Possible relative efficiency", min=ing_df$eff_limit_enh[1], max=ing_df$eff_limit_enh[2], value = ing_df$eff_limit_enh[1] + ing_df$eff_limit_enh[2]/2 - ing_df$eff_limit_enh[1]/2)
    })


    # Build Des
    output$buildes <- DT::renderDataTable({
        datatable(ing_df$design, rownames=F, options = list(paging = FALSE, searching = FALSE)) %>% formatRound(c(1,2), 3)
    })

    # Give des
    output$givedes <- DT::renderDataTable({
        datatable(ing_df$design_std, rownames=F, options = list(paging = FALSE, searching = FALSE)) %>% formatRound(c(1,2), 3)
    })

    # Restricted des
    output$woptdes <- DT::renderDataTable({
        datatable(ing_df$design_w_opt, rownames=F, options = list(paging = FALSE, searching = FALSE)) %>% formatRound(c(1,2), 3)
    })

    # Enhaced des
    output$enhdes <- DT::renderDataTable({
        datatable(ing_df$enhaced_des, rownames=F, options = list(paging = FALSE, searching = FALSE)) %>% formatRound(c(1,2), 3)
    })

    # Industrial des
    output$ind_df <- DT::renderDataTable({
        datatable(ing_df$ind_des, rownames=F, options = list(paging = FALSE, searching = FALSE)) %>% formatRound(c(1,2), 3)
    })

    # Show/hide add points
    output$show_rest <- reactive({
        length(ing_df$fac_reg_opt) != 0
    })

    outputOptions(output, "show_rest", suspendWhenHidden = FALSE)

    # Show/hide add points enh
    output$show_enh <- reactive({
        length(ing_df$fac_reg_build) != 0
    })

    outputOptions(output, "show_enh", suspendWhenHidden = FALSE)



    # Calculate optimal design
    observeEvent(input$calc_opt_design, {
        model <- input$model
        if(identical(model, "Antoine Equation")){

            restart()
            ing_df$opt_design <- doptAntoine(input$a_par, input$b_par, input$c_par, input$design_sp[[1]], input$design_sp[[2]])
            sens_opt <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design))
            p <- ggplot() + theme_ipsum()

            x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
            y_val <- purrr::map_dbl(x_val, sens_opt)

            ing_df$sens_opt <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "")
            ing_df$calculated_design <- T
            ing_df$points_to_add <- data.frame("Point" = numeric(), "Weight" = numeric())
            ing_df$design_w_opt <- bind_rows(ing_df$weighted_opt(), ing_df$points_to_add)
        }
        else if(identical(model, "Quadratic")){

            restart()
            ing_df$opt_design <- doptCuad(input$design_sp[[1]], input$design_sp[[2]])
            sens_opt <- dsensCuad(dmatrixCuad(ing_df$opt_design))
            p <- ggplot() + theme_ipsum()

            x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
            y_val <- purrr::map_dbl(x_val, sens_opt)

            ing_df$sens_opt <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Points", y = "Value")
            ing_df$calculated_design <- T
            ing_df$points_to_add <- data.frame("Point" = numeric(), "Weight" = numeric())
            ing_df$design_w_opt <- bind_rows(ing_df$weighted_opt(), ing_df$points_to_add)
        }
        else if(identical(model, "Michaelis-Menten")){

            restart()
            ing_df$opt_design <- doptMM(input$k_par, input$v_par, 0, input$design_sp_MM)
            sens_opt <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$opt_design))
            p <- ggplot() + theme_ipsum()

            x_val <- seq(0, input$design_sp_MM, length.out = 10000)
            y_val <- purrr::map_dbl(x_val, sens_opt)

            ing_df$sens_opt <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  2, color = "goldenrod3") + xlim(0, input$design_sp_MM) + labs(x = "Points", y = "Value")
            ing_df$calculated_design <- T
            ing_df$points_to_add <- data.frame("Point" = numeric(), "Weight" = numeric())
            ing_df$design_w_opt <- bind_rows(ing_df$weighted_opt(), ing_df$points_to_add)
        }
        else if(identical(model, "Quadratic Heteroscedastic")){

            restart()
            ing_df$opt_design <- doptCuadHet(input$order)
            sens_opt <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$opt_design))
            p <- ggplot() + theme_ipsum()

            x_val <- seq(0, input$design_sp_CH, length.out = 10000)
            y_val <- purrr::map_dbl(x_val, sens_opt)

            ing_df$sens_opt <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(0, input$design_sp_CH) + labs(x = "Points", y = "Value")
            ing_df$calculated_design <- T
            ing_df$points_to_add <- data.frame("Point" = numeric(), "Weight" = numeric())
            ing_df$design_w_opt <- bind_rows(ing_df$weighted_opt(), ing_df$points_to_add)
        }
        else{
            shinyalert("Enter a valid model!", type = "error")
        }
    })

    # Render optimal design dataframe
    output$opt_df <- DT::renderDataTable({
        datatable(ing_df$opt_design, rownames=F, options = list(paging = FALSE, searching = FALSE), filter = 'none') %>% formatRound(c(1,2), 3)
    })


    # Show feasible points for given values
    observeEvent(input$give_points,{
        if(nrow(ing_df$opt_design) < 1){
            shinyalert("You need to calculate the optimal design first", type = "error")
        }
        else {
            if(identical(input$model, "Antoine Equation")){
                sens_opt <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design))
                sens_min <- findminval(sens_opt, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds <- delta_bound(input$delta, 3, sens_min)
                if(ing_df$eff_limit[2] == 1){
                    shinyalert("Choose weight for the new points", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                    val <- sens_val_to_add(input$deff, input$delta, 3)
                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_opt)
                    par <- getPar(cross)
                    ing_df$fac_reg_opt <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)

                    x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                    eff <- function(x){
                        return((1-input$delta)*(1+input$delta*sens_opt(x)/(1-input$delta))^(1/3))
                    }
                    y_val <- purrr::map_dbl(x_val, eff)

                    p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") +
                        geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
                        xlim(input$design_sp[[1]], input$design_sp[[2]]) +
                        labs(x = "x", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

                    efficiency <- eff(cross[1])
                    for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
                        p <- p + eval(parse(text=loop_input))
                    }



                    values <- map_dbl(cross, eff)
                    cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

                    p <- p + geom_point(data = cutoffpoints, aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

                    p <- p + geom_segment(aes(x = input$design_sp[[1]], xend = input$design_sp[[1]], y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)

                    ing_df$sens_new <- p
                    ing_df$sens_res <- p
                }
            }
            else if(identical(input$model, "Quadratic")){
                sens_opt <- dsensCuad( dmatrixCuad(ing_df$opt_design))
                sens_min <- findminval(sens_opt, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds <- delta_bound(input$delta, 3, sens_min)
                if(ing_df$eff_limit[2] == 1){
                    shinyalert("Choose weight for the new points", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                    val <- sens_val_to_add(input$deff, input$delta, 3)
                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_opt)
                    par <- getPar(cross)
                    ing_df$fac_reg_opt <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)

                    x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                    eff <- function(x){
                        return((1-input$delta)*(1+input$delta*sens_opt(x)/(1-input$delta))^(1/3))
                    }
                    y_val <- purrr::map_dbl(x_val, eff)

                    p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") +
                        geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
                        xlim(input$design_sp[[1]], input$design_sp[[2]]) +
                        labs(x = "x", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

                    efficiency <- eff(cross[1])
                    for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
                        p <- p + eval(parse(text=loop_input))
                    }



                    values <- map_dbl(cross, eff)
                    cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

                    p <- p + geom_point(data = cutoffpoints, aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

                    p <- p + geom_segment(aes(x = input$design_sp[[1]], xend = input$design_sp[[1]], y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)

                    ing_df$sens_new <- p
                    ing_df$sens_res <- p
                }
            } else if(identical(input$model, "Michaelis-Menten")){
                sens_opt <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$opt_design))
                sens_min <- findminval(sens_opt, 0, input$design_sp_MM, 1000)
                bounds <- delta_bound(input$delta, 2, sens_min)
                if(ing_df$eff_limit[2] == 1){
                    shinyalert("Choose weight for the new points", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(input$deff, input$delta, 2, sens_opt, 1000, 10^(-3), 0, input$design_sp_MM))
                    val <- sens_val_to_add(input$deff, input$delta, 2)
                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, 0, input$design_sp_MM, val, sens_opt)
                    par <- getPar(cross)
                    ing_df$fac_reg_opt <- getCross2(cross, 0, input$design_sp_MM, start, par)

                    x_val <- seq(0, input$design_sp_MM, length.out = 10000)
                    eff <- function(x){
                        return((1-input$delta)*(1+input$delta*sens_opt(x)/(1-input$delta))^(1/2))
                    }
                    y_val <- purrr::map_dbl(x_val, eff)

                    p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") +
                        geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
                        xlim(0, input$design_sp_MM) +
                        labs(x = "x", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

                    efficiency <- eff(cross[1])
                    for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
                        p <- p + eval(parse(text=loop_input))
                    }



                    values <- map_dbl(cross, eff)
                    cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

                    p <- p + geom_point(data = cutoffpoints, aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

                    p <- p + geom_segment(aes(x = 0, xend = 0, y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)

                    ing_df$sens_new <- p
                    ing_df$sens_res <- p
                }
            } else if(identical(input$model, "Quadratic Heteroscedastic")){
                sens_opt <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$opt_design))
                sens_min <- findminval(sens_opt, 0, input$design_sp_CH, 1000)
                bounds <- delta_bound(input$delta, 3, sens_min)
                if(ing_df$eff_limit[2] == 1){
                    shinyalert("Choose weight for the new points", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), 0, input$design_sp_CH))
                    val <- sens_val_to_add(input$deff, input$delta, 3)
                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, 0, input$design_sp_CH, val, sens_opt)
                    par <- getPar(cross)
                    ing_df$fac_reg_opt <- getCross2(cross, 0, input$design_sp_CH, start, par)

                    x_val <- seq(0, input$design_sp_CH, length.out = 10000)
                    eff <- function(x){
                        return((1-input$delta)*(1+input$delta*sens_opt(x)/(1-input$delta))^(1/3))
                    }
                    y_val <- purrr::map_dbl(x_val, eff)

                    p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") +
                        geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
                        xlim(0, input$design_sp_CH) +
                        labs(x = "x", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

                    efficiency <- eff(cross[1])
                    for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
                        p <- p + eval(parse(text=loop_input))
                    }



                    values <- map_dbl(cross, eff)
                    cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

                    p <- p + geom_point(data = cutoffpoints, aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

                    p <- p + geom_segment(aes(x = 0, xend = 0, y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)

                    ing_df$sens_new <- p
                    ing_df$sens_res <- p
                }
            }
            else{
                shinyalert("Enter a valid model!", type = "error")
            }
        }
    })


    # Show feasible points for build design
    observeEvent(input$enhace_points,{
        if(nrow(ing_df$opt_design) < 1){
            shinyalert("You need to calculate the optimal design first", type = "error")
        }
        if((nrow(ing_df$design) < 3 && !identical(input$model, "Michaelis-Menten")) || nrow(ing_df$design) < 2 || !is.numeric(ing_df$effbuild)){
            shinyalert("You need to build a design and evaluate it", type = "error")
        }
        else {
            model <- input$model
            if(identical(model, "Antoine Equation")){
                sens_des <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design))
                sens_min <- findminval(sens_des, input$design_sp[[1]], input$design_sp[[2]], 1000)
                sens_max <- findmaxval(sens_des, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds <- delta_bound(input$deltaenh, 3, sens_min, sens_max)
                if(ing_df$eff_limit_enh[2] == 1){
                    shinyalert("Choose weight for the new points", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(floor(input$deffenh#/ing_df$effbuild
                                                    *10000)/10000, input$deltaenh, 3, sens_des, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                    val <- sens_val_to_add(input$deffenh#/ing_df$effbuild
                                           , input$deltaenh, 3)
                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_des)
                    par <- getPar(cross)
                    ing_df$fac_reg_build <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)

                    x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                    eff <- function(x){
                        return((1-input$deltaenh)*(1+input$deltaenh*sens_des(x)/(1-input$deltaenh))^(1/3))
                    }
                    y_val <- purrr::map_dbl(x_val, eff)

                    p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") +
                        geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
                        xlim(input$design_sp[[1]], input$design_sp[[2]]) +
                        labs(x = "x", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

                    efficiency <- eff(cross[1])
                    for(i in 1:(length(ing_df$fac_reg_build)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_build[2*i-1],",xend=",ing_df$fac_reg_build[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
                        p <- p + eval(parse(text=loop_input))
                    }



                    values <- map_dbl(cross, eff)
                    cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

                    p <- p + geom_point(data = cutoffpoints, aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

                    p <- p + geom_segment(aes(x = input$design_sp[[1]], xend = input$design_sp[[1]], y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)

                    ing_df$sens_enh <- p
                }
            }
            else if(identical(model, "Quadratic")){
                sens_des <- dsensCuad( dmatrixCuad(ing_df$design))
                sens_min <- findminval(sens_des, input$design_sp[[1]], input$design_sp[[2]], 1000)
                sens_max <- findmaxval(sens_des, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds <- delta_bound(input$deltaenh, 3, sens_min, sens_max)
                if(ing_df$eff_limit_enh[2] == 1){
                    shinyalert("Choose weight for the new points", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(floor(input$deffenh#/ing_df$effbuild
                                                    *10000)/10000, input$deltaenh, 3, sens_des, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                    val <- sens_val_to_add(input$deffenh#/ing_df$effbuild
                                           , input$deltaenh, 3)
                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_des)
                    par <- getPar(cross)

                    ing_df$fac_reg_build <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)

                    x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                    eff <- function(x){
                        return((1-input$deltaenh)*(1+input$deltaenh*sens_des(x)/(1-input$deltaenh))^(1/3))
                    }
                    y_val <- purrr::map_dbl(x_val, eff)

                    p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") +
                        geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
                        xlim(input$design_sp[[1]], input$design_sp[[2]]) +
                        labs(x = "x", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

                    efficiency <- eff(cross[1])
                    for(i in 1:(length(ing_df$fac_reg_build)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_build[2*i-1],",xend=",ing_df$fac_reg_build[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
                        p <- p + eval(parse(text=loop_input))
                    }



                    values <- map_dbl(cross, eff)
                    cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

                    p <- p + geom_point(data = cutoffpoints, aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

                    p <- p + geom_segment(aes(x = input$design_sp[[1]], xend = input$design_sp[[1]], y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)

                    ing_df$sens_enh <- p
                }
            } else if(identical(model, "Michaelis-Menten")){
                sens_des <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$design))
                sens_min <- findminval(sens_des, 0, input$design_sp_MM, 1000)
                sens_max <- findmaxval(sens_des, 0, input$design_sp_MM, 1000)
                bounds <- delta_bound(input$deltaenh, 2, sens_min, sens_max)
                if(ing_df$eff_limit_enh[2] == 1){
                    shinyalert("Choose weight for the new points", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(floor(input$deffenh#/ing_df$effbuild
                                                    *10000)/10000, input$deltaenh, 2, sens_des, 1000, 10^(-3), 0, input$design_sp_MM))
                    val <- sens_val_to_add(input$deffenh#/ing_df$effbuild
                                           , input$deltaenh, 2)
                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, 0, input$design_sp_MM, val, sens_des)
                    par <- getPar(cross)
                    ing_df$fac_reg_build <- getCross2(cross, 0, input$design_sp_MM, start, par)

                    x_val <- seq(0, input$design_sp_MM, length.out = 10000)
                    eff <- function(x){
                        return((1-input$deltaenh)*(1+input$deltaenh*sens_des(x)/(1-input$deltaenh))^(1/2))
                    }
                    y_val <- purrr::map_dbl(x_val, eff)

                    p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") +
                        geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
                        xlim(0, input$design_sp_MM) +
                        labs(x = "x", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

                    efficiency <- eff(cross[1])
                    for(i in 1:(length(ing_df$fac_reg_build)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_build[2*i-1],",xend=",ing_df$fac_reg_build[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
                        p <- p + eval(parse(text=loop_input))
                    }



                    values <- map_dbl(cross, eff)
                    cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

                    p <- p + geom_point(data = cutoffpoints, aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

                    p <- p + geom_segment(aes(x = 0, xend = 0, y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)

                    ing_df$sens_enh <- p
                }
            } else if(identical(model, "Quadratic Heteroscedastic")){
                sens_des <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$design))
                sens_min <- findminval(sens_des, 0, input$design_sp_CH, 1000)
                sens_max <- findmaxval(sens_des, 0, input$design_sp_CH, 1000)
                bounds <- delta_bound(input$deltaenh, 3, sens_min, sens_max)
                if(ing_df$eff_limit_enh[2] == 1){
                    shinyalert("Choose weight for the new points", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(floor(input$deffenh#/ing_df$effbuild
                                                    *10000)/10000, input$deltaenh, 3, sens_des, 1000, 10^(-3), 0, input$design_sp_CH))
                    val <- sens_val_to_add(input$deffenh#/ing_df$effbuild
                                           , input$deltaenh, 3)
                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, 0, input$design_sp_CH, val, sens_des)
                    par <- getPar(cross)

                    ing_df$fac_reg_build <- getCross2(cross, 0, input$design_sp_CH, start, par)

                    x_val <- seq(0, input$design_sp_CH, length.out = 10000)
                    eff <- function(x){
                        return((1-input$deltaenh)*(1+input$deltaenh*sens_des(x)/(1-input$deltaenh))^(1/3))
                    }
                    y_val <- purrr::map_dbl(x_val, eff)

                    p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") +
                        geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
                        xlim(0, input$design_sp_CH) +
                        labs(x = "x", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

                    efficiency <- eff(cross[1])
                    for(i in 1:(length(ing_df$fac_reg_build)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_build[2*i-1],",xend=",ing_df$fac_reg_build[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
                        p <- p + eval(parse(text=loop_input))
                    }



                    values <- map_dbl(cross, eff)
                    cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

                    p <- p + geom_point(data = cutoffpoints, aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

                    p <- p + geom_segment(aes(x = 0, xend = 0, y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)

                    ing_df$sens_enh <- p
                }
            }
            else{
                shinyalert("Enter a valid model!", type = "error")
            }
        }
    })



    # Calculate the design with the given points and calculate its efficiency and sensitivity function
    observeEvent(input$stand_design, {
        if(input$delta == 0){
            shinyalert("Choose weight for the new points", type = "error")
        }
        else{
            if(identical(input$model, "Antoine Equation")) {
                sens_opt <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design))
                sens_min <- findminval(sens_opt, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds <- delta_bound(input$delta, 3, sens_min)
                if(input$deff < bounds[1] || input$deff > bounds[2]){
                    shinyalert("The value of the efficiency is not feasible", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                    val <- sens_val_to_add(input$deff, input$delta, 3)

                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_opt)
                    par <- getPar(cross)
                    ing_df$fac_reg_opt <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)

                    ing_df$design_std <- add_points(cross, input$delta, ing_df$opt_design)
                    ing_df$eff <- deff(dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design_std), dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design), 3)

                    # Obtener nueva función de sensibilidad, diseño y plot
                    sens_new <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design_std))

                    new_des_data <- data.frame("Point" = ing_df$design_std[["Point"]], "Weight" = ing_df$design_std[["Weight"]], "Value" = rep(0, length(ing_df$design_std[["Point"]])))
                    p <- ggplot() + theme_ipsum()

                    x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                    y_val <- purrr::map_dbl(x_val, sens_new)

                    ing_df$sens_new <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                    # Loop para pintar las regiones factibles
                    for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
                        ing_df$sens_new <- ing_df$sens_new + eval(parse(text=loop_input))
                    }
                }
            }
            else if (identical(input$model, "Quadratic")){
                sens_opt <- dsensCuad(dmatrixCuad(ing_df$opt_design))
                sens_min <- findminval(sens_opt, input$design_sp[[1]], input$design_sp[[2]], 1000)
                bounds <- delta_bound(input$delta, 3, sens_min)
                if(input$deff < bounds[1] || input$deff > bounds[2]){
                    shinyalert("The value of the efficiency is not feasible", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]])
                    val <- sens_val_to_add(input$deff, input$delta, 3)

                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_opt)
                    par <- getPar(cross)
                    ing_df$fac_reg_opt <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)


                    ing_df$design_std <- add_points(cross, input$delta, ing_df$opt_design)
                    ing_df$eff <- deff(dmatrixCuad(ing_df$design_std), dmatrixCuad(ing_df$opt_design), 3)

                    # Obtener nueva función de sensibilidad, diseño y plot
                    sens_new <- dsensCuad(dmatrixCuad(ing_df$design_std))

                    new_des_data <- data.frame("Point" = ing_df$design_std[["Point"]], "Weight" = ing_df$design_std[["Weight"]], "Value" = rep(0, length(ing_df$design_std[["Point"]])))

                    p <- ggplot() + theme_ipsum()

                    x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                    y_val <- purrr::map_dbl(x_val, sens_new)

                    ing_df$sens_new <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                    # Loop para pintar las regiones factibles
                    for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
                        ing_df$sens_new <- ing_df$sens_new + eval(parse(text=loop_input))
                    }
                }
            } else if(identical(input$model, "Michaelis-Menten")) {
                sens_opt <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$opt_design))
                sens_min <- findminval(sens_opt, 0, input$design_sp_MM, 1000)
                bounds <- delta_bound(input$delta, 2, sens_min)
                if(input$deff < bounds[1] || input$deff > bounds[2]){
                    shinyalert("The value of the efficiency is not feasible", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- sort(crosspoints(input$deff, input$delta, 2, sens_opt, 1000, 10^(-3), 0, input$design_sp_MM))
                    val <- sens_val_to_add(input$deff, input$delta, 2)

                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, 0, input$design_sp_MM, val, sens_opt)
                    par <- getPar(cross)
                    ing_df$fac_reg_opt <- getCross2(cross, 0, input$design_sp_MM, start, par)

                    ing_df$design_std <- add_points(cross, input$delta, ing_df$opt_design)
                    ing_df$eff <- deff(dmatrixMM(input$k_par, input$v_par, ing_df$design_std), dmatrixMM(input$k_par, input$v_par, ing_df$opt_design), 2)

                    # Obtener nueva función de sensibilidad, diseño y plot
                    sens_new <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$design_std))

                    new_des_data <- data.frame("Point" = ing_df$design_std[["Point"]], "Weight" = ing_df$design_std[["Weight"]], "Value" = rep(0, length(ing_df$design_std[["Point"]])))
                    p <- ggplot() + theme_ipsum()

                    x_val <- seq(0, input$design_sp_MM, length.out = 10000)
                    y_val <- purrr::map_dbl(x_val, sens_new)

                    ing_df$sens_new <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  2, color = "goldenrod3") + xlim(0, input$design_sp_MM) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                    # Loop para pintar las regiones factibles
                    for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
                        ing_df$sens_new <- ing_df$sens_new + eval(parse(text=loop_input))
                    }
                }
            } else if (identical(input$model, "Quadratic Heteroscedastic")){
                sens_opt <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$opt_design))
                sens_min <- findminval(sens_opt, 0, input$design_sp_CH, 1000)
                bounds <- delta_bound(input$delta, 3, sens_min)
                if(input$deff < bounds[1] || input$deff > bounds[2]){
                    shinyalert("The value of the efficiency is not feasible", type = "error")
                }
                else{
                    # Puntos de corte y valor del corte
                    cross <- crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), 0, input$design_sp_CH)
                    val <- sens_val_to_add(input$deff, input$delta, 3)

                    # Obtener start y par para tener las regiones
                    start <- getStart(cross, 0, input$design_sp_CH, val, sens_opt)
                    par <- getPar(cross)
                    ing_df$fac_reg_opt <- getCross2(cross, 0, input$design_sp_CH, start, par)


                    ing_df$design_std <- add_points(cross, input$delta, ing_df$opt_design)
                    ing_df$eff <- deff(dmatrixCuadHet(input$order, ing_df$design_std), dmatrixCuadHet(input$order, ing_df$opt_design), 3)

                    # Obtener nueva función de sensibilidad, diseño y plot
                    sens_new <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$design_std))

                    new_des_data <- data.frame("Point" = ing_df$design_std[["Point"]], "Weight" = ing_df$design_std[["Weight"]], "Value" = rep(0, length(ing_df$design_std[["Point"]])))

                    p <- ggplot() + theme_ipsum()

                    x_val <- seq(0, input$design_sp_CH, length.out = 10000)
                    y_val <- purrr::map_dbl(x_val, sens_new)

                    ing_df$sens_new <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(0, input$design_sp_CH) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                    # Loop para pintar las regiones factibles
                    for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                        loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
                        ing_df$sens_new <- ing_df$sens_new + eval(parse(text=loop_input))
                    }
                }
            }
            else{
                shinyalert("Error", "Enter a valid model!", type = "error")
            }
        }
    })

    # Building the design from scratch
    observeEvent(input$both_designs, {
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

        if((length(ing_df$design[["Point"]]) < 3 && !identical(input$model, "Michaelis-Menten")) || length(ing_df$design[["Point"]]) < 2)
        {
            shinyalert("The design is singular!", type = "error")
            compute <- FALSE
        }
        else if(0 %in% ing_df$design[["Weight"]])
        {
            compute <- FALSE
            shinyalert("Error", "There are points without weight!", type = "error")
        }
        else if(((max(ing_df$design[["Point"]]) > input$design_sp[[2]] || min(ing_df$design[["Point"]]) < input$design_sp[[1]]) && !(input$model %in% c("Michaelis-Menten", "Quadratic Heteroscedastic"))) || ((max(ing_df$design[["Point"]]) > input$design_sp_MM || min(ing_df$design[["Point"]]) < 0) && identical(input$model, "Michaelis-Menten")) || ((max(ing_df$design[["Point"]]) > input$design_sp_CH || min(ing_df$design[["Point"]]) < 0) && identical(input$model, "Quadratic Heteroscedastic")))
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
            ing_df$design[["Weight"]] <- ing_df$design[["Weight"]]/sum(ing_df$design[["Weight"]])
        }
        if(isTRUE(compute)) {
            if(identical(input$model, "Antoine Equation")) {
                ing_df$built_design <- TRUE
                ing_df$opt_design <- doptAntoine(input$a_par, input$b_par, input$c_par, input$design_sp[[1]], input$design_sp[[2]])
                ing_df$effbuild <- deff(dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design), dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design), 3)
                ing_df$enhaced_des <- bind_rows(weighted_build(), ing_df$points_to_add_build)

                sens_opt <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design))
                sens_new <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design))

                new_des_data <- data.frame("Point" = ing_df$design[["Point"]], "Weight" = ing_df$design[["Weight"]], "Value" = rep(0, length(ing_df$design[["Point"]])))
                p <- ggplot() + theme_ipsum()

                x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_build <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3")  + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))
            }
            else if(identical(input$model, "Quadratic")) {
                ing_df$built_design <- TRUE
                ing_df$opt_design <- doptCuad(input$design_sp[[1]], input$design_sp[[2]])
                ing_df$effbuild <- deff(dmatrixCuad(ing_df$design), dmatrixCuad(ing_df$opt_design), 3)
                ing_df$enhaced_des <- bind_rows(weighted_build(), ing_df$points_to_add_build)

                sens_opt <- dsensCuad(dmatrixCuad(ing_df$opt_design))
                sens_new <- dsensCuad(dmatrixCuad(ing_df$design))

                new_des_data <- data.frame("Point" = ing_df$design[["Point"]], "Weight" = ing_df$design[["Weight"]], "Value" = rep(0, length(ing_df$design[["Point"]])))

                p <- ggplot() + theme_ipsum()

                x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_build <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))
            } else if(identical(input$model, "Michaelis-Menten")) {
                ing_df$built_design <- TRUE
                ing_df$opt_design <- doptMM(input$k_par, input$v_par, 0, input$design_sp_MM)
                ing_df$effbuild <- deff(dmatrixMM(input$k_par, input$v_par, ing_df$design), dmatrixMM(input$k_par, input$v_par, ing_df$opt_design), 2)
                ing_df$enhaced_des <- bind_rows(weighted_build(), ing_df$points_to_add_build)

                sens_opt <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$opt_design))
                sens_new <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$design))

                new_des_data <- data.frame("Point" = ing_df$design[["Point"]], "Weight" = ing_df$design[["Weight"]], "Value" = rep(0, length(ing_df$design[["Point"]])))
                p <- ggplot() + theme_ipsum()

                x_val <- seq(0, input$design_sp_MM, length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_build <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  2, color = "goldenrod3")  + xlim(0, input$design_sp_MM) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))
            } else if(identical(input$model, "Quadratic Heteroscedastic")) {
                updateSliderInput(session, "design_sp",
                                  min = -100, max = 374, value = c(0, input$design_sp_CH))
                ing_df$built_design <- TRUE
                ing_df$opt_design <- doptCuadHet(input$order)
                ing_df$effbuild <- deff(dmatrixCuadHet(input$order, ing_df$design), dmatrixCuadHet(input$order, ing_df$opt_design), 3)
                ing_df$enhaced_des <- bind_rows(weighted_build(), ing_df$points_to_add_build)

                sens_opt <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$opt_design))
                sens_new <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$design))

                new_des_data <- data.frame("Point" = ing_df$design[["Point"]], "Weight" = ing_df$design[["Weight"]], "Value" = rep(0, length(ing_df$design[["Point"]])))

                p <- ggplot() + theme_ipsum()

                x_val <- seq(0, input$design_sp_CH, length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_build <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(0, input$design_sp_CH) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))
            }
            else{
                shinyalert("Error", "Enter a valid model!", type = "error")
            }
        }

    })


    # With restricted design from the optimum
    observeEvent(input$with_opt, {
        compute <- TRUE
        length1 <- nrow(ing_df$design_w_opt)
        ing_df$design_w_opt <- ing_df$design_w_opt %>%
            group_by(Point) %>%
            summarise_all(funs(sum))

        if(length1 != nrow(ing_df$design_w_opt)){
            shinyalert("", "There are repeated points in the design, they have been added together"
                       , type = "warning"
                       , timer = 1500)
        }

        if((length(ing_df$design_w_opt[["Point"]]) < 4 && !(input$model %in% c("Michaelis-Menten", "Quadratic Heteroscedastic"))) || length(ing_df$design_w_opt[["Point"]]) < 3)
        {
            shinyalert("You need to add points!", type = "error")
            compute <- FALSE
        }
        else if(0 %in% ing_df$design_w_opt[["Weight"]])
        {
            compute <- FALSE
            shinyalert("Error", "There are points without weight!", type = "error")
        }
        else if(((max(ing_df$design_w_opt[["Point"]]) > input$design_sp[[2]] || min(ing_df$design_w_opt[["Point"]]) < input$design_sp[[1]]) && !(input$model %in% c("Michaelis-Menten", "Quadratic Heteroscedastic"))) || ((max(ing_df$design_w_opt[["Point"]]) > input$design_sp_MM || min(ing_df$design_w_opt[["Point"]]) < 0) && identical(input$model, "Michaelis-Menten")) || ((max(ing_df$design_w_opt[["Point"]]) > input$design_sp_CH || min(ing_df$design_w_opt[["Point"]]) < 0) && identical(input$model, "Quadratic Heteroscedastic")))
        {
            compute <- FALSE
            shinyalert("Error", "There are design points outside the design space!", type = "error")
        }
        else if(!near(input$delta, sum(ing_df$points_to_add[["Weight"]])))
        {
            shinyalert("", "The weights don't add up to 1, it'll be corrected"
                       # , do you want us to fix that for you?"
                       , type = "warning"
                       # , showCancelButton = TRUE, showConfirmButton = TRUE, callbackR = mycallback
                       , timer = 1500)
            ing_df$points_to_add[["Weight"]] <- ing_df$points_to_add[["Weight"]]/sum(ing_df$points_to_add[["Weight"]])*input$delta
            ing_df$design_w_opt <- bind_rows(ing_df$weighted_opt(), ing_df$points_to_add)
        }
        if(isTRUE(compute)) {
            if(identical(input$model, "Antoine Equation")) {
                ing_df$opt_design <- doptAntoine(input$a_par, input$b_par, input$c_par, input$design_sp[[1]], input$design_sp[[2]])
                ing_df$effres <- deff(dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design_w_opt), dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design), 3)

                sens_opt <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design))
                sens_new <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design_w_opt))

                # Puntos de corte y valor del corte
                cross <- sort(crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                val <- sens_val_to_add(input$deff, input$delta, 3)
                # Obtener start y par para tener las regiones
                start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_opt)
                par <- getPar(cross)
                ing_df$fac_reg_opt <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)

                new_des_data <- data.frame("Point" = ing_df$design_w_opt[["Point"]], "Weight" = ing_df$design_w_opt[["Weight"]], "Value" = rep(0, length(ing_df$design_w_opt[["Point"]])))

                p <- ggplot() + theme_ipsum()

                x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_res <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                # Loop para pintar las regiones factibles
                for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                    loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
                    ing_df$sens_res <- ing_df$sens_res + eval(parse(text=loop_input))
                }
            }
            else if(identical(input$model, "Quadratic")) {
                ing_df$opt_design <- doptCuad(input$design_sp[[1]], input$design_sp[[2]])
                ing_df$effres <- deff(dmatrixCuad(ing_df$design_w_opt), dmatrixCuad(ing_df$opt_design), 3)

                sens_opt <- dsensCuad(dmatrixCuad(ing_df$opt_design))
                sens_new <- dsensCuad(dmatrixCuad(ing_df$design_w_opt))

                # Puntos de corte y valor del corte
                cross <- sort(crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                val <- sens_val_to_add(input$deff, input$delta, 3)
                # Obtener start y par para tener las regiones
                start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_opt)
                par <- getPar(cross)
                ing_df$fac_reg_opt <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)

                new_des_data <- data.frame("Point" = ing_df$design_w_opt[["Point"]], "Weight" = ing_df$design_w_opt[["Weight"]], "Value" = rep(0, length(ing_df$design_w_opt[["Point"]])))
                p <- ggplot() + theme_ipsum()

                x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_res <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                # Loop para pintar las regiones factibles
                for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                    loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
                    ing_df$sens_res <- ing_df$sens_res + eval(parse(text=loop_input))
                }
            } else if(identical(input$model, "Michaelis-Menten")) {
                ing_df$opt_design <- doptMM(input$k_par, input$v_par, 0, input$design_sp_MM)
                ing_df$effres <- deff(dmatrixMM(input$k_par, input$v_par, ing_df$design_w_opt), dmatrixMM(input$k_par, input$v_par, ing_df$opt_design), 2)

                sens_opt <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$opt_design))
                sens_new <- dsensMM(input$k_par, input$v_par, dmatrixMM(input$k_par, input$v_par, ing_df$design_w_opt))

                # Puntos de corte y valor del corte
                cross <- sort(crosspoints(input$deff, input$delta, 2, sens_opt, 1000, 10^(-3), 0, input$design_sp_MM))
                val <- sens_val_to_add(input$deff, input$delta, 2)
                # Obtener start y par para tener las regiones
                start <- getStart(cross, 0, input$design_sp_MM, val, sens_opt)
                par <- getPar(cross)
                ing_df$fac_reg_opt <- getCross2(cross, 0, input$design_sp_MM, start, par)

                new_des_data <- data.frame("Point" = ing_df$design_w_opt[["Point"]], "Weight" = ing_df$design_w_opt[["Weight"]], "Value" = rep(0, length(ing_df$design_w_opt[["Point"]])))

                p <- ggplot() + theme_ipsum()

                x_val <- seq(0, input$design_sp_MM, length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_res <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  2, color = "goldenrod3") + xlim(0, input$design_sp_MM) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                # Loop para pintar las regiones factibles
                for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                    loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
                    ing_df$sens_res <- ing_df$sens_res + eval(parse(text=loop_input))
                }
            } else if(identical(input$model, "Quadratic Heteroscedastic")) {
                ing_df$opt_design <- doptCuadHet(input$order)
                ing_df$effres <- deff(dmatrixCuadHet(input$order, ing_df$design_w_opt), dmatrixCuadHet(input$order, ing_df$opt_design), 3)

                sens_opt <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$opt_design))
                sens_new <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$design_w_opt))

                # Puntos de corte y valor del corte
                cross <- sort(crosspoints(input$deff, input$delta, 3, sens_opt, 1000, 10^(-3), 0, input$design_sp_CH))
                val <- sens_val_to_add(input$deff, input$delta, 3)
                # Obtener start y par para tener las regiones
                start <- getStart(cross, 0, input$design_sp_CH, val, sens_opt)
                par <- getPar(cross)
                ing_df$fac_reg_opt <- getCross2(cross, 0, input$design_sp_CH, start, par)

                new_des_data <- data.frame("Point" = ing_df$design_w_opt[["Point"]], "Weight" = ing_df$design_w_opt[["Weight"]], "Value" = rep(0, length(ing_df$design_w_opt[["Point"]])))
                p <- ggplot() + theme_ipsum()

                x_val <- seq(0, input$design_sp_CH, length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_res <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(0, input$design_sp_CH) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                # Loop para pintar las regiones factibles
                for(i in 1:(length(ing_df$fac_reg_opt)/2)){
                    loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_opt[2*i-1],",xend=",ing_df$fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
                    ing_df$sens_res <- ing_df$sens_res + eval(parse(text=loop_input))
                }
            }
            else{
                shinyalert("Error", "Enter a valid model!", type = "error")
            }
        }

    })

    # With restricted design from the optimum
    observeEvent(input$enhace_design, {
        compute <- TRUE
        length1 <- nrow(ing_df$enhaced_des)
        ing_df$enhaced_des <- ing_df$enhaced_des %>%
            group_by(Point) %>%
            summarise_all(funs(sum))
        if((nrow(ing_df$design) < 3 && !(input$model %in% c("Michaelis-Menten", "Quadratic Heteroscedastic"))) || nrow(ing_df$design) < 2){
            shinyalert("Build a regular design!", type = "error")
            compute <- FALSE
        }
        if(length1 != nrow(ing_df$enhaced_des)){
            shinyalert("", "There are repeated points in the design, they have been added together"
                       , type = "warning"
                       , timer = 1500)
        }
        if(nrow(ing_df$enhaced_des) <= nrow(ing_df$design))
        {
            shinyalert("You need to add points!", type = "error")
            compute <- FALSE
        }
        else if(0 %in% ing_df$enhaced_des[["Weight"]])
        {
            compute <- FALSE
            shinyalert("Error", "There are points without weight!", type = "error")
        }
        else if(((max(ing_df$enhaced_des[["Point"]]) > input$design_sp[[2]] || min(ing_df$enhaced_des[["Point"]]) < input$design_sp[[1]]) && !(input$model %in% c("Michaelis-Menten", "Quadratic Heteroscedastic"))) || ((max(ing_df$enhaced_des[["Point"]]) > input$design_sp_MM || min(ing_df$enhaced_des[["Point"]]) < 0) && identical(input$model, "Michaelis-Menten")) || ((max(ing_df$enhaced_des[["Point"]]) > input$design_sp_CH || min(ing_df$enhaced_des[["Point"]]) < 0) && identical(input$model, "Quadratic Heteroscedastic")))
        {
            compute <- FALSE
            shinyalert("Error", "There are design points outside the design space!", type = "error")
        }
        else if(!near(input$deltaenh, sum(ing_df$points_to_add_build[["Weight"]])))
        {
            shinyalert("", "The weights don't add up to 1, it'll be corrected"
                       # , do you want us to fix that for you?"
                       , type = "warning"
                       # , showCancelButton = TRUE, showConfirmButton = TRUE, callbackR = mycallback
                       , timer = 1500)
            ing_df$points_to_add_build[["Weight"]] <- ing_df$points_to_add_build[["Weight"]]/sum(ing_df$points_to_add_build[["Weight"]])*input$deltaenh
            ing_df$enhaced_des <- bind_rows(weighted_build(), ing_df$points_to_add_build)
        }
        if(isTRUE(compute)) {
            if(identical(input$model, "Antoine Equation")) {
                ing_df$opt_design <- doptAntoine(input$a_par, input$b_par, input$c_par, input$design_sp[[1]], input$design_sp[[2]])
                ing_df$effenh <- deff(dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$enhaced_des), dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design), 3)

                sens_des <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design))
                sens_new <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$enhaced_des))

                # Puntos de corte y valor del corte
                cross <- sort(crosspoints(floor(input$deffenh#/ing_df$effbuild
                                                *10000)/10000, input$deltaenh, 3, sens_des, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                val <- sens_val_to_add(input$deffenh#/ing_df$effbuild
                                       , input$deltaenh, 3)
                # Obtener start y par para tener las regiones
                start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_des)
                par <- getPar(cross)
                ing_df$fac_reg_build <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)


                new_des_data <- data.frame("Point" = ing_df$enhaced_des[["Point"]], "Weight" = ing_df$enhaced_des[["Weight"]], "Value" = rep(0, length(ing_df$enhaced_des[["Point"]])))
                p <- ggplot() + theme_ipsum()

                x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_enh <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                # Loop para pintar las regiones factibles
                for(i in 1:(length(ing_df$fac_reg_build)/2)){
                    loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_build[2*i-1],",xend=",ing_df$fac_reg_build[2*i],",y=0,yend=0), color = 'green3')", sep="")
                    ing_df$sens_enh <- ing_df$sens_enh + eval(parse(text=loop_input))
                }
            }
            else if(identical(input$model, "Quadratic")) {
                ing_df$opt_design <- doptCuad(input$design_sp[[1]], input$design_sp[[2]])
                ing_df$effenh <- deff(dmatrixCuad(ing_df$enhaced_des), dmatrixCuad(ing_df$opt_design), 3)

                sens_des <- dsensCuad(dmatrixCuad(ing_df$design))
                sens_new <- dsensCuad(dmatrixCuad(ing_df$enhaced_des))

                # Puntos de corte y valor del corte
                cross <- sort(crosspoints(floor(input$deffenh#/ing_df$effbuild
                                                *10000)/10000, input$deltaenh, 3, sens_des, 1000, 10^(-3), input$design_sp[[1]], input$design_sp[[2]]))
                val <- sens_val_to_add(input$deffenh#/ing_df$effbuild
                                       , input$deltaenh, 3)
                # Obtener start y par para tener las regiones
                start <- getStart(cross, input$design_sp[[1]], input$design_sp[[2]], val, sens_des)
                par <- getPar(cross)
                ing_df$fac_reg_build <- getCross2(cross, input$design_sp[[1]], input$design_sp[[2]], start, par)

                new_des_data <- data.frame("Point" = ing_df$enhaced_des[["Point"]], "Weight" = ing_df$enhaced_des[["Weight"]], "Value" = rep(0, length(ing_df$enhaced_des[["Point"]])))

                p <- ggplot() + theme_ipsum()

                x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_enh <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                # Loop para pintar las regiones factibles
                for(i in 1:(length(ing_df$fac_reg_build)/2)){
                    loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_build[2*i-1],",xend=",ing_df$fac_reg_build[2*i],",y=0,yend=0), color = 'green3')", sep="")
                    ing_df$sens_enh <- ing_df$sens_enh + eval(parse(text=loop_input))
                }
            } else if(identical(input$model, "Michaelis-Menten")) {
                ing_df$opt_design <- doptMM(input$k_par, input$v_par,  0, input$design_sp_MM)
                ing_df$effenh <- deff(dmatrixMM(input$k_par, input$v_par,  ing_df$enhaced_des), dmatrixMM(input$k_par, input$v_par,  ing_df$opt_design), 2)

                sens_des <- dsensMM(input$k_par, input$v_par,  dmatrixMM(input$k_par, input$v_par,  ing_df$design))
                sens_new <- dsensMM(input$k_par, input$v_par,  dmatrixMM(input$k_par, input$v_par,  ing_df$enhaced_des))

                # Puntos de corte y valor del corte
                cross <- sort(crosspoints(floor(input$deffenh#/ing_df$effbuild
                                                *10000)/10000, input$deltaenh, 2, sens_des, 1000, 10^(-3), 0, input$design_sp_MM))
                val <- sens_val_to_add(input$deffenh#/ing_df$effbuild
                                       , input$deltaenh, 2)
                # Obtener start y par para tener las regiones
                start <- getStart(cross, 0, input$design_sp_MM, val, sens_des)
                par <- getPar(cross)
                ing_df$fac_reg_build <- getCross2(cross, 0, input$design_sp_MM, start, par)


                new_des_data <- data.frame("Point" = ing_df$enhaced_des[["Point"]], "Weight" = ing_df$enhaced_des[["Weight"]], "Value" = rep(0, length(ing_df$enhaced_des[["Point"]])))
                p <- ggplot() + theme_ipsum()

                x_val <- seq(0, input$design_sp_MM, length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_enh <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  2, color = "goldenrod3") + xlim(0, input$design_sp_MM) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                # Loop para pintar las regiones factibles
                for(i in 1:(length(ing_df$fac_reg_build)/2)){
                    loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_build[2*i-1],",xend=",ing_df$fac_reg_build[2*i],",y=0,yend=0), color = 'green3')", sep="")
                    ing_df$sens_enh <- ing_df$sens_enh + eval(parse(text=loop_input))
                }
            } else if(identical(input$model, "Quadratic Heteroscedastic")) {
                ing_df$opt_design <- doptCuadHet(input$order)
                ing_df$effenh <- deff(dmatrixCuadHet(input$order, ing_df$enhaced_des), dmatrixCuadHet(input$order, ing_df$opt_design), 3)

                sens_des <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$design))
                sens_new <- dsensCuadHet(input$order, dmatrixCuadHet(input$order, ing_df$enhaced_des))

                # Puntos de corte y valor del corte
                cross <- sort(crosspoints(floor(input$deffenh#/ing_df$effbuild
                                                *10000)/10000, input$deltaenh, 3, sens_des, 1000, 10^(-3), 0, input$design_sp_CH))
                val <- sens_val_to_add(input$deffenh#/ing_df$effbuild
                                       , input$deltaenh, 3)
                # Obtener start y par para tener las regiones
                start <- getStart(cross, 0, input$design_sp_CH, val, sens_des)
                par <- getPar(cross)
                ing_df$fac_reg_build <- getCross2(cross, 0, input$design_sp_CH, start, par)

                new_des_data <- data.frame("Point" = ing_df$enhaced_des[["Point"]], "Weight" = ing_df$enhaced_des[["Weight"]], "Value" = rep(0, length(ing_df$enhaced_des[["Point"]])))

                p <- ggplot() + theme_ipsum()

                x_val <- seq(0, input$design_sp_CH, length.out = 10000)
                y_val <- purrr::map_dbl(x_val, sens_new)

                ing_df$sens_enh <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(0, input$design_sp_CH) + labs(x = "X", y = "") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))

                # Loop para pintar las regiones factibles
                for(i in 1:(length(ing_df$fac_reg_build)/2)){
                    loop_input = paste("geom_segment(aes(x=",ing_df$fac_reg_build[2*i-1],",xend=",ing_df$fac_reg_build[2*i],",y=0,yend=0), color = 'green3')", sep="")
                    ing_df$sens_enh <- ing_df$sens_enh + eval(parse(text=loop_input))
                }
            }
            else{
                shinyalert("Error", "Enter a valid model!", type = "error")
            }
        }
    })


    # Efficiency of the industry designs
    # observeEvent(input$calc_ind_eff, {
    #     calculate <- T
    #     if(identical(input$model, "Antoine Equation")) {
    #         ing_df$opt_design <- doptAntoine(input$a_par, input$b_par, input$c_par, input$design_sp[[1]], input$design_sp[[2]])
    #         dopt_mat <- dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design)
    #
    #         if(identical(input$family, "Equidistant")){
    #             ing_df$ind_des <- uniform_design(input$ind_points, input$design_sp[[1]], input$design_sp[[2]])
    #         }
    #         else if(identical(input$family, "Arithmetic")){
    #             ing_df$ind_des <- arithmetic_design(as.numeric(input$ind_points), as.numeric(input$design_sp[[1]]), as.numeric(input$design_sp[[2]]), dopt_mat, input$model, input$a_par, input$b_par, input$c_par)
    #         }
    #         else if(identical(input$family, "Geometric")){
    #             ing_df$ind_des <- geometric_design(as.numeric(input$ind_points), as.numeric(input$design_sp[[1]]), as.numeric(input$design_sp[[2]]), dopt_mat, input$model, input$a_par, input$b_par, input$c_par)
    #         }
    #         else{
    #             shinyalert("Error", "Choose a family of designs!", type = "error")
    #             calculate <- F
    #         }
    #
    #         if(calculate){
    #             ing_df$effind <- deff(dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$ind_des), dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design), 3)
    #
    #             sens_opt <- dsensAntoine(input$a_par, input$b_par, input$c_par, dopt_mat)
    #             sens_new <- dsensAntoine(input$a_par, input$b_par, input$c_par, dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$ind_des))
    #
    #
    #             new_des_data <- data.frame("Point" = ing_df$ind_des[["Point"]], "Weight" = ing_df$ind_des[["Weight"]], "Value" = rep(0, length(ing_df$ind_des[["Point"]])))
    #             p <- ggplot() + theme_ipsum()
    #
    #             x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
    #             y_val <- purrr::map_dbl(x_val, sens_new)
    #
    #             ing_df$sens_ind <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "Temperature (ºC)", y = "Pressure") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))
    #         }
    #     }
    #     else if(identical(input$model, "Quadratic")) {
    #         ing_df$opt_design <- doptCuad(input$design_sp[[1]], input$design_sp[[2]])
    #         dopt_mat <- dmatrixCuad(ing_df$opt_design)
    #
    #         if(identical(input$family, "Equidistant")){
    #             ing_df$ind_des <- uniform_design(input$ind_points, input$design_sp[[1]], input$design_sp[[2]])
    #         }
    #         else if(identical(input$family, "Arithmetic")){
    #             ing_df$ind_des <- arithmetic_design(as.numeric(input$ind_points), as.numeric(input$design_sp[[1]]), as.numeric(input$design_sp[[2]]), dopt_mat, input$model)
    #         }
    #         else if(identical(input$family, "Geometric")){
    #             ing_df$ind_des <- geometric_design(as.numeric(input$ind_points), as.numeric(input$design_sp[[1]]), as.numeric(input$design_sp[[2]]), dopt_mat, input$model)
    #         }
    #         else{
    #             shinyalert("Error", "Choose a family of designs!", type = "error")
    #             calculate <- F
    #         }
    #
    #         if(calculate){
    #             ing_df$effind <- deff(dmatrixCuad(ing_df$ind_des), dopt_mat, 3)
    #
    #             sens_opt <- dsensCuad(dmatrixCuad(ing_df$opt_design))
    #             sens_new <- dsensCuad(dmatrixCuad(ing_df$ind_des))
    #
    #             new_des_data <- data.frame("Point" = ing_df$ind_des[["Point"]], "Weight" = ing_df$ind_des[["Weight"]], "Value" = rep(0, length(ing_df$ind_des[["Point"]])))
    #             p <- ggplot() + theme_ipsum()
    #
    #             x_val <- seq(input$design_sp[[1]], input$design_sp[[2]], length.out = 10000)
    #             y_val <- purrr::map_dbl(x_val, sens_new)
    #
    #             ing_df$sens_ind <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  3, color = "goldenrod3") + xlim(input$design_sp[[1]], input$design_sp[[2]]) + labs(x = "X", y = "Y") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))
    #         }
    #     }
    #     else if(identical(input$model, "Michaelis-Menten")) {
    #         ing_df$opt_design <- doptMM(input$k_par, input$v_par,  0, input$design_sp_MM)
    #         dopt_mat <- dmatrixMM(input$k_par, input$v_par,  ing_df$opt_design)
    #
    #         if(identical(input$family, "Equidistant")){
    #             ing_df$ind_des <- uniform_design(input$ind_points, 0, input$design_sp_MM)
    #         }
    #         else if(identical(input$family, "Arithmetic")){
    #             ing_df$ind_des <- arithmetic_design(as.numeric(input$ind_points), as.numeric(0), as.numeric(input$design_sp_MM), dopt_mat, input$model, input$k_par, input$v_par)
    #         }
    #         else if(identical(input$family, "Geometric")){
    #             ing_df$ind_des <- geometric_design(as.numeric(input$ind_points), as.numeric(0), as.numeric(input$design_sp_MM), dopt_mat, input$model, input$k_par, input$v_par)
    #         }
    #         else{
    #             shinyalert("Error", "Choose a family of designs!", type = "error")
    #             calculate <- F
    #         }
    #
    #         if(calculate){
    #             ing_df$effind <- deff(dmatrixMM(input$k_par, input$v_par,  ing_df$ind_des), dmatrixMM(input$k_par, input$v_par,  ing_df$opt_design), 2)
    #
    #             sens_opt <- dsensMM(input$k_par, input$v_par,  dopt_mat)
    #             sens_new <- dsensMM(input$k_par, input$v_par,  dmatrixMM(input$k_par, input$v_par,  ing_df$ind_des))
    #
    #
    #             new_des_data <- data.frame("Point" = ing_df$ind_des[["Point"]], "Weight" = ing_df$ind_des[["Weight"]], "Value" = rep(0, length(ing_df$ind_des[["Point"]])))
    #             p <- ggplot() + theme_ipsum()
    #
    #             x_val <- seq(0, input$design_sp_MM, length.out = 10000)
    #             y_val <- purrr::map_dbl(x_val, sens_new)
    #
    #             ing_df$sens_ind <- p + geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + geom_hline(yintercept =  2, color = "goldenrod3") + xlim(0, input$design_sp_MM) + labs(x = "Temperature (ºC)", y = "Pressure") + geom_point(data = new_des_data, aes(x = Point, y = Value, size = Weight), shape = 16, color = "steelblue3") + scale_size_continuous(range = c(2, 4))
    #         }
    #     }
    #     else{
    #         shinyalert("Error", "Enter a valid model!", type = "error")
    #     }
    # })


    # Compute Effinciency givedes
    efficiency <- reactive({
        if(is.numeric(ing_df$eff))
            round(ing_df$eff*100, 1)
        else
            ing_df$eff
    })

    # Compute Effinciency restricteddes
    efficiencyres <- reactive({
        if(is.numeric(ing_df$effres))
            round(ing_df$effres*100, 1)
        else
            ing_df$effres
    })

    # Compute Effinciency builddes
    efficiencybuild <- reactive({
        if(is.numeric(ing_df$effbuild))
            round(ing_df$effbuild*100, 1)
        else
            ing_df$effbuild
    })

    # Compute Effinciency enhanced
    efficiencyenh <- reactive({
        if(is.numeric(ing_df$effenh))
            round(ing_df$effenh*100, 1)
        else
            ing_df$effenh
    })

    # Compute Effinciency industry
    # efficiency_indus <- reactive({
    #     if(is.numeric(ing_df$effind))
    #         round(ing_df$effind*100, 1)
    #     else
    #         ing_df$effind
    # })

    # Plot sens
    ing_df$sens_opt <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()

    output$sens_opt <- renderPlotly({
        ggplotly(ing_df$sens_opt)
    })

    # Plot sens give
    ing_df$sens_new <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()

    output$sens_new <- renderPlotly({
        ggplotly(ing_df$sens_new)
    })

    # Plot sens res
    ing_df$sens_res <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()

    output$sens_res <- renderPlotly({
        ggplotly(ing_df$sens_res)
    })

    #Plot sens build
    ing_df$sens_build <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()

    output$sens_build <- renderPlotly({
        ggplotly(ing_df$sens_build)
    })

    #Plot sens enh
    ing_df$sens_enh <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()

    output$sens_enh <- renderPlotly({
        ggplotly(ing_df$sens_enh)
    })

    #Plot sens ind
    # ing_df$sens_ind <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()
    #
    # output$sens_ind <- renderPlotly({
    #     ggplotly(ing_df$sens_ind)
    # })







    # value box optimal design senstitivy
    output$op_sensitivity <- renderValueBox({
        valueBox(
            paste0("Sensitivity Function"#, output$efficiency
            ), "For the optimum design", icon = icon("bar-chart-o"),
            color = "purple"
        )
    })

    # value box industry design
    # output$industry_des <- renderValueBox({
    #     valueBox(
    #         paste0("Sensitivity Function"#, output$efficiency
    #         ), "For the optimum design", icon = icon("bar-chart-o"),
    #         color = "yellow"
    #     )
    # })

    # value box industry design senstitivy
    # output$industry_sens <- renderValueBox({
    #     valueBox(
    #         paste0("Sensitivity Function"#, output$efficiency
    #         ), "For the industry design", icon = icon("bar-chart-o"),
    #         color = "purple"
    #     )
    # })

    # Value box for different efficiencies
    output$efficiency <- renderValueBox({
        valueBox(
            paste0("Efficiency ", efficiency(), "%" #input$count
            ), "For the input design", icon = icon("divide"),
            color = "red"
        )
    })

    output$efficiencyresdes <- renderValueBox({
        valueBox(
            paste0("Efficiency ", efficiencyres(), "%" #input$count
            ), "For the input design", icon = icon("divide"),
            color = "red"
        )
    })

    output$efficiencybuildes <- renderValueBox({
        valueBox(
            paste0("Efficiency ", efficiencybuild(), "%" #input$count
            ), "For the input design", icon = icon("divide"),
            color = "red"
        )
    })

    output$efficiencyenhdes <- renderValueBox({
        valueBox(
            paste0("Efficiency ", efficiencyenh(), "%" #input$count
            ), "With respect to the optimal design", icon = icon("divide"),
            color = "red"
        )
    })

    # output$efficiency_ind <- renderValueBox({
    #     valueBox(
    #         paste0("Efficiency ", efficiency_indus(), "%" #input$count
    #         ), "For the industry design", icon = icon("divide"),
    #         color = "red"
    #     )
    # })


    observeEvent(input$model, {
        header <- input$model

        # you can use any other dynamic content you like
        shinyjs::html("pageHeader", header)
    })


    restart <- function(){
        updateSliderInput(session, "delta",
                          min = 0, max = 0.99, value = 0)

        updateSliderInput(session, "deltaenh",
                          min = 0, max = 0.99, value = 0)

        # Diseño en blanco que el usuario va construyendo
        ing_df$design <- data.frame("Point" = numeric(),
                                    "Weight" = numeric())

        # dataset "standard", diseño añadiendo al óptimo los puntos de cortes con igula peso
        ing_df$design_std <- data.frame("Point" = numeric(),
                                        "Weight" = numeric())

        # Diseño final al partir del óptimo y añadir puntos
        # ing_df$ind_des <- data.frame("Point" = numeric(),
        #                              "Weight" = numeric())

        # Punto a añadir al construido
        ing_df$points_to_add_build <- data.frame("Point" = numeric(),
                                                 "Weight" = numeric())

        # Diseño final al partir del construido valiendo (1-alpha)
        ing_df$weighted_build <- data.frame("Point" = numeric(),
                                            "Weight" = numeric())

        # Diseño final al partir del construido y añadir puntos
        ing_df$enhaced_des <- data.frame("Point" = numeric(),
                                         "Weight" = numeric())

        # Se ha calculado el diseño óptimo?
        ing_df$calculated_design <- F

        # Se ha calculado un diseño propio?
        ing_df$built_design <- F

        # Límites de eficiencia en funcion de delta
        ing_df$eff_limit <- c(0, 1)

        # Límites de eficiencia en funcion de delta para enhace design
        ing_df$eff_limit_enh <- c(0, 1)

        # Regiones factibles añadir óptimo
        ing_df$fac_reg_opt <- vector("numeric")

        # Regiones factibles añadir custom
        ing_df$fac_reg_build <- vector("numeric")

        # Plot sens
        ing_df$sens_opt <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()


        # Plot sens give
        ing_df$sens_new <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()


        # Plot sens res
        ing_df$sens_res <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()


        #Plot sens build
        ing_df$sens_build <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()


        #Plot sens enh
        ing_df$sens_enh <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()


        #Plot sens ind
        # ing_df$sens_ind <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum()





    }

}

# Run the application
shinyApp(ui = ui, server = server)



