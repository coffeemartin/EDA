library(shiny)
library(shinyWidgets)
library(dplyr)
library(wesanderson)
library(ggplot2)

#https://www.youtube.com/watch?v=k9l2WqSfrn0&ab_channel=FrancoM

shiny_df <- read.csv("df_Shiny.csv", sep=",")
a <- shiny_df %>% select(where(is.numeric),) %>% select(-c(Gross_tertiary_education_enrollment_, Population, Unemployment_rate,Urban_population,Latitude,Longitude))
b <- shiny_df %>% select(-where(is.numeric),) %>% select(-c(Youtuber, Title, created_date_1))
# Define the UI



# Define the UI
ui <- fluidPage(
  
  
  
  titlePanel(htmlOutput("title_panel")),
  tags$head(tags$style("#title_panel{background-color: #033E3E;color: #F2AD00;
                             font-size: 20px;
                         font-style: strong;
                         }"
  )
  ),
  sidebarPanel(
    radioButtons("plot_type", "Select plot type:", choices = c("BarPlot", "BoxPlot","Scatter"),selected = "BarPlot"),
    pickerInput("colour_p", "Wes Anderson Colour Palette:", choices =
                  c("BottleRocket1","BottleRocket2","Rushmore1","Rushmore","Royal1","Royal2","Zissou1","Darjeeling1","Darjeeling2","Chevalier1","FantasticFox1",
                    "Moonrise1","Moonrise2","Moonrise3","Cavalcanti1","GrandBudapest1","GrandBudapest2","IsleofDogs1","IsleofDogs2"),selected = "BottleRocket2"),
    conditionalPanel(
      condition = "input.plot_type == 'BarPlot' ",
      selectInput("Categorical_variable", "Select a Categorical attribute for Y axis:", names(b), selected = "created_year"),
      selectInput("Numerical_variable", "Select a Numerical attribute for X axis:", names(a),selected = "subscribers"),
      radioButtons("Sort_Method", label = "How would you like to sort your Y axis?", choices = c("By Value","By Categories"), selected ="By Categories"),
      radioButtons("y_trans", label = "X axis Transformation", choices = c("No Transformation", "log Transformation"), selected = "No Transformation")
      
    ),
    conditionalPanel(
      condition = "input.plot_type == 'BoxPlot'",
      selectInput("Categorical_variable_1", "Select a Categorical attribute for Y axis:", names(b), selected = "created_year"),
      selectInput("Numerical_variable_1", "Select a Numerical attribute for X axis:", names(a),selected = "subscribers"),
      radioButtons(inputId = "y_trans_1", label = "X axis Transformation", choices = c("No Transformation", "log Transformation"), selected = "No Transformation")
    ),
    conditionalPanel(
      condition = "input.plot_type == 'Scatter'",
      selectInput("x_attr", "Select x-axis attribute:", names(a), selected = "subscribers"),
      radioButtons(inputId = "x_transformation", label = "x-axis Transformation", choices = c("No Transformation", "log Transformation"), selected = "No Transformation"),
      selectInput("y_attr", "Select y-axis attribute:", names(a),selected = "video_views"),
      radioButtons(inputId = "y_transformation", label = "y-axis Transformation", choices = c("No Transformation", "log Transformation"), selected = "No Transformation"),
      radioButtons(inputId = "Add_third_dimension", label = "Third_dimension", choices = c("None", "Continent"), selected = "None"),
      p("Instructions of zoom function:", class = "my-class1"),
      tags$ul(
        tags$li("click and hold to draw a box on the plot, define where you would like to zoom in"), 
        tags$li("double click to zoom in"),
        tags$li("double click some where else to zoom out")),
      tags$head(tags$style('p {color:red;}'))
      
      
    ),
    hr(),
    print("Data Source: 'Global YouTube Statistics 2023' "),
    tags$a(href="https://www.kaggle.com/datasets/nelgiriyewithana/global-youtube-statistics-2023 ", "(Link)"),
    hr(),
    print("Franco Meng - 23370209"),
    tags$script('
          $(document).ready(function(){
          var d = new Date();
          var target = $("#clientTime");
          target.val(d.toLocaleString());
          target.trigger("change");
          });
          '),
    textInput("clientTime", "", value = "")
  ),
  mainPanel(
    plotOutput("plot",height = "800px", width = "700px",dblclick = "plot_dblclick",
               brush = brushOpts(
                 id = "plot_brush",
                 resetOnNew = TRUE))
  )
  
  
)

# Define the server
server <- function(input, output) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  output$title_panel <- renderText({
    if (input$plot_type == "BarPlot") {
      log_condition <- if(input$y_trans == "log Transformation"){"(Log Transformed)"} else{" "}
      paste("Barplot:</p> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Between [",input$Categorical_variable, "] and [",input$Numerical_variable, "] ", "- Sorted [", input$Sort_Method, "] ", log_condition)
    }
    
    else if (input$plot_type == "BoxPlot") {
      log_condition <- if(input$y_trans_1 == "log Transformation"){"(Log Transformed)"} else{" "}
      paste("Boxplot:</p> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Between [",input$Categorical_variable_1, "] and [",input$Numerical_variable_1, "] ", log_condition)
    }
    
    else if (input$plot_type == "Scatter") {
      log_condition <- if(input$x_transformation == "log Transformation" || input$y_transformation == "log Transformation"){"(Log Transformed)"} else{" "}
      paste("Scatter plot:</p> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Between [",input$x_attr, "] and [",input$y_attr,"] ", log_condition)
    }
    
  })
  
  
  
  
  
  output$plot <- renderPlot({
    if (input$plot_type == "BarPlot") {
      x_attr <- input$Categorical_variable
      y_attr <- ifelse(input$y_trans == "log Transformation",paste("log(",input$Numerical_variable,")",collapse ="") ,input$Numerical_variable)
      n_attr <- n_distinct(shiny_df[,input$Categorical_variable])
      if (input$Sort_Method == "By Value"){
        x_attr_1 <- paste("reorder(",input$Categorical_variable,",", y_attr, ",function(x){ sum(x)})",collapse = "") 
        
      }
      else
      {x_attr_1 <- input$Categorical_variable}
      ggplot(shiny_df,aes_string(x = x_attr_1, y = y_attr, fill = x_attr)) + stat_summary(geom = "col", fun = sum) + xlab(x_attr) + coord_flip() + theme(legend.position = "none", axis.text=element_text(size=11,face="bold"), axis.title=element_text(size=14))+ scale_fill_manual(values = rev(wes_palette(n_attr, name = input$colour_p, type = "continuous")), name = "") 
    } 
    
    else if (input$plot_type == "BoxPlot") {
      x_attr <- input$Categorical_variable_1
      y_attr <- ifelse(input$y_trans_1 == "log Transformation",paste("log(",input$Numerical_variable_1,")",collapse ="") ,input$Numerical_variable_1)
      n_attr <- n_distinct(shiny_df[,input$Categorical_variable_1])
      ggplot(shiny_df,aes_string(x = x_attr, y = y_attr, fill = x_attr)) + geom_boxplot()  + coord_flip() +  theme(legend.position = "none", axis.text=element_text(size=11,face="bold"), axis.title=element_text(size=14)) + scale_fill_manual(values = rev(wes_palette(n_attr, name = input$colour_p, type = "continuous")), name = "")
    } 
    
    else if(input$plot_type == "Scatter") {
      x_attr <- ifelse(input$x_transformation == "log Transformation",paste("log(",input$x_attr,")",collapse ="") ,input$x_attr)
      y_attr <- ifelse(input$y_transformation == "log Transformation",paste("log(",input$y_attr,")",collapse ="") ,input$y_attr)
      factor_var <- ifelse(input$Add_third_dimension == "Continent","continent" ,"NULL")
      ggplot(shiny_df, aes_string(x = x_attr, y = y_attr, colour = factor_var)) + 
        geom_point(alpha=0.5)+ geom_smooth(se=FALSE, size = 1.5)+ coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) + theme( axis.text=element_text(size=11,face="bold"), axis.title=element_text(size=14),legend.key.size = unit(4, 'cm'), 
                                                                                                                                             legend.key.height = unit(1.5, 'cm'),
                                                                                                                                             legend.key.width = unit(2, 'cm'), 
                                                                                                                                             legend.title = element_text(size=13), 
                                                                                                                                             legend.text = element_text(size=11)) + scale_color_manual(values = rev(wes_palette(6, name = input$colour_p, type = "continuous")), name = "")
      
      
    }
  })
  
  #aes(colour = factor(new_df_3$continent)), alpha = 0.8
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}
# Run the Shiny app
shinyApp(ui = ui, server = server)
