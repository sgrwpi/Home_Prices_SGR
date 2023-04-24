library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
library(tidyverse)
library(ggstatsplot)
library(corrplot)
library(olsrr)
library(leaps)
library(rsample)
library(rpart)
library(recipes)
library(broom)


read_data = read_csv('train.csv', show_col_types = FALSE)
df = data.frame(read_data)
desc_data = read_csv('desc.csv', col_names=FALSE)
names(desc_data) <- c('features', 'desc')
desc_df <- data.frame(desc_data)
colSums(is.na(df))

df2 <- within(df, rm('Alley', 'PoolQC', 'Fence', 'MiscFeature','FireplaceQu', 'Utilities'))
df2 <- na.omit(df2)
df2$var <- sample(c("A", "B"), size = 1094, replace = TRUE)
features = colnames(df2)
my_numeric <- df2[, sapply(df2, is.numeric)]

set.seed(123)
split <- initial_split(df2, prop = 0.7)
train_houses <- training(split)
test_houses  <- testing(split)
test_houses$var[1] <- "C"
all_features <- lm(SalePrice ~ ., data=train_houses)

rec <- 
  recipe(SalePrice ~ ., data = train_houses) %>% 
  step_other(var) %>%
  prep()

new_train_houses <- bake(rec, new_data = train_houses)
new_test_houses  <- bake(rec, new_data = test_houses)

model <- rpart(
  formula = SalePrice ~ 0+.,
  data = new_train_houses)

predPrice = data.frame(predict(all_features, newdata=new_test_houses))
names(predPrice)[1] = 'Predicted'
predPrice$Reference = new_test_houses[,c('SalePrice')]
predPrice <- na.omit(predPrice)

my_corr <- round(cor(my_numeric),1)


# Define UI for application

ui <- fluidPage(
  titlePanel(h1('Exploring Home Prices')),
  navbarPage("DS 501 - Final Project",
             tabPanel("Background & Motivation",
                      tabName = "background",
                      icon = icon("book"),
                      
                      fluidPage(theme=shinytheme("readable"),
                                fluidRow(
                                  column(4,
                                p(strong("Background on Housing DataSet")),
                                p("Over the past few years, housing has been one of the hottest news items. COVID resulted in seismic shifts related to where
                                and how people wanted to live. We saw movement from cities to suburbs. We saw people changing locations because
                                of workplace dynamics. Now, we're seeing some reversion as employers require employees to return to office. Interest rates
                                have made it even harder for people to afford a home. Related to our scope, predicting prices has proven to be incredibly challenging.
                                Even with the most sophisticated algorithm, mistakes are made."),
                                tags$a(href="https://www.nytimes.com/2021/11/02/business/zillow-q3-earnings-home-flipping-ibuying.html","A November 2021
                                       headline in the NY Times"), 
                                ("declared 'Zillow, facing big losses, quits flipping houses and will lay off a quarter of its staff. The real estate 
                                website had been relying on its algorithm that estimates home values to buy and resell homes. That part of its business
                                lost about $420 million in three months.'"),
                                ("A key motivation for this project is understanding what factors most influece housing prices and building a linear
                                regression model in hopes of developing quality predictions for home prices. By understanding which features are most
                                  important in predicting home prices, we as home buyers can make better informed decisions in an effort to pay a price
                                  in line with what the market has historically tolerated."),
                                tags$a(href="https://kaggle.com/competitions/home-data-for-ml-course",'You can find the dataset at this Kaggle link.'),
                                p(),
                                p(strong("Navigating this App")),
                                p("The",span('Exploratory Analysis - Scatterplot', style = "color:blue"),"tab allows you to explore the dataset and see how the
                                variables in the dataset, including our dependent variable which is Sale Price, are related."),
                                p("The",span('Correlation Matrix', style="color:blue"),"provides a visual overview of how correlated all of
                                  our variables are. You can also select two variables and calculate their correlation coefficient."),
                                p("The",span("Predicted Prices",style='color:blue'),"tab allows you to explore the independent variables and see their coefficients."),
                                p("",span("Explore the Dataset",style='color:blue'),"gives you an opportuntiy to dynamically explore the dataset underlying this analysis."),
                                p("",span("The Math Behind Regression",style='color:blue'),"tab provides an overview of the mathematical principles of linear regression."),
                                p('Finally, the',span("Variable definitions", style = "color:blue"), "allows you to explore each variable and understand the specific definition
                                  as provided from the original source."),
                                
                                br(),
                                ),
                               column(8,
                               p(tags$img(src="house.jpeg",alt = "White house with red windows",width=800, style="display: block; margin-left: auto; margin-right: auto;"))
                               ),
                                
                    ))),
             
             
             tabPanel("Exploratory Analysis - Scatterplot",
                      tabname="eda",
                      icon=icon("chart-simple"),
                      fluidPage(theme=shinytheme("readable"),
                                p(strong("A quick overview of what a scatterplot can tell us:")),
                                p("The scatterplot below gives us a visual understanding of the relationship between an independent variable
                                  and our dependent variable, Sales Price. If we see a clear linear relationship in the scatterplot, then we
                                  may be able to guess that the independent variable is helpful in predicting the Sale Price. In the starting plot
                                  below we can already see that as GrLivArea - Above grade (ground) living area square feet increases, the correlation
                                  between the two variables decreases. For this project, we are not removing outliers, but it should be considered for
                                  future iterations.
                                  "),  
                                br()),
                      selectInput("var1", "Choose an Independent Variable",
                                  as.list(features), selected = 'GrLivArea'),
                      selectInput("var2", "Choose a Dependent Variable (Sale Price)",
                                  as.list(features), selected = 'SalePrice'),
                      plotOutput('myplot')
                      
                  ),
             
             
             tabPanel("Correlation Matrix",
                      tabname="findings",
                      icon=icon("magnifying-glass"),
                      fluidPage(theme=shinytheme("readable"),
                      plotOutput('corr', height = "1200px",
                                 width = "100%"))
                     

                      
            ),
            tabPanel("Predict Prices",
                     tabname="predict",
                     icon=icon("layer-group"),
                     fluidPage(theme=shinytheme("readable"),
                       fluidRow(
                         column(8,
                     p('The Adjusted R-squared of our model = 92.8% which means that our model explains 92.8% of
                     the variance in our sale sprice is explained by the independent variables in our model.'),
                     plotOutput('predict'),
                     DT::DTOutput('coefficients')
                     
                     
            )))),
            
            tabPanel("Explore The Data",
                     tabname="predict",
                     icon=icon("splotch"),
                     DT::DTOutput('mytable'),
           ),
            
            tabPanel("The Math Behind Regression",
                     tabname="regression",
                     icon=icon("calculator"),
                     p(strong("An Overview of Linear Regression")),
                                p("Linear Regression, specifically multivariate linear regression, is used to predict a dependent variable,
                                in our case Sale Price of a property, using independent variables that we already know – like square footage
                                of the lot or the number of full bathrooms. The output of regression is an equation of the
                                line of best fit given our independent variables that minimizes the difference between our predicted price
                                and the actual price from our dataset. The underlying assumption with linear regression is that the 
                                regression function is approximately linear. The function below represents our regression model
                                with Y representing a vector of our predicted sales price, each x representing an independent variable and each b
                                representing the corresponding coefficient or weight associated with the independent variable. n represents the number
                                of indepdent variables in the model and e is our error component. Error occurs because we will likely never have a model
                                that predicts the sale price of a home with 100% accuracy 100% of the time."),
                                p(tags$img(src="linear_eq.jpeg",alt = "linear regression equation",width=800, style="display: block; margin-left: auto; margin-right: auto;")),
                                br(),
                                strong('A Quick Note on Variables'),
                                p("In our study on home prices, we have a variety of types of variables: continuous, discrete, and categorial. A an example of a continuous
                                variable is total basement square footage, a discrete example is number of full bathrooms, and a categorical example is Garage type. The easiest
                                variables to work with in linear regression are continuous. The more challenging variable type to work with is Not only
                                are we trying to calculate sale price of a home, we also want to determine the coefficients/weights associated with each independent variable
                                as well as the function's intercept. You can explore these coefficients on the Predict Prices tab."),
                                strong('Understanding the quality of our model'),
                                p('The quality of this model is good with an adjusted R-squared = 92.8%')
            ),
                     
                     
                  tabPanel("Variable Definitions",
                         tabname="define",
                         icon=icon("book"),
                         DT::DTOutput('define')),             
                            
                        
                  
                     
                     
            )
        )
# Define server logic 
server <- function(input, output){
  #output for scatterplot on Exploratory analysis tab
  output$myplot <- renderPlot({
    ggplot(df2, aes_string(x=input$var1,y=input$var2))+
      geom_point() +
      geom_smooth(method='lm')
    })
  
  #Output for correlation matrix tab
  output$corr <- renderPlot({
     corrplot(my_corr, method = 'number')
    })
   
  #Output for home price prediction
  output$predict <- renderPlot({
    ggplot(predPrice, aes(x=Predicted,y=Reference$SalePrice))+
      geom_point() +
      geom_smooth(method='lm')
  })
  
  output$coefficients <- DT::renderDT({tidy(lm(SalePrice ~ ., data=new_train_houses))})

  
  
  #output for Dataset tab
  output$mytable <- DT::renderDT({df2
    })
   
  output$define <- DT::renderDT({desc_df})
  }
  


#-------------------REGRESSION-------------------#



#   output$indep <- renderUI({
#     selectInput(inputId = "indep", label = "Independent Variables", 
#                 multiple = TRUE, choices = as.list(features[features!= input$dependent]), selected = features[1])
#   })
#   
#   
#   
#   recipe_formula <- reactive({
#     req(input$indep)
#     df %>%
#       recipe() %>%
#       update_role(!!!input$dependent, new_role = "outcome") %>%
#       update_role(!!!input$indep, new_role = "predictor") %>%
#       prep() %>% 
#       formula()
#   })
#   
#   lm_reg <- reactive(
#     lm(recipe_formula(),data = df)
#   )
#   
#   
#   
#   
#   output$RegOut = renderPrint({
#     summary(lm_reg())
#   })
#   

# Run the application 

shinyApp(ui, server)