library(foreign)
library(memisc)
library(Rmisc)
library(corrplot)
library(kableExtra)
library(reshape2)
library(readxl)
library(cluster)
library(factoextra)
library(stats)
library(ggdendro)
library(lavaan)
library(mclust)
library(Hmisc)
library(janitor)
library(ggcorrplot)
library(skimr)
library(caret)
library(RANN)
library(superheat)
library(ppsr)
library(tidyverse)
library(clustertend)
library(factoextra)
library(clValid)
library(clustertend)
library(pdftools)
library(tabulizer)
library(shiny)
library(caret)
library(shinyAce)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)

min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
 #read files
df_demo <- as.data.set(spss.portable.file("demographics.por"), to.data.frame=T) %>% 
  as.data.frame() %>% 
  drop_na(SEX) %>% 
  select(-ETHNIC, -SEXN) %>%
  mutate(EDUC = gsub("grd 0-8", "grd 9-11", EDUC)) %>% 
  mutate_at(vars(EDUC, EMPLOY,MARITAL), as.character) %>% 
  mutate_at(vars(EDUC, EMPLOY,MARITAL), ~replace_na(.x,"other")) %>%  
  mutate_at(vars(EDUC, EMPLOY,MARITAL), as.factor) 

df_JPIR <- as.data.set(spss.portable.file("JPIR_scales-1.por"), to.data.frame=T) %>% as.data.frame() %>%
  mutate_if(is.numeric, impute) %>% 
     
    mutate(ANALYTICAL = COMPLEX+BREADTH+INNOVAT+TOLERAN, EMOTIONAL = EMPATH+ANXIETY+COOPER, EXTROVERTED = SOCIAL+CONFID+ENERGY, OPPORTUNISTIC = ASTUTE+RISK, DEPENDABLE = ORGAN+TRADIT+RESPON) 

df_NEO <- as.data.set(spss.portable.file("NEO_PIR_scales-1.por"), to.data.frame=T) %>% 
    as.data.frame() %>% 
    dplyr::rename(Anxiety = N1,
                  AngryHostility = N2,
                  Depression = N3,
                  SelfConsciousness = N4,
                  Impulsiveness = N5,
                  Vulnerability = N6,
                  Warmth = E1,
                  Gregariousness = E2,
                  Assertiveness = E3,
                  Activity = E4,
                  ExcitementSeeking = E5,
                  PositiveEmotions = E6,
                  Fantasy = O1,
                  Aesthetics = O2,
                  Feelings = O3,
                  Actions = O4,
                  Ideas = O5,
                  Values = O6,
                  Trust = A1,
                  Straightforwardness = A2,
                  Altruism = A3,
                  Compliance = A4,
                  Modesty = A5,
                  TenderMindedness = A6,
                  Competence = C1,
                  Order = C2,
                  Dutifulness = C3,
                  AchievementStriving = C4,
                  SelfDiscipline = C5,
                  Deliberation = C6,
                  N_neuroticism = N,
                  E_extraversion = E,
                  O_openness = O,
                  A_agreeableness = A,
                  C_conscientiousness = C) 
summary(df_demo)
summary(df_JPIR)

df <- df_JPIR %>% 
  full_join(df_NEO) %>% 
  left_join(df_demo)
 
JPIR_Facets <- c("COMPLEX", "BREADTH", "INNOVAT", "TOLERAN", "EMPATH", "ANXIETY", "COOPER", "SOCIAL", "CONFID", "ENERGY", "ASTUTE", "RISK", "ORGAN", "TRADIT", "RESPON")
JPIR_Dimensions  <- c("ANALYTICAL", "EMOTIONAL", "EXTROVERTED", "OPPORTUNISTIC", "DEPENDABLE")
NEO_Facets <- c("Anxiety", "AngryHostility", "Depression","SelfConsciousness","Impulsiveness","Vulnerability","Warmth","Gregariousness","Assertiveness","Activity","ExcitementSeeking","PositiveEmotions","Fantasy","Aesthetics","Feelings","Actions","Ideas","Values","Trust", "Straightforwardness", "Altruism", "Compliance","Modesty","TenderMindedness", "Competence","Order","Dutifulness","AchievementStriving","SelfDiscipline","Deliberation")
NEO_Dimensions <- c("N_neuroticism", "E_extraversion", "O_openness", "A_agreeableness", "C_conscientiousness")
Dimensions <- c(NEO_Dimensions, JPIR_Dimensions)
Facets <- c(NEO_Facets, JPIR_Facets)
summary(df_demo$EMPLOY)

edlist <- c("Some college", "Post-coll degree","College grad","Some post coll","H.S. grad","Voc or Tech", "grd 9-11")

marlist <- c("married", "sep or divorced","other", "widowed", "single", "living tog")

employlist <- c("full time", "retired", "part time","other", "homemaker","unemployed")

######

#linear modeling
lm_function <- function (target) {
  df %>%
    mutate_at(vars(N_neuroticism:Deliberation), scale) %>%
    mutate_at(vars(N_neuroticism:Deliberation), pnorm) %>%
    mutate_at(vars(N_neuroticism:Deliberation), ~round(.x*100,1)) %>%
    gather(COMPLEX:Deliberation, key=measure, value=score) %>% 
    filter(measure %in% c(target, JPIR_Facets)) %>% 
    pivot_wider(names_from="measure", values_from = "score") %>% 
    dplyr::select(-c(ID, SEX:MARITAL)) %>% 
    drop_na() %>% 
    dplyr::rename(target = 16)
}

output <- NULL;
output_b <- NULL;
for (i in c(NEO_Dimensions, NEO_Facets)) {
  target <- i
  trainIndex <- createDataPartition(lm_function(target)$target , p = 0.8, list=FALSE)
  subTrain <- lm_function(target)[trainIndex,]
  subTest <- lm_function(target)[-trainIndex,]
  Train_pred<-lm_function(target)[trainIndex,]$target
  #typeof(Train_pred)
  Test_pred <-lm_function(target)[-trainIndex,]$target
  control <- trainControl(method="repeatedcv", number=10, repeats=1)
  metric <- "RMSE"
  lm_default <- train(target~., data=subTrain, method="lm", metric=metric, trControl=control )
  model_predictions<-predict(lm_default, subTest)
  N_ave <- mean(Train_pred)
  eval <- cbind(Test_pred, model_predictions) %>%
    as.data.frame() %>% 
    mutate(linear_model = sqrt((model_predictions-Test_pred)^2)) %>% 
    mutate(control = sqrt((Test_pred-N_ave)^2)) 
  temp <- eval %>% gather(linear_model:control, key=metric, value=value) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(RMSE = mean(value)) %>% 
    mutate(feature = target)
  
  df_coeff <- lm_default$finalModel
  df_coeff2 <- df_coeff$coefficients %>% as.data.frame() %>% rename("Coef" = 1) %>% rownames_to_column("JPIR_feature") %>% mutate(cor = if_else(Coef>0, "positive", "negative"))
  
  temp_b <- varImp(lm_default)$importance %>%
    as.data.frame() %>%
    rownames_to_column("JPIR_feature") %>%
    rename("importance"="Overall") %>%
    left_join(df_coeff2) %>% 
    mutate(feature=target)
  
  output <- rbind(output, temp)
  output_b <- rbind(output_b, temp_b)
  saveRDS(lm_default, paste(i,"model.rds", sep = "_"))
}

#lm performance calculations
df_performance <- output  %>% 
  mutate(level = if_else(feature %in% Dimensions, "Dimension", "Facet")) %>%
  pivot_wider(names_from = "metric", values_from ="RMSE") %>% 
  mutate(perc_gain = round((control-linear_model)/control,2)*100) %>% 
  
  gather(control:linear_model, key=metric, value=RMSE) %>%
  mutate(feature = paste(feature,"(",perc_gain,"%)"))

df_importance <- output_b  %>% 
  mutate(level = if_else(feature %in% Dimensions, "Dimension", "Facet"))

df_cor <- round(cor(df %>% 
              drop_na(Anxiety, ANXIETY) %>%
              gather(COMPLEX:Deliberation, key=Measure, value=Value) %>% 
              pivot_wider(names_from = "Measure", values_from="Value") %>% 
              dplyr::select(-(ID:MARITAL)) %>% 
              as.matrix),2) %>%
    as.data.frame() %>% 
  rownames_to_column("Scale") %>% 
  mutate(Scale2 = Scale) %>% 
  column_to_rownames("Scale2") %>% 
  mutate(Level = if_else(Scale %in% c(JPIR_Dimensions, NEO_Dimensions), "Dimension", "Facet")) %>% 
  mutate(Inventory = if_else(Scale %in% c(JPIR_Dimensions, JPIR_Facets), "JPIR", "NEO")) %>% 
  select(Scale, Level, Inventory, everything() )


###################################O
ui <- fluidPage(

    # Application title
    titlePanel("NEO_JPIR Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
                     h6("Data downloaded from ES Community Sample."),
            h6("Number of records:"),
            verbatimTextOutput("count"),  
            radioButtons("level","Level of Analysis:", choices=c("Dimension level"="Dimension", "Facet level"="Facet"), selected= "Dimension"),
            
            sliderInput("age","Subject Age Range",min = 0, max = 100,
                        value = c(18,85)),
            checkboxGroupInput("gender","Gender", choices=c("Male"="M", "Female"="F"), selected=c("M", "F")),
            checkboxGroupInput("employment","Employment", choices=c(employlist), selected=c(employlist)),
            checkboxGroupInput("education","Education", choices = c(edlist), selected = c(edlist)),
            checkboxGroupInput("marital","Marital status", choices = c(marlist), selected = c(marlist))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            
            tabPanel(title = "Trait Distributions",
                     plotOutput("plot2", height = "600px"),
                     h4("Note: JPIR in UPPERCASE. NEO in lowercase. Raw scores scaled to total sample of participants.")),
            tabPanel(title = "Trait Correlations",
                     plotOutput("plot1", height = "600px"),
                     h4("Note: JPIR in UPPERCASE. NEO in lowercase. Dendrogram based on hierarchical clustering of scale intercorrelations.")),
            
            tabPanel(title = "JPIR-NEO Information Value",
                     plotOutput("plot3", height = "600px"),
                     h5("Note: Linear modeling using the caret package in R predicts participant NEO scores based on data from 15 JPIR facets.  20% of the overall participant sample (N~640) was used for testing the accuracy of the linear models.  Information gain percentages are based on prediction improvement over summary statistics, specifically a comparison of model residuals to residuals determined from a comparison of one's score to the sample mean score.")),
            tabPanel(title = "JPIR Feature Importance",
                    
                         selectInput("dimension", "NEO Dimension or Facet", choices = c(NEO_Dimensions, NEO_Facets)),
                                                
                       plotOutput("plot4", height = "600px")
                     ),
            tabPanel(title = "NEO Calculator", icon = icon("dashboard"),
                     h5("Enter JPIR results here (averages are pre-loaded):"),
                       box(status="primary",  solidHeader = TRUE, width = 4,height = "400px",
                         numericInput("complex", "COMPLEX", min=0, max=20, value=median(df$COMPLEX, na.rm = TRUE)),
                         numericInput("breadth", "BREADTH", min=0, max=20, value=median(df$BREADTH, na.rm = TRUE)),
                         numericInput("innovat", "INNOVAT", min=0, max=20, value=median(df$INNOVAT, na.rm = TRUE)),
                         numericInput("toleran", "TOLERAN", min=0, max=20, value=median(df$TOLERAN, na.rm = TRUE)),
                         numericInput("empath", "EMPATH", min=0, max=20, value=median(df$EMPATH, na.rm = TRUE))
                       ),
                       box(status="primary",  solidHeader = TRUE, width = 4, height = "400px",
                           numericInput("anxiety", "ANXIETY", min=0, max=20, value=median(df$ANXIETY, na.rm = TRUE)),
                         numericInput("cooper", "COOPER", min=0, max=20, value=median(df$COOPER, na.rm = TRUE)),
                           numericInput("social", "SOCIAL", min=0, max=20, value=median(df$SOCIAL, na.rm = TRUE)),
                           numericInput("confid", "CONFID", min=0, max=20, value=median(df$CONFID, na.rm = TRUE)),
                           numericInput("energy", "ENERGY", min=0, max=20, value=median(df$ENERGY, na.rm=TRUE))
                           ),
                         box(status="primary",  solidHeader = TRUE, width = 4,height = "400px",
                             numericInput("astute", "ASTUTE", min=0, max=20, value=median(df$ASTUTE, na.rm = TRUE)),
                             numericInput("risk", "RISK", min=0, max=20, value=median(df$RISK, na.rm = TRUE)),
                       numericInput("organ", "ORGAN", min=0, max=20, value=median(df$ORGAN, na.rm = TRUE)),
                       numericInput("tradit", "TRADIT", min=0, max=20, value=median(df$TRADIT, na.rm = TRUE)),
                       numericInput("respon", "RESPON", min=0, max=20, value=median(df$RESPON, na.rm = TRUE))
                       ),
                     box(title="NEO Prediction",status="primary",  solidHeader = TRUE, width = 6,height = "500px",
                         
                     #verbatimTextOutput("test"),
                     verbatimTextOutput("test2"),
                     h6("Note: Predictions based on results of linear models developed from Eugene-Springfield Community Sample Data (N=638). https://dataverse.harvard.edu/dataverse/ESCS-Data")
                     ),
                     box(status="primary",  solidHeader = TRUE, width = 6,height = "500px",
                         
                         #verbatimTextOutput("test"),
                         plotOutput("plot5")
                     )
                     )
            
            )
          )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe(showNotification("Created by Peraton for Army Analysis", duration = 15))
    
    dfb <- reactive ({
        df %>%
        mutate_at(vars(COMPLEX:Deliberation), min_max_norm) %>% 
                         filter(SEX %in% input$gender) %>% 
                         filter(AGE >= input$age[1] & AGE <= input$age[2]) %>% 
                         filter(EMPLOY %in% c(input$employment)) %>% 
                         filter(EDUC %in% c(input$education)) %>% 
                         filter(MARITAL %in% c(input$marital)) 
    })
    
    output$count <- renderText({
      paste("NEO:",nrow(dfb() %>% drop_na(Anxiety)),
            "JPIR:",nrow(dfb() %>% drop_na(ANXIETY)),
            "Both:", nrow(dfb() %>% drop_na(Anxiety,ANXIETY)))
    })

 df_predict <- reactive({
   cbind(input$complex, input$breadth, input$innovat, input$toleran, input$empath, input$anxiety, input$cooper, input$social, input$confid, input$energy, input$astute, input$risk, input$organ, input$tradit, input$respon) %>% 
     as.data.frame() %>% 
     rename("COMPLEX"=1, "BREADTH"=2, "INNOVAT"=3, "TOLERAN"=4, "EMPATH"=5, "ANXIETY"=6, "COOPER"=7, "SOCIAL"=8, "CONFID"=9, "ENERGY"=10, "ASTUTE"=11, "RISK"=12, "ORGAN"=13, "TRADIT"=14, "RESPON"=15)
   })
 
 NEO_predict <- reactive({
   cbind(
   predict(readRDS("N_neuroticism_model.rds"),df_predict()),
   predict(readRDS("E_extraversion_model.rds"),df_predict()),
   predict(readRDS("O_openness_model.rds"),df_predict()),
   predict(readRDS("A_agreeableness_model.rds"),df_predict()),
   predict(readRDS("C_conscientiousness_model.rds"),df_predict())
   )%>% 
     as.data.frame() %>% 
     rename("Neuroticism"=1, "Extraversion"=2, "Openness"=3, "Agreeableness"=4, "Consientiousness"=5) %>% 
     mutate_if(is.numeric, ~round(.x,1)) %>% 
     gather(everything(), key="NEO_trait", value="Percentile") %>% arrange(-Percentile)
   
 })
 
 output$test <- renderPrint({df_predict()})
 output$test2 <- renderPrint({NEO_predict()})
 
    
    output$plot1 <- renderPlot({
      hclust(
        dist(
        cor(dfb() %>%
              drop_na(Anxiety, ANXIETY) %>%
              gather(COMPLEX:Deliberation, key=Measure, value=Value) %>%           mutate(level = if_else(Measure %in% c(Dimensions), "Dimension", "Facet")) %>% 
              filter(level == input$level) %>%
              dplyr::select(ID, Measure, Value) %>% 
              pivot_wider(names_from = "Measure", values_from = "Value") %>% 
              dplyr::select(-c(ID)) %>%
              as.matrix %>%
              as.data.frame() ))) %>%
         ggdendrogram(rotate=TRUE, theme_dendro = TRUE, size=1) +
         ggtitle(paste("Hierarchical Clustering by:", input$level))
    })
                  
  output$plot2 <- renderPlot({
      dfb() %>% 
      gather(COMPLEX:Deliberation, key=Measure, value=Value) %>%
      drop_na(Value) %>% 
      left_join(
        dfb() %>% 
          gather(COMPLEX:Deliberation, key=Measure, value=Value) %>%
          drop_na(Value) %>% 
          mutate(level = if_else(Measure %in% c(Dimensions), "Dimension", "Facet")) %>% 
          mutate(inventory = if_else(Measure %in% c(JPIR_Dimensions, JPIR_Facets), "JPIR", "NEO")) %>%
          dplyr::group_by(inventory, level, Measure) %>% 
          dplyr::summarise(comparison = median(Value, na.rm=TRUE)) %>% 
          mutate(distribution = if_else(comparison<=.3,
                                        "low",
                                        if_else(comparison<=.7, "normal", "high"))) %>% 
          arrange(inventory, level, -comparison) %>% 
          rownames_to_column("rank") %>% 
          mutate(rank = as.numeric(rank))
        ) %>%

      filter(level == input$level) %>% 
      dplyr::ungroup() %>%
      ggplot() +
      geom_boxplot(aes(x=reorder(Measure, rank, FUN=mean), y=Value, fill=distribution))+
      geom_hline(yintercept = c(.3,.7), linetype = "dashed", color=c("black", "black"))+
      # theme(axis.text.x = element_text(color = if_else(distribution=="high", "red", "black"))) +
      scale_fill_manual(values=c("normal"="gray", "high"="tomato", "low"="skyblue")) +
        xlab("") +
        ylim(0,1)+
      ylab("Scaled score (min-max scaling)") +
      coord_flip() +
      labs(title = paste("Score Distribution by:", input$level),
           caption = "Note: scores scaled (min-max scaling) by feature and based on total distribution of participants")
    })

  output$plot3 <- renderPlot({
    df_performance %>% 
      filter(level == input$level) %>%
    ggplot(aes(x=reorder(feature, perc_gain, fun=mean), y=RMSE, group=feature, color=metric)) + 
      geom_point(size = 3) + 
      geom_line(color="darkgray") +
      coord_flip() + xlab("") +
      ggtitle("Information gain from Linear Modeling (Control: sample mean)") +
      labs(caption = "RMSE metric is based on prediction of one's NEO percentile (pnorm) score from JPIR results")
      })
  
 output$plot4 <- renderPlot ({
   df_importance %>% filter(feature==input$dimension) %>%
     mutate(importance = round(importance,0)) %>% 
     mutate(rank = if_else(importance>=25, "important", "not important")) %>% 
     ggplot(aes(x=JPIR_feature, y=importance)) + 
     geom_col(aes(fill=rank)) + 
     coord_flip() + 
     theme(legend.position = "blank") +
     xlab("") +
     geom_text(aes(label=round(importance,1), color=cor), nudge_y = -1) +
     scale_fill_manual(values = c("skyblue", "gray"))+
     scale_color_manual(values=c("negative"="red", "positive"="black")) +
     ylab(paste("importance for predicting:", input$dimension)) +
     labs(caption="Note: important features highlighted in SKY BLUE; RED text indicates negative correlation between JPIR feature and NEO feature")
 })
 
 output$plot5 <- renderPlot({
   NEO_predict() %>% ggplot()+geom_col(aes(x=reorder(NEO_trait, Percentile, fun=mean), y=Percentile), fill="skyblue") + 
     coord_flip() + 
     ylab("Percentile score") + 
     xlab("") +
     ggtitle("NEO Prediction") + 
     ylim(0,100) + 
     geom_hline(yintercept = 50, linetype = "dashed", color="black")
 })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
