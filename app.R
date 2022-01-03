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

###

  df %>% 
    gather(COMPLEX:Deliberation, key=Measure, value=Value) %>%
    drop_na(Value) %>% 
    mutate(level = if_else(Measure %in% c(Dimensions), "Dimension", "Facet")) %>% 
  left_join(
    df %>% 
      gather(COMPLEX:Deliberation, key=Measure, value=Value) %>%
      drop_na(Value) %>% 
      dplyr::group_by(Measure) %>% 
      dplyr::summarise(comparison = median(Value)) %>% 
      mutate(distribution = if_else(comparison<=(-.5), "low", 
                                    if_else(comparison<=.5, "normal", "high")))
  ) %>%
    mutate(inventory = if_else(Measure %in% c(NEO_Dimensions, NEO_Facets), "NEO", "JPIR")) %>% 
    filter(level == "Dimension") %>% 
    mutate(Measure = as.factor(Measure)) %>%
    dplyr::ungroup() %>% 
    ggplot() + 
    geom_boxplot(aes(x=reorder(Measure, Value, FUN=median), y=Value, fill=distribution))+
    geom_hline(yintercept = c(-.5,0,.5), linetype = "dashed", color=c("red", "black", "red"))+
    coord_flip() +
    
    scale_fill_manual(values=c("normal"="gray", "high"="tomato", "low"="skyblue")) +
    xlab("") +
    ylim(-3,3)+
    ylab("Scaled score (z score)") +facet_grid(.~inventory)
    
  ##



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
 
summary(df)

JPIR_Facets <- c("COMPLEX", "BREADTH", "INNOVAT", "TOLERAN", "EMPATH", "ANXIETY", "COOPER", "SOCIAL", "CONFID", "ENERGY", "ASTUTE", "RISK", "ORGAN", "TRADIT", "RESPON")
JPIR_Dimensions  <- c("ANALYTICAL", "EMOTIONAL", "EXTROVERTED", "OPPORTUNISTIC", "DEPENDABLE")
NEO_Facets <- c("Assertiveness","Activity","ExcitementSeeking","PositiveEmotions","Fantasy","Aesthetics","Feelings","Actions","Ideas","Values","Trust", "Straightforwardness", "Altruism", "Compliance","Modesty","TenderMindedness", "Competence","Order","Dutifulness","AchievementStriving","SelfDiscipline","Deliberation")
NEO_Dimensions <- c("N_neuroticism", "E_extraversion", "O_openness", "A_agreeableness", "C_conscientiousness")
Dimensions <- c(NEO_Dimensions, JPIR_Dimensions)
Facets <- c(NEO_Facets, JPIR_Facets)
summary(df_demo$EMPLOY)

edlist <- c("Some college", "Post-coll degree","College grad","Some post coll","H.S. grad","Voc or Tech", "grd 9-11")

marlist <- c("married", "sep or divorced","other", "widowed", "single", "living tog")

employlist <- c("full time", "retired", "part time","other", "homemaker","unemployed")

######
#linear modeling
rf_function <- function (target) {
  df %>% gather(COMPLEX:Deliberation, key=measure, value=score) %>% 
    filter(measure %in% c(target, JPIR_Facets)) %>% 
    pivot_wider(names_from="measure", values_from = "score") %>% 
    dplyr::select(-c(ID, SEX:MARITAL)) %>% 
    drop_na() %>% 
    dplyr::rename(target = 16)
}

output <- NULL;
output_b <- NULL;
for (i in NEO_Dimensions) {
  target <- i
  trainIndex <- createDataPartition(rf_function(target)$target , p = 0.8, list=FALSE)
  subTrain <- rf_function(target)[trainIndex,]
  subTest <- rf_function(target)[-trainIndex,]
  Train_pred<-rf_function(target)[trainIndex,]$target
  #typeof(Train_pred)
  Test_pred <-rf_function(target)[-trainIndex,]$target
  control <- trainControl(method="repeatedcv", number=10, repeats=1)
  metric <- "RMSE"
  rf_default <- train(target~., data=subTrain, method="lm", metric=metric, trControl=control )
  model_predictions<-predict(rf_default, subTest)
  N_ave <- mean(Train_pred)
  eval <- cbind(Test_pred, model_predictions) %>%
    as.data.frame() %>% 
    mutate(rf_model = sqrt((model_predictions-Test_pred)^2)) %>% 
    mutate(control = sqrt((Test_pred-N_ave)^2)) 
  temp <- eval %>% gather(rf_model:control, key=metric, value=value) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(RMSE = mean(value)) %>% 
    mutate(feature = target)
  
  df_coeff <- rf_default$finalModel
  df_coeff2 <- df_coeff$coefficients %>% as.data.frame() %>% rename("Coef" = 1) %>% rownames_to_column("JPIR_feature") %>% mutate(cor = if_else(Coef>0, "positive", "negative"))
  
  temp_b <- varImp(rf_default)$importance %>%
    as.data.frame() %>%
    rownames_to_column("JPIR_feature") %>%
    rename("importance"="Overall") %>%
    left_join(df_coeff2) %>% 
    mutate(feature=target)
  
  output <- rbind(output, temp)
  output_b <- rbind(output_b, temp_b)
}


output2 <- NULL;
output_2b <- NULL;
for (i in NEO_Facets) {
  target <- i
  rf_function(target)
  trainIndex <- createDataPartition(rf_function(target)$target , p = 0.8, list=FALSE)
  subTrain <- rf_function(target)[trainIndex,]
  subTest <- rf_function(target)[-trainIndex,]
  Train_pred<-rf_function(target)[trainIndex,]$target
  #typeof(Train_pred)
  Test_pred <-rf_function(target)[-trainIndex,]$target
  control <- trainControl(method="repeatedcv", number=10, repeats=1)
  metric <- "RMSE"
  rf_default <- train(target~., data=subTrain, method="lm", metric=metric, trControl=control )
  model_predictions<-predict(rf_default, subTest)
  N_ave <- mean(Train_pred)
  eval <- cbind(Test_pred, model_predictions) %>%
    as.data.frame() %>% 
    mutate(rf_model = sqrt((model_predictions-Test_pred)^2)) %>% 
    mutate(control = sqrt((Test_pred-N_ave)^2)) 
  temp <- eval %>% gather(rf_model:control, key=metric, value=value) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(RMSE = mean(value)) %>% 
    mutate(feature = target)
  
  df_coeff <- rf_default$finalModel
  df_coeff2 <- df_coeff$coefficients %>% as.data.frame() %>% rename("Coef" = 1) %>% rownames_to_column("JPIR_feature") %>% mutate(cor = if_else(Coef>0, "positive", "negative"))
  
  temp_2b <- varImp(rf_default)$importance %>%
    as.data.frame() %>%
    rownames_to_column("JPIR_feature") %>%
    rename("importance"="Overall") %>%
    left_join(df_coeff2) %>% 
    mutate(feature=target)
  
 output2 <- rbind(output2, temp)
  output_2b <- rbind(output_2b, temp_2b)
}

#lm performance calculations
df_performance <- output  %>% 
  mutate(level = "Dimension") %>%
  bind_rows(
    output2  %>% 
      mutate(level = "Facet")
  ) %>%
  pivot_wider(names_from = "metric", values_from ="RMSE") %>% 
  mutate(perc_gain = round((control-rf_model)/control,2)*100) %>% 
  
  gather(control:rf_model, key=metric, value=RMSE) %>%
  mutate(feature = paste(feature,"(",perc_gain,"%)"))

#lm feature importance
df_importance <- output_b  %>% 
  mutate(level = "Dimension") %>%
  bind_rows(
    output_2b  %>% 
      mutate(level = "Facet") 
  )



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
                     plotOutput("plot2", height = "500px"),
                     h4("Note: JPIR in UPPERCASE. NEO in lowercase. Raw scores scaled to total sample of participants.")),
            tabPanel(title = "Trait Correlations",
                     plotOutput("plot1", height = "500px"),
                     h4("Note: JPIR in UPPERCASE. NEO in lowercase. Dendrogram based on hierarchical clustering of scale intercorrelations.")),
            
            tabPanel(title = "JPIR-NEO Information Value",
                     plotOutput("plot3", height = "500px"),
                     h5("Note: Linear modeling using the caret package in R predicts participant NEO scores based on data from 15 JPIR facets.  20% of the overall participant sample (N~700) was used for testing the accuracy of the linear models.  Information gain percentages are based on prediction improvement over summary statistics, specifically a comparison of model residuals to residuals determined from a comparison of one's score to the sample mean score.")),
            tabPanel(title = "JPIR Feature Importance",
                    
                         selectInput("dimension", "NEO Dimension or Facet", choices = c(NEO_Dimensions, NEO_Facets)),
                                                
                       plotOutput("plot4")
                     )
            )
          )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dfb <- reactive ({
        df %>%
        mutate_at(vars(COMPLEX:Deliberation), scale) %>% 
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
    
    output$countNEO <- renderText({
      nrow(dfb() %>% drop_na(Anxiety))
    })
    output$countBoth <- renderPrint({
      nrow(dfb() %>% drop_na(Anxiety, ANXIETY))
    })
    
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
      mutate(level = if_else(Measure %in% c(Dimensions), "Dimension", "Facet")) %>% 
    mutate(inventory = if_else(Measure %in% c(JPIR_Dimensions, JPIR_Facets), "JPIR", "NEO")) %>% 
      left_join(
        dfb() %>% 
          gather(COMPLEX:Deliberation, key=Measure, value=Value) %>%
          drop_na(Value) %>% 
          dplyr::group_by(Measure) %>% 
          dplyr::summarise(comparison = median(Value)) %>% 
          mutate(distribution = if_else(comparison<=(-.5), "low", 
                                        if_else(comparison<=.5, "normal", "high")))
        ) %>%

      filter(level == input$level) %>% 
      mutate(Measure = as.factor(Measure)) %>%
      dplyr::ungroup() %>% 
      ggplot() + 
        geom_boxplot(aes(x=reorder(Measure, Value, FUN=median), y=Value, fill=distribution))+
        geom_hline(yintercept = c(-.5,0,.5), linetype = "dashed", color=c("red", "black", "red"))+
        coord_flip() +
        theme(axis.text = element_text(color = if_else(input$level == "Dimension", "blue", "darkgreen"))) +
      scale_fill_manual(values=c("normal"="gray", "high"="tomato", "low"="skyblue")) +
      # scale_color_manual(values=c("NEO"="gray", "JPIR"="blue")) +
        xlab("") +
        ylim(-3,3)+
      ylab("Scaled score (z score)") +
      labs(title = paste("Score Distribution by:", input$level),
                           caption = "Note: scores normalized by feature and based on total distribution of participants")
    })

  output$plot3 <- renderPlot({
    df_performance %>% 
      filter(level == input$level) %>%
    ggplot(aes(x=reorder(feature, perc_gain, fun=mean), y=RMSE, group=feature, color=metric)) + 
      geom_point(size = 3) + 
      geom_line(color="darkgray") +
      coord_flip() + xlab("") +
      ggtitle("Information gain from Linear Modeling (Control: sample mean)") +
      labs(caption = "RMSE metric is based on prediction of one's NEO score from JPIR results")
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
