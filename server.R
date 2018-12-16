setwd("C:/Users/Newton/Desktop/jh/wine")


library(shiny)
library(shinydashboard)
library(GGally)
library(caret)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(stringi)
library(tidyr)
white <- read.csv("winequality-white.csv", sep = ";")
whitecopy <- white
library(Hmisc)
# fixed acid
cut <- function(x){
  sapply(x, function(x) if(x <= 5)"Bad" else if(x > 5 & x <= 7)"Average"
         else "Good")
}
category <- lapply(white[12], cut) %>% as.data.frame()
colnames(category) <- "category"
whitec <- cbind(white, category)
server <- function(input, output,session) {
    
    output$qh <- renderPlot({
    req(input$qualitySlider)
    if(input$sidebarmenu == "exdata"){
      
      whitecopy %>%
        filter(quality >= input$qualitySlider[1] & quality <= input$qualitySlider) %>%
        group_by(quality)%>%
        ggplot(aes(quality, fill=factor(quality)))+
        geom_histogram(bins = 9)
    }
    })
    
    output$varBP <- renderPlot({
      df <- data.frame(key = c(colnames(white)[1:11]),
                       var = c("fa","va","ca", "rs","cl","fsd", "tsd", "ds", "ph", "sh", 
                               "al"))
      if(input$sidebarmenu == 'exdata' & !is.null(input$variable)){
          whitecopy %>% 
            gather() %>%
            left_join(., df) %>%
             filter(var %in% input$variable) %>%
            ggplot(aes(x = key, y = value, group=key))+
             geom_boxplot(aes(fill=key))+geom_jitter(aes(color = key),alpha=.2)
      }
      
      else if(input$sidebarmenu == 'exdata' & is.null(input$variable)) {
        whitecopy %>% 
          gather() %>%
          left_join(., df) %>%
          filter(var %in% c("al","ds")) %>%
          ggplot(aes(x = key, y = value, group=key))+
          geom_boxplot(aes(fill=key))+geom_jitter(aes(color = key),alpha=.2)
      }    })
  
  output$title <- renderUI({
    switch (input$sidebarmenu,
      exdata = h1("Exploratory Data"),
      glm = h1("Prediction with GLM")
    )
  })
  
  output$qdb <- renderUI({
    if(input$sidebarmenu == 'exdata' & !is.null(input$variable))h3("Wine Quality Distribution")
    })
  
  output$vardb <- renderUI({
    if(input$sidebarmenu == 'exdata')h3("Variable Distribution exclude Quality")
      })
  
  output$st1 <- renderUI({
    if(input$sidebarmenu == 'exdata') {
      h5("Intuitively, accidity(fixed, volatile, and citric) which related to ph will affect the quality of
       the wine, while alcohol and sugar may have contribution to density which also affect the quality of the wine.")
      
    }
 })
  output$corq <- renderUI({
    if(input$sidebarmenu == "exdata")h3("Correlation between Quality and Others")
  })
  load("C:/Users/Newton/Desktop/jh/wine/winee/help.RData")
  output$crp <- renderPlot({
    
    if(input$sidebarmenu == 'exdata') {
          white$quality <- as.factor(white$quality)
              if(input$gr == "ac")ac
              else if(input$gr == "ds")ds
              else if(input$gr == "ph")ph
              else mix
    }
  })
  
  output$st2 <- renderUI({
    if(input$sidebarmenu == 'exdata') {
      h3("Correlation between two variable, group by wine quality")
      
    }
  })
  
  
  output$alDs <- renderPlot({
    if(input$sidebarmenu == 'exdata'){
      
      if(length(input$variable) < 3){
        if(!is.null(input$variable)){
          df <- data.frame(nam = c(colnames(white)[1:12]),
                           var = c("fa","va","ca", "rs","cl","fsd", "tsd", "ds", "ph", "sh", 
                                   "al","ql"))
          
          tmp <- df %>% filter(var %in% input$variable)
          i1 <- as.character(tmp$nam[1]);i2 <- as.character(tmp$nam[2])
          d <- white %>% select(i1, i2, quality)
          ggplot(d, aes_string(names(d)[1], names(d)[2], colour="quality"))+
            geom_point(alpha=.1)+geom_smooth(method="lm",colour="#FFFACD")+theme_classic2()
          
        }
        
        else {
          ggplot(white, aes(alcohol,density, colour=quality))+
            geom_point(alpha=.1)+geom_smooth(method="lm",colour="#FFFACD")+theme_classic2()
        }  
        
      }
       
    }
    
  })
  #-------........................---------------------------glm 
  output$glm1 <- renderUI({
    if(input$sidebarmenu == "glm")h4("Note:\n GLM model: the quality group into 2 category: Bad(3-6), Good(7-9), \n 
                                     NB model: the quality group into 2 category: Bad(3-4), Average(5-6), Good(7-9)")
  })
  
  output$glm2 <- renderUI({
    if(input$sidebarmenu == "glm" & !is.null(input$glmvar))h3("Predict the quality of wine by specifying the value for each variable")
  })
  
   AllInputs <- reactive({
    v <- names(white)[1:11]%>% as.data.frame()
    colnames(v) <- "Variable"; v <- v %>% mutate(value = 0.0)
    ls <- data.frame(Variable = names(white)[1:11], value =rbind(input$fa, input$va, input$ca, input$rs, input$cl, input$fsd, input$tsd, input$ds,
                              input$ph, input$sh, input$al)) 
    merge <- left_join(v,ls, by="Variable")
    merge %>% select(Variable,value.y) 
    merge
  })
   load("C:/Users/Newton/Desktop/jh/wine/winee/model.RData")
  output$showinput <- renderDataTable({
   AllInputs()
  })
  
  observeEvent(input$predict, {
    df <- rbind(rep(0,12)) %>% as.data.frame()
    colnames(df) <- names(traindat1)
    df$category <- as.factor(df$category)
    df_ <- 
      df %>% mutate(fixed.acidity = ifelse(input$fa,input$fa, 0)) %>%
      mutate(volatile.acidity = ifelse(input$va,input$va, 0)) %>%
      mutate(citric.acid = ifelse(input$ca,input$ca, 0))%>%
      mutate(residual.sugar = ifelse(input$rs,input$rs, 0))%>%
      mutate(chlorides = ifelse(input$cl,input$cl, 0))%>%
      mutate(free.sulfur.dioxide = ifelse(input$fsd,input$fsd, 0))%>%
      mutate(total.sulfur.dioxide  = ifelse(input$tsd,input$tsd, 0))%>%
      mutate(density = ifelse(input$ds,input$ds, 0))%>%
      mutate(pH = ifelse(input$ph,input$ph, 0))%>%
      mutate(sulphates= ifelse(input$sh,input$sh, 0))%>%
      mutate(alcohol = ifelse(input$al,input$al, 0))
    df_$category <- as.factor(df_$category)
    
  output$pred <- renderUI({
    if(input$mod == "dm"){
      pr <- predict(trglm, newdata =df);pr
    }
    
    else{ pr <- predict(tr, newdata =df);pr}
    
  })
  })#end observe event
  output$rawdat <- renderDataTable({
    if(input$whichdf == "glmmod"){
      summary(traindat1)
      traindat1
    }
    
    else{
      summary(traindat)
      traindat
    }
  })
  
}

