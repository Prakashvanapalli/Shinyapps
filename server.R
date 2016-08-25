# server.R
options(shiny.maxRequestSize=300*1024^2)# Required to upload large files


shinyServer(
  function(input, output,session) {
    
    observe({
      
      if (input$browse == 0) return()
      
      updateTextInput(session, "file",  value = file.choose())
    })
    
    data <- reactive({
      
      if(input$upload == 0) return()
      
      isolate({
        fread(input$file)
      })
    })
    
   
    
    output$summary <- renderTable({
      df <- data()
      if (is.null(df)) return(NULL)
      summary(df)
      
    })
    
    
    
    output$nrows <- renderText({
      df <- data()
      if (is.null(df)) return(NULL)
      df = filter(df, sub_grade == input$sub_group)
      df = filter(df,loan_status == c("Fully Paid","Charged Off"))
      
      # Removing Insignificant Variables 
      df = select(df,-c(2,5:9,11,13,15:18,20,22,24,26:29))
      
      #En-coding categorical variables 
      df$loan_status = ifelse(df$loan_status == "Fully Paid",0,1) # Class Variable
      df$term = ifelse(df$term == 36,1,0)
      
      
      df$emp_length = ifelse(df$emp_length == 0,1,0) # Experienced vs In-experienced
      df$delinq_2yrs = ifelse(df$delinq_2yrs <=2,1,0)
      df$inq_last_6mths <- ifelse(df$inq_last_6mths ==0,1,0)
      df$pub_rec <- ifelse(df$pub_rec ==0,1,0)
      df$tax_liens = ifelse(df$tax_liens ==0,1,0)
      
      #divide the data into train and test data 
      set.seed(1)
      n = sample(1:nrow(df),round(0.8*(nrow(df))))
      
      X_train_1 = df[n,]
      X_test_1 = df[-n,]
      X_train_rose <- ROSE(loan_status~.,data=X_train_1,seed=1)$data # Use rose package
      if (input$var == "Logistic Regression"){
        #Logistic Regression
        logit.rose = glm(loan_status~emp_length+annual_inc+delinq_2yrs+fico_range_low+inq_last_6mths,data=X_train_rose,family = "binomial")
        pred.logit.rose = predict(logit.rose,newdata=X_test_1)
        
        #roc 
        roc1 = roc.curve(X_test_1$loan_status,pred.logit.rose) #0.611
        paste("Area under the curve:",roc1$auc)
        
        
      }else if(input$var == "GBM"){
        GBM_ITERATIONS = 3000
        GBM_LEARNING_RATE = 0.01
        GBM_MINOBS = 10
        gbm.rose <- gbm(loan_status ~ .-member_id,
                        distribution = "bernoulli",
                        data = X_train_rose,
                        n.trees = GBM_ITERATIONS,
                        interaction.depth = 2,
                        n.minobsinnode = GBM_MINOBS,
                        shrinkage = GBM_LEARNING_RATE,
                        bag.fraction = 0.6,
                        train.fraction = 1.0,
                        cv.folds=5,
                        keep.data = FALSE,
                        verbose = FALSE,
                        class.stratify.cv=TRUE,
                        n.cores = 2)
        
        pred.gbm.rose = predict(gbm.rose,newdata=X_test_1)
        
        #roc 
        roc1 =roc(X_test_1$loan_status,pred.gbm.rose)
        paste("Area under the curve:",roc1$auc)
        
      }else if(input$var == "Decision Trees"){
        tree.rose = rpart(loan_status~.-member_id,data=X_train_rose,control = rpart.control(cp = 0.005))
        pred.tree.rose = predict(tree.rose,newdata=X_test_1)
        roc1 = roc(X_test_1$loan_status,pred.tree.rose)
        paste("Area under the curve:",roc1$auc)
        
      }else if(input$var == "Random Forest"){
        X_train_rose_randomforest = X_train_rose
        X_train_rose_randomforest$loan_status = as.factor(X_train_rose_randomforest$loan_status)
        randomforest.rose <- randomForest(loan_status ~.-member_id,ntree=3000,data = X_train_rose_randomforest,replace = T)
        pred.randomforest.rose = predict(randomforest.rose,newdata=X_test_1)
        #roc 
        roc1 = roc.curve(X_test_1$loan_status,pred.randomforest.rose)
        paste("Area under the curve:",roc1$auc)
        
      }
    })
    
  }
)
