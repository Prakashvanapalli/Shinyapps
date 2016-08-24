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
      df = filter(df,loan_status == c("Fully Paid","Charged Off"))
      df = select(df,-c(7,8,9))
      #En-coding categorical variables 
      df$loan_status = ifelse(df$loan_status == "Fully Paid",1,0)
      df$term = ifelse(df$term == 36,1,0)
      
      # remove all the difficult variables 
      df <- select(df,-c(6,8,10,12:14,17))
      df$loan_status <- as.factor(df$loan_status)
      
      ##### Model Building 
      
      set.seed(1)
      train= sample(1:nrow(df),140000)
      test = - train
      
      training_data = df[train,]
      testing_data = df[test,]
      if (input$var == "Logistic Regression"){
        #Logistic Regression
        model1 = glm(loan_status~.-member_id, data = training_data, family ="binomial")
        
        predicted_y = predict(model1, testing_data, type = "response")
      
        
        ### create the categorical variable using the predicted probabilties 
        predicted_y_cat = rep("0",32390)
        predicted_y_cat[predicted_y >0.5] = "1"
        paste("Accuarcy:",(table(testing_data$loan_status,predicted_y_cat)[4]+table(testing_data$loan_status,predicted_y_cat)[1])/nrow(testing_data),sep = " ")
        
        
      }else if(input$var == "GBM"){
        print("you have selected somethingelse")
      }else{
        print ("This should be something else")
      }
    })
    
  }
)

