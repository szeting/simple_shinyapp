

library(shiny)

ui<-fluidPage(pageWithSidebar(
    headerPanel('Simple Calculator'),
    sidebarPanel(numericInput('v1','Please enter the first value:',value=0),
                 numericInput('v2','Please enter the second value:',value=0),
                 checkboxGroupInput('calculation','Please select the calculation you want to perform:',
                                    c('Addition','Substraction','Multiplication','Division'))),
    mainPanel(h5('Your input values are:'),
              verbatimTextOutput('value1'),
              verbatimTextOutput('value2'),
              verbatimTextOutput('a1'))
))

server<-function(input,output){
    output$value1 <- renderPrint(as.numeric(input$v1))
    output$value2 <- renderPrint(as.numeric(input$v2))
    output$a1<-renderPrint(ProcessOptions({input$calculation}))
    ProcessOptions<-function(x){
        input<-'Please select the calculation you want to perform:'
        if(length(x)==0){
            return(cat(paste(input,'You have choose nothing!',sep='\n')))
        }
        else if(length(x)>0){
            for (i in 1:length(x)){
                input<-paste(input,showOption(x[i]),sep="\n")
            }
            return(cat(input))
        }
    }
    showOption<-function(i){
        if(i=='Addition'){
            add_ans=input$v1+input$v2
            return(paste('Your addition answer is: ',add_ans))
        }
        else if(i=='Substraction'){
            sub_ans=input$v1-input$v2
            return(paste('Your substraction answer is: ',sub_ans))
        }
        else if(i=='Multiplication'){
            mul_ans=input$v1*input$v2
            return(paste('Your multiplication answer is: ',mul_ans))
        }
        else if(i=='Division'){
            if(input$v2==0){
                return('Please enter a valid denominator')
            }
            else{
                div_ans=input$v1/input$v2
                return(paste('Your division answer is: ',div_ans))
            }
        }
    }
}

shinyApp(ui,server)
