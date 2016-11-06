library(shiny)
library(ggplot2)

LR2 = function(age, ascites, blood_flow, solid_component, 
               internal_wall, acoustic_shadow)
{    
    z = -5.3718 + 
         0.0354 * as.numeric(age) +
         1.6159 * as.numeric(ascites) +
         1.1768 * as.numeric(blood_flow) +
         0.0697 * min(as.numeric(solid_component), 50) +
         0.9586 * as.numeric(internal_wall) -
         2.9486 * as.numeric(acoustic_shadow)
    
    return(c(z, 1/(1+exp(-z))))
}

LR2.cutoff = 0.1

LR2.yrange = seq(-8,8,0.1)

shinyServer(function(input, output)
{
    lr2 = reactive({
            LR2(input$age,
                input$ascites,
                input$blood_flow,
                input$solid_component,
                input$internal_wall,
                input$acoustic_shadow)
        })
    
    output$raw_result.text = renderText({
        if (input$calcButton)
            isolate("Raw predictor value (the lower, the better):")
    })
    output$raw_result = renderText({
        if (input$calcButton)
            isolate(round(lr2()[2], 3))
    })

    output$class_result.text = renderText({
        if (input$calcButton)
            isolate("Class of the tumor:")
    })
    output$class_result = renderText({
        if (input$calcButton)
            isolate(ifelse(lr2()[2] < LR2.cutoff, "benign", "malignant"))
    })
    
    output$plot = renderPlot({
        if (input$calcButton)
            isolate({
                
                df = data.frame(x=lr2()[1],y=lr2()[2])
                
                p = qplot(LR2.yrange, 1/(1+exp(-LR2.yrange)), 
                          geom="line",
                          xlab="premises",
                          ylab="prediction") + 
                    geom_hline(yintercept=LR2.cutoff, linetype="dashed") +
                    geom_point(data=df,aes(x,y),colour="red",size=4) + 
                    theme_bw() + 
                    geom_text(aes(6, .15, label=paste("malignancy threshold:", LR2.cutoff)))
                
                print(p)
            })
    })
})