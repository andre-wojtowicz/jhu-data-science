library(shiny)


shinyUI(pageWithSidebar(
    
    headerPanel("Ovarian tumor risk of malignancy index calculator (IOTA LR2)"),
    
    sidebarPanel(
        sliderInput("age", 
                    "Age of the patient (years):", 
                    min = 14,
                    max = 100, 
                    value = 40),
        selectInput("ascites", "Presence of ascites:",
                    list("no" = 0, 
                         "yes" = 1)),
        selectInput("blood_flow", "Presence of blood flow within a papillary projection:",
                    list("no" = 0, 
                         "yes" = 1)),
        sliderInput("solid_component", 
                    "Largest diameter of the solid component (in mm):", 
                    min = 0,
                    max = 200, 
                    value = 0),
        selectInput("internal_wall", "Irregular internal cyst wall:",
                    list("no" = 0, 
                         "yes" = 1)),
        selectInput("acoustic_shadow", "Presence of acoustic shadows:",
                    list("no" = 0, 
                         "yes" = 1)),
        actionButton("calcButton", "Calculate")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        p("This application is designed for gynaecologists and implements ovarian tumor risk 
           malignancy index based on IOTA LR2 algorithm. It also visualises an output
           of the logistic regression."),
          
        p("For a detailed description of the algorithm please refer to the paper:
          Timmerman D, Testa AC, Bourne T, [et al.]. Logistic regression model to distinguish between
          the benign and malignant adnexal mass before surgery: a multicenter study by the International
          Ovarian Tumor Analysis Group. J Clin Oncol. 2005, 23, 8794-8801."),
        
        h1("Malignancy prediction algorithm"),
        
        p("In general, LR2 algorithm predicts a tumor as a benign when a patient is young, 
          a solid component of lesion is small and acoustic shadows are present. 
          You may check it empirically by different combinations of input values."),
        
        p("Fill in the form and click", strong("Calculate"), "button."),

        p(textOutput('raw_result.text', inline=T), strong(textOutput('raw_result', inline=T))),
    
        p(textOutput('class_result.text', inline=T), strong(textOutput('class_result', inline=T))),
        
        plotOutput('plot')
    )
))