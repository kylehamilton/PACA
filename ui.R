

library(shiny)

shinyUI(bootstrapPage(


    headerPanel("PACA: Principal Component Analysis with Shiny"),
    sidebarPanel(

      helpText("Click here to update your results, you need to do this after you change the data, model, or setting"),
      submitButton("Update View"),
      br(),
      helpText("Press Quit to exit the application"),
      actionButton("quit", "Quit")
      
      
    ),
    mainPanel(
        tabsetPanel(

        tabPanel("Main",

            h3("Data"),
            p('Input values must be separated by tabs. Copy and paste from Excel.'),
            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Please make sure that your data includes the header (variable names) in the first row.</div></b>")),

            strong('Option:'),
            checkboxInput("rowname", label = strong("The first column contains case names."), value = T),

            aceEditor("text", value="Unit\tWordsPerSentence\tFleschKincaidGrade\tNarrativity\tSyntacticSimplicity\tWordConcreteness\tReferentialCohesion\nU01\t13.5\t6.2\t79.1\t70.19\t60.64\t24.51\nU02\t16.6\t8.1\t57.53\t68.79\t95.91\t59.87\nU03\t15.9\t9.4\t31.21\t54.38\t29.81\t6.43\nU04\t17.5\t9.8\t42.07\t56.36\t47.21\t27.76\nU05\t14.9\t8.8\t53.19\t72.24\t58.32\t13.57\nU06\t16.9\t10\t34.83\t63.31\t57.93\t5.82\nU07\t20\t7.7\t41.29\t33.72\t84.38\t93.57\nU08\t14.3\t7.1\t69.85\t82.38\t35.2\t3.22\nU09\t16.7\t7.6\t89.07\t34.46\t89.62\t32.64\nU10\t14\t6.5\t44.04\t75.49\t71.9\t19.49\nU11\t17.4\t6\t93.82\t50.8\t57.53\t47.61\nU12\t21.5\t11.1\t37.83\t35.57\t43.64\t20.9\nU13\t18.5\t10.2\t28.77\t59.48\t32.28\t18.41\nU14\t17.6\t9.5\t64.8\t42.47\t65.91\t50.4\nU15\t20.2\t9.9\t28.43\t34.46\t84.61\t15.15",
                mode="r", theme="cobalt"),

            br()
        ),
        
        tabPanel("Basic statistics",
            h3("Basic statistics"),
            verbatimTextOutput("textarea.out"),

            br(),

            h3("Correlation"),
            verbatimTextOutput("correl.out"),
            h3("Results of principal component analysis"),
            verbatimTextOutput("pcaresult.out"),
            downloadButton('downloadData', 'Download the data with principal component scores'),

            br()
        ),
        tabPanel("Plot Output",
            strong("Scatter plot matrices"),

            br(),
            downloadButton('downloadCorPlot', 'Download the plot as pdf'),

            plotOutput("corPlot"),

            br(),

            h3("Scree plot"),
            downloadButton('downloadSPlot', 'Download the plot as pdf'),

            plotOutput("sPlot", width="80%"),

            br(),
            br(),

            h3("Plot"),
            downloadButton('downloadPlot1', 'Download the plot as pdf'),
            plotOutput("pcPlot1", height = "600px"),

            br(),

            downloadButton('downloadPlot2', 'Download the plot as pdf'),
            plotOutput("pcPlot2", height = "500px"),

            br(),

            downloadButton('downloadPlot3', 'Download the plot as pdf'),
            plotOutput("pcPlot3", height = "500px"),

            br(),

            h4("Biplot"),
            downloadButton('downloadPlot4', 'Download the plot as pdf'),
            plotOutput("pcPlot4", height = "700px"),

            br(),

            h4("Cluster analysis using the principal component scores"),
            p("(Ward method with the squared Euclidean distance technique)"),
            downloadButton('downloadPlot5', 'Download the plot as pdf'),
            plotOutput("pcPlot5"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info.out")

            ),


        tabPanel("About",
                 
                 strong('Principal Component Analysis'),
                 p("The goal of this project is to help students and researchers run principal component analysis as easily as possible."),
                 p('This application is developed with',
                   a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
                   ''),
                 p('The code for this application is available at this',
                   a('GitHub.', href='https://github.com/kylehamilton/PACA', target="_blank")),
                 
                 
                 br(),
                 
                 strong('List of Packages Used'), br(),
                 code('library(shiny)'),br(),
                 code('library(shinyAce)'),br(),
                 code('library(psych)'),br(),
                    
                 br(),
                 
                 h4('Acknowledgments and Authors'),
                 
                 strong('Acknowledgments'),
                 
                 p('William Kyle Hamilton would like to thank the ',
                   a("Health Communications and Interventions Lab at UC Merced", href="http://cameronhcilab.com/", target="_blank"),
                   'for their comments and beta testing efforts on this application. '),
                 
                 p('Atsushi Mizumoto would like to thank',
                   a("Dr. Luke Plonsky", href="http://oak.ucc.nau.edu/ldp3/", target="_blank"), 'and',
                   a("Dr. Yo In'nami", href="https://sites.google.com/site/yoinnami/", target="_blank"),
                   'for their support and feedback to create this web application.'),
                 
                 br(),
                 
                 
                 
                 h5('Authors'),
                 
                 HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/kyle80.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
                 p(a("William Kyle Hamilton - University of California, Merced", href="http://www.kylehamilton.com", target="_blank")),
                 p("William Kyle Hamilton maintains this application and has authored new features."),
                 
                 br(),
                 HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/atsushi80.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
                 p(a("Atsushi Mizumoto, PhD - Kansai University", href="http://mizumot.com", target="_blank"),br(),
                   p("Atsushi Mizumoto wrote the first version of this application; this application is a fork of the original which can be found", a("here", href="https://github.com/mizumot/pca", target="_blank"))
                   
                   
                 ),


            p(br())

            )

))
))
