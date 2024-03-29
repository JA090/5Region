#'# A visualization tool for the design and analysis of trial data. Details of the procedure are in 'A Visualization Tool to Support a Multiple Testing Strategy 
#'and Encourage Thoughtful Statistical Analyses' by Aisbett, Drinkwater, Quarrie and Woodcock.
#' The tool uses a multiple testing procedure featuring one-sided t-tests against margins of material significance (i.e., of practically meaningful effect sizes). 
#' The test rejection regions are simultaneously presented at multiple alpha levels. Optionally, confidence intervals can be presented for data points entered as mean + SE. 
#' We outline how this tool might be used in the statistical design and analysis phases.  Visualizing graded interpretations of the evidence against a parameter being in 
#' a certain range could encourage more nuanced reporting of findings as well as improve communication with stakeholders. 
#' Our tool is based on the Magnitude-Based Inference procedure developed in sport science.
#' 'Rejection regions are identified on a chart of effect size vs standard error for the four one-sided tests against the endpoints of the interval of effect sizes equivalent to zero.
#' Users nominate up to 3 alpha levels at which to conduct each test, and can choose to ignore one or more of the one-side tests.
#' Findings entered as mean effects and standard errors are plotted onto the chart and their rejection regions correspond to the MBD decisions.
#' The tool also supports estimating the sample size needed to achieve nominated powers over each of the nominated tests.
#' Users can also change the terminology used to describe the rejections regions/decisions.
#' Finally, users can opt for color or black and white display and can set the maximum co-ordinates displayed.
#' Clicking on the chart will return the coordinates (effect size and either SE or sample size depending on mode), and drawing a box then 
#' double clicking will zoom the chart.

library(shinyMatrix)
library(shinyjs)
library(shinyjqui)
library(miceadds)
library(knitr)
library(shinyWidgets)

miceadds::source.all(".\\R")


#'Start up values for potential user-nominated terms of magnitude and test strength
mmagTerms=paste0("size < L, size > U, size \u2264 U, size \u2265 L, size between L & U")
mmag2Terms="no test significant"
levelTerms="p < 0.25, p < .05, p < .001"
magTermsNC = "negative, positive,  not positive, not negative, trivial"
magTerms = "harmful, beneficial, non beneficial, non harmful, trivial"
mag2Terms = "unclear"
mlevelTerms="likely, very likely, most likely"
colorvec <<- c("lightblue","dodgerblue","blue", #inferiority
               "darkseagreen", "limegreen","green", #superiority
               "lightgray", "darkgray", "gray35","gray20") # equivalence and inconclusive
alpss<<-matrix(data=rep(NA,15),nrow=5,ncol=3)

#'Start up values for matrix of test levels and power for the 4 one-sided tests   
D=c(0.25, "Y","Y","","","Y",0.05, "Y","Y","Y","","Y",0.001, "Y","","Y","Y","Y")
DP=c(80,80,0,0,80,70,70,80,0,80, 60,0,  70,70,70)
xmin=-4; xmax=4

dimnames=list( c("test /\u03b1", "E \u2265 L", "E \u2264 U", "E > U", "E < L", "L \u2264 E \u2264 U"))

inpAlpha<-matrix(D, nrow=6,ncol=3,dimnames=dimnames)
inpAlphaSS<-matrix(D, nrow=6,ncol=3,
                   dimnames=dimnames)
inpPower<-matrix(DP, nrow=5,ncol=3,
                 dimnames=list(c("E \u2265 L", "E \u2264 U", "E > U", "E < L","L \u2264 E \u2264 U"),c("alpha 1", "alpha 2", "alpha 3")))

#'Start up values for data points if analysis phase     
dataFile<- "ledewski.txt" #: Rinnella & James -.145,.0725,.8972/ 0.8408329, 0.8408329 ,0.8443933"bartolomei.txt"  # "
d<-read.csv(dataFile,header=F,sep="\t")
dataMean="-.145,.0725,.8972" #paste((d[1,]))
dataSE= " 0.8408329, 0.8408329, 0.8443933" #paste((d[2,]))    

#' INTERFACE:

ui <- fluidPage(
  div(style= "font-family: Arial;font-size: 13px; font-weight: 300;",
      tags$head(tags$style("[type = 'number']{font-weight: 300;font-size: 12px;font-family: Courier}"),
                tags$style("label {font-weight: 300; font-style: italic;font-size: 12px}"),
                tags$style("[type = 'text']{font-weight: 300;font-size: 12px;font-family: Courier}"),
                tags$style("#magTerms {font-weight: 300;font-size: 12px;font-family: Courier}"),
                tags$style("#levelTerms {font-weight: 300;font-size: 12px;font-family: Courier}"),
                tags$style("#mag2Terms {font-weight: 300;font-size: 12px;font-family: Courier}"),
                tags$style("#dataMean {font-weight: 300;font-size: 12px;font-family: Courier}"),
                tags$style("#dataSE {font-weight: 300;font-size: 12px;font-family: Courier}")),
      tags$style("#LUX {font-weight: 300;font-size: 12px;font-family: Courier}"),
      chooseSliderSkin("Modern"),
      tags$style(type="text/css", ".shiny-output-error{visibility: hidden; }",
                 ".shiny-output-error:before {visibility: hidden; }"),
      
      titlePanel(             
        div(style = "display: flex; flex-wrap: wrap;color:black",
            h3("Visualization Tool to Support Five-Region Hypothesis Testing"),
            
        )),
      
      p(("Tool to explore tests and sample sizes before a study or to explore decisions about effect sizes after data collection.")),
      p("Given an interval [", em("L, U"), "] that is practically equivalent to zero, the tool considers 5 tests: whether the effect size", em("E")," is
                  at least or is less than ", em("L"),", is at most or is more than ", em("U"),", or is between ", em("L")," and ", em("U"),". Tests are at up to 3 alpha levels."),
      div( fluidRow(column(2, em("Select interval equivalent to 0:")),
                    column(10,uiOutput("Slider")),
      )),
      
      
      tabsetPanel(
        id="tab",
        
      #'Tab for analysis phase
        tabPanel("Analysis",h5(strong("Plot findings against rejection regions of nominated tests or as confidence intervals.")),
                 p(" The color behind a finding corresponds to the decision about effect size shown in the legend. Click on chart to get point co-ordinates, plus the standard 95% confidence interval for a finding with
                                    those co-ordinates. Draw a box in the chart and double click in it to zoom. Double click again to unzoom. Legend is draggable."),
                 sidebarLayout(
                   
           #'Analysis sidebar offers a table to input desired tests and alpha levels. 
                   sidebarPanel(
                     width = 3,
                     strong(" TABLE OF TESTS"),
                     em(matrixInput("alps",value=inpAlpha,rows=list(names=T),cols=list(names=F))),
                     p(em("Modify table to suit. Enter Y to conduct test at given alpha level. 
                      "),  width="400px") ,
                     br(),
                     #'Analysis sidebar also allow choice to display confidence intervals at multiple confidence levels about data points entered in Study tab        
                     checkboxInput("drawCIs",label = h5("Check box to display findings as confidence intervals (CIs)."), value=F,width ="100%"),
                     p("Studies will be labelled in order of data entry. For each study, CIs corresponding to different test levels are overlaid,
                      with the longest CI corresponding to the most stringent test. The CI panel is draggable, and will be closed
                     when the box is unchecked."),
                   ),
                   
                   mainPanel(width=9,
                             
          #'Analysis main panel optionally displays a draggable display of confidence intervals 
                             jqui_draggable(
                               conditionalPanel(condition = "input.drawCIs == true",plotOutput("CIs"),draggable=TRUE),
                               option=list(cancel = ".shiny-input-container")),
          
          #'Analysis main panel  displays the co-ordinates of a point if the chart is clicked on
          #'The chart is displayed beneath this, with ability to draw a box and double click to zoom (hence chart is not draggable)
                             div(style="width: 450px;",verbatimTextOutput("clickss")),
                             tags$head(tags$style("#'clickss{color:blue;width:200px}")),
                             tags$head(tags$style("#'Fig{cursor:default}")),
                             plotOutput("Fig", inline=T,  click = "plot_click", dblclick="plot_dclick1",
                                        brush=brushOpts(id="brush1", resetOnNew=T)),
           #'Legend is  a draggable panel
                             absolutePanel(plotOutput("Legend"),draggable=TRUE,top="510px"),    
                             
                   ),
                 )), 
        #' End analysis tab.
        
    #'Tab for sample size calculation phase
        tabPanel("Sample size",h5(strong("Estimate minimum sample size to achieve desired power over all nominated tests.")),
                 p("A vertical line is shown at each estimated effect size. The color behind a point on the line shows the strongest test with sufficient power at that sample size. The legend is draggable."),
                 p("Draw a box in the chart and double click in it to zoom. Double click again to unzoom. Single click on chart to get co-ordinates.
              Legend is draggable."),
                 sidebarLayout(
                   
       #'Sample size sidebar offers tables to input desired tests/alpha levels and powers. 
                   sidebarPanel(
                     width = 3,
                     div(style="margin-bottom:0px;flex:1;flex-wrap=wrap;color:black",
                         strong("TABLE OF TESTS"),
                         em(matrixInput("alpsS",value=inpAlphaSS,rows=list(names=T),cols=list(names=F))),
                         p(em("Modify table to suit. Enter Y to conduct this test at this alpha level."), width="400px") ,
                         br(),
                         div(style = "flex:1;color:black", em(matrixInput("power",value=inpPower,rows=list(names=T),cols=list(names=F),class="numeric"))),
                         p(em("For each test, enter desired power at that alpha level.", width="400px")) ),
                     br(),
        
        #'Sample size sidebar also is where estimated effect size(s) and estimated variance are entered.
                     textInput("ES", label = h5(paste0("One or more estimates of effect size, comma separated", intToUtf8(8194))),value="-.3,2.0 "),
                     numericInput("var", label = h5(paste0("Estimated variance.", intToUtf8(8194))), value = 1),
                   ),
        #'Sample size main panel is as for analysis phase, without optional CI display.The chart now shows rejection region THAT HAVE DESIRED POWER 
                   mainPanel(width=9,
                             div(style="width:350px;color:blue;",verbatimTextOutput("clickss1")),
                             tags$head(tags$style("#'FigSS{cursor:default}")),
                             plotOutput("FigSS", inline=T,  click = "plot_click", dblclick="plot_dclick",
                                        brush=brushOpts(id="brush", resetOnNew=T)),
                             absolutePanel(plotOutput("Legend1"),draggable=TRUE,top="510px"),
                   ),
                 )), 
      #' End sample size tab.
        
      #'Tab for chart display parameters
        tabPanel("Display set up", h5(strong("Set up basic parameters  of the display")),
                 #' Checkbox for B&W display versus color.
                 div(style = "flex:1;flex-wrap; color: black",
                     checkboxInput("chartBW",label = "Check box for B&W display.", value=F,width ="100%"),
                    
         #' Checkbox to change axes ranges, then input boxes for these.      
                     div(style= "color: red",checkboxInput("rangeChange",label = "Toggle box after re-setting chart ranges.", value=T,width ="100%")),
                     br(),
                     fluidRow(
                       column(width=2, p(("Range of effect sizes."))),
                       div(style = "flex:1;color:black", 
                           column(2,numericInput("xmin", label = paste0("smallest", intToUtf8(8194)), value = -4)),
                           column(2,numericInput("xmax", label = paste0("largest", intToUtf8(8194)), value = 4)),
                       )),
                     fluidRow(
                       column(width=2,  p(("Range of standard errors.*"))),
                       div(style = "flex:1;color:black", 
                           column(2,numericInput("ymin", label = paste0("smallest", intToUtf8(8194)), value = 0)),
                           column(2,numericInput("ymax", label = paste0("largest", intToUtf8(8194)), value = 4)),
                       )),
                     fluidRow(
                       column(width=2,  p(("Range of total sample sizes.*"))),
                       div(style = "flex:1;color:black", 
                           column(2,numericInput("yminSS", label = paste0("smallest", intToUtf8(8194)), value = 6)),
                           column(2,numericInput("ymaxSS", label = paste0("largest", intToUtf8(8194)), value = 100)),
                       )),
                     p("* Enter range of standard errors if doing an analysis, otherwise range of total sample size."),
                     br(),
         
         #' Checkbox to enter terminology, then input boxes for this
                     checkboxInput("newTerms",label = "Check box to enter your own descriptions into the chart legend. Uncheck to restore terms.", value=F,width ="100%"),
                     p("Use comma separated format, and follow the order of entries suggested in the boxes."),
                     br(),
                     div(style = "color:black; font-weight: 300;",
                         textAreaInput("levelTerms", label = paste0("Your description of your test levels (max. 3)",intToUtf8(8194)),  value = levelTerms,width="200%"),
                         tags$head(tags$style(type="areaText/css","#levelTerms {width: 1000px}")),
                         textAreaInput("magTerms", label = paste0("Your terms for effect size ranges", intToUtf8(8194)), value = gsub("Delta", "\u394",magTerms),width=1000),
                         textAreaInput("mag2Terms", label = "How you say no test is rejected",  value = mag2Terms,width="1000px"),
                     ),
                 )), 
      #'End set up Display tab.
        
    #'Tab for study parameters and study data. 
        tabPanel("Study design & data", h5(strong("Set up basic parameters of the study, and enter findings")),br(),
                 div(style = "flex:1;flex-wrap; color: black",
       
        #' Can be one or two group design with possibly unequal groups.
                     em(p("Enter 1 for a single group design or the larger group proportion for a 2 group design.")),
                     numericInput("Study",label="",width = "100px",value = 0.5,min=.5,max=1,step=.1),
                     br(),
        
        #' Sample size (or an estimate if calculating sample size, used in drawing regions)  
                     em("Enter total trial sample size, or your initial estimate if computing sample size."),
                     numericInput("sample", label = "",width="100px",value=40,step=10,min=4),
                     br(),
                     #' Checkbox to enter findings and a conditional panel to use to input.    
                     checkboxInput("ESES",label = "Check to enter findings", value=F,width ="100%"),
                     h5("Use comma separated format to enter the effect sizes and the standard errors. Your findings will be plotted as numbers in the order you entered them."),
                     conditionalPanel(condition = "input.ESES == true",
                                      textAreaInput("dataMean", label = "List of effect sizes", value = dataMean),br(),
                                      textAreaInput("dataSE", label = "List of standard errors", value = dataSE)
                     ),
                 )), 
        #'End study set up tab.
      )))

###################################################################
#' @details

#' OUTPUT
server <- function(input, output, session) {
  
#' set range variables for analysis and sample size charts
  xmin0=-4;xmax0=4;ymin0=0; ymax0=4; ymin0SS=6;ymax0SS=100
  xmin=-4;xmax=4;ymax=4;ymin=0;ymaxSS=100;yminSS=6
  ranges <-reactiveValues()
  ranges$xmin=xmin; ranges$xmax=xmax;ranges$ymin=yminSS; ranges$ymax=ymaxSS
  ranges1 <-reactiveValues()
  ranges1$xmin=xmin; ranges1$xmax=xmax;ranges1$ymin=ymin; ranges1$ymax=ymax
 
#' Update chart and slider ranges when range change checkbox changes.
  observeEvent(input$rangeChange,{ranges1$xmin=input$xmin; ranges1$xmax=input$xmax;ranges1$ymin=input$ymin; ranges1$ymax=input$ymax;
  ranges$xmin=input$xmin; ranges$xmax=input$xmax; ranges$ymin=input$yminSS; ranges$ymax=input$ymaxSS
  updateSliderInput(session, "LUX",min=ranges$xmin,max=ranges$xmax)
  })
  
#' Slider for entry of range of effect sizes equivalent to zero.
  output$Slider <- renderUI({step=(xmax-xmin)/1000
  sliderInput("LUX", "",min=xmin,max=xmax,value=c(-1,1),step=step,width="100%")})
  
#' Chart for sample size computation.      
  output$FigSS <-
    renderPlot({
      
    #' Set up variables for plot range. 
      xmin0=input$xmin;xmax0=input$xmax;ymin0=input$yminSS;ymax0=input$ymaxSS
      fac = input$var 
      if (input$Study <1) fac=fac*(1 / input$Study + 1 / (1 - input$Study))
      
    #' Draw the chart.
      reactive(param1)
      if(length(input$LUX[1]>0))
        param1<-buildPlot(
          input$alpsS,input$power/100,
          input$sample,input$var,
          input$LUX[1],input$LUX[2],
          ranges$xmin,ranges$xmax,ranges$ymin,ranges$ymax,
          input$tab, input$Study,input$chartBW,
          input$newTerms,input$magTerms,input$mag2Terms,input$levelTerms
        )
      
    #' Update tables of tests and powers; also update the test table on the analysis tab.
      if(length(param1)>0)updateMatrixInput(session, "alpsS", value =matrix(data=param1[1:18], nrow=6,ncol=3,dimnames=dimnames))
      if(length(param1)>0)updateMatrixInput(session=session, "alps", value=matrix(data=param1[1:18], nrow=6,ncol=3,dimnames=dimnames))
      if(length(param1)>0)updateMatrixInput(session, "power", value =matrix(data=param1[19:33], nrow=5,ncol=3,dimnames=list( c( "E \u2265 L", "E \u2264 U", "E > U", "E < L","L \u2264 E \u2264 U"))))
      
    #' Draw vertical lines through potential effect sizes.
      if(!is.na(input$ES))
        for(x in suppressWarnings(as.numeric(unlist(strsplit(input$ES,split=",")))))if (x<ranges$xmax) if (x>ranges$xmin)lines(c(x,x),param1[34:35],lwd=2)
      
    #' Return co-ordinates of a point on click.
      output$clickss1 <- 
        renderText({ cc<-clickPointsVT(input$plot_click$x,input$plot_click$y,input$var,input$tab, input$Study)
                    paste(" Effect size: ", cc[1]," Sample size ",cc[2])
          })
      
    #' Zoom into box on double click.       
      observeEvent(input$plot_dclick,{
        b<-input$brush
        if (!is.null(b)){ranges$xmin=b$xmin; ranges$xmax=b$xmax
        ranges$ymax=fac/(b$ymin)^2; ranges$ymin<- fac/(b$ymax)^2}
        else
        {ranges$xmin=xmin0;ranges$xmax=xmax0;ranges$ymin=ymin0SS; ranges$ymax=ymax0SS; }
      })
      
    },height=500, width=500) 
  #' Set chart plot dimensions at end of renderPlot for sample size.
  
  
  #'ANALYSIS PHASE OUTPUT 
  #'Draw the draggable legend.
  output$Legend1<- output$Legend <- renderPlot({
       if(length(input$LUX[1]>0)) colorvec <<- drawLegendDrag(input$newTerms,input$magTerms,input$mag2Terms,
          input$levelTerms, input$alps,0,1, 0,1,input$LUX[2]-input$LUX[1],input$chartBW)},height=400,width=400) 
  #' Optionally draw CIs.
  output$CIs <- renderPlot({ if(length(input$dataMean)>0)if (length (input$LUX[1])>0)
    drawCIs(input$dataMean, input$dataSE,input$LUX[1], input$LUX[2],as.numeric(input$alps[1,]),input$xmin,input$xmax,
            input$sample, input$Study,input$chartBW,input$levelTerms,
            lab="effect size",legendPos="topleft")}
    ,height=400,width=400)
  
  #' Draw the chart.  
  output$Fig <- renderPlot({
    xmin0=input$xmin;xmax0=input$xmax;ymax0=input$ymax
    reactive(param)
    param<-if(length(input$LUX[1]>0))buildPlot(
      input$alps,matrix(data =rep(0,15),nrow=5,ncol=3),
      input$sample,0,
      input$LUX[1],input$LUX[2],
      ranges1$xmin,ranges1$xmax,ranges1$ymin, ranges1$ymax,
      input$tab, input$Study,input$chartBW,
      input$newTerms,input$magTerms,input$mag2Terms,input$levelTerms
    )
    #' Update table of tests ; also update the test table on the sample size tab.
    if(length(param)>0) updateMatrixInput(session=session, "alps", value=matrix(data=param[1:18], nrow=6,ncol=3,dimnames=dimnames))
    if(length(param)>0) updateMatrixInput(session=session, "alpsS", value=matrix(data=param[1:18], nrow=6,ncol=3,dimnames=dimnames))
    
    #' Return co-ordinates of a point on click.
    output$clickss <- 
      renderText({ cc<-clickPointsVT(input$plot_click$x,input$plot_click$y,input$sample,input$tab, input$Study)
        paste("Effect size: ", cc[1], "SE:",cc[2],"  95% CI: (",cc[3]," ,", cc[4],")")
      })
    
    #' Zoom into box on double click.
    observeEvent(input$plot_dclick1,{
      b<-input$brush1
      if (!is.null(b)){ranges1$xmin=b$xmin; ranges1$xmax=b$xmax
      ranges1$ymax=b$ymax; ranges1$ymin<- b$ymin}
      else
      {ranges1$xmin=xmin0;ranges1$xmax=xmax0;ranges1$ymin=ymin0; ranges1$ymax=ymax0; }
    })
    
    #' Display points representing study findings,           
    if(input$ESES)if(length(input$dataMean)>0)
      datadisplay(input$dataMean, input$dataSE,input$chartBW,ranges1$xmin, ranges1$xmax, ranges1$ymin,ranges1$ymax)
 
  },height=500, width=500)
  #' Set chart plot dimensions at end of renderPlot for Analysis.
}
shinyApp(ui = ui, server = server)

#' @references  Aisbett, Drinkwater, Quarrie & Woodcock.
#' @export
