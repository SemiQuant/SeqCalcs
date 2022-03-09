require(shiny)
require(shinythemes)
require(shinyBS)
require(shinydashboard)
require(DT)
require(dashboardthemes)
require(plotly)
require(shinyjs)
require(dashboardthemes)
require(shinyalert)

#################
dark_grey_edited <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Helvetica Neue, Helvetica, Arial, sans-serif"
    ,appFontColor = "rgb(205,205,205)"
    ,bodyBackColor = "rgb(39,43,48)"
    
    ### header
    ,logoBackColor = "rgb(70,80,90)"
    
    ,headerButtonBackColor = "rgb(70,80,90)"
    ,headerButtonIconColor = "rgb(198, 253, 168)"
    ,headerButtonBackColorHover = "rgb(40,50,60)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    
    ,headerBackColor = "rgb(70,80,90)"
    ,headerBoxShadowColor = "rgb(198, 253, 168)"
    ,headerBoxShadowSize = "0px 0px 0px"
    
    ### sidebar
    ,sidebarBackColor = "rgb(52,62,72)"
    ,sidebarPadding = 0
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"
    
    ,sidebarUserTextColor = "rgb(205,205,205)"
    
    ,sidebarSearchBackColor = "rgb(39,43,48)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(39,43,48)"
    
    ,sidebarTabTextColor = "rgb(205,205,205)"
    ,sidebarTabTextSize = 14
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = 0
    
    ,sidebarTabBackColorSelected = "rgb(70,80,90)"
    ,sidebarTabTextColorSelected = "rgb(198, 253, 168)"
    ,sidebarTabRadiusSelected = "5px"
    
    ,sidebarTabBackColorHover = "rgb(55,65,75)"
    ,sidebarTabTextColorHover = "rgb(255,255,255)"
    ,sidebarTabBorderStyleHover = "none"
    ,sidebarTabBorderColorHover = "none"
    ,sidebarTabBorderWidthHover = 0
    ,sidebarTabRadiusHover = "5px"
    
    ### boxes
    ,boxBackColor = "rgb(52,62,72)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(52,62,72)"
    ,boxPrimaryColor = "rgb(200,200,200)"
    ,boxSuccessColor = "rgb(155,240,80)"
    ,boxWarningColor = "rgb(240,80,210)"
    ,boxDangerColor = "rgb(240,80,80)"
    
    ,tabBoxTabColor = "rgb(52,62,72)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(205,205,205)"
    ,tabBoxTabTextColorSelected = "rgb(205,205,205)"
    ,tabBoxBackColor = "rgb(52,62,72)"
    ,tabBoxHighlightColor = "rgb(70,80,90)"
    ,tabBoxBorderRadius = 5
    
    ### inputs
    ,buttonBackColor = "rgb(230,230,230)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(50,50,50)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(180,180,180)"
    ,buttonTextColorHover = "rgb(50,50,50)"
    ,buttonBorderColorHover = "rgb(50,50,50)"
    
    ,textboxBackColor = "rgb(68,80,90)"
    ,textboxBorderColor = "rgb(76,90,103)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(80,90,100)"
    ,textboxBorderColorSelect = "rgb(255,255,255)"
    
    ### tables
    ,tableBackColor = "rgb(52,62,72)"
    ,tableBorderColor = "rgb(70,80,90)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)

#####################
title <- "SeqCalcs"

header <- dashboardHeader(title =  div(img(src = "sq.png", height="42.885", width="34.29"), title), titleWidth = 300,
                          dropdownMenu(type = "notifications", icon = icon("medkit"), badgeStatus = NULL,
                                       notificationItem(text = "SemiQuant",
                                                        href = "http://www.semiquant.com")
                          )
)



sidebar <- dashboardSidebar(
    width = 300,
    h3("Select a Test", align = "center"),
    dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
                    convertMenuItem(
                        menuItem("Amplicon Coverage", tabName = "calcov", icon = icon("calculator"),
                                 tipify(
                                     sliderInput("loss", "Expected loss (percentage):",
                                                 min = 0, max = 99, value = 40, step = 1
                                     ), "Predicted percentage of the total 'Expected Gb output' of flow cell which will be lost due to technical error", placement="bottom", trigger = "hover"),
                                 numericInput("amp_len", "Amplicon length:", 850, min = 1),
                                 numericInput("amps", "Number of different amplicons:", 10, min = 1),
                                 numericInput("out", "Expected Gb output of flow cell:", 1),
                                 numericInput("samples", "Number of samples multiplexed:", 1),
                                 numericInput("amp_read_len", "Read length for illumina or fragmented:", NA)
                        ), "calcov"
                    ),
                    
                    convertMenuItem(
                        menuItem("Illumina WGS Coverage", tabName = "WGScov", icon = icon("calculator"),
                                 numericInput("Gsize", "Genome size (Mb):", 4.5, min = 1),
                                 sliderInput("Gdup", "Duplicates (percentage):",
                                             min = 0, max = 99, value = 2, step = 1),
                                 sliderInput("Gtar", "On target (percentage):",
                                             min = 0, max = 99, value = 90, step = 1),
                                 # "Enter either",
                                 numericInput("GcovX", "Coverage needed (X):", 10, min = 1),
                                 # "Or",
                                 numericInput("GcovG", "Depth Given (Mb):", 1000, min = 1)
                        ), "WGScov"
                        
                        #          convertMenuItem(
                        #              menuItem("Output Required", tabName = "WGScov1",
                        #                       numericInput("GcovX", "Coverage needed (X):", 10, min = 1)
                        #              ), "WGScov1"),
                        # convertMenuItem(
                        #          menuItem("Depth Given", tabName = "WGScov2",
                        #                   numericInput("GcovG", "Depth Given (Mb):", 1000, min = 1)
                        #                   ), "WGScov2")
                        
                        
                    ),
                    
                    
                    convertMenuItem(
                        menuItem("PCR optimization", tabName = "pcr", icon = icon("calculator"),
                                 numericInput("len", "Amplicon length (bp):", 2000, min = 10, step = 1),
                                 numericInput("dNTPs", "dNTP's concentration is (mM):", 0.2, min = 0.05, step = 0.05),
                                 numericInput("pconc", "Primer quantity (pmol):", 20, min = 0.1, step = 0.1),
                                 numericInput("pol", "Units of polymerase:", 1, min = 0.1, step = 0.1),
                                 numericInput("time", "Elongation time (s):", 30, min = 1, step = 1),
                                 numericInput("vol", "Reaction volume (ul):", 15, min = 1, step = 1),
                                 numericInput("mo", "Template quantity:", 0.1, min = 0.0001, step = 0.0001),
                                 selectInput("unit", "Quantity units:",
                                             c("nmol" = "nmol",
                                               "pmol" = "pmol",
                                               "fmol" = "fmol",
                                               "amol" = "amol",
                                               "zmol" = "zmol",
                                               "copies" = "copies",
                                               "ng" = "ng",
                                               "pg" = "pg",
                                               "fg" = "fg",
                                               "ag" = "ag",
                                               "zg" = "zg"), selected = "fg"),
                                 checkboxInput("log_scale", "Log scale y-axis", TRUE),
                                 tipify(
                                     sliderInput("per_loss", "Percent loss per cycle:", min = 0, max = 50, 1, step = 0.1),
                                     "The percentage of copies that will not be amplified in a single PCR cycle. i.e., in round 2, only 95/100 amplicons present had primers anneal and were amplified to form 190 copies at the end of the cycle."),
                                 tipify(
                                     sliderInput("linear_noise", "Noise in linear phase:", min = 0, max = 0.5, 0.2, step = 0.05),
                                     "How much noise to add to the amplification in the linear phase of the PCR. This generates random error that more closely resembles reality."
                                 )
                                 # radioButtons("pwrRCT", "Select type:", inline = T, choices = c("Power", "Sample Size"), selected = "Power"),
                                 # sliderInput("alp", "Select alpha:", min = 0.001, max = 0.2, 0.05, step = 0.001),
                                 
                        ),"pcr"),
                    
                    menuItem("Website", icon = icon("chrome"),
                             href = "http://www.semiquant.com")
        )
    ),
    br()
)





body <- dashboardBody(
    dark_grey_edited,
    tags$style(type = "text/css", "
             .irs-max {font-family: 'arial'; color: white;}
             .irs-min {font-family: 'arial'; color: white;}
             "),
    
    tabItems(
        tabItem(tabName = "dashboard",
                h3("This app allows you to calculate:"),
                # h3("If you have nonspecific amplification, esitmate the effect size and adjust the input quantity accordingly"),
                # h3("Click the sidebar to get started"),
                # br(),
                # h3("Still in dev, to add:"),
                tags$ol(
                    tags$li("The expected coverage of amplicons in a sequencing run"), 
                    tags$li("The needed amount of sequencing to get a specific coverage"),
                    tags$li("The  depth of coverage given by a given by a specified amount of sequencing"),
                    tags$li("The expected number of PCR amplicons given various conditions"), 
                ),
                
                # br(),
                tags$video(src = "SeeK.mp4", type = "mp4", autoplay = F, controls = T,
                           width = "100%", poster = "sq_pad._widerpng.png")
                # br(),
                # HTML(
                #   '<a href="mailto:Jason.Limberis@uct.ac.za?
                #   body=""
                #   &subject="Sample Size Calculator Suggestion">Please email if you want to request a specific test or have a suggestion.</a>'
                # )
        ),
        
        tabItem(tabName = "calcov",
                # includeHTML("info.html"),
                h3("This is to calculate the expected coverage of amplicons when running on the Oxford Nanopore (can also be used for Illumina)"),
                p("Input read length currently nonfunctinal"),
                br(),
                h4("Nanopore reported/estimated values"),
                DTOutput("nano_est"),
                br(),
                textOutput("calcov")
        ),
        
        tabItem(tabName = "WGScov",
                h3("This app allows you to calculate the depth or data needed for WGS"),
                "Enter value for either Output Required or Depth Given on the side",
                br(),br(),
                h4("Megabases of sequencing data"),
                textOutput("WGScov1_res"),
                
                br(),br(),
                h4("Coverage of genome"),
                textOutput("WGScov2_res")
        ),
        # # 
        # tabItem(tabName = "WGScov1",
        #         "Enter value for either Output Required or Depth Given",
        #         br(),br(),
        #         "Megabases needed is:", 
        #         textOutput("WGScov1_res"),
        #         "megabases"
        #         
        # ),
        # tabItem(tabName = "WGScov2",
        #         "Enter value for either Output Required or Depth Given",
        #         br(),br(),
        #         "Coverage given is:", 
        #         textOutput("WGScov2_res"),
        #         "X"
        #         
        # ),
        
        tabItem(tabName = "pcr",
                h3("This app allows you to calculate the optimal PCR reaction"),
                h4("If you have nonspecific amplification, esitmate the effect size and adjust the input quantity accordingly"),
                # h3("Click the sidebar to get started"),
                
                br(),br(),
                
                plotlyOutput("PCRplot"),
                br(),
                box(title = "Detailed Information",  width = 12, collapsed = F, collapsible=TRUE,
                    DTOutput("PCRDT")),
                
                br(),br(),
                
                
        )
    )
)


dashboardPage(
    title=title,
    header,
    sidebar,
    body
)



