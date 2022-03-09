require(shiny)
require(shinyjs)
require(DT)
require(plotly)

shinyServer(function(input, output, session){
    
    output$calcov <- renderText({
        print(c(paste0("Your expected average coverage for each of the ", input$amps, " amplicons of length ", input$amp_len, " for each of the ", input$samples, " samples is: "),
                calcCoverage(amplicon_len = input$amp_len, amplicons = input$amps,
                             gb_output = input$out, percent_loss = input$loss, samples = input$samples)
        ))
    })
    
  
    output$WGScov1_res <-  renderText({
        paste0("Based an input coverage of ", input$GcovX, "X, ",
               "a genome size of ", input$Gsize, "Mb , ",
               "a duplication rate of ", input$Gdup, "% , ",
               "and an on target rate of ", input$Gtar, "%, ",
               "You will need: ", 
        cov_needed(coverage = input$GcovX,
                              region_size = input$Gsize,
                              duplicates = input$Gdup,
                              on_target = input$Gtar),
        " megabases of sequencing data")
    })
    
    output$WGScov2_res <-  renderText({
        paste0(
            "Based an sequencing output of ", input$GcovG, "Mb, ",
               "a genome size of ", input$Gsize, "Mb , ",
               "a duplication rate of ", input$Gdup, "% , ",
               "and an on target rate of ", input$Gtar, "%, ",
               "You will need: ", 
            
            
            "Coverage given is: ", 
        cov_given(Output_given = input$GcovG,
                             region_size = input$Gsize,
                             duplicates = input$Gdup,
                             on_target = input$Gtar),
        "X")
    })
    
    
    output$nano_est <- renderDT({
        est <- tibble::tribble(
                     ~'Flow Cell', ~`Max.Output (Gb)`, ~`Expected Output`,
                 "Flongle R9.4",                 2L,              "1",
                  "MinION R9.4",                50L,              "?",
                 "MinION R10.3",                50L,              "?"
                 )

        datatable(est, rownames = F, options = list(dom = 't'))
    })
    
    
    
    
    ### PCR opt
    
    pcr_dat <- reactive({
        # # https://www.bioline.com/media/calculator/01_14.html
        # PCR Optimization
        
        
        
        # Assumptions 
        # Reaction conditions are close to normal, so it is not necessary to worry about the course of the reaction (e.g. too much polymerase can lead to unspecific amplification; too much primers can lead to primer-dimers).
        # A, T and G, C are equivalently presented in PCR product.
        # There are no primer-dimers.
        # Taq polymerase does not loose activity during the reaction.
        
        
        # len=length of the PCR product is "L" [bp]
        # dNTPs=dNTP's concentration is "c" [mM]
        # pconc=primers quantity is "q" [pmol]
        # pol=quantity of Taq polymerase is "a" [u]
        # time=elongation time is "t" [s]
        # vol=reaction volume is "V" [µl]
        # mo=template quantity is "mo"
        
        ################
        #### Inputs ####
        len <- input$len
        dNTPs <- input$dNTPs
        pconc <- input$pconc
        pol <- input$pol
        time <- input$time
        vol <- input$vol
        mo <- input$mo
        
        # len <- 2000
        # dNTPs <- 0.2
        # pconc <- 20
        # pol <- 5
        # time <- 60
        # vol <- 100
        # mo <- 0.1310
        # 
        
        time <- time/60
        
        #make it fg
        if (input$unit != "copies"){
            mol <- data.frame(
                Val = c("nmol","pmol","fmol","amol","zmol","ng",
                        "pg","fg","ag","zg"),
                Factor = c(1, 0.001, 1, 1000, 1, 1, 0.001, 1, 1000, 1)
            )
            
            if (input$unit %in% c("nmol", "pmol", "fmol", "amol", "zmol")){
                mo <- mo * mol[mol$Val == input$unit, ]$Factor
            }else{
                mo <- mo * mol[mol$Val == input$unit, ]$Factor
                mo <- (mo) * ((len * 617.96) + 36.04)
            }
            
        }else{
            # DNA copy number = moles of dsDNA x 6.022e23 molecules/mol
            moles <- mo/(6.022 * 10^23)
            # moles dsDNA (mol) = mass of dsDNA (g)/((length of dsDNA (bp) x 617.96 g/mol/bp) + 36.04 g/mol)
            cn <- moles * ((len * 617.96) - 36.04)
            mo <- cn * 10^15
        }
        
        ################
        
        outputs <- data.frame(
            Variable = 0,
            Value = 0
        )
        
        
        # maximal yield is the minimum from two evaluations:
        # if all nucleotides will be consumed:
        # mn = 4[nucleotides] x c[mmol/l] 324.5[g/mol] x V[µl] = 1300cV [ng]
        mn <- 1298 * dNTPs * vol
        outputs <- rbind(outputs, c("ng product if all dNTPs consumed", mn))
        
        
        
        # also output as conc
        
        # if all primers will be consumed:
        # mp = q[pmol] x 2[strands] 324.5[g/mol] x L[kbp] = 650qL [ng]
        mp <- 649 * pconc * len/1000
        outputs <- rbind(outputs, c("ng product if all primers consumed", mp))
        
        # also output as conc
        
        
        mm <- min(c(mn, mp))
        
        
        # maximum quantity of PCR product per one cycle depends on two factors:
        # Taq polymerase velocity: 2-4[kbp/min];
        # Taq polymerase activity (1 u is the amount of enzyme, that incorporate 10nmol of all four dNTP’s in 30 min at 72oC).
        # mcycle = 10[nmol] x 324.5[g/mol] x a [u] t[min] / 30[min] = 108at [ng]
        # Q5 is ~30s/kb
        # One unit is defined as the amount of enzyme that will incorporate 10 nmol of dNTP into acid insoluble material in 30 minutes at 74°C
        mcycle <- 108.1666 * pol * time
        outputs <- rbind(outputs, c("ng maximum quantity of PCR product per cycle", round(mcycle, 0)))
        
        # the relationship of mass and mole quantities is:
        # m[µg] = 649[g/mol] x q[µmol] x L[kbp] x 1000
        # m <- 649 * len * pconc/1000000
        
        
        # the number of cycles, which are necessary for synthesis of "mmax" PCR-product is:
        # mmax = 2n x mo     =>     n = ln(mmax/mo)/ln2
        # pcr.nc.value  = 1 + Math.floor(Math.log(D*mm/m)/Math.log(2))
        D <- 1e9 #fg
        mm <- mm/1000
        
        nc <- 1 + floor(
            log(D*mm/mo)/log(2)
        )
        outputs <- rbind(outputs, c("For the ideal doubling in each cycle, it would be necessary to perform ", nc))
        
        # pcr.na.value = 1 + Math.floor(Math.log(D*mc/m)/Math.log(2))
        na <- 1 + floor(
            log(D*mcycle/1000/mo)/log(2)
        )
        outputs <- rbind(outputs, c("Amplification will be linear due to the restriction by polymerase activity after ", na))
        outputs <- outputs[-1,]
        
        
        # dsDNA
        # number of copies = ( ng_amount * 6.022x1023) / (length * 1x109 * 650)
        
        copies <- ((mo/1e+06) * 6.022*10^23) / (len * 10^9 * 649)
        
        
        outputs <- rbind(outputs, c("Starting copies: ", copies))
        perfect <- copies * 2^nc
        outputs <- rbind(outputs, c("Copies assuming perfect amplification: ", perfect))
        outputs <- rbind(outputs, c("ug assuming perfect amplification: ", 
                                    ((perfect * (len * 10^9 * 649))/(6.022*10^23))*1000)) #for ng 1e+06
        
        linear_cycles <- 0
        per_loss <- input$per_loss #percent loss per cycle
        per_loss <- ifelse(per_loss <=0, 0.001, per_loss)
        per_loss <- per_loss/100
        # pre_loss_add <- 0.02
        linear_noise <- input$linear_noise #0.2
        copies_cycle <- NULL
        noise <- 0
        per <- copies
        for (i in 1:nc){
            # per_loss <- per_loss + runif(1, 0, pre_loss_add)
            per_loss <- per_loss + per_loss*runif(1, 0.3, 1)
            if (per_loss > 0.5)
                per_loss <- 0.5
            if (i < na){
                # copies <- copies * 2
                copies <- copies + (copies * (1 - per_loss))
            }else{
                # linear_cycles <- linear_cycles + 1
                noise <- runif(1, 0, linear_noise)
                loss <- 1 - (per_loss + noise)
                if (loss > 0){
                    copies <- copies + (copies * loss)
                }else{
                    copies <- copies + (copies * noise)
                }
            }
            copies_cycle <- c(copies_cycle, copies)
            per <- c(per, tail(per, n=1)*2)
        }
        
        
        outputs <- rbind(outputs, c(paste0("Copies assuming ", ceiling(na), " rounds of exponential amplification: "), copies))
        outputs <- rbind(outputs, c(paste0("ug assuming ", ceiling(na), " rounds of exponential amplification: "), 
                                    ((copies * (len * 10^9 * 649))/(6.022*10^23))*1000)) #for ng 1e+06
        outputs <- rbind(outputs, c(paste0("Percent efficiency assuming ", ceiling(na), " rounds of exponential  amplification: "), round(copies/perfect*100,0)))
        
        list(outputs, copies_cycle, per)
        
        
    })
    
    
    
    output$PCRplot <- renderPlotly({
      copies_cycle <- pcr_dat()[[2]]
      per <- pcr_dat()[[3]]
      
      # p.tmp <- plot_ly(x = ~1:length(copies_cycle), y = ~copies_cycle, type = 'scatter', mode = 'lines+markers', name = "estimated") %>% 
      #   add_trace(y = ~per[-1], type = 'scatter', mode = 'lines+markers', name = "optimal") %>%
      #   layout(yaxis = list(title = "Copies"),
      #          xaxis = list(title = "Cycle")
      #   ) %>%
      #   layout(plot_bgcolor='rgb(254, 247, 234)') %>%
      #   layout(paper_bgcolor='rgb(254, 247, 234)')
      
      copies_cycle <- round(copies_cycle, 0)
      per <- round(per, 0)
      fmol <- round((copies_cycle/6.022e23)*1e15, 0)
      
      dat <- data.frame(copies_cycle = copies_cycle,
                        Cycle = 1:length(copies_cycle),
                        fmol = fmol)
      
      p.tmp <- dat %>% 
        plot_ly(x = ~Cycle, y = ~copies_cycle, type = 'scatter', mode = 'lines+markers', name = "estimated",
                text = ~paste('<br><b>Copies</b>: ', format(copies_cycle, scientific = T),
                              '<br><b>fmol</b>: ', fmol
                ),
                hovertemplate = paste('<b>Copies</b>: %{y:.0f}',
                                      '%{text}',
                                      '<br><b>Cycle</b>: %{x}<br>'
                )
        ) %>% 
        add_trace(y = ~per[-1], type = 'scatter', mode = 'lines+markers', name = "optimal") %>%
        layout(yaxis = list(title = "Copies"),
               xaxis = list(title = "Cycle")
        ) %>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)') %>% 
        layout(legend = list(orientation = 'h'))
      
      
      if (input$log_scale)
        p.tmp <- p.tmp %>% 
        layout(yaxis = list(type = 'log'))
      
      p.tmp
    })
    
    
    output$PCRDT <- renderDT({
        out.tmp <- pcr_dat()[[1]]
        # test <<- out.tmp
        out.tmp$Value <- round(as.numeric(as.character(out.tmp$Value)), 0)
        datatable(out.tmp,  extensions = 'Buttons', options = list(buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                                   dom = 't',
                                                                   pageLength = 11),
                  rownames = F
        )
    })
    ### PCR opt end

    
    
})



