convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

calcCoverage <- function(amplicon_len = 600, average_read_len = NA, amplicons = 1, gb_output = 2, percent_loss = 20, samples = 1) {
  # gb_output is gigabases
  if (is.na(average_read_len))
    average_read_len <- amplicon_len
  
  print(average_read_len)
  
  gb_output <- gb_output * 1000000000
  gb_output <- gb_output - (gb_output*percent_loss/100)
  amp_dat <- ceiling((amplicon_len/average_read_len))*samples
  
  round(gb_output/(amp_dat*amplicons*average_read_len), 0)
}



# inputs in megabases
cov_needed <- function(coverage = 10, #X
                       region_size, 
                       duplicates = 2, #%
                       on_target = 90 #%
){
  # Calculation of Results Using Coverage Needed
  Output_required <- region_size * coverage / ((1-duplicates/100) * on_target/100)
  return(round(Output_required))
}


# input must be in megabases
cov_given <- function(Output_given,
                      region_size, 
                      duplicates = 2, #%
                      on_target = 90 #%
){
  coverage <- (Output_given * ((1-duplicates/100) * on_target/100))/region_size
  return(round(coverage))
}




