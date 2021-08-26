library(shinydashboard)
library(openxlsx)
library(DT)
library(tidyverse)



# returns listing of files in data directory
directorycontent <- function() {
  return(list.files("Pending_Logs"))
}

# return the file name of the most recent file in the data directory
# used as check function when polling, since the file name will be 
# different when the contents change
latestfilename <- function() {
  tmpshot <- directorycontent()
  
  newestfile <- tail(tmpshot, 1 )
  return(newestfile)
}

# return the contents of the most recent file in the data directory
# used as a value function when polling
sphlatestfilecontents <- function() {
  tmpshot <- fileSnapshot("Pending_Logs")
  
  newestfile <- rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
  
  pathtomostrecent <- paste("Pending_Logs", newestfile, sep = "/")
  
  # find time of creation of file
  timestamp <- file.mtime(pathtomostrecent)
  
  justtime <-strftime(timestamp, format="%H:%M:%S")
  
  #beginning of morning and night shifts
  sunrise <- '2021-06-17 5:00:00 PDT'
  
  morning <- strftime(sunrise, format="%H:%M:%S")
  
  sundown <- '2021-06-17 23:00:00 PDT'
  
  night <- strftime(sundown, format="%H:%M:%S")
  
  mostrecent <- read.xlsx(pathtomostrecent) %>%
    
    mutate(Collect = convertToDateTime(Collect)) %>%
    mutate(Receive = convertToDateTime(Receive)) %>%
    
    # filter out samples not received
    filter(!is.na(Receive)) %>%
    #TSH
    filter(!(Test == 'TSH' & MinFromReceipt <= 40 & (Location != 'SPHEH' | 
                                                       Location != 'SPHED') 
             & (morning <= justtime)
             & (justtime <= night))) %>%
    # QA EXCLU
    filter(!((Location == 'QA' | Location == 'TEST') & (morning <= justtime)
             & (justtime <= night))) %>%
    # CHEM EXCLU
    filter(!(Test == 'TRFE' | Test == 'PALB' | Test == 'TRIGF' | Test == 'HDLC' | 
               Test == 'NONHDL' | Test == 'CHOL' | Test == 'TFER' | Test == 'FT3' |
               Test == 'FER' | Test == 'MALBR' | Test == 'LIPID' | Test == 'LDLC' |
               Test == 'LIPCOM' | Test == 'FE' | Test == 'PTHI' | Test == 'COR60'|
               Test == 'COR30' | Test == 'COAM' | Test == 'COPM' | Test == 'CORF' |
               Test == 'CORT' | Test == 'FT3' | Test == 'FT4')) %>%
    
    #BGAS
    filter(!((Test == 'VBG' | Test == 'MVBG' | Test == 'FOHB' | Test == 'VO2HB'|
                Test == 'ABGAS'| Test == 'VO2' | Test == 'ABGASP' | Test == 'OHGB' |
                Test == 'MO2HB' | Test == 'ABGO' | Test == 'BGC' | Test == 'ABGCO'|
                Test == 'VBGO' | Test == 'MVBGO' | Test == 'CBGAC') & 
               (MinFromReceipt < 15) & 
               !((morning <= justtime) & (justtime <= night)))) %>%
    
    # HEM EXCL
    filter(!(Test == 'MORF' | Test == 'G6PB' | Test == 'MALSP' | Test == 'AMOR' |
               Test == 'MORFX' | Test == 'DIF' | Test == 'MALS' | Test == 'SCR')) %>%
    # NONBLOOD EXCL
    filter(!(Test == 'FTRIG' | Test == 'CLU' | Test == 'CAR' | Test == 'NAU' |
               Test == 'UARU' | Test == 'PO4U' | Test == 'UE3U' | Test == 'PO4RU' |
               Test == 'MGU' | Test == 'STPHOS' | Test == 'UE3' | Test =='CRU' |
               Test == 'CAU' | Test == 'STE2' | Test == 'KU' | Test == 'PRCRR' | 
               Test == 'NAR' | Test == 'FCHOL' | Test == 'URU' | Test == 'MGRU' |
               Test == 'STMG')) %>%
    # PIA1
    filter(!((Test == 'CTROPT' | Test == 'HCG' | Test == 'LBNP') & MinFromReceipt 
             <= 40)) %>%
    # filter out MSJ
    filter(!(str_detect(Location, 'MSJ'))) %>%
    # CSF HEM
    filter(!(Test == 'FCSF' & MinFromReceipt <= 60)) %>%
    # CSF CHEM
    filter(!((Test == 'CSGL' | Test == 'CSLA' | Test == 'CSFTP' | Test == 'CBIL') &
               MinFromReceipt <= 45 & !((morning <= justtime)
                                        & (justtime <= night)))) %>%
    filter(!((Test == 'CSGL' | Test == 'CSLA' | Test == 'CSFTP' | Test == 'CBIL') &
               MinFromReceipt <= 60 & (morning <= justtime)
             & (justtime <= night))) %>%
    # hidden column to determine color
    mutate(background_colour = 
             if_else(Test == 'RMP' & MinFromReceipt >= 1, 'magenta',
                     if_else((Test == 'VBG' | Test == 'VBGO' | Test == 'V02HB' |
                                Test == 'BGC' | Test == 'MO2HB' | Test == 'MVBG' |
                                Test == 'MVBGO' | Test == 'ABGASP' | Test == 'ABGAS' |
                                Test == 'ABGCO' | Test == 'ABGO' | Test == 'FOHB' |
                                Test == 'CBGAC' | Test == 'CBGVC' | Test == 'OHGB') & 
                               MinFromReceipt >= 15, 'magenta',
                             if_else((Test == 'HB' | Test == 'CBC') & 
                                       MinFromReceipt >= 25, 'red',
                                     if_else((Priority == 'S' | Priority == 'U') & 
                                               MinFromReceipt >= 60, 'red',
                                             if_else(Test == 'CSGL' | Test == 'CSLA' | Test == 'CSFTP' | 
                                                       Test == 'CBIL' & 
                                                       MinFromReceipt >= 60, 'red',
                                                     if_else((Location == 'SPHEH' | Location == 'SPHED' |
                                                                Location == 'SPHNICU' | Location == 'SPHOR') & 
                                                               MinFromReceipt >= 60, 'red','none')))))))
  
  sph_count_table <- count(mostrecent, Location, sort = FALSE) 
  
  # Calculate the spacer placement by counting rows of the blocks
  
  sph_eh_row <- which(sph_count_table[,1] == "SPHEH")
  if (length(sph_eh_row) > 0) { sph_eh_count = sph_count_table[sph_eh_row, 2] 
  } else { 
    sph_eh_count = 0 }
  
  sph_ed_row <- which(sph_count_table[,1] == "SPHED")
  if (length(sph_ed_row) > 0) { sph_ed_count = sph_count_table[sph_ed_row, 2] 
  } else { 
    sph_ed_count = 0 }
  
  sph_icu_row <- which(sph_count_table[,1] == "SPHICU")
  if (length(sph_icu_row) > 0) { sph_icu_count = sph_count_table[sph_icu_row, 2] 
  } else { 
    sph_icu_count = 0 }
  
  sph_spacer_1 <- sph_eh_count                                                                                                                                                                                     
  sph_spacer_2 <- sph_ed_count + sph_icu_count + sph_eh_count + 1 
  
  # No tests vector in case of no sph tests
  NoTests <- c('No Tests')
  
  # Arrange so that ED and ICU are at the top and the rest is chronological
  if (nrow(mostrecent) > 0){
    mostrecent$location_type <- "WARD"
    mostrecent$location_type[str_detect(mostrecent$Location, "ED")] <- "EDICU"
    mostrecent$location_type[str_detect(mostrecent$Location, "SPHICU")] <- "EDICU"
    mostrecent$location_type[str_detect(mostrecent$Location, "EH")] <- "1EDICU"
    mostrecent <- arrange(mostrecent, location_type, desc(MinFromReceipt)) %>%
      # add spacer rows
      add_row(Accession = '-', .after = sph_spacer_1) %>%
      add_row(Accession = '-',.after = sph_spacer_2)
    
    sphfilecontents <- datatable(select(mostrecent, -location_type), 
                                 rownames = FALSE, options = list(
                                   columnDefs = list(list(visible=FALSE,targets=10)), 
                                   pageLength = 100, dom = 't', lengthChange = FALSE)) %>%
      
      # styling  based on background_colour column
      formatStyle('background_colour', target ='row', backgroundColor = 
                    styleEqual(c('magenta', 'red', 'none'),c('#e892db', 
                                                             '#d17979', 
                                                             'white'))) %>%
      formatStyle('background_colour', target ='row', Color = 
                    styleEqual(c('magenta', 'red', 'none'), c('white', 'white', 
                                                              'none'))) %>%
      formatStyle('background_colour', target ='row', fontWeight = 'bold') %>%
      formatDate('Collect', method = "toLocaleString", params = NULL) %>%
      formatDate('Receive', method = "toLocaleString", params = NULL)}else{
        sphfilecontents <- datatable(data.frame(NoTests), rownames = FALSE, 
                                     options = list(dom = 't')) %>%
          formatStyle('NoTests', target ='row', fontWeight = 'bold')}
  
  
  return(sphfilecontents)
}

msjlatestfilecontents <- function() { tmpshot <- fileSnapshot("Pending_Logs")

newestfile <- rownames(tmpshot$info[which.max(tmpshot$info$mtime),])

pathtomostrecent <- paste("Pending_Logs", newestfile, sep = "/")

# find time of creation of file
timestamp <- file.mtime(pathtomostrecent)

justtime <-strftime(timestamp, format="%H:%M:%S")

#beginning of morning and night shifts
sunrise <- '2021-06-17 5:00:00 PDT'

morning <- strftime(sunrise, format="%H:%M:%S")

sundown <- '2021-06-17 23:00:00 PDT'

night <- strftime(sundown, format="%H:%M:%S")

mostrecent <- read.xlsx(pathtomostrecent) %>%
  
  mutate(Collect = convertToDateTime(Collect)) %>%
  mutate(Receive = convertToDateTime(Receive)) %>%
  filter(!is.na(Receive)) %>%
  
  # filter out SPH
  filter(!str_detect(Location, 'SPH')) %>%
  
  # filter out samples not received
  filter(!is.na(Receive)) %>%
  
  #TSH
  filter(!(Test == 'TSH' & MinFromReceipt <= 40 & (Location != 'MSJEH' | 
                                                     Location != 'MSJED') 
           & (morning <= justtime)
           & (justtime <= night))) %>%
  # QA EXCLU
  filter(!((Location == 'QA' | Location == 'TEST') & (morning <= justtime)
           & (justtime <= night))) %>%
  # CHEM EXCLU
  filter(!(Test == 'TRFE' | Test == 'PALB' | Test == 'TRIGF' | Test == 'HDLC' | 
             Test == 'NONHDL' | Test == 'CHOL' | Test == 'TFER' | Test == 'FT3' |
             Test == 'FER' | Test == 'MALBR' | Test == 'LIPID' | Test == 'LDLC' |
             Test == 'LIPCOM' | Test == 'FE' | Test == 'PTHI' | Test == 'COR60'|
             Test == 'COR30' | Test == 'COAM' | Test == 'COPM' | Test == 'CORF' |
             Test == 'CORT' | Test == 'FT3' | Test == 'FT4')) %>%
  
  #BGAS
  filter(!((Test == 'VBG' | Test == 'MVBG' | Test == 'FOHB' | Test == 'VO2HB'|
              Test == 'ABGAS'| Test == 'VO2' | Test == 'ABGASP' | Test == 'OHGB'|
              Test == 'MO2HB' | Test == 'ABGO' | Test == 'BGC' | Test == 'ABGCO'|
              Test == 'VBGO' | Test == 'MVBGO' | Test == 'CBGAC') & 
             (MinFromReceipt < 15) & 
             !((morning <= justtime) & (justtime <= night)))) %>%
  
  # HEM EXCL
  filter(!(Test == 'MORF' | Test == 'G6PB' | Test == 'MALSP' | Test == 'AMOR' |
             Test == 'MORFX' | Test == 'DIF' | Test == 'MALS' | Test == 'SCR')) %>%
  # NONBLOOD EXCL
  filter(!(Test == 'FTRIG' | Test == 'CLU' | Test == 'CAR' | Test == 'NAU' |
             Test == 'UARU' | Test == 'PO4U' | Test == 'UE3U' | Test == 'PO4RU' |
             Test == 'MGU' | Test == 'STPHOS' | Test == 'UE3' | Test =='CRU' |
             Test == 'CAU' | Test == 'STE2' | Test == 'KU' | Test == 'PRCRR' | 
             Test == 'NAR' | Test == 'FCHOL' | Test == 'URU' | Test == 'MGRU' |
             Test == 'STMG')) %>%
  # PIA1
  filter(!((Test == 'CTROPT' | Test == 'HCG' | Test == 'LBNP') & MinFromReceipt 
           <= 40)) %>%
  # CSF HEM
  filter(!(Test == 'FCSF' & MinFromReceipt <= 60)) %>%
  # CSF CHEM
  filter(!((Test == 'CSGL' | Test == 'CSLA' | Test == 'CSFTP' | Test == 'CBIL') &
             MinFromReceipt <= 45 & !((morning <= justtime)
                                      & (justtime <= night)))) %>%
  filter(!((Test == 'CSGL' | Test == 'CSLA' | Test == 'CSFTP' | Test == 'CBIL') &
             MinFromReceipt <= 60 & (morning <= justtime)
           & (justtime <= night))) %>%
  
  # hidden background colour column
  mutate(background_colour = 
           if_else(Test == 'RMP' & MinFromReceipt >= 1, 'magenta',
                   if_else((Test == 'VBG' | Test == 'VBGO' | Test == 'V02HB' |
                              Test == 'BGC' | Test == 'MO2HB' | Test == 'MVBG' |
                              Test == 'MVBGO' | Test == 'ABGASP' | Test == 'ABGAS' |
                              Test == 'ABGCO' | Test == 'ABGO' | Test == 'FOHB' |
                              Test == 'CBGAC' | Test == 'CBGVC' | Test == 'OHGB') & 
                             MinFromReceipt >= 15, 'magenta',
                           if_else((Test == 'HB' | Test == 'CBC') & 
                                     MinFromReceipt >= 25, 'red',
                                   if_else((Priority == 'S' | Priority == 'U') & 
                                             MinFromReceipt >= 60, 'red',
                                           if_else(Test == 'CSGL' | Test == 'CSLA' | Test == 'CSFTP' | 
                                                     Test == 'CBIL' & 
                                                     MinFromReceipt >= 60, 'red',
                                                   if_else((Location == 'SPHEH' | Location == 'MSJED' |
                                                              Location == 'MSJNICU' | Location == 'MSJOR') & 
                                                             MinFromReceipt >= 60, 'red','none')))))))



#  Calculate the spacer placement by counting rows of the blocks

msj_count_table <- count(mostrecent, Location, sort = FALSE) 

msj_eh_row <- which(msj_count_table[,1] == "MSJEH")
if (length(msj_eh_row) > 0) { msj_eh_count = msj_count_table[msj_eh_row, 2] 
} else { 
  msj_eh_count = 0 }

msj_ed_row <- which(msj_count_table[,1] == "MSJED")
if (length(msj_ed_row) > 0) { msj_ed_count = msj_count_table[msj_ed_row, 2] 
} else { 
  msj_ed_count = 0 }

msj_icu_row <- which(msj_count_table[,1] == "MSJICU")
if (length(msj_icu_row) > 0) { msj_icu_count = msj_count_table[msj_icu_row, 2] 
} else { 
  msj_icu_count = 0 }

msj_spacer_1 <- msj_eh_count
msj_spacer_2 <- msj_ed_count + msj_icu_count + msj_eh_count + 1

# No tests vector in case of no msj tests
NoTests <- c('No Tests')

# Arrange so that ED and ICU are at the top
if (nrow(mostrecent) > 0){
  mostrecent$location_type <- "WARD"
  mostrecent$location_type[str_detect(mostrecent$Location, "ED")] <- "EDICU"
  mostrecent$location_type[str_detect(mostrecent$Location, "MSJICU")] <- "EDICU"
  mostrecent$location_type[str_detect(mostrecent$Location, "EH")] <- "1EDICU"
  mostrecent <- arrange(mostrecent,location_type,desc(MinFromReceipt)) %>%
    add_row(Accession = '-', .after = msj_spacer_1) %>%
    add_row(Accession = '-', .after = msj_spacer_2)
  
  
  msjfilecontents <- datatable(select(mostrecent, -location_type), 
                               rownames = FALSE, options = 
                                 list(columnDefs = 
                                        list(list(visible=FALSE,targets=10)), 
                                      pageLength = 100, dom = 't', lengthChange = FALSE)) %>%
    
    # styling  based on background_colour column
    formatStyle('background_colour', target ='row', backgroundColor = 
                  styleEqual(c('magenta', 'red', 'none'),c('#e892db', 
                                                           '#d17979', 
                                                           'white'))) %>%
    formatStyle('background_colour', target ='row', Color = 
                  styleEqual(c('magenta', 'red', 'none'), c('white', 'white', 
                                                            'none'))) %>%
    formatStyle('background_colour', target ='row', fontWeight = 'bold') %>%
    formatDate('Collect', method = "toLocaleString", params = NULL) %>%
    formatDate('Receive', method = "toLocaleString", params = NULL) }else{
      # data table with 'No Tests' 
      msjfilecontents <- datatable(data.frame(NoTests), rownames = FALSE, 
                                   options = list(dom = 't')) %>%
        formatStyle('NoTests', target ='row', fontWeight = 'bold')}

return(msjfilecontents)}

mostrecentupdate <- function() {
  
  tmpshot <- fileSnapshot("~/Downloads/Pending_Logs")
  
  newestfile <- rownames(tmpshot$info[which.max(tmpshot$info$mtime),])
  
  pathtomostrecent <- paste("~/Downloads/Pending_Logs", newestfile, sep = "/")
  
  # find time of creation of file
  timestamp <- file.mtime(pathtomostrecent)
  
  return(toString(timestamp))
}


server <- function(input, output, session) {
  
  sphfiledata <- reactivePoll(1000, session, latestfilename, sphlatestfilecontents)
  msjfiledata <- reactivePoll(1000, session, latestfilename, msjlatestfilecontents)
  latestupdate <- reactivePoll(1000 * 60, session, latestfilename, mostrecentupdate)
  
  output$sphdatatable <-renderDataTable({sphfiledata()})
  output$msjdatatable <-renderDataTable({msjfiledata()})
  output$sphlatestupdate <- renderText({latestupdate()})
  output$msjlatestupdate <- renderText({latestupdate()})
  
}