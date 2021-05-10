#Packages to be installed and loaded############################
library(dplyr)
library(plyr)
library(tibble)
library(FSA)
library(formattable)
library(shinyjs)
library(DT)

# .csv's to upload
sampleTemplate <- read.csv("sampleTemplate.csv")
ageTemplate <- read.csv("ageTemplate.csv")
lakeCodes <- read.csv("lakeinfo.csv")
gearCodes <- read.csv("gearinfo.csv")
speciesCodes <- read.csv("speciesinfo.csv")
WSnames <- read.csv("WSnames.csv")


function(input, output, session) {
  
# Adds ability to upload files greater than 5 MB (max now 100 MB)
options(shiny.maxRequestSize=100*1024^2)
  
# Downloadable csv of sample data template
  output$sampleTemplate <- downloadHandler(
    filename = function() {
      paste("sampleTemplate", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(sampleTemplate, file, row.names = FALSE)
    }
  )

# Downloadable csv of age data template
output$ageTemplate <- downloadHandler(
  filename = function() {
    paste("ageTemplate", "csv", sep = ".")
  },
  content = function(file) {
    write.csv(ageTemplate, file, row.names = FALSE)
  }
)
  
# read in .csv of sample data from file input
  sampleData <- reactive({
    sampleData <- input$sampleData
    if(is.null(sampleData)){
      return(NULL)
    } else{
      #sampleData <- read.csv(sampleData$datapath)
      sampleData <- read.csv(sampleData$datapath, na.strings = c("","NA"))#automatically converst blanks and "NA" to NA's...but will flag these as needing to be ".".  Code for exporting validated file will also convert "." to NA
      sampleData <- mutate(sampleData, Gear.Code = as.numeric(as.character(Gear.Code)),
                           Number.of.individuals = as.numeric(as.character(Number.of.individuals)))

      sampleData$Station[sampleData$Station=="."] <- NA #need this to test for missing station as this should never be blank, but cannot get test for blank station code to check for "." for some reason...so this fixes that issue
      #below code was when we had button on shiny app to convert "." to NA.  We decided to just require "." rather than blanks or NA so this is not needed and was removed
      # if(input$period2NA == TRUE){
      #   sampleData[sampleData == "."] <- NA
      # }
      sampleData
    }
  })
  
# read in .csv of age data from file input
ageData <- reactive({
  ageData <- input$ageData
  if(is.null(ageData)){
    return(NULL)
  } else{
    ageData <- read.csv(ageData$datapath, na.strings = c(".","NA"))
    #ageData <- read.csv(ageData$datapath)
    ageData <- mutate(ageData, Gear = as.numeric(as.character(Gear)),
                      Number.of.individuals = as.numeric(as.character(Number.of.individuals)))
    ageData[ageData == "."] <- NA #replace periods with "NA"...now hard coded rather than using next 3 lines and check box
      #we are not requiring "." for age data...basically nothing should be blank, "NA" or ".", so this is ok to convert here so we can
      #check for NA's using code below and capture true NA, "", and "." at same time
    # if(input$ageperiod2NA == TRUE){
    #   ageData[ageData == "."] <- NA
    # }
    ageData
  }
})
  
# Download validated sample data
  output$valSampleData <- downloadHandler(
    filename = function() {
      paste(date(), "validatedSample.csv", sep = ".")
    },
    content = function(file) {
      sampleData <- sampleData()
      sampleData[sampleData == "."] <- NA #Automatically convert period to NA's
      sampleData[sampleData == "NA"] <- NA
      sampleData$SampleID <- paste(sampleData$Lake.Code, sampleData$Station, sampleData$Month,
                                   sampleData$Day, sampleData$Year, sampleData$Gear.Code,
                                   sep = "")
      sampleData <- sampleData[,c(1,19,2:18)]
      write.csv(sampleData, file, row.names = FALSE)
    }
  )
  
# Download validated age data
output$valAgeData <- downloadHandler(
  filename = function() {
    paste(date(), "validatedAge.csv", sep = ".")
  },
  content = function(file) {
    ageData <- ageData()
    write.csv(ageData, file, row.names = FALSE)
  }
)

#creates table for display at bottom of pg that can be filtered for errors...includes calculated Wr
sampleDataDisplay<- reactive({
  if (is.null(sampleData())) {NULL}
      else{
  sampleDataDisplay <- sampleData()
  sampleDataDisplay <- join(sampleDataDisplay, WSnames, by="Species.Code")#adds text that matches species code to Ogles required species for Ws equations#
  sampleDataDisplay$ID <- seq.int(nrow(sampleDataDisplay))#create an "ID" column that numbers rows
  wr_data <- sampleDataDisplay[!is.na(sampleDataDisplay$TL_mm) & !is.na(sampleDataDisplay$Wt_g)
                               & !is.na(sampleDataDisplay$wsname),] #filter to only rows that have needed values for Wr calculation
  wr_data["Wr"] <- (wrAdd(as.numeric(as.character(Wt_g)) ~ as.numeric(as.character(TL_mm)) + wsname,
                          units = "metric", data = wr_data))#calculates Wr value
  WrData <- subset(wr_data, select=c("ID", "Wr"))
  sampleDataDisplay <- join(sampleDataDisplay, WrData, by="ID")
  sampleDataDisplay["row.numb"] <- sampleDataDisplay$ID +1
  sampleDataDisplay["Row.numb"] <- sampleDataDisplay$ID +1
  sampleDataDisplay <- sampleDataDisplay %>% select(Row.numb,everything())
  sampleDataDisplay["ID"] <- NULL
  sampleDataDisplay["wsname"] <- NULL
  sampleDataDisplay$Wr <- round(sampleDataDisplay$Wr, 2)
  sampleDataDisplay
       }
  })

#output$sampleDataDisplayTable <- renderDataTable({sampleDataDisplay()})#being depreciated and caused version upgrade issue, use DT package instead
output$sampleDataDisplayTable <- DT::renderDataTable({DT::datatable(sampleDataDisplay(), rownames = FALSE)})

  
# Run data validation function when action button pressed#####################
  
  validateSample <- reactive({

      errorTable <- data.frame(0, "Okay", stringsAsFactors = FALSE)
      colnames(errorTable) <- c("Error", "Status")
      errorTable <- errorTable
      
      sampleData <- sampleData()
      
      # Check if blank cells exist###########################################
      #sampleData$invalidBlanks <- apply(sampleData=="EMPTY", 1, which)#identifies columns with blanks on each row
        #problem with above line...if no blanks exist, it does not produce the column...not sure why.
        #running it outside of shiny gives error I need to debug but code below works so Dan is using this instead
      #sampleData$invalidBlanks <- sampleData$invalidBlanks!="integer(0)" #replaces value with True if blanks existed, False if no blanks
      # sampleData <- sampleData %>%mutate(invalidBlanks=ifelse(Lake.Code=="EMPTY",TRUE,ifelse(SampleID=="EMPTY",
      #    TRUE, ifelse(Month=="EMPTY",TRUE, ifelse(Day=="EMPTY",TRUE, ifelse(Year=="EMPTY",TRUE, ifelse(Time=="EMPTY",TRUE,
      #    ifelse(Pool.Elevation=="EMPTY",TRUE, ifelse(Secchi=="EMPTY", TRUE, ifelse(Gear.Code=="EMPTY",TRUE, ifelse(Gear.Length=="EMPTY",
      #    TRUE, ifelse(Number.of.individuals=="EMPTY",TRUE,ifelse(TL_mm=="EMPTY",TRUE, ifelse(Wt_g=="EMPTY",TRUE, FALSE))))))))))))))

      sampleData <- sampleData %>%mutate(invalidBlanks=case_when(is.na(Lake.Code)~TRUE, is.na(Month)~TRUE, 
       is.na(Day)~TRUE, is.na(Year)~TRUE, is.na(Time)~TRUE, is.na(Pool.Elevation)~TRUE,is.na(Surface.Temp)~TRUE,
       is.na(Secchi)~TRUE, is.na(Conductivity)~TRUE, is.na(Gear.Code)~TRUE, is.na(Gear.Length)~TRUE, is.na(Habitat)~TRUE, 
       is.na(Effort)~TRUE, is.na(Species.Code)~TRUE, is.na(Number.of.individuals)~TRUE, is.na(TL_mm)~TRUE, is.na(Wt_g)~TRUE, 
       TRUE~FALSE))
      invalidBlanks <- filter(.data = sampleData, invalidBlanks == "TRUE")
      
      if(nrow(invalidBlanks) == 0){
        okay <- c("Blank Cells instead of periods", "Okay")
      } else{
        okay <- c("Blank Cells instead of periods", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      errorTable <- filter(.data = errorTable, Error != "0")
      sampleData <- sampleData()
      
      # Check if Lake.Code is consistent with defined codes, and not null##########    
      sampleData$invalidLake <- sampleData$Lake.Code %in% lakeCodes$Lake.Code
      invalidLake <- filter(.data = sampleData, invalidLake == "FALSE")
      
      if(nrow(invalidLake) == 0){
        okay <- c("Invalid Lake Code", "Okay")
      } else{
        okay <- c("Invalid Lake Code", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      #errorTable <- filter(.data = errorTable, Error != "0")
      
      sampleData <- sampleData()
      
      # Check if Station is not null (required field)#############################
      sampleData$nullStation <- lapply(sampleData$Station, is.na)
      sampleData$nullStation[sampleData$Station=="."] <-  TRUE
      nullStation <- filter(.data = sampleData, nullStation == "TRUE")
      
      if(nrow(nullStation) == 0){
        okay <- c("Blank Station", "Okay")
      } else{
        okay <- c("Blank Station", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      sampleData <- sampleData()
      
      # Check if Month is within acceptable options (integer 1-12; Required Field)#
      sampleData$invalidMonth <- sampleData$Month %in% c(seq(1,12,1))
      invalidMonth <- filter(.data = sampleData, invalidMonth == "FALSE")
      
      if(nrow(invalidMonth) == 0){
        okay <- c("Invalid Month", "Okay")
      } else{
        okay <- c("Invalid Month", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      sampleData <- sampleData()
      
      # Check if Day is within acceptable options (integer 1-31; Required Field)#
      sampleData$invalidDay <- sampleData$Day %in% c(seq(1,31,1))
      invalidDay <- filter(.data = sampleData, invalidDay == "FALSE")
      
      if(nrow(invalidDay) == 0){
        okay <- c("Invalid Day", "Okay")
      } else{
        okay <- c("Invalid Day", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      sampleData <- sampleData()
      
      # Check if Year is within acceptable options (integer 1980-current year; Required Field)#
      sampleData$invalidYear <- sampleData$Year %in% c(seq(1980,as.integer(format(Sys.Date(), "%Y")),1))
      invalidYear <- filter(.data = sampleData, invalidYear == "FALSE")
      
      if(nrow(invalidYear) == 0){
        okay <- c("Invalid Year", "Okay")
      } else{
        okay <- c("Invalid Year", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      sampleData <- sampleData()
      
      
      # Only one gear code in sample data...needed so we can validate all rows have legit gear length/efforts###################
      checkCode <- length(unique(sampleData$Gear.Code))
      
      if(checkCode == 1){
        okay <- c("One Gear Code", "Okay")
        errorTable <- rbind(errorTable, okay)
        
        # If only one Gear.Code, proceed to check if it is an established Gear.Code and check Gear.Length 
        # Gear.Code is one from established list##########################
        sampleData$invalidGear <- sampleData$Gear.Code %in% gearCodes$Gear.Code
        invalidGear <- filter(.data = sampleData, invalidGear == "FALSE")
        sampleData <- sampleData()
        
        if(nrow(invalidGear) == 0){
          okay <- c("Invalid Gear Code", "Okay")
          errorTable <- rbind(errorTable, okay)
          
          # if Gear.Code is not invalid, we can check Gear.Length and Effort
          # Check Gear.Length according to Gear.Code######################
          # if electrofishing, gear length (minutes of efishing) must be between 1 and 300 (total minutes fished for station sample) and not null
          # if electrofishing, effort (1 unit = 10 min) must be between 0.1 (1-min) and 24 (240 min, 4 hrs) and not null
          if(unique(sampleData$Gear.Code) >= 41){
            invalidLength <- filter(.data = sampleData, Gear.Length > 300 | Gear.Length < 1)
            sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
            sampleData$NAlength[sampleData$Gear.Length=="."] <-  TRUE
            NAlength <- filter(.data = sampleData, NAlength == "TRUE")
            NAlength <- NAlength[,-19]
            invalidLength <- rbind(invalidLength, NAlength)
            
            sampleData <- sampleData()
            
            ###Below was dissabled 3-29-2019 per request from Ashly Nealis...some old historic data had reasons for 
            ###using wierd gear efforts and this column is not needed for CPUE calculation, so it is unecessary for
            ###people to have to fix these.
            invalidEffort <- filter(.data = sampleData, Effort <0) #> 24 | Effort < 0.1)
            sampleData$NAeffort <- lapply(sampleData$Effort, is.na)
            sampleData$NAeffort[sampleData$Effort=="."] <-  TRUE
            NAeffort <- filter(.data = sampleData, NAeffort == "TRUE")
            NAeffort <- NAeffort[,-19]
            invalidEffort <- rbind(invalidEffort, NAeffort)
          }
          # if trapnet, gear length (length of lead), must be > 5 ft and < 150 ft and not null
          # if trapnet, effort (# hrs) must be between 0.1 and 96 and not null
          if(unique(sampleData$Gear.Code) < 40 && unique(sampleData$Gear.Code) > 30){
            ###Below was dissabled 3-29-2019 per request from Ashly Nealis...some old historic data had reasons for 
            ###using wierd gear lengths and this column is not needed for CPUE calculation, so it is unecessary for
            ###people to have to fix these.
            invalidLength <- filter(.data = sampleData, Gear.Length <0) #> 150 | Gear.Length < 5)
            sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
            sampleData$NAlength[sampleData$Gear.Length=="."] <-  TRUE
            NAlength <- filter(.data = sampleData, NAlength == "TRUE")
            NAlength <- NAlength[,-19]
            invalidLength <- rbind(invalidLength, NAlength)
            
            sampleData <- sampleData()
            
            invalidEffort <- filter(.data = sampleData, Effort > 96 | Effort < 0.1)
            sampleData$NAeffort <- lapply(sampleData$Effort, is.na)
            sampleData$NAeffort[sampleData$Effort=="."] <-  TRUE
            NAeffort <- filter(.data = sampleData, NAeffort == "TRUE")
            NAeffort <- NAeffort[,-19]
            invalidEffort <- rbind(invalidEffort, NAeffort)
          }
          # if gillnet, gear length (length of net), must be > 10 ft and < 2000 ft and not null
          # if gillnet, effort (# hrs) must be between 0.1 and 96 and not null
          if(unique(sampleData$Gear.Code) < 30 && unique(sampleData$Gear.Code) > 20){
            ###Below was dissabled 3-29-2019 per request from Ashly Nealis...some old historic data had reasons for 
            ###using wierd gear lengths and this column is not needed for CPUE calculation, so it is unecessary for
            ###people to have to fix these.
            invalidLength <- filter(.data = sampleData, Gear.Length <0)# > 2000 | Gear.Length < 10)
            sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
            sampleData$NAlength[sampleData$Gear.Length=="."] <-  TRUE
            NAlength <- filter(.data = sampleData, NAlength == "TRUE")
            NAlength <- NAlength[,-19]
            invalidLength <- rbind(invalidLength, NAlength)
            
            sampleData <- sampleData()
            
            invalidEffort <- filter(.data = sampleData, Effort > 96 | Effort < 0.1)
            sampleData$NAeffort <- lapply(sampleData$Effort, is.na)
            sampleData$NAeffort[sampleData$Effort=="."] <-  TRUE
            NAeffort <- filter(.data = sampleData, NAeffort == "TRUE")
            NAeffort <- NAeffort[,-19]
            invalidEffort <- rbind(invalidEffort, NAeffort)
          }
          # if seine, gear length (length of seine), must be > 5 ft and < 100 ft and not null
          # if seine, effort (area sampled, ft^2) must be more than 100 and not null
          if(unique(sampleData$Gear.Code) == 10){
            invalidLength <- filter(.data = sampleData, Gear.Length > 100 | Gear.Length < 5)
            sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
            sampleData$NAlength[sampleData$Gear.Length=="."] <-  TRUE
            NAlength <- filter(.data = sampleData, NAlength == "TRUE")
            NAlength <- NAlength[,-19]
            invalidLength <- rbind(invalidLength, NAlength)
            
            sampleData <- sampleData()
            
            invalidEffort <- filter(.data = sampleData, Effort < 100)
            sampleData$NAeffort <- lapply(sampleData$Effort, is.na)
            sampleData$NAeffort[sampleData$Effort=="."] <-  TRUE
            NAeffort <- filter(.data = sampleData, NAeffort == "TRUE")
            NAeffort <- NAeffort[,-19]
            invalidEffort <- rbind(invalidEffort, NAeffort)
          }
          
          # if statement for adding Invalid Gear Length error to table
          if(nrow(invalidLength) == 0){
            okay <- c("Invalid Gear Length", "Okay")
          } else{
            okay <- c("Invalid Gear Length", "Error")
          }
          errorTable <- rbind(errorTable, okay)
          # if statement for adding Invalid Effort error to table 
          if(nrow(invalidEffort) == 0){
            okay <- c("Invalid Effort", "Okay")
          } else{
            okay <- c("Invalid Effort", "Error")
          }
          errorTable <- rbind(errorTable, okay)
          
        } else{
          # if Gear.Code is invalid, we can not check Gear.Length 
          okay <- c("Invalid Gear Code", "Error")
          errorTable <- rbind(errorTable, okay)
        }
      } else{
        # if more than one Gear.Code, invalid, cannot check Gear.Code or Gear.Length
        okay <- c("One Gear Code", "Error")
        errorTable <- rbind(errorTable, okay)
      }
      sampleData <- sampleData()
      
      # Check if species codes are consistent with defined codes, and not null###############
      sampleData$invalidSpp <- sampleData$Species.Code %in% speciesCodes$Species.Code
      invalidSpp <- filter(.data = sampleData, invalidSpp == "FALSE")
      
      if(nrow(invalidSpp) == 0){
        okay <- c("Invalid Species Code", "Okay")
      } else{
        okay <- c("Invalid Species Code", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      sampleData <- sampleData()
      
      # Check if Number of individuals is not 0 and not null #########################
      # if(sampleData$Species.Code==98){
      #   invalidNOI <- NA
      #   sampleData$NAnoi <- FALSE 
      # }else{
      #   invalidNOI <- filter(.data = sampleData, Number.of.individuals < 1)
      #   sampleData$NAnoi <- lapply(sampleData$Number.of.individuals, is.na)
      # }
      invalidNOI <- filter(.data = sampleData, Number.of.individuals < 1)
      sampleData$NAnoi <- lapply(sampleData$Number.of.individuals, is.na)
      sampleData$NAnoi[sampleData$Number.of.individuals=="."] <-  TRUE
      NAnoi <- filter(.data = sampleData, NAnoi == "TRUE")
      NAnoi <- NAnoi[,-19]
      invalidNOI <- rbind(invalidNOI, NAnoi)
      
      if(nrow(invalidNOI) == 0){
        okay <- c("Invalid Number of individuals", "Okay")
      } else{
        okay <- c("Invalid Number of individuals", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      sampleData <- sampleData()

      
      # TL_mm or Wt_g must not = 0...(should be null)####################################################
      invalidTL <- filter(.data = sampleData, TL_mm == 0)
      
      if(nrow(invalidTL) == 0){
        okay <- c("Invalid Total Length", "Okay")
      } else{
        okay <- c("Invalid Total Length", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      
      invalidWt <- filter(.data = sampleData, Wt_g == 0)
      
      if(nrow(invalidWt) == 0){
        okay <- c("Invalid Weight", "Okay")
      } else{
        okay <- c("Invalid Weight", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      sampleData <- sampleData()
      
      # Relative weight between 30 and 180...indicates abnormal length-weight combination############
      sampleData <- join(sampleData, WSnames, by = "Species.Code")
      sampleData <- filter(sampleData, !is.na(TL_mm) & !is.na(Wt_g) & !is.na(wsname) & TL_mm!="." & Wt_g!=".")
      
      #if no records are available for Wr analysis, then "okay", otherwise calc Wr and see if in range 20-200
      if(nrow(sampleData) == 0){
        okay <- c("Invalid Relative Weight", "Okay")
        errorTable <- rbind(errorTable, okay)
      } else{
        sampleData$Wr <- wrAdd(as.numeric(as.character(Wt_g)) ~ as.numeric(as.character(TL_mm)) + wsname, units = "metric", data = sampleData)
        invalidWr <- filter(.data = sampleData, Wr < 20 | Wr >200)
        
        if(nrow(invalidWr) == 0){
          okay <- c("Invalid Relative Weight", "Okay")
        } else{
          okay <- c("Invalid Relative Weight", "Error")
        }
        errorTable <- rbind(errorTable, okay)
      }
      
      sampleData <- sampleData()
      
      errorTable
  })
  
# Render sample error table#######################
  output$sampleError <- renderFormattable({
      if(input$validateSamp != 0){
        validated <- validateSample()
        # function to conditionally color the text
        f1 <- formatter("span",
                        style = ~ ifelse(Status == "Error", "color:red", "color:green"))
        validated <- formattable(validated, list(Status = f1))
      }else{return(NULL)}
  })
  
  
# Return row numbers of errors with checkbox inputs for sample data#####################
  output$Blanks <- renderText({
    if(input$Blanks == TRUE){
      sampleData <- sampleData()
      sampleData <- sampleData %>%mutate(invalidBlanks=case_when(is.na(Lake.Code)~TRUE, is.na(Month)~TRUE, 
       is.na(Day)~TRUE, is.na(Year)~TRUE, is.na(Time)~TRUE, is.na(Pool.Elevation)~TRUE,is.na(Surface.Temp)~TRUE,
       is.na(Secchi)~TRUE, is.na(Conductivity)~TRUE, is.na(Gear.Code)~TRUE, is.na(Gear.Length)~TRUE, is.na(Habitat)~TRUE, 
       is.na(Effort)~TRUE, is.na(Species.Code)~TRUE, is.na(Number.of.individuals)~TRUE, is.na(TL_mm)~TRUE, is.na(Wt_g)~TRUE, 
       TRUE~FALSE))
      Blanks <- rownames_to_column(sampleData, "Rows")
      Blanks <- filter(.data = Blanks, invalidBlanks == "TRUE")
      Blanks <- mutate(.data = Blanks, Rows = as.numeric(as.character(Rows)) + 1)
      c(Blanks$Rows)
    }
  })

  output$sampLake <- renderText({
    if(input$sampLake == TRUE){
      sampleData <- sampleData()
      sampleData$invalidLake <- sampleData$Lake.Code %in% lakeCodes$Lake.Code
      sampLake <- rownames_to_column(sampleData, "Rows")
      sampLake <- filter(.data = sampLake, invalidLake == "FALSE")
      sampLake <- mutate(.data = sampLake, Rows = as.numeric(as.character(Rows)) + 1)
      c(sampLake$Rows)
    }
  })
  
  output$sampStation <- renderText({
    if(input$sampStation == TRUE){
      sampleData <- sampleData()
      sampleData$nullStation <- lapply(sampleData$Station, is.na)
      nullStation <- rownames_to_column(sampleData, "Rows")
      nullStation <- filter(.data = nullStation, nullStation == "TRUE")
      nullStation <- mutate(.data = nullStation, Rows = as.numeric(as.character(Rows)) + 1)
      c(nullStation$Rows)
    }
  })
  
  output$sampMonth <- renderText({
    if(input$sampMonth == TRUE){
      sampleData <- sampleData()
      sampleData$invalidMonth <- sampleData$Month %in% c(seq(1,12,1))
      sampMonth <- rownames_to_column(sampleData, "Rows")
      sampMonth <- filter(.data = sampMonth, invalidMonth == "FALSE")
      sampMonth <- mutate(.data = sampMonth, Rows = as.numeric(as.character(Rows)) + 1)
      c(sampMonth$Rows)
    }
  })
  
  output$sampDay <- renderText({
    if(input$sampDay == TRUE){
      sampleData <- sampleData()
      sampleData$invalidDay <- sampleData$Day %in% c(seq(1,31,1))
      sampDay <- rownames_to_column(sampleData, "Rows")
      sampDay <- filter(.data = sampDay, invalidDay == "FALSE")
      sampDay <- mutate(.data = sampDay, Rows = as.numeric(as.character(Rows)) + 1)
      c(sampDay$Rows)
 
    }
  })
  
  output$sampYear <- renderText({
    if(input$sampYear == TRUE){
      sampleData <- sampleData()
      sampleData$invalidYear <- sampleData$Year %in% c(seq(1980,(as.integer(format(Sys.Date(), "%Y"))),1))
      sampYear <- rownames_to_column(sampleData, "Rows")
      sampYear <- filter(.data = sampYear, invalidYear == "FALSE")
      sampYear <- mutate(.data = sampYear, Rows = as.numeric(as.character(Rows)) + 1)
      c(sampYear$Rows)

    }
  })

  output$sampGear <- renderText({
    if(input$sampGear == TRUE){
      sampleData <- sampleData()
      sampleData$invalidGear <- sampleData$Gear.Code %in% gearCodes$Gear.Code
      sampGear <- rownames_to_column(sampleData, "Rows")
      sampGear <- filter(.data = sampGear, invalidGear == "FALSE")
      sampGear <- mutate(.data = sampGear, Rows = as.numeric(as.character(Rows)) + 1)
      c(sampGear$Rows)
    }
  })
  
  output$sampLength <- renderText({
    if(input$sampLength == TRUE){
      sampleData <- sampleData()
      if(unique(sampleData$Gear.Code) >= 41){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidLength <- filter(.data = sampleData, Gear.Length > 300 | Gear.Length < 1)
        sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
        NAlength <- filter(.data = sampleData, NAlength == "TRUE")
        NAlength <- NAlength[,-20]
        invalidLength <- rbind(invalidLength, NAlength)
        invalidLength <- mutate(.data = invalidLength, Rows = as.numeric(as.character(Rows))+1)
        sampLength <- sort.int(c(invalidLength$Rows))
      }
      if(unique(sampleData$Gear.Code) < 40 && unique(sampleData$Gear.Code) > 30){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidLength <- filter(.data = sampleData, Gear.Length > 150 | Gear.Length < 5)
        sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
        NAlength <- filter(.data = sampleData, NAlength == "TRUE")
        NAlength <- NAlength[,-20]
        invalidLength <- rbind(invalidLength, NAlength)
        invalidLength <- mutate(.data = invalidLength, Rows = as.numeric(as.character(Rows))+1)
        sampLength <- sort.int(c(invalidLength$Rows))
      }
      if(unique(sampleData$Gear.Code) < 30 && unique(sampleData$Gear.Code) > 20){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidLength <- filter(.data = sampleData, Gear.Length > 2000 | Gear.Length < 10)
        sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
        NAlength <- filter(.data = sampleData, NAlength == "TRUE")
        NAlength <- NAlength[,-20]
        invalidLength <- rbind(invalidLength, NAlength)
        invalidLength <- mutate(.data = invalidLength, Rows = as.numeric(as.character(Rows))+1)
        sampLength <- sort.int(c(invalidLength$Rows))
      }
      if(unique(sampleData$Gear.Code) == 10){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidLength <- filter(.data = sampleData, Gear.Length > 100 | Gear.Length < 5)
        sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
        NAlength <- filter(.data = sampleData, NAlength == "TRUE")
        NAlength <- NAlength[,-20]
        invalidLength <- rbind(invalidLength, NAlength)
        invalidLength <- mutate(.data = invalidLength, Rows = as.numeric(as.character(Rows))+1)
        sampLength <- sort.int(c(invalidLength$Rows))
      }
      sampLength
    }
  })
  
  output$sampEffort <- renderText({
    if(input$sampEffort == TRUE){
      sampleData <- sampleData()
      if(unique(sampleData$Gear.Code) >= 41){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidEffort <- filter(.data = sampleData, Effort > 24 | Effort < 0.1)
        sampleData$NAEffort <- lapply(sampleData$Effort, is.na)
        NAEffort <- filter(.data = sampleData, NAEffort == "TRUE")
        NAEffort <- NAEffort[,-20]
        invalidEffort <- rbind(invalidEffort, NAEffort)
        invalidEffort <- mutate(.data = invalidEffort, Rows = as.numeric(as.character(Rows))+1)
        sampEffort <- sort.int(c(invalidEffort$Rows))
      }
      if(unique(sampleData$Gear.Code) < 40 && unique(sampleData$Gear.Code) > 30){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidEffort <- filter(.data = sampleData, Effort > 96 | Effort < 0.1)
        sampleData$NAEffort <- lapply(sampleData$Effort, is.na)
        NAEffort <- filter(.data = sampleData, NAEffort == "TRUE")
        NAEffort <- NAEffort[,-20]
        invalidEffort <- rbind(invalidEffort, NAEffort)
        invalidEffort <- mutate(.data = invalidEffort, Rows = as.numeric(as.character(Rows))+1)
        sampEffort <- sort.int(c(invalidEffort$Rows))
      }
      if(unique(sampleData$Gear.Code) < 30 && unique(sampleData$Gear.Code) > 20){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidEffort <- filter(.data = sampleData, Effort > 96 | Effort < 0.1)
        sampleData$NAEffort <- lapply(sampleData$Effort, is.na)
        NAEffort <- filter(.data = sampleData, NAEffort == "TRUE")
        NAEffort <- NAEffort[,-20]
        invalidEffort <- rbind(invalidEffort, NAEffort)
        invalidEffort <- mutate(.data = invalidEffort, Rows = as.numeric(as.character(Rows))+1)
        sampEffort <- sort.int(c(invalidEffort$Rows))
      }
      if(unique(sampleData$Gear.Code) == 10){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidEffort <- filter(.data = sampleData, Effort < 100)
        sampleData$NAEffort <- lapply(sampleData$Effort, is.na)
        NAEffort <- filter(.data = sampleData, NAEffort == "TRUE")
        NAEffort <- NAEffort[,-20]
        invalidEffort <- rbind(invalidEffort, NAEffort)
        invalidEffort <- mutate(.data = invalidEffort, Rows = as.numeric(as.character(Rows))+1)
        sampEffort <- sort.int(c(invalidEffort$Rows))
      }
      sampEffort
    }
  })
  
  output$sampSpp <- renderText({
    if(input$sampSpp == TRUE){
      sampleData <- sampleData()
      sampleData$invalidSpp <- sampleData$Species.Code %in% speciesCodes$Species.Code
      sampSpp <- rownames_to_column(sampleData, "Rows")
      sampSpp <- filter(.data = sampSpp, invalidSpp == "FALSE")
      sampSpp <- mutate(.data = sampSpp, Rows = as.numeric(as.character(Rows)) + 1)
      c(sampSpp$Rows)
    }
  })
  
  output$sampNOI <- renderText({
    if(input$sampNOI == TRUE){
      sampleData <- sampleData()
      sampleData <- rownames_to_column(sampleData, "Rows")
      invalidNOI <- filter(.data = sampleData, Number.of.individuals < 1)
      sampleData$NAnoi <- lapply(sampleData$Number.of.individuals, is.na)
      NAnoi <- filter(.data = sampleData, NAnoi == "TRUE")
      NAnoi <- NAnoi[,-20]
      invalidNOI <- rbind(invalidNOI, NAnoi)
      invalidNOI <- mutate(.data = invalidNOI, Rows = as.numeric(as.character(Rows))+1)
      sampNOI <- sort.int(c(invalidNOI$Rows))
    }
  })
  
  output$sampTL <- renderText({
    if(input$sampTL == TRUE){
      sampleData <- sampleData()
      sampTL <- rownames_to_column(sampleData, "Rows")
      sampTL <- filter(.data = sampTL, TL_mm == 0)
      sampTL <- mutate(.data = sampTL, Rows = as.numeric(as.character(Rows)) + 1)
      c(sampTL$Rows)
    }
  })
  
  output$sampWt <- renderText({
    if(input$sampWt == TRUE){
      sampleData <- sampleData()
      sampWt <- rownames_to_column(sampleData, "Rows")
      sampWt <- filter(.data = sampWt, Wt_g == 0)
      sampWt <- mutate(.data = sampWt, Rows = as.numeric(as.character(Rows)) + 1)
      c(sampWt$Rows)
    }
  })
  
  output$sampWr <- renderText({
    if(input$sampWr == TRUE){
      sampleData <- sampleData()
        sampleData <- rownames_to_column(sampleData, "Rows")
      sampleData <- join(sampleData, WSnames, by = "Species.Code")
      sampleData <- filter(sampleData, !is.na(TL_mm) & !is.na(Wt_g) & !is.na(wsname))
      if(nrow(sampleData) == 0){
        sampWr <- "Okay"
      } else{
        sampleData$Wr <- wrAdd(as.numeric(as.character(Wt_g)) ~ as.numeric(as.character(TL_mm)) + wsname, units = "metric", data = sampleData)
        sampWr <- sampleData
        sampWr <- filter(.data = sampWr, Wr < 20 | Wr >200)
        sampWr <- mutate(.data = sampWr, Rows = as.numeric(as.character(Rows)) + 1)
        sampWr <- c(sampWr$Rows)
      }
      sampWr
    }
  })
  
# Run age data validation function when action button pressed#####################
  
  validateAge <- reactive({
    
    errorTableAge <- data.frame(0, "Okay", stringsAsFactors = FALSE)
    colnames(errorTableAge) <- c("Error", "Status")
    errorTableAge <- errorTableAge
    
    ageData <- ageData()
    
    # Check if Lake.Code is consistent with defined codes, and not null##########    
    ageData$invalidLakeAge <- ageData$Lake.Code %in% lakeCodes$Lake.Code
    invalidLakeAge <- filter(.data = ageData, invalidLakeAge == "FALSE")
    
    if(nrow(invalidLakeAge) == 0){
      okay <- c("Invalid Lake Code", "Okay")
    } else{
      okay <- c("Invalid Lake Code", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    errorTableAge <- filter(.data = errorTableAge, Error != "0")
    
    ageData <- ageData()
    
    # Check if Month is within acceptable options (integer 1-12; Required Field)#
    ageData$invalidMonthAge <- ageData$Month %in% c(seq(1,12,1))
    invalidMonthAge <- filter(.data = ageData, invalidMonthAge == "FALSE")
    
    if(nrow(invalidMonthAge) == 0){
      okay <- c("Invalid Month", "Okay")
    } else{
      okay <- c("Invalid Month", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    ageData <- ageData()
    
    # Check if Day is within acceptable options (integer 1-31; Required Field)#
    ageData$invalidDayAge <- ageData$Day %in% c(seq(1,31,1))
    invalidDayAge <- filter(.data = ageData, invalidDayAge == "FALSE")
    
    if(nrow(invalidDayAge) == 0){
      okay <- c("Invalid Day", "Okay")
    } else{
      okay <- c("Invalid Day", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    ageData <- ageData()
    
    # Check if Year is within acceptable options (integer 1980-current year; Required Field)#
    ageData$invalidYearAge <- ageData$Year %in% c(seq(1980,as.integer(format(Sys.Date(), "%Y")),1)) 
    invalidYearAge <- filter(.data = ageData, invalidYearAge == "FALSE")
    
    if(nrow(invalidYearAge) == 0){
      okay <- c("Invalid Year", "Okay")
    } else{
      okay <- c("Invalid Year", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    ageData <- ageData()
  
    # Gear.Code is one from established list##########################
    ageData$invalidGearAge <- ageData$Gear %in% gearCodes$Gear.Code
    invalidGearAge <- filter(.data = ageData, invalidGearAge == "FALSE")
    ageData <- ageData()
    
    if(nrow(invalidGearAge) == 0){
      okay <- c("Invalid Gear Code", "Okay")
    } else{
      okay <- c("Invalid Gear Code", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    ageData <- ageData()
    
    # Check if species codes are consistent with defined codes, and not null###############
    ageData$invalidSppAge <- ageData$Species.Code %in% speciesCodes$Species.Code
    invalidSppAge <- filter(.data = ageData, invalidSppAge == "FALSE")
    
    if(nrow(invalidSppAge) == 0){
      okay <- c("Invalid Species Code", "Okay")
    } else{
      okay <- c("Invalid Species Code", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    ageData <- ageData()
    
    # Check if Number of individuals is not 0 and not null #########################
    invalidNOIAge <- filter(.data = ageData, Number.of.individuals < 1)
    ageData$NAnoiAge <- lapply(ageData$Number.of.individuals, is.na)
    NAnoiAge <- filter(.data = ageData, NAnoiAge == "TRUE")
    NAnoiAge <- NAnoiAge[,-10]
    invalidNOIAge <- rbind(invalidNOIAge, NAnoiAge)
    
    if(nrow(invalidNOIAge) == 0){
      okay <- c("Invalid Number of individuals", "Okay")
    } else{
      okay <- c("Invalid Number of individuals", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    ageData <- ageData()
    
    # TLmm must not = 0...(should be null)####################################################
    invalidTLAge <- filter(.data = ageData, TLmm == 0)
    ageData$NATLAge <- lapply(ageData$TLmm, is.na)
    NATLAge <- filter(.data = ageData, NATLAge == "TRUE")
    NATLAge <- NATLAge[,-10]
    invalidTLAge <- rbind(invalidTLAge, NATLAge)
    
    if(nrow(invalidTLAge) == 0){
      okay <- c("Invalid Total Length", "Okay")
    } else{
      okay <- c("Invalid Total Length", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    
    ageData <- ageData()
    
    # Check if Age is within acceptable options (integer 0-40; Required Field)#
    ageData$invalidAge <- ageData$Age %in% c(seq(0,40,1))
    invalidAge <- filter(.data = ageData, invalidAge == "FALSE")
    
    if(nrow(invalidAge) == 0){
      okay <- c("Invalid Age", "Okay")
    } else{
      okay <- c("Invalid Age", "Error")
    }
    errorTableAge <- rbind(errorTableAge, okay)
    ageData <- ageData()
    
    errorTableAge
  })
  
# Render age error table#######################
output$ageError <- renderFormattable({
  if(input$validateAge != 0){
    validatedAge <- validateAge()
    # function to conditionally color the text
    f1 <- formatter("span",
                    style = ~ ifelse(Status == "Error", "color:red", "color:green"))
    validatedAge <- formattable(validatedAge, list(Status = f1))
  }else{return(NULL)}
})

# Return row numbers of errors from age sample validation rules#############
  output$ageLake <- renderText({
    if(input$ageLake == TRUE){
      ageData <- ageData()
      ageData$invalidLakeAge <- ageData$Lake.Code %in% lakeCodes$Lake.Code
      ageLake <- rownames_to_column(ageData, "Rows")
      ageLake <- filter(.data = ageLake, invalidLakeAge == "FALSE")
      ageLake <- mutate(.data = ageLake, Rows = as.numeric(as.character(Rows)) + 1)
      c(ageLake$Rows)
    }
  })
  
  output$ageMonth <- renderText({
    if(input$ageMonth == TRUE){
      ageData <- ageData()
      ageData$invalidMonthAge <- ageData$Month %in% c(seq(1,12,1))
      ageMonth <- rownames_to_column(ageData, "Rows")
      ageMonth <- filter(.data = ageMonth, invalidMonthAge == "FALSE")
      ageMonth <- mutate(.data = ageMonth, Rows = as.numeric(as.character(Rows)) + 1)
      c(ageMonth$Rows)
    }
  })
  
  output$ageDay <- renderText({
    if(input$ageDay == TRUE){
      ageData <- ageData()
      ageData$invalidDayAge <- ageData$Day %in% c(seq(1,31,1))
      ageDay <- rownames_to_column(ageData, "Rows")
      ageDay <- filter(.data = ageDay, invalidDayAge == "FALSE")
      ageDay <- mutate(.data = ageDay, Rows = as.numeric(as.character(Rows)) + 1)
      c(ageDay$Rows)
      
    }
  })
  
  output$ageYear <- renderText({
    if(input$ageYear == TRUE){
      ageData <- ageData()
      ageData$invalidYearAge <- ageData$Year %in% c(seq(1980,as.integer(format(Sys.Date(), "%Y")),1))
      ageYear <- rownames_to_column(ageData, "Rows")
      ageYear <- filter(.data = ageYear, invalidYearAge == "FALSE")
      ageYear <- mutate(.data = ageYear, Rows = as.numeric(as.character(Rows)) + 1)
      c(ageYear$Rows)
      
    }
  })
  
  output$ageGear <- renderText({
    if(input$ageGear == TRUE){
      ageData <- ageData()
      ageData$invalidGearAge <- ageData$Gear %in% gearCodes$Gear.Code
      ageGear <- rownames_to_column(ageData, "Rows")
      ageGear <- filter(.data = ageGear, invalidGearAge == "FALSE")
      ageGear <- mutate(.data = ageGear, Rows = as.numeric(as.character(Rows)) + 1)
      c(ageGear$Rows)
    }
  })
  
  output$ageSpp <- renderText({
    if(input$ageSpp == TRUE){
      ageData <- ageData()
      ageData$invalidSppAge <- ageData$Species.Code %in% speciesCodes$Species.Code
      ageSpp <- rownames_to_column(ageData, "Rows")
      ageSpp <- filter(.data = ageSpp, invalidSppAge == "FALSE")
      ageSpp <- mutate(.data = ageSpp, Rows = as.numeric(as.character(Rows)) + 1)
      c(ageSpp$Rows)
    }
  })
  
  output$ageNOI <- renderText({
    if(input$ageNOI == TRUE){
      ageData <- ageData()
      ageData <- rownames_to_column(ageData, "Rows")
      invalidNOIAge <- filter(.data = ageData, Number.of.individuals < 1)
      ageData$NAnoi <- lapply(ageData$Number.of.individuals, is.na)
      NAnoiAge <- filter(.data = ageData, NAnoi == "TRUE")
      NAnoiAge <- NAnoiAge[,-11]
      invalidNOIAge <- rbind(invalidNOIAge, NAnoiAge)
      invalidNOIAge <- mutate(.data = invalidNOIAge, Rows = as.numeric(as.character(Rows))+1)
      sampNOIAge <- sort.int(c(invalidNOIAge$Rows))
    }
  })
  
  output$ageTL <- renderText({
    if(input$ageTL == TRUE){
      ageData <- ageData()
      ageData <- rownames_to_column(ageData, "Rows")
      invalidageTL <- filter(.data = ageData, TLmm == 0)
      ageData$NATL <- lapply(ageData$TLmm, is.na)
      NATL <- filter(.data = ageData, NATL == "TRUE")
      NATL <- NATL[,-11]
      invalidageTL<- rbind(invalidageTL, NATL)
      invalidageTL <- mutate(.data = invalidageTL, Rows = as.numeric(as.character(Rows)) + 1)
      c(invalidageTL$Rows)
    }
  })
  
  output$age <- renderText({
    if(input$age == TRUE){
      ageData <- ageData()
      ageData$invalidAge <- ageData$Age %in% c(seq(0,40,1))
      invalidAge <- rownames_to_column(ageData, "Rows")
      invalidAge <- filter(.data = invalidAge, invalidAge == "FALSE")
      invalidAge <- mutate(.data = invalidAge, Rows = as.numeric(as.character(Rows)) + 1)
      c(invalidAge$Rows)
      
    }
  })
  
  
# Validation Rule Details#################################
  
  # Downloadable csv of lake codes
  output$lakeCodeList <- downloadHandler(
    filename = function() {
      paste("lakeCodes", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(lakeCodes, file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of gear codes
  output$gearCodeList <- downloadHandler(
    filename = function() {
      paste("gearCodes", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(gearCodes, file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of gear codes
  output$sppCodeList <- downloadHandler(
    filename = function() {
      paste("sppCodes", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(speciesCodes, file, row.names = FALSE)
    }
  )
  
}





