#Packages to be installed and loaded############################
library(dplyr)
library(plyr)
library(tibble)
library(FSA)
library(formattable) #used for formattableOutput() function to produce error table more easily
library(shinyjs)
library(DT)

# .csv's to upload
sampleTemplate <- read.csv("sampleTemplate.csv")
ageTemplate <- read.csv("ageTemplate.csv")
lakeCodes <- read.csv("lakeinfo.csv")
gearCodes <- read.csv("gearinfo.csv")
speciesCodes <- read.csv("speciesinfo.csv")
WSnames <- read.csv("WSnames.csv")
minMaxTL <- read.csv("minMaxTL.csv")%>% mutate(Species.Code = as.numeric(Species.Code))


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
# 
# #for debuggging
# observeEvent(input$WriteFile,{
#         write.csv(validateSample(), "validationResults.csv")
#         })
  
# read in .csv of sample data from file input
  sampleData <- reactive({
    sampleData <- input$sampleData
    if(is.null(sampleData)){
      return(NULL)
    } else{
      #sampleData <- read.csv(sampleData$datapath)
      #below automatically converts blanks and "NA" to NA's...but will flag these as needing to be ".".  Code for exporting validated file will also convert "." to NA
      sampleData <- read.csv(sampleData$datapath, na.strings = c("","NA")) %>% 
                       mutate(Gear.Code = as.numeric(as.character(Gear.Code)),
                           Species.Code = as.numeric(Species.Code),
                           Number.of.individuals = as.numeric(as.character(Number.of.individuals)))
      #create Verified.TL/Wr columns if they do not exist. This will be used to mark abnormal TL/Wr values that have been verified
      if("Verified.TL" %in% colnames(sampleData)){
        sampleData <- sampleData %>% mutate(Verified.TL = as.character(Verified.TL))
        sampleData$Verified.TL[sampleData$Verified.TL == ""] <- NA
      }else{
        sampleData$Verified.TL <- as.character(NA)
      }
      if("Verified.Wr" %in% colnames(sampleData)){
        sampleData <- sampleData %>% mutate(Verified.Wr = as.character(Verified.Wr))
        sampleData$Verified.Wr[sampleData$Verified.Wr == ""] <- NA
      }else{
        sampleData$Verified.Wr <- as.character(NA)
      }
      
        #if only Verified.Wr was in original file, Verified.TL ends up being built after Verified Wr, so we need to move it
        sampleData <- sampleData %>% relocate(Verified.TL, .before = Verified.Wr)
      
      sampleData$Station[sampleData$Station=="."] <- NA #need this to test for missing station as this should never be blank, 
      #(i.e., any period is an illegitimate value anyhow) but I cannot get test for blank station code to check for "." 
      #for some reason...so this approach fixes that issue by searching for periods and NA's simply by searching for NAs.
      
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
    ageData <- mutate(ageData, Gear = as.numeric(as.character(Gear)), Species.Code = as.numeric(Species.Code),
                      Number.of.individuals = as.numeric(as.character(Number.of.individuals)))
    #create Verified.TL column if it does not exist. This will be used to mark abnormal TL values that have been verified.
    if("Verified.TL" %in% colnames(ageData)){
      ageData <- ageData %>% mutate(Verified.TL = as.character(Verified.TL))
      ageData$Verified.TL[ageData$Verified.TL == ""] <- NA
    }else{
      ageData$Verified.TL <- as.character(NA)
    }
    ageData[ageData == "."] <- NA #replace periods with "NA"...now hard coded rather than using next 3 lines and check box
      #we are not requiring "." for age data...basically nothing should be blank, "NA" or ".", so this is ok to convert here so we can
      #check for NA's using code below and capture true NA, "", and "." at same time
    # if(input$ageperiod2NA == TRUE){
    #   ageData[ageData == "."] <- NA
    # }
    ageData
  }
})


#Test validated data to see if safe to download
downloadSampleOK <- reactive({
  # validateSample <- validateSample() %>% filter(Error != "Unusual Relative Weight (<50 or >150)" &
  #                         Error != "Abnormally large or small TL")
      #originally allowed download if only errors were unusal TL or Wr (above 2 lines). now we verify the TL and Wr, so those
      #should not be downloadable if they exist too.
  validateSample <- validateSample()
  errors <- as.character(validateSample$Status)
  if("Error" %in% errors){
    FALSE #don't download
  }else{
    TRUE #download ok
  }
})

lakeYrGear <- reactive({
  sampleData <- sampleData()
  lake <- sampleData$Lake.Code[1]
  yr <- sampleData$Year[1]
  gear <- sampleData$Gear.Code[1]
  paste(lake, yr, gear, sep="_")
})


# Download validated sample data
    #remove download button if data not valid
    observe({
      if(is.null(validateSample())){
                return(NULL)
        }else{
          if(downloadSampleOK() == TRUE){
            enable("valSampleData")
            output$valSampleData <- downloadHandler(
              filename = function() {
                paste(lakeYrGear(), "_Sample_","validated_", date(), ".csv", sep = "")
              },
              content = function(file) {
                sampleData <- sampleData()
                sampleData[sampleData == "."] <- NA #Automatically convert period to NA's
                sampleData[sampleData == "NA"] <- NA
                sampleData$SampleID <- paste(sampleData$Lake.Code, sampleData$Station, sampleData$Month,
                                             sampleData$Day, sampleData$Year, sampleData$Gear.Code,
                                             sep = "")
                sampleData <- sampleData[,c(1,21,2:20)] #with separate Verified.TL & .Wr column
                write.csv(sampleData, file, row.names = FALSE)
              }
            )
            output$downloadMessage <- NULL #this removes the message if it was previously displayed
          }else{
            disable("valSampleData")
            output$downloadMessage <- renderText("File did not pass validation, so it cannot be downloaded.
                  Please edit the csv file and test it again.  You will be able to download once it passes
                  the required validation tests.")
          }
        }
      })

    
#Test Age data to see if safe to download
downloadAgeOK <- reactive({
  validateAge <- validateAge() 
  errors <- as.character(validateAge$Status)
  if("Error" %in% errors){
    FALSE #don't download
  }else{
    TRUE #download ok
  }
})

# Download Age sample data

#function to make naming downloaded file easier
LkYrGrSp <- reactive({
  ageData <- ageData()
  lake <- ageData$Lake.Code[1]
  yr <- ageData$Year[1]
  gear <- ageData$Gear[1]
  spp <- ageData$Species.Code[1]
  paste(lake, yr, gear, spp, sep="_")
})
    #remove download button if data not valid
    observe({
      if(is.null(validateAge())){
                return(NULL)
        }else{
          if(downloadAgeOK() == TRUE){
            enable("valAgeData")
            output$valAgeData <- downloadHandler(
              filename = function() {
                paste(LkYrGrSp(), "_Age_", "validated_", date(), ".csv", sep = "")
              },
              content = function(file) {
                ageData <- ageData()
                write.csv(ageData, file, row.names = FALSE)
              }
            )
            output$downloadMessageAge <- NULL #this removes the message if it was previously displayed
          }else{
            disable("valAgeData")
            output$downloadMessageAge <- renderText("File did not pass validation, so it cannot be downloaded.
                  Please edit the csv file and test it again.  You will be able to download once it passes
                  the required validation tests.")
          }
        }
      })
  
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

#creates table for display of sample data at bottom of pg that can be filtered for errors...includes calculated Wr
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
# output$sampleDataDisplayTable <- DT::renderDataTable({DT::datatable(sampleDataDisplay(), rownames = FALSE)})
output$sampleDataDisplayTable <- DT::renderDT(sampleDataDisplay(), editable = TRUE, rownames = FALSE)


#creates table for display of Age data at bottom of pg that can be filtered for errors...includes calculated Wr
#
#Below code is not working...comments on ui.R file, but something causing error where mutate trying to operate
#on Gear.Code when the df is empty.  I'm disabling for now (I was in the process of building this table for
#the first time...it has never worked despite the one on the sample validation tab working fine)
ageDataDisplay<- reactive({
  req(ageData())
  if (is.null(ageData())) {NULL}
      else{
  ageDataDisplay <- ageData()
  ageDataDisplay$ID <- seq.int(nrow(ageDataDisplay))#create an "ID" column that numbers rows
  ageDataDisplay["row.numb"] <- ageDataDisplay$ID +1
  ageDataDisplay["Row.numb"] <- ageDataDisplay$ID +1
  ageDataDisplay <- ageDataDisplay %>% select(Row.numb,everything())
  # sampleDataDisplay["ID"] <- NULL
  ageDataDisplay
       }
  })
output$ageDataDisplayTable <- DT::renderDataTable({DT::datatable(ageDataDisplay(), rownames = F)})


  
# Run data validation function when action button pressed#####################
# This reactive statement builds an error table with one row per test indicating if "Okay" or "Error" for that 
# test.  This is a bit inefficient as the next section will rerun all of these tests a second time to identify
# row numbers violating the test.  Someday we should rewrite this as a series of reactive functions, then use
# the reactive function in below validateSample to make errorTable and could again use the same reactive functions
# to build the row number information...would not have to re-run the code twice if we did it that way.

  validateSample <- reactive({
    req(sampleData())

      #create errorTable to track "Okay" vs "Error" for each test
      errorTable <- data.frame(0, "Okay", stringsAsFactors = FALSE)
      colnames(errorTable) <- c("Error", "Status")
      errorTable <- errorTable
      
    #test column names and order ################################
      #creates a data frame of the column names in order 
        columnTemplate <- data.frame(colNames = colnames(read.csv("sampleTemplate.csv")))
        columnNames <- data.frame(colNames = colnames(sampleData()))
      #test if same names and same order (row order is now the list of columns in order)
          if(identical(columnTemplate, columnNames) != TRUE){
          okay <- c("Correct column names and order", "Error")
        }else{
          okay <- c("Correct column names and order", "Okay")
        }
      errorTable <- rbind(errorTable, okay)
      
    # Check if blank cells exist###########################################
      sampleData <- sampleData()
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
      
    #test if there are more time codes than station values suggesting bad station ID's
      stationTimeCount <- sampleData() %>% group_by(Lake.Code, Gear.Code, Year, Month, Day) %>% summarize(Time=n_distinct(Time), 
                                                                   Station=n_distinct(Station))
      badSationTimeCount <- stationTimeCount %>% filter(Time > Station)
      
      if(nrow(badSationTimeCount) == 0){
        okay <- c("Missing Stations ID's based on time codes", "Okay")
      } else{
        okay <- c("Missing Stations ID's based on time codes", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      
    # Check if Month is within acceptable options (integer 1-12; Required Field)#
      sampleData <- sampleData()
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
      
    #test month, day, & year make a legitimate date
      DateData <- sampleData()
      DateData$Date <- as.Date(paste(DateData$Month, DateData$Day, DateData$Year,sep="-"),format="%m-%d-%Y")
      DateData$Date[DateData$Date<1980-01-01] <- NA #don't allow dates before first ODWC data in 1980
      DateData$Date[DateData$Date>as.Date(format(Sys.Date(), "%Y-%m-%d"))] <- NA #don't allow dates in the future
      invalidDate <- DateData %>% filter(is.na(Date))

      if(nrow(invalidDate) == 0){
        okay <- c("Year, month, & day make valid date", "Okay")
      } else{
        okay <- c("Year, month, & day make valid date", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      
      # Only one gear code in sample data...needed so we can validate all rows have legit gear length/efforts###################
      
      sampleData <- sampleData()
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
          
          # if Gear.Code is valid, we can check Gear.Length and Effort
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
            ###using wierd gear efforts and this column is not needed for CPUE calculation, so it is unnecessary for
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
          
        }else{
          # if Gear.Code is invalid, we can not check Gear.Length 
          okay <- c("Invalid Gear Code", "Error")
          errorTable <- rbind(errorTable, okay)
        }
      }else{
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

      
    # TL_mm must not = 0...(should be null)####################################################
      invalidTL <- filter(.data = sampleData, TL_mm == 0)
      
      if(nrow(invalidTL) == 0){
        okay <- c("Invalid Total Length", "Okay")
      } else{
        okay <- c("Invalid Total Length", "Error")
      }
      errorTable <- rbind(errorTable, okay)
      
   #Test TL for min and max values########################################
      sampleDataTL <- filter(sampleData(), !is.na(TL_mm))
      unusualTL <- left_join(sampleDataTL, minMaxTL, by= "Species.Code") %>% 
                  mutate(TL_mm = as.numeric(as.character(TL_mm)))%>% #some files pull this in as factor and can't use > and < then
                  filter((Gear.Code == 10 & TL_mm > maxTL) | (Gear.Code!=10 & (TL_mm < minTL | TL_mm > maxTL))) %>% 
            #next 2 lines skip any row marked verified (needed to replace NA's as filter won't work with NA's)
            mutate(Verified.TL = case_when(is.na(Verified.TL) | Verified.TL == "." | Verified.TL == ""  ~ "notVerif", TRUE~Verified.TL)) %>% 
              filter(Verified.TL != "verified" & Verified.TL != "Verified" & Verified.TL != "\"verified\"" & Verified.TL != "\"Verified\"")
                
        if(nrow(unusualTL)==0){
          okay <- c("Abnormally large or small TL", "Okay")
        }else{
          okay <- c("Abnormally large or small TL", "Error")
        }
        errorTable <- rbind(errorTable, okay)

    #Test that TL is an integer with no decimal places##################################
      non_integerTL <- sampleDataTL %>% filter(TL_mm != ".") %>% 
          mutate(roundTL = round(as.numeric(TL_mm),0)) %>% filter(roundTL!=TL_mm)
        if(nrow(non_integerTL)==0){
          okay <- c("TL must be whole number", "Okay")
        }else{
          okay <- c("TL must be whole number", "Error")
        }
        errorTable <- rbind(errorTable, okay)
        
    # Wt_g must not = 0...(should be null)####################################################
      invalidWt <- sampleData() %>% filter(Wt_g == 0)
      
      if(nrow(invalidWt) == 0){
        okay <- c("Invalid Weight", "Okay")
      } else{
        okay <- c("Invalid Weight", "Error")
      }
      errorTable <- rbind(errorTable, okay)
        
      
    # Relative weight between 50 and 150...indicates abnormal length-weight combination############
          #used to be 20 and 200, but SSP committee tightened to this as a check of abnormally heavy
          #or light fish...went to 60 and 120, but that was too restrictive
      sampleData <- join(sampleData, WSnames, by = "Species.Code")
      sampleData <- filter(sampleData, !is.na(TL_mm) & !is.na(Wt_g) & !is.na(wsname) & TL_mm!="." & Wt_g!=".")
      
      #if no records are available for Wr analysis, then "okay", otherwise calc Wr and see if in range 50-150
      if(nrow(sampleData) == 0){
        okay <- c("Unusual Relative Weight (<50 or >150)", "Okay")
        errorTable <- rbind(errorTable, okay)
      }else{
        sampleData$Wr <- wrAdd(as.numeric(as.character(Wt_g)) ~ as.numeric(as.character(TL_mm)) + wsname, units = "metric", data = sampleData)
        invalidWr <- filter(.data = sampleData, Wr < 50 | Wr >150) %>% 
          #next 2 lines skip any row marked verified (needed to replace NA's as filter won't work with NA's)
          mutate(Verified.Wr = case_when(is.na(Verified.Wr) | Verified.Wr == "." | Verified.Wr == "" ~ "notVerif", TRUE ~Verified.Wr)) %>% 
            filter(Verified.Wr != "verified" & Verified.Wr != "Verified" &  Verified.Wr != "\"verified\"" &  Verified.Wr != "\"Verified\"")
        
        if(nrow(invalidWr) == 0){
          # okay <- c("Invalid Relative Weight", "Okay")
          okay <- c("Unusual Relative Weight (<50 or >150)", "Okay")
        }else{
          # okay <- c("Invalid Relative Weight", "Error")
          okay <- c("Unusual Relative Weight (<50 or >150)", "Error")
        }
        errorTable <- rbind(errorTable, okay)
      }
      
      # sampleData <- sampleData()
      
      errorTable
  })
  

# Render sample error table#######################
  output$sampleError <- renderFormattable({
        validated <- validateSample()
        # function to conditionally color the text
        f1 <- formatter("span",
                        style = ~ ifelse(Status == "Error", "color:red", "color:green"))
        validated <- formattable(validated, list(Status = f1))
  })
  
  
# Return row numbers of errors with checkbox inputs for sample data#####################
# This is a bit inefficient as it basically just reruns the same code that was used above to build the error
# table (in the validateSample() reactive statement) to find the offending row number.  We should make each
# of these blocks of code (i.e., the validation tests) a separate reactive function, then each of these reactive
# statements could be called above in validateSample() and here in the needed renderText statements.  Would save
# a lot of lines of code and prevent the program from having to run the same code multiple times.

#pattern for below is as follows:
  #1) test if statement tests if check box from ui.R has been checked
  #2) reads in SampleData, adds row names as a column, and does whatever calculations are needed
    #to test for the validation rule
  #3)filter to offending rows
  #4)return the row number column values as renderText value to return...this is stored in the output
    #and is called in code next to the check box in ui.R to display the row numbers if the box is checked.


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
  
  output$sampDate <- renderText({
    if(input$sampDate == TRUE){
      DateData <- sampleData()
      DateData$Date <- as.Date(paste(DateData$Month, DateData$Day, DateData$Year,sep="-"),format="%m-%d-%Y")
      DateData$Date[DateData$Date<1980-01-01] <- NA #don't allow dates before first ODWC data in 1980
      DateData$Date[DateData$Date>as.Date(format(Sys.Date(), "%Y-%m-%d"))] <- NA #don't allow dates in the future
      sampDate <- rownames_to_column(DateData, "Rows")
      sampDate <- sampDate %>% filter(is.na(Date)) %>%
                    mutate(Rows = as.numeric(as.character(Rows)) + 1)
      c(sampDate$Rows)

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
      if(length(unique(sampleData$Gear.Code))>1){
          sampLength <- "More than one Gear.Code was used, so gear length cannot be evaluated"
        }else if(length(unique(sampleData$Gear.Code))==0){
          sampLength <- "Gear.Code is not valid, so gear length cannot be evaluated"
        }else if(unique(sampleData$Gear.Code) >= 41){
          sampleData <- rownames_to_column(sampleData, "Rows")
          invalidLength <- filter(.data = sampleData, Gear.Length > 300 | Gear.Length < 1)
          sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
          NAlength <- filter(.data = sampleData, NAlength == "TRUE")
          NAlength <- NAlength[,-20]
          invalidLength <- rbind(invalidLength, NAlength)
          invalidLength <- mutate(.data = invalidLength, Rows = as.numeric(as.character(Rows))+1)
          sampLength <- sort.int(c(invalidLength$Rows))
        }else if(unique(sampleData$Gear.Code) < 40 && unique(sampleData$Gear.Code) > 30){
          sampleData <- rownames_to_column(sampleData, "Rows")
          invalidLength <- filter(.data = sampleData, Gear.Length > 150 | Gear.Length < 5)
          sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
          NAlength <- filter(.data = sampleData, NAlength == "TRUE")
          NAlength <- NAlength[,-20]
          invalidLength <- rbind(invalidLength, NAlength)
          invalidLength <- mutate(.data = invalidLength, Rows = as.numeric(as.character(Rows))+1)
          sampLength <- sort.int(c(invalidLength$Rows))
        }else if(unique(sampleData$Gear.Code) < 30 && unique(sampleData$Gear.Code) > 20){
          sampleData <- rownames_to_column(sampleData, "Rows")
          invalidLength <- filter(.data = sampleData, Gear.Length > 2000 | Gear.Length < 10)
          sampleData$NAlength <- lapply(sampleData$Gear.Length, is.na)
          NAlength <- filter(.data = sampleData, NAlength == "TRUE")
          NAlength <- NAlength[,-20]
          invalidLength <- rbind(invalidLength, NAlength)
          invalidLength <- mutate(.data = invalidLength, Rows = as.numeric(as.character(Rows))+1)
          sampLength <- sort.int(c(invalidLength$Rows))
        }else if(unique(sampleData$Gear.Code) == 10){
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
      if(length(unique(sampleData$Gear.Code))>1){
        sampEffort <- "More than one Gear.Code was used, so Effort cannot be evaluated"
      }else if(length(unique(sampleData$Gear.Code))==0){
        sampEffort <- "Gear.Code is not valid, so Effort cannot be evaluated"
      }else if(unique(sampleData$Gear.Code) >= 41){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidEffort <- filter(.data = sampleData, Effort > 24 | Effort < 0.1)
        sampleData$NAEffort <- lapply(sampleData$Effort, is.na)
        NAEffort <- filter(.data = sampleData, NAEffort == "TRUE")
        NAEffort <- NAEffort[,-20]
        invalidEffort <- rbind(invalidEffort, NAEffort)
        invalidEffort <- mutate(.data = invalidEffort, Rows = as.numeric(as.character(Rows))+1)
        sampEffort <- sort.int(c(invalidEffort$Rows))
      }else if(unique(sampleData$Gear.Code) < 40 && unique(sampleData$Gear.Code) > 30){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidEffort <- filter(.data = sampleData, Effort > 96 | Effort < 0.1)
        sampleData$NAEffort <- lapply(sampleData$Effort, is.na)
        NAEffort <- filter(.data = sampleData, NAEffort == "TRUE")
        NAEffort <- NAEffort[,-20]
        invalidEffort <- rbind(invalidEffort, NAEffort)
        invalidEffort <- mutate(.data = invalidEffort, Rows = as.numeric(as.character(Rows))+1)
        sampEffort <- sort.int(c(invalidEffort$Rows))
      }else if(unique(sampleData$Gear.Code) < 30 && unique(sampleData$Gear.Code) > 20){
        sampleData <- rownames_to_column(sampleData, "Rows")
        invalidEffort <- filter(.data = sampleData, Effort > 96 | Effort < 0.1)
        sampleData$NAEffort <- lapply(sampleData$Effort, is.na)
        NAEffort <- filter(.data = sampleData, NAEffort == "TRUE")
        NAEffort <- NAEffort[,-20]
        invalidEffort <- rbind(invalidEffort, NAEffort)
        invalidEffort <- mutate(.data = invalidEffort, Rows = as.numeric(as.character(Rows))+1)
        sampEffort <- sort.int(c(invalidEffort$Rows))
      }else if(unique(sampleData$Gear.Code) == 10){
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
  
  output$unusualTL <- renderText({
    if(input$unusualTL == TRUE){
      sampleData <- sampleData()
      unusualTL <- rownames_to_column(sampleData, "Rows")
      unusualTL <- left_join(unusualTL, minMaxTL, by= "Species.Code") %>% 
                  mutate(TL_mm = as.numeric(as.character(TL_mm)))%>% #some files pull this in as factor and can't use > and < then
                  filter((Gear.Code == 10 & TL_mm > maxTL) | (Gear.Code!=10 & (TL_mm < minTL | TL_mm > maxTL)))  %>% 
        #next 2 lines skip any row marked verified (needed to replace NA's as filter won't work with NA's)
        mutate(Verified.TL = case_when(is.na(Verified.TL) | Verified.TL == "." | Verified.TL == "" ~ "notVerif", TRUE~Verified.TL)) %>% 
          filter(Verified.TL != "verified" & Verified.TL != "Verified" & Verified.TL != "\"verified\"" & Verified.TL != "\"Verified\"")
      
      unusualTL <- unusualTL %>% mutate(Rows = as.numeric(as.character(Rows)) + 1)
      c(unusualTL$Rows)
    }
  })

  output$integerTL <- renderText({
    if(input$integerTL == TRUE){
      sampleData <- sampleData()
      non_integerTL <- rownames_to_column(sampleData, "Rows") %>% filter(TL_mm!=".") %>% 
          mutate(roundTL = round(as.numeric(TL_mm),0)) %>% filter(roundTL!=TL_mm) 
      non_integerTL <- mutate(.data = non_integerTL, Rows = as.numeric(as.character(Rows)) + 1)
      c(non_integerTL$Rows)
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
      sampleData <- filter(sampleData, !is.na(TL_mm) & !is.na(Wt_g) & !is.na(wsname)) %>% 
        #next 2 lines skip any row marked verified (needed to replace NA's as filter won't work with NA's)
        mutate(Verified.Wr = case_when(is.na(Verified.Wr) | Verified.Wr == "." | Verified.Wr == "" ~ "notVerif")) %>% 
          filter(Verified.Wr != "verified" & Verified.Wr != "Verified" &  Verified.Wr != "\"verified\"" &  Verified.Wr != "\"Verified\"")
      
      if(nrow(sampleData) == 0){
        # sampWr <- "Okay"
        sampWr <- NULL
      } else{
        sampleData$Wr <- wrAdd(as.numeric(as.character(Wt_g)) ~ as.numeric(
          as.character(TL_mm)) + wsname, units = "metric", data = sampleData)
        sampWr <- sampleData
        sampWr <- filter(.data = sampWr, Wr < 50 | Wr >150)
        sampWr <- mutate(.data = sampWr, Rows = as.numeric(as.character(Rows)) + 1)
        sampWr <- c(sampWr$Rows)
      }
      sampWr
    }
  })
  
####Age tab####################################################
# Run age data validation function when file has been uploaded#####################
  
  validateAge <- reactive({
      
      req(ageData())
      errorTableAge <- data.frame(0, "Okay", stringsAsFactors = FALSE)
      colnames(errorTableAge) <- c("Error", "Status")
      errorTableAge <- errorTableAge
      
      #test column names and order ################################
        #creates a data frame of the column names in order
          columnTemplateAge <- data.frame(colNames = colnames(read.csv("ageTemplate.csv")))
          columnNamesAge <- data.frame(colNames = colnames(ageData()))
        #test if same names and same order (row order is now the list of columns in order)
          if(identical(columnTemplateAge, columnNamesAge) != TRUE){
            okay <- c("Correct column names and order", "Error")
          }else{
            okay <- c("Correct column names and order", "Okay")
          }
        errorTableAge <- rbind(errorTableAge, okay)
      
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
    
      #test month, day, & year make a legitimate date
        DateDataAge <- ageData()
        DateDataAge$Date <- as.Date(paste(DateDataAge$Month, DateDataAge$Day, DateDataAge$Year,sep="-"),format="%m-%d-%Y")
        DateDataAge$Date[DateDataAge$Date<1980-01-01] <- NA #don't allow dates before first ODWC data in 1980
        DateDataAge$Date[DateDataAge$Date>as.Date(format(Sys.Date(), "%Y-%m-%d"))] <- NA #don't allow dates in the future
        invalidDate <- DateDataAge %>% filter(is.na(Date))
  
        if(nrow(invalidDate) == 0){
          okay <- c("Year, month, & day make valid date", "Okay")
        } else{
          okay <- c("Year, month, & day make valid date", "Error")
        }
        errorTableAge <- rbind(errorTableAge, okay)
        
      
      # Gear is one from established list##########################
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
      
      #Test TL for min and max values########################################
      unusualTLAge <- reactive({
        sampleDataTLAge <- filter(ageData(), !is.na(TLmm))
        unusualTLAge <- left_join(sampleDataTLAge, minMaxTL, by= "Species.Code") %>%  
                    mutate(TLmm = as.numeric(as.character(TLmm)))%>% #some files pull this in as factor and can't use > and < then
                    filter((Gear == 10 & TLmm > maxTL) | (Gear != 10 & (TLmm < minTL | TLmm > maxTL))) %>% 
          #next 2 lines skip any row marked verified (needed to replace NA's as filter won't work with NA's)
          mutate(Verified.TL = case_when(is.na(Verified.TL) | Verified.TL == "." | Verified.TL == "" ~ "notVerif", TRUE~Verified.TL)) %>% 
            filter(Verified.TL != "verified" & Verified.TL != "Verified" & Verified.TL != "\"verified\"" & Verified.TL != "\"Verified\"")
        return(unusualTLAge)
      })
        
        if(nrow(unusualTLAge())==0){
          okay <- c("Abnormally large or small TL", "Okay")
        }else{
          okay <- c("Abnormally large or small TL", "Error")
        }
        errorTableAge <- rbind(errorTableAge, okay)
          
      #Test that Age data TL is an integer with no decimal places##################################
        non_integerTLAge <- ageData() %>% filter(TLmm!=".") %>% mutate(roundTL = round(as.numeric(TLmm),0)) %>% 
          filter(roundTL!=TLmm)
        if(nrow(non_integerTLAge)==0){
          okay <- c("TL must be whole number", "Okay")
        }else{
          okay <- c("TL must be whole number", "Error")
        }
        errorTableAge <- rbind(errorTableAge, okay)
        
      
      # Check if Age is within acceptable options (integer 0-40; Required Field)#####
        ageData <- ageData()
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
    validatedAge <- validateAge()
    # function to conditionally color the text
    f1 <- formatter("span",
                    style = ~ ifelse(Status == "Error", "color:red", "color:green"))
    validatedAge <- formattable(validatedAge, list(Status = f1))
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
  
  output$ageDate <- renderText({
    if(input$ageDate == TRUE){
      DateData <- ageData()
      DateData$Date <- as.Date(paste(DateData$Month, DateData$Day, DateData$Year,sep="-"),format="%m-%d-%Y")
      DateData$Date[DateData$Date<1980-01-01] <- NA #don't allow dates before first ODWC data in 1980
      DateData$Date[DateData$Date>as.Date(format(Sys.Date(), "%Y-%m-%d"))] <- NA #don't allow dates in the future
      sampDate <- rownames_to_column(DateData, "Rows")
      sampDate <- sampDate %>% filter(is.na(Date)) %>%
                    mutate(Rows = as.numeric(as.character(Rows)) + 1)
      c(sampDate$Rows)

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
  
   output$unusualAgeTL <- renderText({
    if(input$unusualAgeTL == TRUE){
      ageData <- ageData()
      unusualAgeTL <- rownames_to_column(ageData, "Rows")
      unusualAgeTL <- left_join(unusualAgeTL, minMaxTL, by= "Species.Code") %>% 
                  mutate(TLmm = as.numeric(as.character(TLmm)))%>% #some files pull this in as factor and can't use > and < then
                  filter(TLmm < minTL | TLmm > maxTL) %>% 
            #next 2 lines skip any row marked verified (needed to replace NA's as filter won't work with NA's)
            mutate(Verified.TL  = case_when(is.na(Verified.TL) | Verified.TL == "." | Verified.TL == "" ~ "notVerif", TRUE~Verified.TL)) %>% 
            filter(Verified.TL != "verified" & Verified.TL != "Verified" & Verified.TL != "\"verified\"" & Verified.TL != "\"Verified\"")
     unusualAgeTL <- unusualAgeTL %>% mutate(Rows = as.numeric(as.character(Rows)) + 1)
      c(unusualAgeTL$Rows)
    }
  })
  
   output$integerAgeTL <- renderText({
     if(input$integerAgeTL == TRUE){
       ageData <- ageData()
       non_integerTL <- rownames_to_column(ageData, "Rows") %>% filter(TLmm!=".") %>% 
         mutate(roundTL = round(as.numeric(TLmm),0)) %>% filter(roundTL!=TLmm)  
       non_integerTL <- mutate(.data = non_integerTL, Rows = as.numeric(as.character(Rows)) + 1)
       c(non_integerTL$Rows)
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
  
  # Downloadable csv of species codes
  output$sppCodeList <- downloadHandler(
    filename = function() {
      paste("sppCodes", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(speciesCodes, file, row.names = FALSE)
    }
  )
  
}





