library(stringr)
library(quantmod)
library("data.table") # fread function to read files faster
library(xts)
library(TTR)
library(tm)

# Load stock data file with datetime, open, high, low, close, volume columns
import.stock <- function(datasource){
  
  file <- paste(datasource, ".csv", sep = "")
  data <- as.data.frame(fread(file))
  
  names(data) <- c("datetime", "open", "high", "low", "close", "volume")
  data$datetime <- as.Date(strptime(data$datetime, "%Y-%m-%d"))
  data <- as.xts(data[,2:6], order.by = data[,1])
  
  return (data)
}

shinyApp(
  ui = fluidPage(
    singleton(tags$head(
      tags$script(src="//cdnjs.cloudflare.com/ajax/libs/annyang/1.4.0/annyang.min.js"),
      includeScript('init.js')
    )),
    div(
      style = 'display: block; margin: auto; width: 100%; max-width: 1000px;',
      plotOutput('plot', height = '700px', width = '100%')
    )
  ),
  
  # define Shiny server
  server = function(input, output) {
    
    # load config file and set default graph parameters
    config <- read.csv("Data/config.csv",
                       colClasses = c(rep('character', 6)))
    params <- config[nrow(config),]
    text <- reactive({input$text
    })
    
    # define Shiny plot output
    output$plot <- renderPlot({
      
      ###### PRE-PROCESS TEXT INPUT ##################################################
      # parse text inputs to extract parameters
      print(input$text)
      raw <- str_to_lower(input$text)
      
      # remove stopwords except ones needed for commands
      raw <- ifelse(length(raw) == 0, 'a', raw) # default string to prevent errors for 0 length inputs
      raw <- removeWords(raw, stopwords("english")[c(-135:-136,-141:-142)]) #on and off, to and from
      nospaces <- str_replace_all(raw, " ", "")
      
      ###### INITIALISE GRAPH AND DATA PARAMETERS ##################################################
      # Check whether reset command is given.
      # This will take all settings from the original config file before applying any new commands
      
      reseton <- grepl("reset", nospaces) # reset
      
      reset <- ifelse(reseton == T,
                      config <- read.csv("Data/config.csv",
                                         colClasses = c(rep('character', 6))),
                      config <- read.csv("Data/config2.csv",
                                         colClasses = c(rep('character', 6))))
      params <- config[nrow(config),]
      
      # set chart theme
      theme <- "white"
      
      # check which stock should be shown: default is Google.
      google <- grepl("goo", nospaces)
      apple <- grepl("app", nospaces)
      baidu <- grepl("bai", nospaces)
      amazon <- grepl("amazon", nospaces)
      datasource <- ifelse(google == T, 'Data/WIKI/GOOGL',
                      ifelse(apple == T, 'Data/WIKI/AAPL',
                             ifelse(baidu == T, 'Data/WIKI/BIDU',
                                    ifelse(amazon == T, 'Data/WIKI/AMZN', params[[1]]))))
      
      # import the stock data
      data <- import.stock(datasource)
      
      ###### FEATURES ##################################################
      # Check for each feature and determine whether they exist or not (TRUE or FALSE)
      
      # graph types
      candlestick <- grepl("candle", nospaces)
      line <- grepl("line", nospaces)
      bar <- grepl("bar", nospaces)
      matchstick <- grepl("stick", nospaces)
      
      # add/remove log.scale
      log.scaleon <- grepl("scale", nospaces) 
      log.scaleoff <- grepl("scaleoff", nospaces)
      
      # Defining Technical Indicators
      volumeon <- grepl("vol", nospaces)
      volumeoff <- grepl("removevol", nospaces)
      emaon <- grepl("expon", nospaces)
      emaoff <- grepl("removeexpon", nospaces)
      RSIon <- grepl("rsi", nospaces)
      RSIoff <- grepl("removersi", nospaces)
      ADXon <- grepl("adx", nospaces)
      ADXoff <- grepl("removeadx", nospaces)
      ATRon <- grepl("truerange", nospaces)
      ATRoff <- grepl("removetruerange", nospaces)
      BBandson <- grepl("band", nospaces)
      BBandsoff <- grepl("removebands", nospaces)      
      SARon <- grepl("sar", nospaces)
      SARoff <- grepl("removesar", nospaces)

      ###### CONDITIONS ##################################################
      # for each condition, check IF there is a feature present,
      # ELSE use the values from the current configuration file

      type <- ifelse(candlestick == T, 'candlestick',
                     ifelse(line == T, 'line',
                            ifelse(bar == T, 'bar',
                                   ifelse(matchstick == T, 'matchstick', params[[3]]))))
      
      log.scale <- ifelse(log.scaleon == T, T, 
                          ifelse(log.scaleoff == T, F, params[[4]]))

      
      # work out which indicators to show
      vol <- ifelse(volumeoff == T, "NULL", 
                    ifelse(volumeon == T, "addVo()", params[[5]]))
      ema <- ifelse(emaoff == T, "NULL",
                    ifelse(emaon == T,
                           "addEMA(n = 200)", 
                             params[[6]]))
      RSI <- ifelse(RSIoff == T, "NULL", 
                    ifelse(RSIon == T, "addRSI(n = 10)", params[[7]]))
      ADX <- ifelse(ADXoff == T, "NULL", 
                    ifelse(ADXon == T, "addADX(n = 20)", params[[8]]))
      ATR <- ifelse(ATRoff == T, "NULL", 
                    ifelse(ATRon == T, 
                           "addTA(data[,4] - 2*ATR(data[,2:4], n = 14)[,'atr'], on = 1, col = 'blue')",
                           params[[9]]))
      BBands <- ifelse(BBandsoff == T, "NULL", 
                       ifelse(BBandson == T, 
                              "addBBands()", 
                              params[[10]]))
      SAR <- ifelse(SARoff == T, "NULL", 
                    ifelse(SARon == T, "addSAR()", params[[11]]))
      
      TA <- paste(vol,ema,RSI,ADX,ATR,BBands,SAR, sep = ';')
      TA <- gsub("NULL;", "", TA)

      # See only the last x days, weeks, months, years.
      zoom.lastperiod <- unlist(strsplit(nospaces, 'last'))[2] #om instead of zoom easier to match
      if(!is.na(zoom.lastperiod)){
        duration <- unlist(strsplit(zoom.lastperiod, 's'))[1]
        duration <- sub('([[:digit:]])([[:lower:]])', '\\1 \\2', duration)
        data <- last(data, duration)
      }

      # generate graphical output
      chartSeries(data, theme = chartTheme(theme),
                  type = type, log.scale = as.logical(log.scale), 
                  name = datasource,
                  TA = TA)

      ###### FIXED TIME PERIODS ####################################
      
      # See only a range between specific years, ie. from 2010 to 2015
      zoom <- unlist(strsplit(nospaces, 'om'))[2] #om instead of zoom easier to match
      if(!is.na(zoom)){
        fromtime <- unlist(strsplit(zoom, 'to'))[1]
        totime <- unlist(strsplit(zoom, 'to'))[2]
        zoomChart(paste(fromtime, '::', totime, sep = ''))
      }

      # overwrite current parameters with latest parameters
      params[1] <- datasource
      params[2] <- theme
      params[3] <- type
      params[4] <- log.scale
      params[5] <- vol
      params[6] <- ema
      params[7] <- RSI
      params[8] <- ADX
      params[9] <- ATR
      params[10] <- BBands
      params[11] <- SAR
      
      # save the state by writing new parameters to config.file
      new_params <- rbind(config, params)
      write.csv(new_params, 'Data/config2.csv', row.names = F)
    })
  }
)