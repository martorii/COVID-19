library(shiny)
library(ggplot2)
library(rvest)
library(deSolve)
library(stringr)

GetData <- function(country){
  if (country == "Germany") {
    # Fetch from Wikipedia
    dataURL <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Germany#Statistics"
    datatable = dataURL %>%
      read_html() %>%
      html_node(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
      html_table(fill = TRUE)
    # Delete first row
    colnames(datatable) = datatable[1,]
    datatable_clean = datatable[-1,]
    # Get infected and remove non-numeric things
    Infected = gsub("[^0-9.-]", "", datatable_clean$`Total infections`)
    Infected = subset(Infected, grepl('^\\d+$', Infected))
    # Get days and remove non dates
    Day = datatable_clean$Date
    Day = Day[2:length(Day)]
    Day = Day[1:length(Day)-1]
    Day = format(as.Date(Day, format="%d %b"), format="%d-%m")
  }
  if (country == "Spain") {
    # Fetch from Wikipedia
    dataURL <- "https://es.wikipedia.org/wiki/Pandemia_de_enfermedad_por_coronavirus_de_2020_en_Espa%C3%B1a"
    datatable = dataURL %>%
      read_html() %>%
      html_node(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
      html_table(fill = TRUE)
    # Delete first rows
    datatable_clean = datatable[11:nrow(datatable)-1,]
    # Get infected and remove non-numeric things
    Infected <- as.data.frame(datatable_clean$X4)
    Infected <- apply(Infected,2,function(x)gsub('\\s+', '',x))
    Infected <- as.data.frame(gsub("\\s*\\([^\\)]+\\)","",as.character(Infected)))
    Infected <- apply(Infected,2,function(x)gsub("[^0-9.-]", "", x))
    # Infected = apply(Infected,2,function(x)gsub('\\s+', '',as.character(x)))
    # Get days and remove non dates
    Day = datatable_clean$X1
    Day = Day[!is.na(as.Date(Day,format="%d-%m-%Y"))]
    Day = format(as.Date(Day, format="%d-%m-%Y"), format="%d-%m")
  }
  df <- data.frame(matrix(unlist(Day), nrow=length(Day), byrow=T))
  
  # Convert total to active (substract results from 14 days ago)
  recovery = 18
  for (i in seq(recovery, length(Infected))) {
    Infected[i] = as.numeric(Infected[i]) - as.numeric(Infected[i-recovery+1])
  }
  df["Infected"]=Infected
  colnames(df) <- c("Date", "Infected")
  # return df
  df
}
GetPoblation <- function(c) {
  if (identical(c, "Spain")) {
    47100396
  } else if (identical(c, "Germany")) {
    83149300 
  }
}

#country <- "Spain"
function(input, output) {
  # Infected plot
  output$Covid_plot <- renderPlot({
    country <- input$countryInput[1]
    dataset <- GetData(country)
    Infected <- as.numeric(dataset$Infected)
    DateLabels <- dataset$Date
    Day <- 1:(length(Infected))
    N <- GetPoblation(country)
    
    old <- par(mfrow = c(1, 2), xaxt="n")
    # First plot
    plot(Day, Infected, type ="b")
    axis(1, at=seq(1, length(Day), by=1), labels = FALSE)
    text(seq(1, length(Day), by=1)-0.3, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),  labels = DateLabels, srt = 90, pos = 1, xpd = TRUE)
    # Second plot
    plot(Day, Infected, log = "y")
    axis(1, at=seq(1, length(Day), by=1), labels = FALSE)
    if (country == "Germany") {
      text(seq(1, length(Day), by=1)-0.3, y=par()$usr[3]+1.5*(par()$usr[4]-par()$usr[3]),  labels = DateLabels, srt = 90, pos = 1, xpd = TRUE)
    } else if (country == "Spain") {
      text(seq(1, length(Day), by=1)-0.3, y=par()$usr[3]+0.2*(par()$usr[4]-par()$usr[3]),  labels = DateLabels, srt = 90, pos = 1, xpd = TRUE)
    }
    
    abline(lm(log10(Infected) ~ Day))
    title(paste("Total infections COVID-19", country, sep = " - "), outer = TRUE, line = -2)
  })
  # Predictions
  output$Covid_predictions <- renderPlot({
    country <- input$countryInput[1]
    model <- input$modelInput[1]
    old <- par(mfrow = c(1, 2))
    dataset <- GetData(country)
    Infected <- as.numeric(dataset$Infected)
    DateLabels <- dataset$Date
    Day <- 1:(length(Infected))
    N <- GetPoblation(country)
    if (model == "SIR") {
      # SIR Model
      SIR <- function(time, state, parameters) {
        par <- as.list(c(state, parameters))
        with(par, {
          dS <- -R0/N * I * S
          dI <- R0/N * I * S -  I
          dR <- I
          list(c(dS, dI, dR))
        })
      }
      init <- c(S = N-Infected[1], I = Infected[1], R = 0)
      RSS <- function(parameters) {
        names(parameters) <- c("R0")
        out <- ode(y = init, times = Day, func = SIR, parms = parameters)
        fit <- out[ , 3]
        sum((Infected - fit)^2)
      }
      
      Opt <- optim(c(1), RSS, method = "L-BFGS-B", lower = 0, upper = 5) # optimize with some sensible conditions
      Opt_par <- setNames(Opt$par, c("R0"))
      
      t <- 1:80 # time in days
      fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
      col <- 1:3 # colour
      
      matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
      matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
      ## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
      ## omitted from logarithmic plot
      
      points(Day, Infected)
      legend("bottomright", c("Susceptible", "Infected", "Recovered"), lty = 1, lwd = 2, col = col, inset = 0.05)
      title(paste("SIR model COVID-19", country, sep= " - "), outer = TRUE, line = -2)
      
      # Parameters table
      par(old)
      
      R0 <- setNames(Opt_par["R0"], "R0")
      max_infected <- floor(max(fit$I))
      severe_cases <- floor(input$severeCasesInput*max_infected/100)
      intensive_care_cases <- floor(input$intensiveCareInput*max_infected/100)
      deaths <- floor(input$fatalityRateInput*max_infected/100)
      
      # Calculate dates
      time_peak <- fit[fit$I == max(fit$I), "time", drop = FALSE]
      firstDay <- min(as.Date(DateLabels,  format="%d-%m"))
      peak_day <- as.Date(firstDay) + max(time_peak) - 1
      peak_day <- format(peak_day, "%d-%m-%Y")
      
      
      # Create table
      pars_table <- matrix(c(N, R0, peak_day, max_infected, severe_cases, intensive_care_cases, deaths), ncol=7)
      colnames(pars_table) <- c("Population", "R0", "Peak day", "Max. infected", "Severe cases", "Intensive care", "Deaths")
      output$Cases_table <- renderTable(pars_table)
      
    } else if (model == "SEIR") {
        SEIR <- function(time, state, parameters) {
          par <- as.list(c(state, parameters))
          with(par, {
            dS <- -R0/N * I * S
            dE <- R0*S*I/N - G0*E
            dI <- G0*E - I
            dR <- I
            list(c(dS, dE, dI, dR))
          })
        }
        init <- c(S = N-Infected[1], E = Infected[1], I = 0, R = 0)
        RSS <- function(parameters) {
          names(parameters) <- c("R0", "G0")
          out <- ode(y = init, times = Day, func = SEIR, parms = parameters)
          fit <- out[ , 3]
          sum((Infected - fit)^2)
        }
        
        Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(5, 5)) # optimize with some sensible conditions
        Opt_par <- setNames(Opt$par, c("R0", "G0"))
        
        t <- 1:80 # time in days
        fit <- data.frame(ode(y = init, times = t, func = SEIR, parms = Opt_par))
        col <- 1:4 # colour
        matplot(fit$time, fit[ , 2:5], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
        matplot(fit$time, fit[ , 2:5], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
        ## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
        ## omitted from logarithmic plot
        points(Day, Infected)
        legend("bottomright", c("Susceptible", "Exposed", "Infected", "Recovered"), lty = 1, lwd = 2, col = col, inset = 0.05)
        title(paste("SEIR model COVID-19", country, sep= " - "), outer = TRUE, line = -2)
        
        # Parameters table
        par(old)
        
        R0 <- setNames(Opt_par["R0"], "R0")
        G0 <- setNames(Opt_par["G0"], "G0")
        max_infected <- floor(max(fit$I))
        severe_cases <- floor(input$severeCasesInput*max_infected/100)
        intensive_care_cases <- floor(input$intensiveCareInput*max_infected/100)
        deaths <- floor(input$fatalityRateInput*max_infected/100)
        
        time_peak <- fit[fit$I == max(fit$I), "time", drop = FALSE]
        firstDay <- min(as.Date(DateLabels,  format="%d-%m"))
        peak_day <- as.Date(firstDay) + max(time_peak) - 1
        peak_day <- format(peak_day, "%d-%m-%Y")
        
        # Create table
        pars_table <- matrix(c(N, R0, G0, peak_day, max_infected, severe_cases, intensive_care_cases, deaths), ncol=8)
        colnames(pars_table) <- c("Population", "R0", "G0", "Peak day", "Max. infected", "Severe cases", "Intensive care", "Deaths")
        output$Cases_table <- renderTable(pars_table)
    }
    
  })
  
}