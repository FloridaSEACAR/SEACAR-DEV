#' Breaks function for Continuous and Discrete WQ Plots
#'
#' Function to determine x-axis breaks and labels.
#'
#' @param plot_data A dataframe containing data to be plotted, including Year and YearMonthDec
#' @param type "Discrete" or "Continuous"
#' @param ret type of return, either "break" to return a sequence of years or "lims" to return min and max year values
#' @return A sequence of breaks or an array of limits
#' @export
#'
breaks <- function(plot_data, type="Discrete", ret="break"){
  if(type=="Discrete"){
    #Determine max and min time (Year) for plot x-axis
    t_min <- min(plot_data$Year)
    t_max <- max(plot_data$YearMonthDec)
    t_max_brk <- as.integer(ceiling(t_max))
    t <- t_max-t_min

    # Sets break intervals based on the number of years spanned by data
    if(t>=30){
      brk <- -10
    }else if(t<30 & t>=10){
      brk <- -4
    }else if(t<10){
      brk <- -1
      if(t<5){
        # Ensure 5 years are included on axis
        total_ticks <- 5
        extra_years <- total_ticks - t
        # Always add 1 year before the first year
        years_before <- min(1, extra_years)
        years_after <- extra_years - years_before
        # Adjust min and max year, without going beyond current year
        t_min <- t_min - years_before
        t_max <- min(t_max + years_after, as.integer(format(Sys.Date(), "%Y")))
        # Re-check if we have enough years (in case t_max hit current year)
        t_min <- max(t_min, t_max - (total_ticks - 1))
        t_max_brk <- t_max
      }
    }
  }

  if(type=="Continuous"){
    #Determine max and min time (Year) for plot x-axis
    t_min <- min(plot_data$Year)
    t_max <- max(plot_data$YearMonthDec)
    t_max_brk <- as.integer(ceiling(t_max))
    t <- t_max-t_min

    # Creates break intervals for plots based on number of years of data
    if(t>=30){
      # Set breaks to every 10 years if more than 30 years of data
      brk <- -10
    }else if(t<30 & t>=10){
      # Set breaks to every 4 years if between 30 and 10 years of data
      brk <- -4
    }else if(t<10 & t>=4){
      # Set breaks to every 2 years if between 10 and 4 years of data
      brk <- -2
    }else if(t<4 & t>=2){
      # Set breaks to every year if between 4 and 2 years of data
      brk <- -1
    }else if(t<2){
      # Set breaks to every year if less than 2 years of data
      brk <- -1
      # Sets t_max to be 1 year greater and t_min to be 1 year lower
      # Forces graph to have at least 3 tick marks
      t_max <- t_max+1
      t_min <- t_min-1
    }
  }

  if(ret=="break"){
    return(seq(t_max_brk, t_min, brk))
  }

  if(ret=="lims"){
    return(c(t_min-0.25, t_max+0.25))
  }
}
