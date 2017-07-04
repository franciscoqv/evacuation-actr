# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###############################################################################
###############################################################################
###############################################################################



# Returns the "Forced choice table". Basically, it returns the database.
get_fc_table <- function(directory) {
  fc_a <- import_fc_table(directory, "A")
  fc_b <- import_fc_table(directory, "B")

  fc_table <- rbind(fc_a, fc_b)

  return(fc_table)
}

# Returns a dataframe for a certain type of experiment (A or B)
import_fc_table <- function(directory, experiment) {

    directory <- paste(directory, experiment, "/", sep="")

    # Get all the subdirectories
    subjects <- list.dirs(path = directory, full.names = FALSE, recursive = FALSE)

    # Import first file
    fc_table <- import_one_fc_table(directory, subjects[1], experiment)

    # Remove the first one because it has been already accounted for.
    other_subjects <- subjects[-1]

    for(s in other_subjects) {
        one_fc_table <- import_one_fc_table(directory, s, experiment)

        fc_table <- fc_table %>%
            bind_rows(one_fc_table)
    }

    return(fc_table)
}

# Imports one specific Forced Choice file
import_one_fc_table <- function(directory, subject, experiment) {
    # Import the file
    one_fc_table <- read.table(paste(directory, subject, "/FC.txt", sep=""), sep="\t", header=TRUE)

    # Add the subject's number and the experiment type (A or B)
    one_fc_table$subject <- subject
    one_fc_table$experiment <- experiment

    return (one_fc_table)
}


# Cue values
get_cues <- function(experiment, door_name) {
    if(experiment=="A"){
        # fast=1 OR slow=-1 ,  safe=1 OR unsafe=-1
        switch( door_name,
                door004 = c(1, 1),
                door001 = c(1, -1),
                door005 = c(-1, 1),
                door003 = c(-1, -1),
                door000 = c(0, 0),
                door002 = c(0, 0),

                door010 = c(1, 1),
                door007 = c(1, -1),
                door011 = c(-1, 1),
                door009 = c(-1, -1),
                door006 = c(0, 0),
                door008 = c(0, 0),

                door016 = c(1, 1),
                door013 = c(1, -1),
                door017 = c(-1, 1),
                door015 = c(-1, -1),
                door012 = c(0, 0),
                door014 = c(0, 0),

                door018 = c(1, 1),
                door019 = c(1, -1),
                door020 = c(-1, 1),
                door021 = c(-1, -1),
                door022 = c(0, 0),
                door023 = c(0, 0),

                door024 = c(1, 1),
                door025 = c(1, -1),
                door026 = c(-1, 1),
                door027 = c(-1, -1),
                door028 = c(0, 0),
                door029 = c(0, 0),

                door030 = c(1, 1),
                door031 = c(1, -1),
                door032 = c(-1, 1),
                door033 = c(-1, -1),
                door034 = c(0, 0),
                door035 = c(0, 0))
    }
    else if(experiment == "B") {
        switch( door_name,
                door000 = c(1, 1),
                door001 = c(1, -1),
                door002 = c(-1, 1),
                door003 = c(-1, -1),
                door004 = c(0, 0),
                door005 = c(0, 0),

                door006 = c(1, 1),
                door007 = c(1, -1),
                door008 = c(-1, 1),
                door009 = c(-1, -1),
                door010 = c(0, 0),
                door011 = c(0, 0),

                door012 = c(1, 1),
                door013 = c(1, -1),
                door014 = c(-1, 1),
                door015 = c(-1, -1),
                door016 = c(0, 0),
                door017 = c(0, 0),

                door022 = c(1, 1),
                door019 = c(1, -1),
                door023 = c(-1, 1),
                door021 = c(-1, -1),
                door018 = c(0, 0),
                door020 = c(0, 0),

                door028 = c(1, 1),
                door025 = c(1, -1),
                door029 = c(-1, 1),
                door027 = c(-1, -1),
                door024 = c(0, 0),
                door026 = c(0, 0),

                door034 = c(1, 1),
                door031 = c(1, -1),
                door035 = c(-1, 1),
                door033 = c(-1, -1),
                door030 = c(0, 0),
                door032 = c(0, 0))
    }
}
