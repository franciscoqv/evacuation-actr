# Doors' definitions
doors <- rbind(c(1,1), c(1,0), c(0,1), c(0,0))
rownames(doors) <- c("a", "b", "c", "d")
colnames(doors) <- c("cue1", "cue2")



analyze <- function() {
    # Load the dataset
    dataset <- load_dataset()

    subjects <- split(dataset, f=dataset$subject_id)

    
    # Density graph of response times per agent
    par(mfrow=c(2,2))
    draw_response_times(dataset, main_2="(every decision)")
    plot.new()
    draw_response_times(dataset, pressed_key="f", main_2="(only 'left')")
    draw_response_times(dataset, pressed_key="j", main_2="(only 'right')")


}

draw_response_times <- function(dataset, pressed_key="", main_2="") {
    plotset <- dataset

    # If pressed_key is not empty
    if(pressed_key!=""){
        plotset <- dataset[dataset$pressed_key==pressed_key, ]
    }

    mean_response_times <- tapply(plotset$response_time, plotset$subject_id, mean)
    graph_title <- paste("Density of response times", main_2)
    density_graph(mean_response_times, main=graph_title, xlab="Response time [s]", ylab="Density [%]")
}

# Draws a histogram but with percentages in the y-axis
density_graph <- function(data, ...) {
    h = hist(data, plot=FALSE)
    h$density = h$counts/sum(h$counts)*100
    plot(h, freq=FALSE, ...)
}

# Loads the dataset and prepares it for analysis
load_dataset <- function() {
    # Read the file
    dataset <- read.csv(file="outputs/experiment1.txt", header=FALSE, sep=",", stringsAsFactors=FALSE)

    # Change the column names
    colnames(dataset) <- c("subject_id", "left_option", "right_option", "pressed_key", "elapsed_time")

    # Add the correctness (boolean) column to the dataset
    dataset <- add_column(dataset, get_correctness_vector(dataset))

    # Add the response_time (seconds) column to the dataset
    dataset <- add_column(dataset, get_response_time_vector(dataset))

    # Change the column names
    colnames(dataset) <- c("subject_id", "left_option", "right_option", "pressed_key", "elapsed_time", "correct", "response_time")

    dataset
}

# Adds a column to a dataset
add_column <- function(dataset, column) {
    dataset[, ncol(dataset)+1] <- column
    dataset
}

# Returns the response time vector for a dataset with "elapsed_time" and "subject_id" columns
get_response_time_vector <- function(dataset) {

    # Subjects
    data_by_subject <- split(dataset, f=dataset$subject_id)

    # Applies the function for each subject
    response_time_list <- lapply(data_by_subject, get_response_time_vector_for_subject)

    # Puts all the data into one dimension
    unlist(response_time_list)
}

get_response_time_vector_for_subject <- function(subject_data) {

    #u <- data_by_subject[[1]]$elapsed_time
    u <- subject_data$elapsed_time

    # Calculate the response_time_vector for one subject
    # (Appends a zero at the beginning, eliminates the last row, then subtracts the vector)
    u - append(0, u, after=1)[1:length(u)]
}

# Returns the correctness vector for a dataset with options and decisions (pressed_key) columns
get_correctness_vector <- function(dataset) {
    # Per element: TRUE if the "left" option is optimal (e.g. because "a"<"b" is TRUE)
    optimal_solutions <- dataset$left_option < dataset$right_option 

    # Per element: TRUE if the user chose the "left" option
    decisions <- dataset$pressed_key == "f" 

    # When both the optimal solution and the decision coincide (not xor), then the answer was correct
    correctness_vector <- !xor(optimal_solutions, decisions)
}