#' ef_wc
#'
#' Epidemiological characteristics with cases
#'
#' @param x where denotes the numerical vector of the number of cases per unit time.
#' @param dc_threshold where is the threshold for the number of cases.
#' @param c_weeks is the consecutive week.
#' @param ef is the epifeature. The options are ds3, ds6, dsmed and ppw.
#'
#' @return This function returns a vector numeric.
#' @export
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @details The function and arguments were based on the work of Almeida et al. (2022) for a more detailed review. Almeida IF, Lana RM, Codeco CT. 2022. How heterogeneous is the dengue transmission profile in Brazil? A study in six Brazilian states. PLO Neglected Tropical Diseases.
#' @importFrom stats median
#' @export
#'
#' @examples ef_wc(x = c(8, 15, 20, 0, 0, 0, 0, 5, 9 ,12), ef = "freq", dc_threshold = 5, c_weeks = 3)
ef_wc <- function(x, dc_threshold, c_weeks, ef){

    if(ef == "freq"){

        # Initialize variable to count occurrences
        frequency <- 0

        # Find consecutive periods with five or more cases
        current_length <- 0
        for (cases in x) {

            if (cases >= dc_threshold) {
                current_length <- current_length + 1

                ####
                if(c_weeks == 1){
                    if (current_length == c_weeks) {
                        frequency <- frequency + 1
                    }
                } else{
                    if (current_length >= c_weeks) {
                        frequency <- frequency + 1
                    }
                }

            } else {
                current_length <- 0
            }



        }

        # Print the result
        print(frequency)

    } else if(ef == "median"){

        consecutive_lengths <- integer()  # Initialize vector to store lengths
        current_length <- 0
        for (cases in x) {
            if (cases >= dc_threshold) {
                current_length <- current_length + 1
            } else {
                if (current_length > 0) {
                    consecutive_lengths <- c(consecutive_lengths, current_length)
                    current_length <- 0
                }
            }
        }
        if (current_length > 0) {
            consecutive_lengths <- c(consecutive_lengths, current_length)
        }

        # Calculate the median length
        median_length <- median(consecutive_lengths, na.rm = TRUE)

        # Print the result
        print(median_length)

    } else if(ef == "max"){

        # Initialize variables
        max_consecutive_weeks <- 0
        current_consecutive_weeks <- 0

        # Loop through the data to find consecutive weeks with 5 or more cases
        for (cases in x) {
            if (cases >= dc_threshold) {
                current_consecutive_weeks <- current_consecutive_weeks + 1
                if (current_consecutive_weeks > max_consecutive_weeks) {
                    max_consecutive_weeks <- current_consecutive_weeks
                }
            } else {
                current_consecutive_weeks <- 0
            }

        }

        max_length <- max(max_consecutive_weeks, na.rm = TRUE)

        # Print the result
        print(max_length)
    } else{}
}
