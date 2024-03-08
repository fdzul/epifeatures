#' Epidemiological characteristics without cases
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
#'
#' @examples cf_freq(x = c(2, 0, 0, 0, 3, 5, 0, 0, 0, 0,2), dc_threshold = 0, c_weeks = 3, ef = "ds3")
ef_woc <- function(x, dc_threshold, c_weeks, ef){
    x <- tibble::tibble(values = unlist(rle(x)[2]),
                        lengths = unlist(rle(x)[1]))

    if(ef %in% c("ds3", "ds6")){
        x <- x |>
            dplyr::filter(values == dc_threshold) |>
            dplyr::filter(lengths >= c_weeks)
        length(x$values)
    } else if(ef == "dsmax"){
        x <- x |>
            dplyr::filter(values == dc_threshold) |>
            dplyr::filter(lengths >= c_weeks)
        max(x$lengths,  na.rm = TRUE)
    } else if(ef == "dsmed"){
        x <- x |>
            dplyr::filter(values == dc_threshold) |>
            dplyr::filter(lengths >= c_weeks)
        median(x$lengths, na.rm = TRUE)
    } else if(ef == "ppw"){
        sum(unique(x$values >= dc_threshold))/sum(x$lengths)
    }
}
