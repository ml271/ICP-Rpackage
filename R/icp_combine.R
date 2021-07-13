#'
#'
#' @title icp_combine
#' @description This function allows you to combine multiple icp files into one csv file and filter a specifc plot and for a  time range
#' @details  A ICP_Forst function: blah blah blah blubbb
#' @param path the path to dircetory containing icp files
#' @param file_type the file type for which the data should be combined. this varies with the data type. eg. ".lfm" for Litterfall-data
#' @param plot_id ICP Forest plot ID to be selected
#' @param start_year first year of which data should be combined
#' @param end_year last year of which data should be combined
#'
#' @return data.frame with the available icp date
#' @import dplyr
#' @keywords ICP
#' @examples icp_combine(path = "./data", plot_id = 856, start_year = 2010, end_year = 2020, file_type = ".lfm")
#' @export


# Version: 0.1
# Autor: Marvin Lorff
# Datum : 13.07.21


# TODO: get roxygen2 manual to work

# #testing the function
# path = paste0(getwd(), "/data")
# plot_id = 856
# start_year = 2010
# end_year = 2020
# file_type = ".lfm"

icp_combine <- function( path, plot_id, start_year, end_year, file_type){

    #get all .lm data filre paths in wd
    l.paths <- list.files(path = path , pattern= file_type, full.names = T)

    #import data form path list

    dat <- l.paths %>% purrr::map_df( ~ read.table(., header= T, sep= ";", dec= ".", na.strings = "NA"))


    #data manipulations

    dat1 <- dat %>% #rename(Sequence = "X.Sequence") %>%
                    mutate(date_start = dmy(date_start)) %>%
                    mutate(date_end = dmy(date_end)) %>%
                    mutate(start_date_analysis = lubridate::dmy(start_date_analysis)) %>%
                    mutate(end_date_analysis = lubridate::dmy(end_date_analysis))

    # filter data for plot and since year + chnage coulmn order
    dat2 <- dat1 %>%  filter(plot == plot_id) %>%
                      filter(year(date_start) >= start_year) %>%
                      filter(year(date_end) <= end_year) %>%
                      mutate(Sequence = seq(1:nrow(.))) %>%
                      relocate(other_observations, .after = last_col()) %>%
                      relocate(c(start_date_analysis ,end_date_analysis), .after = date_end) %>%
                      relocate(dry_weight_105, .after = dry_weight_70) %>%
                      relocate(pooled, .after = dry_mass)

    #export as csv file
    write.csv2(dat2, file= paste0("combine", file_type, "_id", plot_id, "_" ,start_year, "-", end_year,".csv"), quote= F, row.names = F)

    #return dataframe
    return(dat2)
}

#tmp <- icp_combine( path = path, plot_id = 856, start_year = 2010, end_year = 2020, file_type = ".lfm")
#plot( tmp$date_start , tmp$dry_weight_70, main= "Conventwald Buche", xlab= "Zeit", ylab = "Litterfall: Dry_weight_70" )
