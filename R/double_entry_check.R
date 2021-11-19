#' Double data entry check
#'
#' This function takes two data frames that have identical column names, 
#' which should also have idenctical records, and returns a table of errors where 
#' the two tables don't match up.
#'
#' @param x,y Two \code{data.frame}s with identical column names
#' @param id Character vector of column(s) to join the two \code{data.frame}s
#' @return A \code{data.frame} with columns \code{id}, \code{field}, \code{x},
#' and \code{y}. The last two columns specify what the discrepancy is for each input
#' \code{data.frame}.
#' @examples
#' repository_A <- data.frame(subject = rep(LETTERS[1:5], 2), visit = rep(1:5, 2), case = 1:10,
#'                            specimen = c(rep("DNA", 2), rep("Plasma", 3),
#'                            rep("Serum", 3), rep("RNA", 2)),
#'                            aliquots = 1:10, stringsAsFactors = FALSE)
#'
#' repository_B <- data.frame(subject = c(rep(LETTERS[1:5], 2), rep(LETTERS[6], 2)),
#'                            visit = c(rep(1:5, 2), 6:7), case = 1:12,
#'                            specimen = c(rep("DNA", 2), rep("Plasma", 2), "RNA",
#'                                         rep("Serum", 2), rep("RNA", 2), rep("DNA", 3)),
#'                            aliquots = c(NA, 2:8, 12, 10, 11, 13), stringsAsFactors = FALSE)
#'
#' errors <- double_entry_check(repository_A, repository_B, id = c("subject", "visit", "case"))
#' @export
#' @import dplyr tidyr
double_entry_check <- function(x, y, id_arg) {


if (sum(!(names(x) %in% names(y))) > 0 | sum(!(names(y) %in%
                                                   names(x))) > 0) {
        stop("column names aren't identical")
    }
    # For multiple id columns
    if (length(id_arg) > 1) {
        id_x <- do.call(paste, c(x[id_arg], sep = "-"))
        x_id$id <- id_x
        id_y <- do.call(paste, c(y[id_arg], sep = "-"))
        y_id$id <- id_y
        id <- "id"
    }
    # For single id columns
    else {
        x_id <- x %>% mutate(id = !!sym(id_arg))
        y_id <- y %>% mutate(id = !!sym(id_arg))
    }

    # Get a tibble of all distinct IDs with the ID columns-values
    all_id <-
            bind_rows(
                x_id
                , y_id
            ) %>%
            select(id, matches(id_arg)) %>%
            distinct()

    # Check the id column matches between the dataframes
    if (sum(!(x_id$id %in% y_id$id)) > 0 | sum(!(y_id$id %in% x_id$id)) >
        0) {
        warning("Some ids don't match")


        # Get ids in x that are not in y
        in_x_not_y <- anti_join(x_id, y_id, by = "id") %>%
        select(-starts_with(id_arg)) %>%
        pivot_longer(
           , cols = -"id"
           , names_to = "field",
           , values_to = "x"
           , values_transform = list(x = as.character))

        # get ids in y that are not in x
        in_y_not_x <-
            anti_join(y_id, x_id, by = "id") %>%
            select(-starts_with(id_arg)) %>%
            pivot_longer(
           # , data = x
           , cols = -"id"
           , names_to = "field",
           , values_to = "y"
           , values_transform = list(y = as.character))
    }

    # Pivot data for matching values
    x_long <- pivot_longer(
        , data = x_id
        , cols = -"id"
        , names_to = "field",
        , values_to = "x"
        , values_transform = list(x = as.character))
    y_long <- pivot_longer(
        , data = y_id
        , cols = -"id"
        , names_to = "field",
        , values_to = "y"
        , values_transform = list(y = as.character))

    # Keep records where there are matching record ID and keep if values not matching
    error_on_match <- inner_join(x_long, y_long, by = c("id", "field")) %>% filter(x !=
                                                                     y | is.na(x) & !is.na(y) | !is.na(x) & is.na(y))

    # Combine mismatched values from matched ids with unmatched ids-values
    error_table <- bind_rows(error_on_match, in_y_not_x, in_x_not_y) %>%
        # Add the columns for the id columns back to the dataframe and remove concatenated version
        left_join(all_id, by = "id") %>%
        select(-id) %>%
        relocate(matches(id_arg))

    return(error_table)
}
