findOrphans <- function(edgelist) {

  edgelist %>%
    group_by(from) %>%
    summarise(sum = sum(weight), .groups = "drop_last") %>%
    filter(sum == 0) %>%
    pull(from)

}
