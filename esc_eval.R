
library(tidyverse)
library(googlesheets4)

sheet_url <- "https://docs.google.com/spreadsheets/d/10a9IjubvdqqTLeghu8Vqmf2yquUzJ4PQvqvKaMO6IN8"
sheet_names <- sheet_names(sheet_url)

# ignore info and template sheet
sheets_to_read <- sheet_names[!sheet_names %in% c("info", "template")]

esc <- 
  data.frame(
    country = vector("character", 0),
    rank = vector("numeric", 0),
    player = vector("character", 0)
  )

for (i in sheets_to_read) {
  temp_sheet <- read_sheet(sheet_url, i)
  temp_sheet$player <- i
  esc <- rbind.data.frame(esc, temp_sheet)
}

# run data checks: all ranks provided?
ranks_ok <- 
  by(
    esc$rank, esc$player, function(x) all(sort(x) == 1:25)
)

if (!all(ranks_ok)) {
  stop(paste("Invalid ranks for player(s):", paste(names(ranks_ok[!ranks_ok]), collapse = ", ")))
}

# read final ranking
info <- read_sheet(sheet_url, "info")

if (all(sort(info$final_rank) == 1:25)) {
  stop("Invalid ranks in info!")
}

# add final rank to player ratings
esc <- 
  merge.data.frame(esc, info[c("country", "final_rank")], by = "country")

# calculate score - the lower, the better
esc$score <- abs(esc$rank - esc$final_rank)

# who has the lowest score?
esc %>% 
  group_by(player) %>% 
  summarise(total_score = sum(score)) %>% 
  arrange(total_score)

# plot predictions
esc %>% 
  ggplot(aes(x = final_rank, y = rank, colour = player)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(1, nrow(info), 5)) +
  scale_y_continuous(breaks = seq(1, nrow(info), 5)) +
  labs(x = "final rank", y = "predicted rank") +
  theme_classic() +
  theme(legend.position = "top")
