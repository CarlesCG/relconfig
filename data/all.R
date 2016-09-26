# Load all datasets
load("./data/onefm.rda")
load("./data/twofm.rda")
load("./data/twofmTruncated.rda")
load("./data/treefm.rda")
load("./data/treefmtruncated.rda")

# treefm$failure_mode <- as.factor(treefm$failure_mode)
# save(list = "treefm", file = "./data/treefm.rda")
# treefm <- read.csv("./data/sample_df_3FM.csv")

# twofm <- read.csv(file = "./data/sample_df_2FM.csv")


# colnames(twofm)[3] <- 'time'
# save(list = "twofm", file = "./data/twofm.rda")
