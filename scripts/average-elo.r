#!/usr/bin/env Rscript

# Calculates the running average Elo ranking and the standard error of the mean
# as a confidence interval using an Elo tournament output file as a command line
# argument.
#
# Usage: ./average-elo.r --run <data file> --output <output file>
# Be sure to make this file as executable!
#
# Requires packages argparser and ggplot2 in order to run. Using the R REPL, run
#     install.package("argparser")
#     install.package("ggplot2")

### parse command line arguments
library(argparser, quietly=TRUE)
parser <- arg_parser("Average Elo Ranking Plot")
parser <- add_argument(parser, '--output', nargs=Inf, help="output file")
parser <- add_argument(parser, '--run', nargs=Inf, help="training data files")
args   <- parse_args(parser)

### load and process the melted data
load.zip.data <- function(file) {
	rounds  <- read.csv(unz(file, "rounds.csv"))
	players <- read.csv(unz(file, "players.csv"))
	rounds$AVG_RANKING <- ave(rounds$RANKING, rounds$PLAYER, FUN=cumsum) / ave(rounds$RANKING, rounds$PLAYER, FUN=seq_along)
	rounds$STD_RANKING <- ave((rounds$RANKING - rounds$AVG_RANKING)^2, rounds$PLAYER, FUN=cumsum) / (ave(rounds$RANKING, rounds$PLAYER, FUN=seq_along) - 1)
	rounds$SDE_RANKING <- rounds$STD_RANKING / sqrt(ave(rounds$RANKING, rounds$PLAYER, FUN=seq_along))
	rounds$PLAYER <- players$NAME[factor(rounds$PLAYER)]
	rounds$FILE <- file
	rounds
}

rounds     <- do.call(rbind, lapply(args$run, load.zip.data));
rounds.agg <- aggregate(cbind(AVG_RANKING,STD_RANKING,SDE_RANKING)~PLAYER+ROUND, data=rounds, FUN=mean)
rounds.agg

library(ggplot2)
#ggplot(mapping=aes(x=ROUND, y=AVG_RANKING, color=PLAYER, fill=PLAYER, group=interaction(PLAYER, FILE)), data=rounds) +
ggplot(mapping=aes(x=ROUND, y=AVG_RANKING, color=PLAYER, fill=PLAYER, group=interaction(PLAYER, FILE)), data=rounds) +
	geom_line(alpha=0.5) +
	geom_line(mapping=aes(x=ROUND, y=AVG_RANKING, group=PLAYER), data=rounds.agg) +
	geom_ribbon(mapping=aes(x=ROUND, ymin=AVG_RANKING-SDE_RANKING, ymax=AVG_RANKING+SDE_RANKING,  group=PLAYER), data=rounds.agg, linetype=0, alpha=0.3) +
	xlab("Rounds") +
	ylab("Average ELO Ranking") +
	labs(color="Player", fill="Player") +
	theme_bw()
ggsave(paste(args$output, "pdf", sep="."), device="pdf", width=9, height=4.5)
