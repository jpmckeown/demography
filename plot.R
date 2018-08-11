# plot AB heatmaps for N, P, etc
library(ggplot2)
library(viridis)

# I dont see how A and B columns from data are getting in plot?
ggplot(faithfuld, aes(waiting, eruptions)) +
	geom_raster(aes(fill = density))

# 0 to 1 grid, 20 July 2018
heatplot <- function(vardata, var, vartitle, ...){
	list(
		geom_tile(aes(fill = vardata), ...),
		ggtitle(vartitle),
		labs(x = "B", y = "A"),
		scale_x_continuous(breaks = seq(0,1,by=1/spacegrid), expand = c(0,0)),
		scale_y_continuous(breaks = seq(0,1,by=1/spacegrid), expand = c(0,0)),
		scale_fill_viridis(direction=-1, name='t', guide=guide_colourbar(barwidth=1, barheight=20)),
		coord_fixed()
	)
}

# broken by adding ggplot to top of list
# runs but all plots blank
gg_heatmap <- function(vardata, var, vartitle, ...){
	list(
		ggplot(data, aes(x=vB, y=vA), ...),
		geom_tile(aes(fill = vardata)),
		ggtitle(vartitle),
		labs(x = "B", y = "A"),
		scale_x_continuous(breaks = seq(0,1,by=1/spacegrid), expand = c(0,0)),
		scale_y_continuous(breaks = seq(0,1,by=1/spacegrid), expand = c(0,0)),
		scale_fill_viridis(direction=-1, name=var, guide=guide_colourbar(barwidth=1, barheight=20)),
		coord_fixed()
	)
}

# save plot to file
save_plot <- function(var){
	# avoid duplication, function to get path and filename bar suffix
	# no future reading. so can afford more readable filename
	# name of saved plot image
	filename <- paste(var, '_t', t.snapshot, '_W', W, '.png', sep='')
	path <- file.path(ppath, 'heat', filename)
	ggsave(path)
}

# draw plot
draw_plot <- function() {
	gg <- ggplot(data, aes(x=vB, y=vA)) + 
		heatplot(vardata, var, vartitle) 
	gg
}

# This works!
# separate heatmap for each variable column in data
# vars came from data colnames, and vardata is also flexible
# but vartitles is lookup from pre-made set of descriptive strings
for(i in seq_along(vars)) {
	var <- vars[i]
	vartitle <- vartitles[i]
	vardata <- data[[var]]
	print(str_c(var, vartitles[i], sep=' '))
	print(vardata)
	#gg_heatmap(vardata, var, vartitle)
	gg <- ggplot(data, aes(x=vB, y=vA)) + 
		heatplot(vardata, var, vartitle) 
	save_plot(var)
}

vartitles <- c(
	'Population size',
	'Natural increase',
	'Number acquiring trait by vertical transmission ',
	'Number acquiring trait by Social Learning',
	'Number acquiring trait by Individual Learning',
	'Number acquiring trait by any path',
	'Frequency of low fertility preference'
	)
# ggtitle(varname)
# this way around as B sideways less readable
# place axis tick at middle of box

# save a plot for each variable
# get list of variable columns from output file or means file
#var <- 'P'
#vartitle <- 'Frequency'
#vardata <- data$P
var <- 'log(P)'
vartitle <- 'log Population'
vardata <- log10(data$N)

vars <- get_var_names()

# functions?
# display plot now
# write plot as image

# loop through vars, and points in AB parameter space
# read from file containing min max mean
# plot as ribbon, g= ending timestep

DT1 <- ggplot(df2[1:g,], aes(x=t, y=mean)) +
	geom_line() +
	geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2) + 
	xlab('Generation (t)') +
	ylab(Yvar2) + 
	scale_x_continuous(breaks=-1:g-1, minor_breaks=-1:g-1) +
	theme_bw() +
	theme(axis.title.x=element_blank(), axis.text.x = element_text(size=12), axis.title.y=element_text(size=14), axis.text.y = element_text(size=12))

# ribbon, make ggplot object
## add Y label
make_ribbon_plot <- function(df){
	gg <- ggplot(df, aes(x=t, y=mean)) +
		geom_line() +
		geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2) + 
		theme_bw()
	return(gg)
}
ribbon_DT <- function(df){
	
	gg <- ggplot(df, aes(x=t, y=mean)) +
		geom_line() +
		geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2) + 
		theme_bw()
	return(gg)
}
read_ribbon_data <- function(A, B, var, ppath){
	file <- ABW_filename(A, B, W=1)
	path <- file.path(ppath, var, file)
	data <- read_tsv(path, col_names=TRUE)
	return(data)
}
test_plot <- function(A, B, var, ppath){
	data <- read_ribbon_data(A, B, var, ppath)
	gg1 <- make_ribbon_plot(data)
	gg1
}
test_folder_plots <- function(){
	filenames <- get_output_filenames()
	for(i in seq_along(filenames)){
		path <- file.path(ppath, filenames[i])
		data <- read_tsv(path, col_names=TRUE)
		filename <- paste(var, '_t', t.snapshot, '_W', W, '.png', sep='')
		path <- file.path(ppath, 'heat', filename)
		ggsave(path)
	}
}
test_plot(A=0.5, B=0.5, var='N', ppath="C:/Google/DT/3a/m5_g2_W1_P0.1_N100_t10")

path="C:/Google/DT/3a/m5_g2_W1_P0.1_N100_t10/mean/A1.0_B0.5_W1.tsv"
data <- read_tsv(path, col_names=TRUE)
data
