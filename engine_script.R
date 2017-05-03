#!/usr/bin/env Rscript

######################################## LIBRARIES ##########################################################

# initializing 'install.load' library and installing if required
# it contains a cool function for package installing/loading
if(!'install.load' %in% rownames(installed.packages())) {
  install.packages('install.load')
  Sys.sleep(6)
}
library(install.load)

# we need to install the latest ggplot2 library from github
tryCatch(devtools::install_github('hadley/ggplot2'), message = function(e) e)

# calling all the other necessary libraries and installing if required
install_load('data.table',
             'functional',
             'qdap',
             'stringr',
             'ggplot2',
             'ggthemes',
             'plotly',
             'htmlwidgets',
             'Rmisc',
             'plyr',
             'dplyr')

######################################## FUNCTIONS ##########################################################

# creating "waiting for press enter" function
pause = function() {
  if (interactive()) {
    invisible(readline(prompt = "Press <Enter> to continue..."))
  }
  else {
    cat("Press <Enter> to continue...")
    invisible(readLines(file("stdin"), 1))
  }
}

######################################## ARGUMENTS ##########################################################

# taking arguments over
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop('At least input file should be selected and supplied.', call. = FALSE)
} else if (length(args) == 1 ) {
  wd = args[1]
}

######################################## SCRIPT ##########################################################
######################################## data munging ####################################################

# setting working directory
setwd(wd)

# reading input file
training <- fread('all_history.csv',
                  header = TRUE,
                  sep = ',',
                  showProgress = TRUE)
test <- fread('public_test.csv',
               header = TRUE,
               sep = ',',
               showProgress = TRUE)

# transforming session_id and commands_in_session columns
training[, ':=' (session_id = as.character(session_id),
                 commands_in_session = str_trim(clean(commands_in_session)))]
test[, ':=' (session_id = as.character(session_id),
             commands_in_session = str_trim(clean(commands_in_session)))]

# creating distinct_commands_in_session column
training[, distinct_commands_in_session := lapply(.SD,
                                                  function(i) paste(sort(unique(unlist(strsplit(i, split = '[[:space:]]+')))), collapse = ' ')),
                                                  .SDcols = 'commands_in_session',
                                                  by = session_id]
test[, distinct_commands_in_session := lapply(.SD,
                                              function(i) paste(sort(unique(unlist(strsplit(i, split = '[[:space:]]+')))), collapse = ' ')),
                                              .SDcols = 'commands_in_session',
                                              by = session_id]

# saving training and test
fwrite(training,
       file = 'training.RData',
       nThread = getDTthreads())
fwrite(test,
       file = 'test.RData',
       nThread = getDTthreads())

# let's take a look at training's basic statistics
tmp <- training
sample_dt <- sample_n(tmp, 10, replace = FALSE)
cat(sprintf(summary(tmp)))
# pause()
Sys.sleep(1.5)

tryCatch(cat(sprintf(str(tmp))), error = function(e) e)
# pause()
Sys.sleep(1.5)
View(sample_dt)
# pause()
Sys.sleep(1.5)
rm(list = c('tmp',
            'sample_dt'))

# creating empty comm_freq tables
training_comm_freq <- c()
test_comm_freq <- c()

# creating session-commands frequency tables
for (i in 1:training[, .N]) {
  if (training[i, nchar(commands_in_session)] > 0) {
    subtmp <- data.table(table(
              training[i, unlist(strsplit(commands_in_session, split = '[[:space:]]+'))]
              ))
    subtmp[, session_id := training[i, session_id]]
    training_comm_freq <- rbindlist(list(training_comm_freq, subtmp))
  }
}
for (i in 1:test[, .N]) {
  if (test[i, nchar(commands_in_session)] > 0) {
    subtmp <- data.table(table(
              test[i, unlist(strsplit(commands_in_session, split = '[[:space:]]+'))]
              ))
    subtmp[, session_id := test[i, session_id]]
    test_comm_freq <- rbindlist(list(test_comm_freq, subtmp))
  }
}

# adding leading "_" to each command name in comm_freq tables
training_comm_freq[, V1 := paste('_', V1, sep = '')]
test_comm_freq[, V1 := paste('_', V1, sep = '')]

# removing unnecessary objects
rm(list = c('i',
            'subtmp'))

# converting comm_freq tables from long to wide
training_comm_freq <- data.table::dcast(training_comm_freq, session_id ~ V1 , value.var = 'N')
test_comm_freq <- data.table::dcast(test_comm_freq, session_id ~ V1 , value.var = 'N')

# replacing NAs with 0 in comm_freq tables
training_comm_freq[is.na(training_comm_freq)] <- 0
test_comm_freq[is.na(test_comm_freq)] <- 0

# let's take a look at training_comm_freq's basic statistics
tmp <- training_comm_freq
sample_dt <- sample_n(tmp, 10, replace = FALSE)
cat(sprintf(summary(tmp)))
# pause()
Sys.sleep(1.5)
tryCatch(cat(sprintf(str(tmp))), error = function(e) e)
# pause()
Sys.sleep(1.5)
View(sample_dt)
# pause()
Sys.sleep(1.5)
rm(list = c('tmp',
            'sample_dt'))

# joining together the original and their corresponding comm_freq table into enriched files
setkey(training_comm_freq, session_id)
setkey(training, session_id)
enriched_training <- training_comm_freq[training]
setkey(test_comm_freq, session_id)
setkey(test, session_id)
enriched_test <- test_comm_freq[test]

# enriching tables with computed variables
enriched_training[, ':=' (response_variable = as.factor(0),
                          nr_of_all_commands = str_count(commands_in_session, '\\S+'),
                          nr_of_distinct_commands = str_count(distinct_commands_in_session, '\\S+'),
                          redundancy_rate_in_session = round(str_count(commands_in_session, '\\S+') / str_count(distinct_commands_in_session, '\\S+'), digits = 2))]
enriched_test[, ':=' (response_variable = as.factor(0),
                      nr_of_all_commands = str_count(commands_in_session, '\\S+'),
                      nr_of_distinct_commands = str_count(distinct_commands_in_session, '\\S+'),
                      redundancy_rate_in_session = round(str_count(commands_in_session, '\\S+') / str_count(distinct_commands_in_session, '\\S+'), digits = 2))]

# setting response_variable variable
enriched_training[username == 'Austin_White', response_variable := as.factor(1)]

# composing leading columns to enriched tables
training_residing_cols <- c('response_variable',
                            'date_of_session',
                            'session_id',
                            'username',
                            'commands_in_session',
                            'distinct_commands_in_session',
                            'nr_of_all_commands',
                            'nr_of_distinct_commands',
                            'redundancy_rate_in_session')
test_residing_cols <- c('session_id',
                        'commands_in_session',
                        'distinct_commands_in_session',
                        'redundancy_rate_in_session')

# collecting command occurence columns
training_command_cols <- sort(setdiff(names(enriched_training), training_residing_cols))
test_command_cols <- sort(setdiff(names(enriched_test), test_residing_cols))

# reordering columns of enriched tables
setcolorder(enriched_training, c(training_residing_cols, training_command_cols))
setcolorder(enriched_test, c(test_residing_cols, test_command_cols))

# replacing NAs in numeric columns
enriched_training[, c(training_command_cols, 'redundancy_rate_in_session') := lapply(.SD,
                                                                                     function(x) replace(x, which(is.na(x)), 0)),
                                                                                     .SDcols = c(training_command_cols, 'redundancy_rate_in_session')]
enriched_test[, c(test_command_cols, 'redundancy_rate_in_session') := lapply(.SD,
                                                                             function(x) replace(x, which(is.na(x)), 0)),
                                                                             .SDcols = c(test_command_cols, 'redundancy_rate_in_session')]

# saving enriched files
fwrite(enriched_training,
       file = 'enriched_training.RData',
       nThread = getDTthreads())
fwrite(enriched_test,
       file = 'enriched_test.RData',
       nThread = getDTthreads())

# let's take a look at enriched_training's basic statistics
tmp <- enriched_training
sample_dt <- sample_n(tmp, 10, replace = FALSE)
cat(sprintf(summary(tmp)))
# pause()
Sys.sleep(1.5)
tryCatch(cat(sprintf(str(tmp))), error = function(e) e)
# pause()
Sys.sleep(1.5)
View(sample_dt)
# pause()
Sys.sleep(1.5)

rm(list = c('tmp',
            'sample_dt'))

######################################## variable selection ##############################################

# creating histogram for number of issued commands within a session
p1 <- ggplotly( p = ggplot(enriched_training) +
                      geom_histogram(aes(x = log10(nr_of_all_commands + 1),
                                         fill = response_variable,
                                         col = response_variable),
                                     position = 'identity',
                                     #bins = 50,
                                     #binwidth = 0.1,
                                     breaks = seq(0, 4, by = 0.1),
                                     alpha = 0.5) +
                      ggtitle('Histogram') +
                      scale_x_discrete(name = 'log10(number of commands) within a session',
                                       limits=c('10', '100', '1000', '10000')) +
                      ylim(c(0, 4000)) +
                      ylab('number of sessions') )

# creating histogram for number of issued distinct commands within a session
p2 <- ggplotly( p = ggplot(enriched_training) +
                      geom_histogram(aes(x = log10(nr_of_distinct_commands + 1),
                                         fill = response_variable,
                                         col = response_variable),
                                     show.legend = FALSE,
                                     position = 'identity',
                                     #bins = 50,
                                     #binwidth = 0.1,
                                     breaks = seq(0, 4, by = 0.1),
                                     alpha = 0.5) +
                    #  xlab('log10(number of distinct commands) within a session') +
                      scale_x_discrete(name = 'log10(number of distinct commands) within a session',
                                       limits=c('10', '100', '1000', '10000')) +
                      ylim(c(0, 4000)) +
                      ylab('number of sessions') )

# putting in one grid
p1_p2 <- subplot(p1, p2, nrows = 2, titleX = TRUE, titleY = TRUE, heights = c(0.5, 0.5)) %>%
            layout(yaxis = list(domain = c(0, 0.4)), yaxis2 = list(domain = c(0.6, 1)))
print(p1_p2)
# pause()
Sys.sleep(1.5)

# saving p1_p2 chart
file_out = paste(getwd(), '/histogram-nr_of_commands.html', sep = '')
tryCatch(htmlwidgets::saveWidget(as_widget(p1_p2), file_out), warning = function(e) e)

# removing unnecessary objects
rm(list = c('training',
            'test',
            'file_out',
            'training_comm_freq',
            'test_comm_freq',
            'p1',
            'p2',
            'p1_p2'))

# let's see the exact numbers
minmax_commands_in_session <- copy(enriched_training[, .(min(nr_of_all_commands),
                                                         max(nr_of_all_commands),
                                                         min(nr_of_distinct_commands),
                                                         max(nr_of_distinct_commands)
                                                        ), by = response_variable])

colnames(minmax_commands_in_session) <- c('response_variable',
                                          'min_nr_of_issued_command_per_session',
                                          'max_nr_of_issued_command_per_session',
                                          'min_nr_of_issued_distinct_command_per_session',
                                          'max_nr_of_issued_distinct_command_per_session')
View(minmax_commands_in_session)
# pause()
Sys.sleep(1.5)

# a brief summary
cat(sprintf("Austin White did not issue empty SSH sessions, albeit other ones did.\nAustin White's longest session contained 233 issued commands although the other one's absolute record is 7770.\nThe maximum commands variability of Austin White within one session was almost half as many as the absolute maximum.\n"))
# pause()
Sys.sleep(1.5)

# removing unnecessary objects
rm(list = c('minmax_commands_in_session'))

# Let's see number of users and their activities in number
users_statistics <- copy(enriched_training[, .(.N,
                                               sum(nr_of_all_commands),
                                               round(mean(nr_of_all_commands))),
                                               by = .(username, response_variable)][order(username)])
setnames(users_statistics, 'N', 'sessions_by_user')
setnames(users_statistics, 'V2', 'commands_by_user')
setnames(users_statistics, 'V3', 'commands_per_session_by_user')
users_statistics[, username := as.factor(username)]
View(users_statistics)
# pause()
Sys.sleep(1.5)

# let's order the usernames by nr_of_sessions
users_statistics[, username := factor(username, levels = users_statistics[, username][order(sessions_by_user, commands_by_user)])]

# let's plot basic statistics of users
p1 <- ggplotly(ggplot(users_statistics) +
                 geom_bar(aes(x = username,
                              y = sessions_by_user,
                              fill = response_variable),
                          stat = 'identity') +
                 guides(fill = FALSE) +
                 coord_flip() +
                 theme(panel.border = element_blank(),
                       axis.text.x = element_text(face = 'bold', size = 10),
                       axis.text.y = element_text(size = 8)) +
                 xlab('') +
                 ylab('sessions by user') )

p2 <- ggplotly(ggplot(users_statistics) +
                   geom_bar(aes(x = username,
                                y = commands_by_user,
                                fill = response_variable),
                            stat = 'identity') +
                   guides(fill = FALSE) +
                   coord_flip() +
                   theme(panel.border = element_blank(),
                       axis.text.x = element_text(face = 'bold', size = 10),
                       axis.text.y = element_text(size = 8)) +
                   xlab('') +
                   ylab('commands by user') )

p3 <- ggplotly(ggplot(users_statistics) +
                 geom_bar(aes(x = username,
                              y = commands_per_session_by_user,
                              fill = response_variable),
                          stat = 'identity') +
                 guides(fill = FALSE) +
                 coord_flip() +
                 theme(panel.border = element_blank(),
                       axis.text.x = element_text(face = 'bold', size = 10),
                       axis.text.y = element_text(size = 8)) +
                 xlab('') +
                 ylab('commands per session by user') )

# putting in one grid
p1_p2_p3 <- subplot(p1, p2, p3, nrows = 1, titleX = TRUE, titleY = TRUE, widths = c(0.33, 0.33, 0.33)) %>%
  layout(xaxis = list(domain = c(0, 0.24)), xaxis2 = list(domain = c(0.38, 0.62)), xaxis3 = list(domain = c(0.76, 1)))
print(p1_p2_p3)
# pause()
Sys.sleep(1.5)

# saving p1_p2_p3 chart
file_out = paste(getwd(), '/user_statistics.html', sep = '')
tryCatch(htmlwidgets::saveWidget(as_widget(p1_p2_p3), file_out), warning = function(e) e)

# removing unnecessary objects
rm(list = c('p1',
            'file_out',
            'p2',
            'p3',
            'p1_p2_p3'))

# a brief summary
cat(sprintf("Austin White did his job relatively parsimonious way in terms of number of issued commands per session.\nI think 'nr_of_all_commands' & 'nr_of_distinct_commands' variables can be used as predictor.\n"))
# pause()
Sys.sleep(1.5)

# let's count usage of each command and order them by occurrence
training_command_cols_sum <- data.table(t(enriched_training[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), .SDcols = training_command_cols, by = response_variable]), keep.rownames = TRUE)[order(V1)]

# let's do some variable transformation
colnames(training_command_cols_sum) <- c('command', 'others', 'Austin_White')
training_command_cols_sum[, ':=' (others = as.numeric(others),
                                  Austin_White = as.numeric(Austin_White),
                                  occurrence_sum = as.numeric(others) + as.numeric(Austin_White),
                                  Austin_White_ratio = round(as.numeric(Austin_White) / (as.numeric(others) + as.numeric(Austin_White)), 4),
                                  others_ratio = 1 - round(as.numeric(Austin_White) / (as.numeric(others) + as.numeric(Austin_White)), 4)
                                  )]

# let's order the commands by occurrence_sum and then by Austin_White_ratio
training_command_cols_sum[, command := factor(command,
        levels = training_command_cols_sum[, command][order(training_command_cols_sum$Austin_White_ratio, training_command_cols_sum$occurrence_sum)])]

# let's convert from wide to long
training_command_cols_sum_long <- melt(training_command_cols_sum,
                                       id.var = 'command',
                                       measure.vars = c('others_ratio', 'Austin_White_ratio'))

# let's check whether there are variables generally used by Austin White and others not + vice versa
p1 <- ggplotly(ggplot(training_command_cols_sum_long) +
                geom_bar(aes(x = command,
                             y = value,
                             fill = variable),
                         stat = 'identity') +
                coord_flip() +
                theme(panel.border = element_blank(),
                      axis.text.x = element_text(face = 'bold', size = 10),
                      axis.text.y = element_text(size = 8)) +
                ylab('ratio'))

# plot p1
print(p1)
# pause()
Sys.sleep(1.5)

# saving p1 chart
file_out = paste(getwd(), '/all_commands_ratio.html', sep = '')
tryCatch(htmlwidgets::saveWidget(as_widget(p1), file_out), warning = function(e) e)

# a brief summary
cat(sprintf("There some commands which were mainly used by Austin White; and there were many which where mainly used by others.\nThese kind of variables can be used as predictors but there are so many of them.\nSo I have to sort further out.\n"))
# pause()
Sys.sleep(1.5)

# removing unnecessary objects
rm(list = c('p1',
            'file_out'))

# let's choose those variables which are generally (at least in 97 %) used by Austin White and others not + vice versa
training_command_cols_sum_2 <- copy(training_command_cols_sum)
training_command_cols_sum_2[, predictor := as.factor(0)]
training_command_cols_sum_2[Austin_White_ratio >= 0.97 | others_ratio >= 0.97 , predictor := as.factor(1)]

# a brief summary
cat(sprintf("Picking out those commands which were used by at least 97 percent by Austin White or the complementary set of sysadmins\n %s 'command-type' variable left as predictor. This is a huge number, so I have to go further. (I arbitrarily set the thresholds at 97 percentage levels.)\n", training_command_cols_sum_2[predictor == as.factor(1), .N]))
# pause()
Sys.sleep(1.5)

# dropping such predictor candidates which occurred less than 2 times
training_command_cols_sum_2[predictor == 1 & Austin_White_ratio >= 0.97 & occurrence_sum <= 1, predictor := as.factor(0)]
training_command_cols_sum_2[predictor == 1 & others_ratio >= 0.97 & occurrence_sum <= 1, predictor := as.factor(0)]

# a brief summary
cat(sprintf("Sorting out those commands which were issued only once %s 'command-type' predictor left.\nThe sorted out variables can be typos and/or may cause overfitting at the end.\n", training_command_cols_sum_2[predictor == 1, .N]))
# pause()
Sys.sleep(1.5)

# let's order the commands by occurrence_sum and then by Austin_White_ratio
training_command_cols_sum_2[, command := factor(command,
                                                levels = training_command_cols_sum_2[, command][order(training_command_cols_sum_2[, predictor],
                                                                                                      training_command_cols_sum_2[, Austin_White_ratio],
                                                                                                      training_command_cols_sum_2[, occurrence_sum])])]


# let's convert from wide to long
training_command_cols_sum_2_long <- melt(training_command_cols_sum_2[predictor == 1, ],
                                         id.var = 'command',
                                         measure.vars = c('others_ratio', 'Austin_White_ratio'))

# let's check whether there are variables generally used by Austin White and others not + vice versa
p1 <- ggplotly(ggplot(training_command_cols_sum_2_long) +
                 geom_bar(aes(x = command,
                              y = value,
                              fill = variable),
                          stat = 'identity') +
                 coord_flip() +
                 theme(panel.border = element_blank(),
                       axis.text.x = element_text(face = 'bold', size = 10),
                       axis.text.y = element_text(size = 8)) +
                 ylab('ratio'))

# plot p1
print(p1)
# pause()
Sys.sleep(1.5)

# saving p1 chart
file_out = paste(getwd(), '/predictor_commands_ratio.html', sep = '')
tryCatch(htmlwidgets::saveWidget(as_widget(p1), file_out), warning = function(e) e)

# removing unnecessary objects
rm(list = c('p1',
            'file_out',
            'users_statistics',
            'training_command_cols_sum',
            'training_command_cols_sum_long'))

# let's see the histogram of predictor command occurence
p1 <- ggplotly( ggplot(training_command_cols_sum_2[predictor == 1, ]) +
                  geom_histogram(aes(x = occurrence_sum),
                                 fill = 'blue',
                                 color = 'darkblue',
                                 position = 'identity',
                                 bins = 50,
                                 #binwidth = 0.1,
                                 alpha = I(0.7)) +
                  ggtitle('Histogram') +
                  scale_x_log10(name = 'occurence frequency of predictor commands',
                                breaks = c(10, 100, 1000, 10000, 50000))  )

# plot p1
print(p1)
# pause()
Sys.sleep(1.5)

# saving p1 chart
file_out = paste(getwd(), '/predictor_commands_histogram.html', sep = '')
tryCatch(htmlwidgets::saveWidget(as_widget(p1), file_out), warning = function(e) e)

# removing unnecessary objects
rm(list = c('p1',
            'file_out'))

# composing work tables and setting variable types
training_predictor_variables <- c(names(enriched_training[, 7:8]),
                         as.character(training_command_cols_sum_2[predictor == 1, command]) )
training_necessary_columns <- c(names(enriched_training[, c(1, 3:5), with = FALSE]))
tmp1 <- copy(enriched_training[, training_necessary_columns, with = FALSE])
tmp2 <- as.data.table(lapply(enriched_training[, training_predictor_variables, with = FALSE], factor))
reduced_enriched_training <- cbind(tmp1, tmp2)
test_necessary_columns <- c(names(enriched_test[, c(1:2), with = FALSE]), 'nr_of_all_commands', 'nr_of_distinct_commands')
test_predictor_variables <- setdiff(c(names(enriched_test[, c(4:ncol(enriched_test)), with = FALSE])),
                                    c(test_necessary_columns, 'redundancy_rate_in_session', 'response_variable'))
tmp1 <- copy(enriched_test[, test_necessary_columns, with = FALSE])
tmp2 <- as.data.table(lapply(enriched_test[, test_predictor_variables, with = FALSE], factor))
reduced_enriched_test <- cbind(tmp1, tmp2)
reduced_enriched_test[, ':=' (nr_of_all_commands = as.factor(nr_of_all_commands), nr_of_distinct_commands = as.factor(nr_of_all_commands))]

# saving work tables
fwrite(reduced_enriched_training,
       file = 'reduced_enriched_training.RData',
       nThread = getDTthreads())
fwrite(reduced_enriched_test,
       file = 'reduced_enriched_test.RData',
       nThread = getDTthreads())

# removing unnecessary objects
rm(list = c('training_predictor_variables',
            'training_necessary_columns',
            'test_predictor_variables',
            'test_necessary_columns',
            'training_command_cols',
            'test_command_cols',
            'training_residing_cols',
            'test_residing_cols',
            'tmp1',
            'tmp2',
            'enriched_training',
            'enriched_test',
            'training_command_cols_sum_2',
            'training_command_cols_sum_2_long'))

# splitting the data into train, test & validation set
set.seed(20140426)
dt <- copy(reduced_enriched_training[, ])
n <- nrow(dt)
idx_train <- sample(1:n, 0.7*n)
idx_test <- sample(setdiff(1:n, idx_train), 0.3*n)
d_train <- dt[idx_train, ]
d_test <- dt[idx_test, ]
d_valid <- reduced_enriched_test[, ]

# printing subset sizes
cat(sprintf('size of train subset: %s\n', dim(d_train)[1]))
# pause()
Sys.sleep(1.5)
cat(sprintf('size of test subset: %s\n', dim(d_test)[1]))
# pause()
Sys.sleep(1.5)
cat(sprintf('size of validation subset: %s\n', dim(d_valid)[1]))
# pause()
Sys.sleep(1.5)

######################################## model building ##############################################

# brief
cat(sprintf("I chose random forest decision tree for sysadmin detection and I used h2o since it is fast. easy & powerful.\nRandom forest is a swiss-army-knife method for classification. It means boostrapping data, building trees, aggregating\nwith random subset of variable at each split.\n"))
#pause()

# initializing 'h2o' library
install_load('h2o')
# pause()
Sys.sleep(1.5)

# computing optimal memory level for h2o (80 % of available RAM)
mem <- paste(as.character(round(as.numeric(system("awk '/Mem/ {print $2}' /proc/meminfo", intern=TRUE))[1]*0.8/1024/1024, 0)), 'g', sep = '')

# initialize h2o Java server (R connects via REST)
h2o.init(max_mem_size = mem, ## setting RAM size
         nthreads = -1)   ## setting thread numbers to maximum available
# pause()
Sys.sleep(1.5)

# uploading data to H2O
dh2o_train <- as.h2o(d_train)
dh2o_test <- as.h2o(d_test)
dh2o_valid <- as.h2o(d_valid)

# Identity the response and predictor columns
ycol <- names(d_train)[1]
xcols <- setdiff(names(d_train), ycol)

# model building
# I played around with several settings and these seemed good enough
model <- h2o.randomForest(x = xcols, # predictor variables
                          y = ycol, # target variable
                          seed = 20170426, # for reproducibility
                          training_frame = dh2o_train, # speaks for itself
                          model_id = 'SysAdminDetection', # being name of the model
                          nfolds = 5, # nr of folds for N-fold cross-validation
                          mtries = -1, # nr of variables randomly sampled as candidates at each split. sqrt(# of predictors)
                          ntrees = 100, # nr of trees
                          max_depth = 30, # maximum tree depth
                          ignore_const_cols = TRUE # Ignore constant columns
                         )

# Let's see how it performs.
cat(sprintf("Our goal is to maximize model accuracy but avoiding overfitting at the same time.\n"))
# pause()
Sys.sleep(1.5)

# model performance: Area Under the Curve results
# You can also see/check/examine the data on the h2o user interface: http://localhost:54321
# https://en.wikipedia.org/wiki/Receiver_operating_characteristic#Area_under_the_curve
AUC_train <- round(h2o.auc(h2o.performance(model, dh2o_train)), 5)
AUC_test <- round(h2o.auc(h2o.performance(model, dh2o_test)), 5)
cat(sprintf("Area Under the Curve results\n0.90-1.00 = excellent\n0.80-0.90 = good\n0.70-0.80 = fair\n0.60-0.70 = poor\n0.50-0.60 = fail\n"))
cat(sprintf("training dataset AUC: %s\n", AUC_train))
cat(sprintf("testing dataset AUC: %s\n", AUC_test))
cat(sprintf("Our AUC is good for the train and the test dataset. In addition they are close to each other which means little overfitting!\nAlthough the false positive rate is high.\n"))
# pause()
Sys.sleep(1.5)

# preparation for plotting ROC for training dataset
ROC_train <- copy(data.table(h2o.performance(model, dh2o_train)@metrics$thresholds_and_metric_scores[18:19]))
ROC_test  <- copy(data.table(h2o.performance(model, dh2o_test)@metrics$thresholds_and_metric_scores[18:19]))

# let's take a look at ROC curves
p1 <- ggplot(ROC_train) +
        geom_line(aes(x = fpr, y = tpr),
                  color = 'blue') +
        coord_fixed(ratio = 1) +
        xlab('False Positive Rate') +
        ylab('True Positive Rate') +
        ggtitle('Receiver Operating Characteristic of model on traning dataset') +
        theme_igray()
p2 <- ggplot(ROC_test) +
        geom_line(aes(x = fpr, y = tpr),
                  color = 'green') +
        coord_fixed(ratio = 1) +
        xlab('False Positive Rate') +
        ylab('True Positive Rate') +
        ggtitle('Receiver Operating Characteristic of model on test dataset') +
        theme_igray()
png('ROC.png')
print(multiplot(p1, p2, cols = 2))
dev.off()
# pause()
Sys.sleep(1.5)

# saving training and test ROC data
fwrite(ROC_train,
       file = 'ROC_train.RData',
       nThread = getDTthreads())
fwrite(ROC_test,
       file = 'ROC_test.RData',
       nThread = getDTthreads())

# Let see the confusion matrices
# You can also see/check/examine the data on the h2o user interface: http://localhost:54321
# https://en.wikipedia.org/wiki/Confusion_matrix
conf_matrix_train <- data.table(h2o.confusionMatrix(model, dh2o_train))
conf_matrix_test <- data.table(h2o.confusionMatrix(model, dh2o_test))
cat(sprintf("The number of missclassified records are below 10 percentage in each dataset\n"))
cat(sprintf("confusion matrix of train dataset\n"))
View(conf_matrix_train)
cat(sprintf("confusion matrix of test dataset\n"))
View(conf_matrix_test)

# saving training and test confusion matrices
fwrite(conf_matrix_train,
       file = 'conf_matrix_train.RData',
       nThread = getDTthreads())
fwrite(conf_matrix_test,
       file = 'conf_matrix_test.RData',
       nThread = getDTthreads())

# all metrics of the model
# You can also see/check/examine the data on the h2o user interface: http://localhost:54321
cat(sprintf("all metrics of the model for training dataset\n"))
print(h2o.performance(model, dh2o_train))
# pause()
Sys.sleep(1.5)
cat(sprintf("all metrics of the model for test dataset\n"))
print(h2o.performance(model, dh2o_test))
# pause()
Sys.sleep(1.5)

# saving model in binary
if (file.exists(paste(wd, 'SysAdminDetection', sep = ''))) {
  file.remove(paste(wd, 'SysAdminDetection', sep = ''))
} else {
  h2o.saveModel(model, path = getwd())
}

# saving model scoring in POJO
if (file.exists(paste(wd, 'SysAdminDetection.java', sep = ''))) {
  file.remove(paste(wd, 'SysAdminDetection.java', sep = ''))
  file.remove(paste(wd, 'h2o-genmodel.jar', sep = ''))
} else {
  h2o.download_pojo(model, path = getwd(), get_jar = TRUE)
}

# get fitted values of the validation dataset (public_test_scored.csv)
SysAdmin.fit = as.data.table(h2o.predict(object = model,
                                         newdata = dh2o_valid,
                                         exact_quantiles = TRUE))
result <- cbind(d_valid[, c(1:2)], SysAdmin.fit[, .(predict, p1)])
setnames(result, 'predict', 'Austin_White')
setnames(result, 'p1', 'p_Austin_White')
View(result)
# pause()
Sys.sleep(1.5)

# saving predicted result set
fwrite(result,
       file = 'public_test_scored.csv',
       nThread = getDTthreads())

# disconnecting from h2o
h2o.shutdown(prompt = FALSE)

# removing unnecessary objects
rm(list = c('conf_matrix_train',
            'conf_matrix_test',
            'ROC_train',
            'ROC_test',
            'p1',
            'p2',
            'AUC_train',
            'AUC_test',
            'model',
            'ycol',
            'xcols',
            'wd',
            'mem',
            'idx_test',
            'idx_train',
            'dh2o_train',
            'dh2o_test',
            'dh2o_valid'))
