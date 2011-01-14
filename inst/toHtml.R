# go.R
#------------------------------------------------------------------------------------------------------------------------
library (RUnit)
library (tools)
#------------------------------------------------------------------------------------------------------------------------
run = function (levels, trace=FALSE)
{
  if (0 %in% levels) {
    if (trace) print ('')
    else {
      #man.directory <<- '~/s/src/R/bioc/RCytoscape/man'
      man.directory <<- '~/s/src/R/bioc/rcy-devel/RCytoscape/man'
      setwd (man.directory)
      rd.files <<-list.files ('.', pattern='Rd')
      } # else
    } # 0

  if (1 %in% levels) {
    if (trace) print ('')
    else {
      for (file in rd.files) {
        target.dir <<- '~/s/src/R/bioc/rcy-devel/RCytoscape/inst/html'
        input <<- paste (man.directory, file, sep='/')
        basename = strsplit (file, '\\.Rd')[[1]][1]
        output <<- paste (target.dir, "/",  basename, '.html', sep='')
        print (sprintf ('%s -> %s', input, output))
        Rd2HTML (input, out=output)
        } # for file
      } # else
    } # 1

  if (2 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 2

  if (3 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 3

  if (4 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 4

  if (5 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 5

    if (6 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 6

  if (7 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 7

  if (8 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 8

  if (9 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 9

  if (10 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 10

  if (11 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 11

  if (12 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 12

  if (13 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 13

  if (14 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 14

  if (15 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 15

    if (16 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 16

  if (17 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 17

  if (18 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 18

  if (19 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 19

  if (20 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 20


} # run
#------------------------------------------------------------------------------------------------------------------------
#run (0)
#run (1)


