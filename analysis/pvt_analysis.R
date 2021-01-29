library("dplyr")
library("data.table")
library("utils")
library("stringr")
library("readr")
library("ggplot2")
library("reshape2")
library("gridExtra")
library(tidyverse)
library(mongolite)
library(corrplot)

rm(list=ls())

# the AB datasheet values
# Original
abDatasheet <- data.frame(si=c("SVT", "LVT", "ULVT"),
                          min=as.numeric(c(258, 350, 456)),
                          max=as.numeric(c(382, 485, 580)))
# 2020.04.22 datasheet
abDatasheet_sp11 <- data.frame(si=c("SVT", "LVT", "ULVT"),
                          min=as.numeric(c(261, 344, 445)),
                          max=as.numeric(c(385, 482, 576)))

corner <- function(u) {
  unlist(lapply(u, function(u) {
    waferLocs[which(waferLocs$Device == u), ]$Corner
  }))
}

corner <- function(u) {
  wafer = str_match(u, "(.*)_(.*\\d+)_(-*\\d+)_(-*\\d+)$")[,3];
  if (wafer == "01") { "TT" }
  else if (wafer == "03") { "TT" }
  else if (wafer == "04") { "TT" }
  else if (wafer == "05") { "FF" }
  else if (wafer == "06") { "FF" }
  else if (wafer == "07") { "FF" }
  else if (wafer == "08") { "FF" }
  else if (wafer == "09") { "FF" }
  else if (wafer == "10") { "FS" }
  else if (wafer == "11") { "SF" }
  else if (wafer == "12") { "SF" }
  else if (wafer == "13") { "SS" }
  else if (wafer == "14") { "SS" }
  else if (wafer == "15") { "SS" }
  else if (wafer == "16") { "SS" }
  else {  "unknown" }
}
corner <- Vectorize(corner)

xy <- function(u) {
  unlist(lapply(u, function(u) {
    c(x= waferLocs[which(waferLocs$Device == u), ]$X,y=waferLocs[which(waferLocs$Device == u), ]$Y)
  }))
}
wafer <- function(u) {
  unlist(lapply(u, function(u) {
    waferLocs[which(waferLocs$Device == u), ]$Wafer
  }))
}
lot <- function(u) {
  unlist(lapply(u, function(u) {
    waferLocs[which(waferLocs$Device == u), ]$Lot
  }))
}

pullParametricDataFromMongo <- function(parametricData) {
  findResult <- parametricData$find('{"$and" : [{"measurement.result" : {"$nin" : [0, 4095,2]}} ,
                                                {"measurement.value" :  {"$regex" : "pmro_[slu]v_begin", "$options" : ""}},
                                                {"unit" : { "$regex" : "T8R646", "$options" : ""}}
                                               ]}')
  adjustment1 <- 156.25/100
  adjustment2 <- 156.25/146.25
  
  # after this time, new program used
  newProgramTime <- strptime("2020-05-15 20:39:56", "%Y-%m-%d %H:%M:%S")
  
  
#  findResult$value <- findResult$measurement$result / adjustment
  # findResult$value <- ifelse(findResult$time >= newProgramTime, findResult$measurement$result / adjustment2, findResult$measurement$result / adjustment1)
  
#  findResult <- findResult  %>% rename("Unit" = unit)
  findResult$variable <- unlist(map(findResult$measurement$value, function (x) { 
    if (str_detect(x, "uv")) {as.factor("ULVT")} 
    else if (str_detect(x, "sv")) {as.factor("SVT")} 
    else if (str_detect(x, "lv")) {as.factor("LVT")}
    else if (str_detect(x, "ts")) {as.factor("TS")}}))
  tempDataQuery <- parametricData$find('{"$and" : [{"measurement.result" : {"$lt" : 50}} ,
                                                   {"measurement.result" : {"$gt" : 1}},
                                                   {"unit" : { "$regex" : "T8R646", "$options" : ""}},
                                                   {"measurement.value" :{ "$regex" : "pmro_ts_begin_nom", "$options" : ""}}
                                       ]}')
  # a 'data.table' can be configured with a fast lookup key
  tempSummarized <- tempDataQuery %>% group_by(unit) %>% summarize(temp = mean(measurement[["result"]]))
  tempTable <- data.table(tempSummarized)
  setkey(tempTable, unit)
  # for each unit of each measurement, annotate the temperature
  findResult$temp <- tempTable[.(findResult$unit)]$temp
  findResult
}

simpleBySkewPlot <- function (f) {
  p <- ggplot(f, aes(
    y = variable,
    x = measurement$result,
    color = corner(unit)
  )) + geom_point(alpha = 0.5, size = 2) +
    geom_errorbar(
      abDatasheet_sp11,
      inherit.aes = FALSE,
      mapping = aes(y = si, xmin = min, xmax = max,),
      width = .2
    ) +
    labs(title = "Ring Oscillator Measurements Against Datasheet Expectation") + xlab("Speed") + ylab("Device Type")
  print(p)
  
}

plotWaferMap <- function(f) {
  # Do wafer map
  expr1 <- "(-*\\d+)_(-*\\d+)$"
  xmin <- min(as.numeric(str_match(d$unit, expr1)[,2]))
  xmax <- max(as.numeric(str_match(d$unit, expr1)[,2]))
  ymin <- min(as.numeric(str_match(d$unit, expr1)[,3]))
  ymax <- max(as.numeric(str_match(d$unit, expr1)[,3]))
  
  p1 <- ggplot(f, 
               aes(as.numeric(str_match(unit, expr1)[,2]), as.numeric(str_match(unit, expr1)[,3]))) + 
    
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax),) +
    scale_x_continuous(breaks = NULL, labels = NULL, ) +
    scale_y_continuous(breaks = NULL, labels = NULL) +
    
    geom_tile(aes(fill = measurement$result)) +
    guides(fill = guide_legend(title = 'Legend')) + xlab("") + 
    facet_grid(cols = vars(str_match(d$unit, "(.*)_(.*\\d+)_(-*\\d+)_(-*\\d+)$")[,3]),rows = vars(variable)) + 
    ggtitle('Wafer Map of Ring Oscillator Performance') +
    scale_fill_gradientn(colors = rainbow(6)) + xlab("") + ylab("")
  print(p1)
}

plotAggregateWaferMap <- function(f) {
  ### now aggregate it, and show the average over each position
  ### challenge for the reader: Aggregate it over the average of the 'rank' of each die's performance within each wafer
  ## Can you show a nice 'ring' around the 
  #  p2_data <- d %>% group_by(x=as.numeric(str_match(unit, expr1)[,2]), y= as.numeric(str_match(unit, expr1)[,3]), variable) %>% summarize(v = mean(value))
  #  p2 <- ggplot(p2_data, 
  #               aes(x, y)) + 
  #    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax),) +
  #    scale_x_continuous(breaks = NULL, labels = NULL, ) +
  #    scale_y_continuous(breaks = NULL, labels = NULL) +
  
  #    geom_tile(aes(fill = v)) +
  #    guides(fill = guide_legend(title = 'Legend')) + xlab("") + 
  #    facet_grid(cols = vars(variable)) + 
  #    ggtitle('Wafer Map of Ring Oscillator Performance') +
  #    scale_fill_gradientn(colors = rainbow(6)) + xlab("") + ylab("")
  #  print(p2)
  
}

plotRoData <- function (f) {
  # Plot Data
  
  simpleBySkewPlot(f)
  plotWaferMap
  
  phist <- ggplot(d, aes(x=measurement$result, fill=variable, color=variable)) + 
    geom_histogram(position="identity", alpha=0.2) + 
    labs(title="RO Value Histogram(Lot T8R646 @ Sort)") + ylab("") + xlab("RO Performance") + 
    geom_errorbarh(abDatasheet_sp11, inherit.aes = FALSE,mapping = aes(y = as.numeric(si) * 10, xmin = min, xmax = max)) +
    facet_grid(cols = vars(as.factor(corner(unit))))
  
  print(phist)
  
  
  # compute sigma observed against datasheet expectation
  # abDatasheet$range <- abDatasheet$max - abDatasheet$min
  #abDatasheet$obsRange <-
  #  unlist(lapply(abDatasheet$si, function(c) {
  #    max(f[which(f$variable == c), ]$value) - min(f[which(f$variable == c), ]$value)
  #  }))
  # abDatasheet$obsRange / abDatasheet$range
  # abDatasheet$obSigma <- abDatasheet$obsRange / abDatasheet$range * 6
  # print(abDatasheet$obSigma)
  
}




computeVariation <- function (name, t) {
  
  roHash <- new.env(hash = TRUE, parent = emptyenv())
  toHash <- d %>% group_by(unit) %>% filter (variable == t) %>% summarize(v = mean(value))
  
  Vectorize(assign, vectorize.args=c("x","value"))(toHash$unit, toHash$v, roHash)
  
  
  die <- function (wafer, x, y) { paste(wafer,"_",x,"_",y,sep="")}
  
  unlist(lapply(name, function(n) {
  parse <- str_match(n, "(.*\\d+)_(-*\\d+)_(-*\\d+)$")
  wafer <- parse[2]
  x <- as.integer(parse[3])
  y <- as.integer(parse[4])
  thisVal <- get(n, roHash)
  toGet <- c(die(wafer,x-1,y), die(wafer,x,y-1), die(wafer,x+1,y), die(wafer,x,y+1),
             die(wafer,x-1,y-1), die(wafer,x+1,y-1), die(wafer,x+1,y+1), die(wafer,x-1,y+1), n)
  neighborGroup <- mget(toGet, roHash,
                        ifnotfound = thisVal)
  inferredEdgeRo = unlist(map(neighborGroup, function(x) { mean(c(x,thisVal)) }))
  result <- unlist((max(inferredEdgeRo) - min(inferredEdgeRo)) / thisVal )
  result
  }))
}

## connect to the database, read-only!
dburl <- "mongodb://BXD_SILICON_ro:0N4SiAyT7R4FrH8@d1fm1smon010.amr.corp.intel.com:7079,d2fm1smon010.amr.corp.intel.com:7079,d3fm1smon010.amr.corp.intel.com:7079/BXD_SILICON?ssl=true&replicaSet=mongo7079"

# sortdata2 also include all the 'pattern' test results
sortData2 <- mongo("sortData2", url=dburl, options = ssl_options(weak_cert_validation = T))

## get the parametric data, attaching a temperature to each reading
d <- pullParametricDataFromMongo(sortData2)

## plot the RO data, several ways
plotRoData(d)

## produce a temperature vs RO plot
tempplot <- ggplot(d, aes(x=as.numeric(temp), y=measurement$result, color=variable)) + geom_point()  +
  labs(title = "Temp Sensor Observation vs. RO Performance") + 
  xlab("PVT Temperature Sensor (untrimmed)") + ylab("RO Value")
print(tempplot)


## Show the temperature rise associated with running the tests
## First measure measurement at test 692, last test after that (typically, around 2000)
tempDataQuery <- sortData2$find('{"$and" : [{"measurement.result" : {"$lt" : 1000}} ,{"measurement.result" : {"$gt" : 0}}, {"measurement.value" :{ "$regex" : "Temp", "$options" : ""}}]}')
tempBeforeAndAfter <- tempDataQuery %>% group_by(unit, testNum) %>% summarize(t = mean(measurement[["result"]])) %>% dcast (unit  ~ (testNum > 700), fun.aggregate=mean) %>% rename(after = "TRUE", before = "FALSE")
tempBeforeAndAfterPlot <- ggplot(tempBeforeAndAfter, aes(x=(after - before))) + geom_histogram(bins=20) + 
  labs(title="Temperature Rise During Test") + xlab("End - Start Temperature (C)")
print(tempBeforeAndAfterPlot)


## show a histogram of the relative variation of RO
uniqueU <- unique(d$unit)
variation <- rbind(data.table(type="SVT", u = uniqueU, v = computeVariation(uniqueU, "SVT")),
                   data.table(type="LVT", u = uniqueU, v = computeVariation(uniqueU, "LVT")),
                   data.table(type="ULVT", u = uniqueU,v = computeVariation(uniqueU, "ULVT")))

intradie <- ggplot(variation, aes(x=v)) + geom_histogram(alpha=0.3, position="identity") + 
  xlab("Relative RO Variation") + labs(title = "Inferred Lower Bound on Intra-Die Variability") + facet_wrap (~type)
print(intradie)


## now, correlate with pass/fail
## Bins 1,2,3 are 'passing', > 3 failing (at least for this analysis)
pfhash <- new.env(hash = TRUE, parent = emptyenv())
failingDie <- sortData2$find('{ "hard_bin" : { "$gt" : 3 } }', fields = '{ "_id" : "true"}')
passingDie <- sortData2$find('{ "hard_bin" : { "$lt" : 4 } }', fields = '{ "_id" : "true"}')
for(x in failingDie[,]) { assign(x, factor("FAIL", levels=c("PASS","FAIL")), pfhash)}
for(x in passingDie[,]) { assign(x, factor("PASS", levels=c("PASS","FAIL")), pfhash)}
pf <- function(u) {
  Vectorize(get, vectorize.args = c("x"))(u, pfhash)
}
pf_ro <- ggplot(variation, aes(x=v, color=pf(u))) + geom_histogram(alpha=0.3, position="identity") + facet_wrap(~type) + 
  xlab("Relative RO Variation") + labs(title = "Inferred Lower Bound on Intra-Die Variability")
ggplot(d, aes(x=value, fill=pf(unit), color=pf(unit))) + geom_histogram(aes(y= ..density..), position="identity", alpha=0.2) + 
  labs(title="RO Performance on Passing and Failing Parts") + scale_y_continuous(breaks = NULL) + xlab("RO Performance") + ylab("") +
  facet_wrap(~variable, scales = "free") 
print(pf_ro)

## Do some mistrack analsis
# for each part, the fraction of the midpoint performance at each transistor flavor
# Assumption! Expected mean of process is midpoint of datasheet extrema values
mt <- d %>% group_by(unit) %>% summarize(ULVT = mean(measurement$result[variable == "ULVT"]) / mean(c(445,576)) , LVT = mean(measurement$result[variable == "LVT"]) / mean(c(344,482)), SVT = mean(measurement$result[variable == "SVT"]) / mean(c(261,385))) 
# plot ULVT vs. SVT performance against process midpoint
ggplot(mt, aes(x=ULVT, y=SVT)) + geom_point()
# histogram mistrack for ULVT vs SVT and LVT vs SVT perofrmance relative to process midpoint
mistrack <- mt %>% group_by(unit) %>% summarize(ulvt_svt_mt = (ULVT-SVT), lvt_svt_mt = (LVT-SVT)) 
mtplot <- ggplot(melt(mistrack, id.vars=c("unit")), aes(x=value, fill=variable)) + geom_histogram(position = "identity")
print(mtplot)

mistrack_lv <- mt %>% group_by(unit) %>% summarize(ulvt_svt_mt = (ULVT-SVT), lvt_svt_mt = (LVT-SVT)) 
mtplot <- ggplot(melt(mistrack, id.vars=c("unit")), aes(x=value, fill=variable)) + geom_histogram(position = "identity")
print(mtplot)

## Just for kicks... Show some analysis of specific failing tests
ro_unit <- d %>% group_by(unit) %>% summarize(ULVT = mean(value[variable == "ULVT"]), 
                                              LVT = mean(value[variable == "LVT"]), SVT = mean(value[variable == "SVT"]))
ro_dt <- data.table(ro_unit)
setkey(ro_dt, unit)

mt_table <- data.table(mistrack)
setkey(mt_table, unit)

testFails <- sortData2$find('{"testFlg" : 128}', fields = '{"testTxt": 0, "testFlg": 0, "testNum": 0, "environment": 0, "time" : 0, "_id" : 0}')


## summarize, by each failing test, the mistrack, the LVT and SVT RO values, and their standard deviation
## Also the count of the number of parts that saw a failure
xx <- testFails %>% group_by(vectName) %>% 
  summarize(c = length(unique(unit)), 
            lvt_mt = mean(mt_table[.(unit)]$lvt_svt_mt, na.rm = TRUE), ulvt_mt = mean(mt_table[.(unit)]$ulvt_svt_mt, na.rm = TRUE),
            lvt = mean(ro_dt[.(unit)]$LVT, na.rm=TRUE), lvt_sd = sd(ro_dt[.(unit)]$LVT, na.rm=TRUE),
            svt = mean(ro_dt[.(unit)]$SVT, na.rm=TRUE), svt_sd = sd(ro_dt[.(unit)]$SVT, na.rm=TRUE),)

## Now show correlation between SVT/LVT/ULVT/temp
# Show that there is a correlation between temperature and ULVT performance, but that it is insignificant
z <- merge(tempBeforeAndAfter, ro_dt)[,c("before", "ULVT", "LVT", "SVT")]
correlation <- cor(z)
testVal <- cor.mtest(z, conf.level =0.95 )
cp <- corrplot(correlation, p.mat=testVal$p, type = "upper", order = "hclust", 
                   tl.col = "black", tl.srt = 45)
print(cp)


## now, try to show a rank order 
ro_dt$wafer <- str_match(ro_dt$unit, expr1)[,2]
ro_dt$x <- str_match(ro_dt$unit, expr1)[,3]
ro_dt$y <- str_match(ro_dt$unit, expr1)[,4]
wafers <- unique(ro_dt$wafer)
a <- lapply(wafers, function(w) { 
  w1 <- ro_dt[which(wafer==w),]
  w1$r <- rank(w1$SVT)
  w1
})


## demonstrate some server side aggregation
# for every unit, get the LVT measurement result
stats <- sortData2$aggregate(
  '[{"$match" : {"unit" : {"$exists" : "true"}}},
  {"$unwind" : "$measurement.value"},
  {"$match" :{ "measurement.value" : "Process_LV  -1 <> Process_LV"}},
  {"$unwind" : "$measurement.result"},
  {"$group":{"_id":"$unit", "count": {"$sum":1}, "average":{"$avg":"$measurement.result"}}}]',
  options = '{"allowDiskUse":true}'
)

# for every wafer, average the LVT measurement result and give the standard deviation within the wafer
perWafer <- sortData2$aggregate(
  '[{"$match" : {"unit" : {"$exists" : "true"}}},
  {"$unwind" : "$measurement.value"},
  {"$match" :{ "measurement.value" : "Process_LV  -1 <> Process_LV"}},
  {"$unwind" : "$measurement.result"},
  {"$match" : { "measurement.result" : {"$nin" : [0, 4095]}}},
  {"$lookup" : {"from" : "sortData2", "localField" : "unit", "foreignField":"_id", "as" : "dieInfo"}},
  {"$unwind" : "$dieInfo"},
  {"$unwind" : "$dieInfo.wafer_id"},
  {"$group":{"_id":"$dieInfo.wafer_id", "count": {"$sum":1}, "average":{"$avg":"$measurement.result"}, "std":{"$stdDevSamp":"$measurement.result"}}}]',
  options = '{"allowDiskUse":true}'
)

#demonstrate two query structures
#there is an DB index on measurement.value, so the second is a much faster way to prune the set before doing any further computation

allTogether <- sortData2$aggregate(
  '[{"$match" : {"unit" : {"$exists" : "true"}}},
  {"$unwind" : "$measurement.value"},
  {"$match" :{ "measurement.value" : "Process_LV  -1 <> Process_LV"}},
  {"$unwind" : "$measurement.result"},
  {"$match" : { "measurement.result" : {"$nin" : [0, 4095]}}},
  {"$lookup" : {"from" : "sortData2", "localField" : "unit", "foreignField":"_id", "as" : "dieInfo"}},
  {"$group":{"_id":null, "count": {"$sum":1}, "average":{"$avg":"$measurement.result"}, "std":{"$stdDevSamp":"$measurement.result"}}}]',
  
  options = '{"allowDiskUse":true}'
)
allTogether <- sortData2$aggregate(
  '[
  {"$match" :{ "measurement.value" : "Process_LV  -1 <> Process_LV"}},
  {"$match" : {"unit" : {"$exists" : "true"}}},
  {"$unwind" : "$measurement.value"},
  {"$unwind" : "$measurement.result"},
  {"$match" : { "measurement.result" : {"$nin" : [0, 4095]}}},
  {"$lookup" : {"from" : "sortData2", "localField" : "unit", "foreignField":"_id", "as" : "dieInfo"}},
  {"$group":{"_id":null, "count": {"$sum":1}, "average":{"$avg":"$measurement.result"}, "std":{"$stdDevSamp":"$measurement.result"}}}]',
  
  options = '{"allowDiskUse":true}'
)

allTogether <- sortData2$aggregate(
  '[
  {"$match" :{ "measurement.value" : "Process_LV  -1 <> Process_LV"}},
  {"$match" : {"unit" : {"$exists" : "true"}}},
  {"$unwind" : "$measurement.value"},
  {"$unwind" : "$measurement.result"},
  {"$match" : { "measurement.result" : {"$nin" : [0, 4095]}}},
  {"$lookup" : {"from" : "sortData2", "localField" : "unit", "foreignField":"_id", "as" : "dieInfo"}},
  {"$unwind" : "$dieInfo"},
  {"$unwind" : "$dieInfo.wafer_id"},
  {"$unwind" : "$environment"},
  {"$unwind" : "$environment.insertion"},
  
  {"$match" : { "dieInfo.wafer_id" : {"$in" : ["JBAY_T4MK98_25"]}}}
  {"$group":{"_id":null, "count": {"$sum":1}, "average":{"$avg":"$measurement.result"}, "std":{"$stdDevSamp":"$measurement.result"}}}]',
  
  
  options = '{"allowDiskUse":true}'
)
  
  
 
## playing around with 'raw' DB
# find all the records  of SV PMRO's ops
q <- rawJbayB0Data$find(query = '{"$and" : [  
                                          {"TEST_TXT" : "pmro_sv_begin_nom_:pmro_sv@JTAG_TDO[1]"},
                                          { "rectype" : "Ptr" },
                                          { "RESULT" :  { "$ne" : 0}},
                                          { "RESULT" :  { "$ne" : 4095}} ] }', 
                       fields='{"RESULT" : true, "pir" : true, "wir" : true, "TEST_TXT" : true}')

# now, use an aggregation pipeline
# ugh -- dont have a pointer back to Mir, where the LOT_ID is...
qq <- rawJbayB0Data$aggregate(
  '[
  {"$match" :{ "$or" : [ {"TEST_TXT": "pmro_sv_begin_nom_:pmro_sv@JTAG_TDO[1]"},
                         {"TEST_TXT": "pmro_uv_begin_nom_:pmro_uv@JTAG_TDO[1]"},        
                         {"TEST_TXT": "pmro_lv_begin_nom_:pmro_lv@JTAG_TDO[1]"}
                         ]}},
  {"$match" :{ "RESULT" : {"$ne" : 0}}},
  {"$match" :{ "RESULT" : {"$ne" : 4095}}},
  
  {"$lookup" : {"from" : "rawJbayB0Data", "localField" : "wir", "foreignField":"_id", "as" : "wir"}},
  {"$unwind" : "$wir"},
  {"$lookup" : {"from" : "rawJbayB0Data", "localField" : "far", "foreignField":"_id", "as" : "far"}},
  {"$project" : {"_id" : 0, "RESULT" : 1, "TEST_TXT" : 1, "wir.WAFER_ID" : 1,"far.LOT_ID" : 1 }}
   ]'
)

wcorner <- function(wafer) {
  if (wafer == "01") { "TT" }
  else if (wafer == "03") { "TT" }
  else if (wafer == "04") { "TT" }
  else if (wafer == "05") { "FF" }
  else if (wafer == "06") { "FF" }
  else if (wafer == "07") { "FF" }
  else if (wafer == "08") { "FF" }
  else if (wafer == "09") { "FF" }
  else if (wafer == "10") { "FS" }
  else if (wafer == "11") { "SF" }
  else if (wafer == "12") { "SF" }
  else if (wafer == "13") { "SS" }
  else if (wafer == "14") { "SS" }
  else if (wafer == "15") { "SS" }
  else if (wafer == "16") { "SS" }
  else {  "unknown" }
}

phist <- ggplot(qq, aes(x=RESULT, fill=TEST_TXT, color=TEST_TXT)) + 
  geom_histogram(position="identity", alpha=0.2) + 
  labs(title="RO Value Histogram(Lot T8R646 @ Sort)") + ylab("") + xlab("RO Performance") + 
  geom_errorbarh(abDatasheet_sp11, inherit.aes = FALSE,mapping = aes(y = as.numeric(si) * 10, xmin = min, xmax = max)) +
  facet_grid(cols = vars(as.factor(wcorner(wir$WAFER_ID))))

print(phist)
