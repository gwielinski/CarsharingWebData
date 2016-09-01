intervalInSequence <- function(interval, seq){
  length(subset(seq, seq > interval["lowlimit"] & seq < interval["uplimit"])) > 0
}
