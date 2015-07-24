windowSize = function(data) {
  data = arrange(data,raw_timestamp_part_1)
  new_window = data$new_window
  raw_timestamp = data$raw_timestamp_part_2
  row = 0
  time = 0
  window_sizes <- double(length=0)
  prev_timestamp = raw_timestamp[1]
  init_timestamp = raw_timestamp[1]
  for(i in 1:length(raw_timestamp)){
    if(new_window[i] == "yes"){
      time = time + (raw_timestamp[i] - init_timestamp)/1000000
      size_v = rep(time,length = i - row)
      row = i
      window_sizes = c(window_sizes,size_v)
      time=0
    }
    if(raw_timestamp[i] < prev_timestamp){
      time = time + (prev_timestamp - init_timestamp)/1000000
      init_timestamp = raw_timestamp[i]
    }
    prev_timestamp = raw_timestamp[i]
  }
  window_sizes
}