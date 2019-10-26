
sort_date = function(dat){

  # Character format
  dat$DATE = as.character(dat$DATE)
  dat$date_format = as.character(dat$date_format)

  # Replace dots and slashes with hyphens
  dat$re_date = gsub('\\.', '-', dat$DATE)
  dat$re_date = gsub('\\/', '-', dat$re_date)
  
  # Extract year from dates formatted ymd, dmy, and ydm, respectively
  dat[dat$date_format=='ymd', 'year'] = sub(pattern = '(^\\d+).*', replacement = '\\1', dat[dat$date_format=='ymd', 're_date'])
  dat[dat$date_format=='dmy', 'year'] = sub('.*-', '', dat[dat$date_format=='dmy', 're_date'])
  dat[dat$date_format=='ydm', 'year'] = sub(pattern = '(^\\d+).*', replacement = '\\1', dat[dat$date_format=='ydm', 're_date'])
  
  # Extract month from dates formatted ymd, dmy, and ydm, respectively
  dat[dat$date_format=='ymd', 'month'] = gsub('^[^-]*-([^-]+).*', '\\1', dat[dat$date_format=='ymd', 're_date'])
  dat[dat$date_format=='dmy', 'month'] = gsub('^[^-]*-([^-]+).*', '\\1', dat[dat$date_format=='dmy', 're_date'])
  dat[dat$date_format=='ydm', 'month'] = sub('.*-', '', dat[dat$date_format=='ydm', 're_date'])
  
  # Extract day from dates formatted ymd, dmy, and ydm, respectively
  dat[dat$date_format=='ymd', 'day'] = sub('.*-', '', dat[dat$date_format=='ymd', 're_date'])
  dat[dat$date_format=='dmy', 'day'] = sub(pattern = '(^\\d+).*', replacement = '\\1', dat[dat$date_format=='dmy', 're_date'])
  dat[dat$date_format=='ydm', 'day'] = gsub('^[^-]*-([^-]+).*', '\\1', dat[dat$date_format=='ydm', 're_date'])
  
  # Arrange final date_format for all dates
  dat$sorted_date = paste0(dat$year, '-', dat$month, '-', dat$day)
  
  # Return
  return(dat)
  
}




# Test example
DATE = c('17.10.2019','18.9.2019','2019-10.18','2019.10.17','23/9/2019','2019/23/9')
test_data = data.frame(DATE)

# Manually enter date format (this is the only part that will need to be manually entered, in principle)
test_data$date_format = c('dmy','dmy','ymd','ymd','dmy','ydm')

# Function
sort_date(test_data)
