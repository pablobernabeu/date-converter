This minimal **`format_date()`** function ([see origin](https://twitter.com/SauChin_Chen/status/1187550520774279168)) converts variously formatted dates to computer format (YYYY-MM-DD). It takes a dataframe (e.g., from a spreadsheet) containing a 'DATE' and a 'date_format' column (among any others). The original date format ('dmy', 'ymd', 'ydm' or 'mdy') needs to be pre-entered, whether in a spreadsheet or in R. All else automatic.

```

# R function for converting variously formatted dates to computer format (YYYY-MM-DD).

format_date = function(dat){
  
  # Character format
  dat$DATE = as.character(dat$DATE)
  dat$date_format = as.character(dat$date_format)
  
  # Replace any special characters with hyphens
  dat$re_date = gsub("\\.|,|, |/|\\-|\"|\\s", "-", dat$DATE)
  
  # Extract year from dates formatted ymd, dmy, ydm, and mdy, respectively
  dat[dat$date_format=='ymd', 'year'] = sub('(^\\d+|\\w+|\\W+)-.*', replacement = '\\1', dat[dat$date_format=='ymd', 're_date'])
  dat[dat$date_format=='dmy', 'year'] = sub('.*-', '', dat[dat$date_format=='dmy', 're_date'])
  dat[dat$date_format=='ydm', 'year'] = sub('(^\\d+|\\w+|\\W+)-.*', replacement = '\\1', dat[dat$date_format=='ydm', 're_date'])
  dat[dat$date_format=='mdy', 'year'] = sub('.*-', '', dat[dat$date_format=='mdy', 're_date'])
  
  # Extract month from dates formatted ymd, dmy, ydm, and mdy, respectively
  dat[dat$date_format=='ymd', 'month'] = gsub('^[^-]*-([^-]+).*', '\\1', dat[dat$date_format=='ymd', 're_date'])
  dat[dat$date_format=='dmy', 'month'] = gsub('^[^-]*-([^-]+).*', '\\1', dat[dat$date_format=='dmy', 're_date'])
  dat[dat$date_format=='ydm', 'month'] = sub('.*-', '', dat[dat$date_format=='ydm', 're_date'])
  dat[dat$date_format=='mdy', 'month'] = sub('(^\\d+|\\w+|\\W+)-.*', replacement = '\\1', dat[dat$date_format=='mdy', 're_date'])
  
  # Change written months to numbers
  dat$month = gsub('^Jan$', '1', dat$month)
  dat$month = gsub('^January$', '1', dat$month)
  dat$month = gsub('^Feb$', '2', dat$month)
  dat$month = gsub('^February$', '2', dat$month)
  dat$month = gsub('^Mar$', '3', dat$month)
  dat$month = gsub('^March$', '3', dat$month)
  dat$month = gsub('^Apr$', '4', dat$month)
  dat$month = gsub('^April$', '4', dat$month)
  dat$month = gsub('^May$', '5', dat$month)
  dat$month = gsub('^June$', '6', dat$month)
  dat$month = gsub('^July$', '7', dat$month)
  dat$month = gsub('^Aug$', '8', dat$month)
  dat$month = gsub('^August$', '8', dat$month)
  dat$month = gsub('^Sept$', '9', dat$month)
  dat$month = gsub('^September$', '9', dat$month)
  dat$month = gsub('^Oct$', '10', dat$month)
  dat$month = gsub('^October$', '10', dat$month)
  dat$month = gsub('^Nov$', '11', dat$month)
  dat$month = gsub('^November$', '11', dat$month)
  dat$month = gsub('^Dec$', '12', dat$month)
  dat$month = gsub('^December$', '12', dat$month)
  
  # Extract day from dates formatted ymd, dmy, ydm, and mdy, respectively
  dat[dat$date_format=='ymd', 'day'] = sub('.*-', '', dat[dat$date_format=='ymd', 're_date'])
  dat[dat$date_format=='dmy', 'day'] = sub('(^\\d+|\\w+|\\W+)-.*', replacement = '\\1', dat[dat$date_format=='dmy', 're_date'])
  dat[dat$date_format=='ydm', 'day'] = gsub('^[^-]*-([^-]+).*', '\\1', dat[dat$date_format=='ydm', 're_date'])
  dat[dat$date_format=='mdy', 'day'] = gsub('^[^-]*-([^-]+).*', '\\1', dat[dat$date_format=='mdy', 're_date'])
  
  # Order parts as YYYY-MM-DD
  dat$formatted_date = paste0(dat$year, '-', dat$month, '-', dat$day)
  
  # Add new date column and save
  saved_data <<- dat[,c('DATE', 'date_format', 'formatted_date')]
  
  # Return
  message("Find new date column 'formatted_date' after your own variables in new dataframe called 'saved_data'")
  return(saved_data)
  
}

```


**Test example**

```

DATE = c('17.10.2019', '18.9.19', '2019-10.18', '19.10.17', '23/9/2019', '2019/23/9', '7 Nov 19', 
         'December 9, 2019', '2019 December 9', '10 Dec 2019')
test_data = data.frame(DATE)

# Manually enter date format (this is the only part that will need to be manually entered, in a spreadsheet or in R)
test_data$date_format = c('dmy','dmy','ymd','ymd','dmy','ydm','dmy','mdy','ymd','dmy')

# Function
format_date(test_data)
saved_data

```


Result

```

Find new date column 'formatted_date' after your own variables in new dataframe called 'saved_data'
               DATE date_format formatted_date
1        17.10.2019         dmy     2019-10-17
2           18.9.19         dmy        19-9-18
3        2019-10.18         ymd     2019-10-18
4          19.10.17         ymd       19-10-17
5         23/9/2019         dmy      2019-9-23
6         2019/23/9         ydm      2019-9-23
7          7 Nov 19         dmy        19-11-7
8  December 9, 2019         mdy      2019-12-9
9   2019 December 9         ymd      2019-12-9
10      10 Dec 2019         dmy     2019-12-10
               DATE date_format formatted_date
1        17.10.2019         dmy     2019-10-17
2           18.9.19         dmy        19-9-18
3        2019-10.18         ymd     2019-10-18
4          19.10.17         ymd       19-10-17
5         23/9/2019         dmy      2019-9-23
6         2019/23/9         ydm      2019-9-23
7          7 Nov 19         dmy        19-11-7
8  December 9, 2019         mdy      2019-12-9
9   2019 December 9         ymd      2019-12-9
10      10 Dec 2019         dmy     2019-12-10

```