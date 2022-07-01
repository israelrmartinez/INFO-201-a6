# Who Is Representing You?

This is your second real data-programming task and it may be more
time-consuming than the previous exercises.  The good side of the
story is that it is a groupwork so you can split the tasks between
your groupmates, and save time in this way.

The central task here is to pull information over google API-s, and
manipulate the resulting json data into nice readable tables.  This involves reading
about the API documentation,
experimenting with the queries, and a lot of hassling with lists, data
frames to squeeze uncooperative
results into the desired form!  Unfortunately, this is a frequent
experience with APIs.  Data comes in a million different ways, the
API-s are fluid, and bad documentation is more of a rule
than exception.

Finally, you put the result in a rmarkdown file and
compile it to an html.

Have fun!

---

Your task is to display some information about elected officials,
representing **two US addresses**.  You are free to pick any address
as long as these are from a **different state**.  Even more, if you
change the address, your report should update accordingly when
knitted, potentially showing completely different officials if the new
address is from another state.

This is a perfect task for API-s.  You can get
all you need from
[google civic information API](https://developers.google.com/civic-information/)
that links addresses to elected officials, and can give you additional
information about the U.S. Representatives.

The repo also includes a few files that help you to get started:
* _index\_stub.rmd_: a stub for the report.  We recommend you to take
  this file as template and start filling it with your code and text.
  It also includes suggestions and example pieces of code.
* We recommend you to rename _index\_stub_ into _index_ as the final
  file should be labelled _index.html_.
* _process-data.R_: template stub file that should contain your main
  data analysis code.  You may take this too as the template for your
  actual code.
* _index.html_: an example how the result should look like
* _styles.css_: a proposed stylesheet (how to use this is visible from
  the .rmd file).
* For those who want to compile it outside RStudio, the set also
  includes _Makefile_.  (If you work in RStudio, it is not needed.)


## Before Starting:

### 1. Become familiar with the example:

See the included _index.html_ file.

### 2. Learn the API basics

See [google civic information
API](https://developers.google.com/civic-information/) documentation.


### 3. Get and store the API key

Google API requires a valid key, follow the instructions on
[google cloud support](https://support.google.com/cloud/answer/6158862).

Store the key on your computer:

* don't upload it to GitHub!
* don't put it in your code that will be uploaded to GitHub!

There are many ways to achieve this.  However, in order to make
grading easier, please do it like this:

1. create an R file _keys.R_ in your _Documents_ folder (the normal
   _Documents_ folder of your computer, not inside this project).  The file
   should contain a single line of R code:

```
google.key <- "123xyz"
```
   The file should not contain anything else.  Please use exactly the
   same names for the keys and the keyfile.

2. in your r-markdown file, in the first code cunk, you load this file
   as
```
source("~/Documents/keys.R")
```


### 4. Get familiar with API-s

I recommend to experiment first with the queries on the interactive
console, provided at google cloud.  In particular, you should manage
to:

* get the elected officials' info from Google's _Civic Information_.
  First read the relevant documentation for
  [Civic Information API reference](https://developers.google.com/civic-information/docs/v2/)
  and 
  [representatives by address](https://developers.google.com/civic-information/docs/v2/representatives/representativeInfoByAddress).
  Note that Civic Platform expects authentication to be provided as
  **key=_your-google-civic-key_** in the query. 
  In this task you only need to extract the full list of
  representatives by address.


## Starting the Assignment:

### 1. Short introduction

Write a short introduction (a few sentences) where you explain where
you get the following information.  Provide API links!  Use
bold/italics according to need.


### 2. Table of all elected officials for the first address

Write a function (in the _process-data.R_ file) that takes address as
an argument and outputs a
markdown table of representatives.  Note that the _GET_ request
provides two tables (embedded into the same json): _offices_, and
_officials_.  Offices are the corresponding position (say, President
of the US), officials are the persons currently at that position (here
Donald J. Trump).  Offices data includes reference for the
corresponding officials.  Note: there are more than one person for
certain offices, such as US Senate.

The final table should contain all elected officials from the Civic Information for
the address. The table should include

* name
* position
* party
* email (link)
* phone
* photo (note: the _style.css_ helps to ensure the photos are of
suitable size)

Ensure that missing information (such as missing phone numbers) are
displayed in an pleasant and informative way (such as _not available_
or '-') and not as `<NA>` or `!Error` or other kind of ugly code.


### 3. Table for the second address

Pick the other address (from another state), and use the function you
created above to create another similar table.  Chances are that
something will look wrong and you have to fix it in the function.


### 5. Does it look good?

* The tables should look human-readable and pleasant, and must not
  include leftovers of JSON, raw HTML, computer code etc.


## Grading

| Section (points)             |  Task                          | Points            |
| ---------------------------- | ------------------------------ | -----------------:|
| intro (5)                    | address                        | 1                 |
|  				               | API links     				    | 2                 |
| 				  		       | appropriate explanations included | 2                 |
| Table of Officials (55) | Names displayed, linked to website | 10
|                         | photos visible | 15
| 						| missing photos replaced by '-' or similar | 10
| 						| phone, party, position included | 5
| 						| missing phones etc replaced by '-' or similar | 10
| 						| emails are links   | 5
| Data processing (40)         | code clear, logical, efficient | 10
|                              | uses the same function for both addresses | 10
|                              | works with different address | 10
| 							 | API keys hidden, not uploaded | 5
| 							 | code organized into appropriate |
| 							 |   functions/files               | 5
