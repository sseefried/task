This manual is formatted to 100 characters wide.

# Commands

## task start
Usage:
  task start <category> <description> <key/value pairs...>

  Each key/value pair is of the form <key>:<value> where <key> and <value> are both JSON strings.

Flags:
  -t, --time <time>       start at time

## `task finish`


Usage:

~~~
  task finish
~~~

  Finish the current task, if there is one.
  
Flags:

~~~
  -t, --time <time>      finish at time as long as it is after start time and not in future.
~~~

## task modify 
Modifies a single entry if it wouldn't overlap with another one or finish in the future.

### Usage

~~~
task modify <flags>
~~~

### Flags

~~~
  --id <id>              Modify the task with id <id>
  -s, --start <start>    Modify start time to <start>
  -f, --finish <finish>  Modify finish time to <finish>
~~~

## `task delete`

### Usage

~~~
task delete <flags>
~~~

### Flags

~~~
  -- id <id>    Delete the task with id <id>
~~~


## `task query`

Allows you to query the database

### Usage

~~~
task query <flags...>
~~~

### Flags

~~~
  -f, --format        Format
                        %s       start time
                        %f       finish time
                        %c       category
                        %d       description
                        %k<key>  key/value pair for key <key>
                        %K       all key value pairs, comma seperated
                        
  --gt <time>         Show all entries greater than <time>. Combines with other flags.
  --ge <time>         Like --gt but "greater than or equal to"
  --lt <time>         Like --gt but "less than"
  --le <time>         Like --gt but "less than or equal to"
~~~

## `task export`

### Usage

~~~
task export <path>
~~~

### Flags

~~~
  --csv    Export as CSV. Incompatible with --json
  --json   Export as JSON. Incompatible with --csv
~~~

# Appendix

## Valid keys

Keys can be any valid identifier other than 'id', 'start', 'finish', 'category', 'description'. (See "Identifiers".)

## Identifiers

Acceptable characters for ids are JSON strings. JSON strings are
a) any Unicode character except '"' or '\' control character.
b) a '\' followed by any of
 - '"'. Quotation mark.
 - '\'. Backslash.
 - '/'. Forward slash.
 - 'b'. Backspace.
 - 'f'. Formfeed.
 - 'n'. Newline.
 - 'r'. Carriage return.
 - 't'. Horizontal tab
 - 'u' and 4 hexadecimal digits.
