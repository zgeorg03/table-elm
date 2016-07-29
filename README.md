Description
===========

This library implements a table with the following functionalities:

* **_Search_**

   User can filter data based on regular expressions. The columns are separated by *#{column number}#*. 
   For example, when you want to search in the third column you type : #3#{regular expression}


* **_Pagination_**
	
	 It defines which entries should appeared on the screen.The default number of entries a page can show is 10.
	 User can change this using the selection box. It also offers the ability to navigate through pages.


* **_Sort_**

   User can sort the table based on the column he clicks.
   Sorting is performed using a permutation table. The list doesn't change so the original sequence can be restored

*** 


Example
========

You can run the example using the following command: 
```bash
git clone https://github.com/zgeorg03/table-elm.git && cd table-elm && elm-reactor 
```
### Visit 
   [http://localhost:8000/src/TableExample.elm](http://localhost:8000/src/TableExample.elm)


*** 


Implementation
========

The library is divided into the following components: 

* **_Value_**
	
   Defines all the possible types used by the other modules.

* **_Cell_**

   Represents a single cell of the table. Its value can be hidden.

* **_Record_**
	
   Represents one entry in a table. It uses a list of _cells_ 


* **_Header_**

   Represents the header of the table. It controls the state of each column. State can be
	 ascending,descending or original( as given).

* **_Pagination_**

   Controls the view of the table.

* **_Search_**

   During initialization the list of data is provided. The user can filter the data using regular expressions.

* **_Table_**

   Implements the table connecting on all the above components together.

* **_Native/SafeRegex_**

   A clone of regex module provided by elm/core package, with a new function that returns
	 a _Result_ instead of a _Regex_. This is necessary because the user types custom regular expressions
	 and the regex module doesn't provide a way to avoid program crash. 

*** 


Further Work
============

1. Column should know the data type not the cell 
2. Separate data from table, as Evan Czaplicki did in  [evancz/elm-sortable-table](https://github.com/evancz/elm-sortable-table) package
