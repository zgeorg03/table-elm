Description
===========

This library implements a table with the following functionalities:

* **_Search_**

   User can filter data based on regular expressions. The columns are separated by *#{column number}#*. 
   For example, when you want to search in the third column you type : #3#{regular expression}


* **_Pagination_**
	
	 The default number of entries a page can show is 10. User can change this using the selection box.
   It also offers the ability to navigate through pages


* **_Sort_**

   User can sort the table based on the column he clicks.
   Sorting is performed using a permutation table.

*** 


Example
========

You can run the example using the following command: 
```bash
git clone https://github.com/zgeorg03/table-elm.git && cd table-elm && elm-reactor 
```
### Visit 
   http://localhost:8000/src/TableExample.elm

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

   Represents the header of the table

* **_Pagination_**

   Controls the view of the table.

* **_Search_**

   Filter the data

* **_Table_**

   Implements the table based on all the above components


Further Work
============

1. Column should know the data type not the cell 
