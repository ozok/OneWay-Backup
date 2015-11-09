##OneWay Backup

OneWay Backup is a tool to create a mirror of folder A in folder B. It can run jobs in batch and can be run from command line.

##Features
----
* Scans for altered files and copies them only
* Jobs can be run in batches
* Can be called from console so scheduling a backup is very easy
* Can create mirrors (i.e. delete files/folders from folder B so it is identical to folder A)
* Easy to use interface

###Command line parameters
OneWay Backup can be called with command line parameters which makes it ideal for backup scheduling.
* /all: Run all jobs, both active and passive jobs
* /exit: Close the program when all jobs are done
* /shutdown: Shutdown the whole system when all jobs are done
* /methodX: Use comparing method x where is 0, 1, 2
   * /method0: Full file search: Reads whole content of files
   * /method1: Uses MD5 checksums for comparesion
   * /method2: Compare file sizes
   * Please note that, OneWay Backup will compare file modification dates before resorting the any of the methods above

###Licence
OneWay Backup is licenced with MIT licence.