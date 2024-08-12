
## Typical man-page installation by root on Linux

The man(1) command can vary significantly from OS to OS
but on most Linux variants you can install the Fortran
man-pages for the intrinsics by doing the following:

```bash
# giving the zip file a known full path for exposition
cp 3fortran.zip /tmp

# go to where you want to install the man-pages. 
# if root on Linux that would typically be
cd /share/user/man/man3

# create many *.3fortran.gz files in the man directory
unzip /tmp/3fortran.zip

# rebuild the man(1) DB(DataBase)
cd /share/user/man
mandb .

# list all the man-pages in section 3fortran
man 3fortran -k .
```
