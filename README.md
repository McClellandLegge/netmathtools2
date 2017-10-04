# netmathtools2

[![Build Status](https://travis-ci.org/McClellandLegge/netmathtools2.svg?branch=master)](https://travis-ci.org/McClellandLegge/netmathtools2)

## Installation:

Windows 7/8/10 only.

```R
devtools::install_github("McClellandLegge/netmathtools2")
```

Additionally:

* Must have Python 3.6.* installed and added to the path
    * Suggest installing to `C:/Python36` instead of the default 
* Must have downloaded and installed the [pywin32](https://sourceforge.net/projects/pywin32/) python module
    * The bitness of this installer must match that of the Python 3.6 install
   
## Example Workflow:

Must log into Nexus with Chrome! The package automatically uses these cookies.

```R

my_netid     <- "mkemp6"
handle       <- netmathtools2::composeNexusHandle(my_netid)
students     <- netmathtools2::getStudents(handle, my_netid)
student_prog <- netmathtools2::getStudentsProgress(handle, students)

```
