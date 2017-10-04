# netmathtools2

[![Build Status](https://travis-ci.org/McClellandLegge/netmathtools2.svg?branch=master)](https://travis-ci.org/McClellandLegge/netmathtools2)

## Installation:

Windows 7/8/10 only.

```R
devtools::install_github("McClellandLegge/netmathtools2")
```
Must also have Python 3+ installed and added to the system or user path.

## Example Workflow:

Must log into Nexus with Chrome! The package automatically uses these cookies.

```R

my_netid     <- "mkemp6"
handle       <- netmathtools2::composeNexusHandle(my_netid)
students     <- netmathtools2::getStudents(handle, my_netid)
student_prog <- netmathtools2::getStudentsProgress(handle, students)

```
