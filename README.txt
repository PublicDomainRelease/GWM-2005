README.TXT       


                 GWM-2005 - Version: 1.3.0 3/07/2011
                 Built from MODFLOW-LGR Version 1.2
       Three-dimensional finite-difference groundwater flow model
          With Local Grid Refinement (LGR) Capability and the
                 Groundwater Management (GWM) Process


NOTE: Any use of trade, product or firm names is for descriptive 
purposes only and does not imply endorsement by the U.S. Government.

This version of the GWM-2005 Process is packaged for personal computers 
using one of the Microsoft Windows operating systems.  An executable file
for personal computers is provided as well as the source code. The
executable file was created using the Intel Visual Fortran 9.1 and Microsoft
Visual Studio 2005 compilers.  The source code can be compiled to run on
other computers.

IMPORTANT: Users should review the file Summary_gwm2005.txt for a 
description of, and references for, this software. Users should also review 
the file release.txt, which describes changes that have been introduced 
into GWM-2005 with each official release; these changes may substantially 
affect users.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. RUNNING THE SOFTWARE
                         D. TESTING
                         E. COMPILING

A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

          gwm_2005.1_3_0.exe

The distribution file contains:

          Compiled DOS executable and source code for GWM-2005.
          GWM documentation and input instructions in PDF files.
          Test data sets.

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows you to specify the directory in which the files should
be restored. The following directory structure will be created in the 
directory that you specify:


   |
   |--GWM2005.1_3_0
   |    |--bin       ; compiled executable
   |    |--data      ; standard data sets used in verification tests
   |    |--doc       ; documentation files
   |    |--src       ; source code
   |    |--test-win  ; batch files to run verification tests


It is recommended that no user files are kept in the GWM2005.1_3_0 
directory structure.  If you do plan to put files in this directory, 
do so only by creating subdirectories.

Included in directory GWM2005.1_3_0\doc are Portable Document Format 
(PDF) files. A PDF file is readable and printable on various computer 
platforms using Acrobat Reader from Adobe. The Acrobat Reader is freely 
available from the following World Wide Web sites:
      http://www.adobe.com/
      http://www.shareware.com/


B. INSTALLING 

To make the executable version of the program accessible from any
directory, the directory containing the executable (GWM2005.1_3_0\bin)
should be included in the PATH environment variable. 

As an alternative, the executable file, gwm2005.exe, in the
GWM2005.1_3_0\bin directory can be copied into a directory already
included in the PATH environment variable.

On Windows NT systems, from the Start menu, select Settings and then
Control Panel. Double-click System and select the Environment tab.
To add a new user variable, enter "PATH" in the Variable field and 
enter

   %PATH%;C:\GWMpathname\GWM2005.1_3_0\bin

in the Value field, Where GWMpathname is the directory path you selected 
for installing GWM-2005.   Click Set and then click OK.  If a PATH user 
variable already is defined, click on it in the User Variables pane, 
add ";C:\GWMpathname\GWM2005.1_3_0\bin" to its definition in the Value 
field, and click OK. Initiate and use a new MS-DOS Command Prompt 
window after making this change.

On Windows 2000 or XP systems, from the Start menu, select Settings and 
then Control Panel. Double-click System and select the Advanced tab.  
Click on Environment Variables. If a PATH user variable already is 
defined, click on it in the User Variables pane, then click Edit. In 
the Edit User Variable window, add ";C:\GWMpathname\GWM2005.1_3_0\bin" 
to the end of the Variable Value (ensure that the current contents of 
the User Value are not deleted) and click OK. If a PATH user variable 
is not already defined, in the User variables pane of the Environment 
Variables window, click New. In the New User Variable window, define a 
new variable PATH as shown above. Click OK. Click OK in the Environment 
Variables window and again in the System Properties window. Initiate 
and use a new MS-DOS Command Prompt window.


C. RUNNING THE SOFTWARE

After the executable file in the GWM2005.1_3_0\bin directory is installed 
in a directory that is included in your PATH, the program is initiated
in a DOS Command-Prompt window using the commands:

          gwm2005 [Fname]

The optional Fname argument is the MODFLOW name file. If no argument is 
used, the user is prompted to enter the name file. If the name file 
ends in ".nam", then the file name can be specified without including 
".nam".  For example, if the name file is named abc.nam, then the 
simulation can be run by entering:

          gwm2005 abc

As an alternative, you can copy the executable (gwm2005.exe) into the 
directory in which your model input files are located (including all of 
the required GWM input files) and double-click on the executable. This 
will activate the program, which will prompt you for the name of the 
MODFLOW name file. 

The data arrays in GWM-2005, as with MODFLOW-2005, are dynamically 
allocated, so models are not limited by hard-coded array limits. 
However, it is best to have enough random-access memory (RAM) available 
to hold all of the required data. If there is less available RAM than 
this, the program will use virtual memory, but this slows computations 
significantly.


D. TESTING

Data files for twelve test problems are provided to confirm that 
GWM-2005 is correctly installed and running on the system.  
The tests also may be looked at as examples of how to use the program.  
The directory GWM2005.1_3_0\data contains the input data and expected 
results for the tests. The file 'README_data.txt' located in the 
\data subdirectory describes the twelve test problems.


E. COMPILING

Although an executable version of the program is provided, the source 
code is available in the GWM2005.1_3_0\src directory so that the 
programs can be recompiled if necessary.  However, no support can be 
provided for users generating their own versions of the software. 

In general, the requirements for compiling GWM-2005 are a Fortran 
compiler, a C compiler, and the knowledge of using the compilers.  

The Fortran source-code files (files ending in .f, .for .inc, or .com) 
must be compiled with a Fortran (90 or 95) compiler.  To compile GWM-2005 
with the GMG Package, the source-code files written in the C language 
(files ending in .c or .h) must be compiled with a C compiler and all 
the resulting object files must be linked together.  Alternatively, 
the GMG solver can be removed so that only a Fortran compiler
is required.  Follow instruction in the Nogmg.txt file in the  
GWM2005.1_3_0\src directory for removing GMG from MODFLOW.  If the calls to GMG 
subroutines are removed, the GMG Package will not be available when 
GWM-2005 is run.  For tips on compiling the mixed-language code of 
GWM-2005 with various compilers, follow the instructions for  
Suggestions on Compiling at the web page 
http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/
GWM-2005 in this distribution has been compiled using the Intel Fortran
Compiler Integration for Microsoft Visual Studio 2005, Version 
11.1.3471.2005.  The C source code was compiled with Microsoft Visual C++ 
2005.

It is important when compiling GWM-2005 to enforce default double precision 
for all REAL variables.  The default for most compilers is single precision.
This is needed to obtain adequate precision in perturbation calculations.  
For the executable file in the \bin directory, which was compiled with the 
Intel Fortran Compiler Integration for Microsoft Visual Studio 2005, this was 
accomplished using the default Release configuration, with the default REAL KIND 
set to 8 rather than 4, so that all real variables are treated as 
double precision.  This is done by clicking on Project>Properties to open the 
Property Pages.  Then go to Fortran/Data and change the Default Real Kind to 8.  
The distributed executable file also has fortran code optimization disabled.

GWM and MODFLOW2005 use FORTRAN 90 MODULE structures and USE statements to 
define and access variables between packages.  During compilation, it is 
important that modules that will be accessed by other packages are compiled 
first.  That is, the order of compilation is important.  Some 
compilers, such as Compaq Visual Fortran, allow multiple compilation 
passes so that modules not available on a first pass will be available 
on subsequent passes.  However, other compilers may have difficulty 
with this.

To facilitate one-pass compilation, two of the GWM packages are divided 
into two separate files: The GWM1BAS3 package consists of the GWM1BAS3 
and GWM1BAS3SUBS files and the GWM1RMS3 package consists of the 
GWM1RMS3MOD and GWM1RMS3 files. The files distributed with GWM should 
be compiled in the following order:
     first compile all MF2005 files then compile:
     GWM1BAS3
     GWM1RMS3MOD
     GWM1DCV3
     GWM1OBJ3
     GWM1DCC3
     GWM1HDC3
     GWM1STC3
     GWM1SMC3
     GWM1RMS3
     GWM1BAS3SUBS
     GWM2005


