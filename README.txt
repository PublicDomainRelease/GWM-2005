README.TXT


                 MF2005-GWM - Version: 1.0 08/12/2007
                 Built from MODFLOW-2005 Version 1.3.01
       Three-dimensional finite-difference ground-water flow model
                                With
                  Ground-Water Management (GWM) Process


NOTE: Any use of trade, product or firm names is for descriptive 
purposes only and does not imply endorsement by the U.S. Government.

This version of MODFLOW-2005 with the GWM Process is packaged for personal 
computers using one of the Microsoft Windows operating systems.  An executable 
file for personal computers is provided as well as the source code.  
The executable file was created using the Intel Visual Fortran 9.1 and Microsoft
Visual Studio 2005 compilers.  The source code can be compiled to run on
other computers.


                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. RUNNING THE SOFTWARE
                         D. TESTING
                         E. COMPILING

A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

         mf2005_gwm.1_0.exe

The distribution file contains:

          Compiled DOS executable and source code for MF2005-GWM.
          GWM documentation and input instructions in pdf files.
          Test data sets.

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows you to specify the directory in which the files should
be restored. The following directory structure will be created in the 
directory that you specify:


   |
   |--mf2005_gwm.1_0
   |    |--bin       ; compiled executable
   |    |--data      ; standard data sets used in verification tests
   |    |--doc       ; documentation files
   |    |--src       ; source code
   |    |--test-win  ; batch files to run verification tests


It is recommended that no user files are kept in the mf2005_gwm.1_0 
directory structure.  If you do plan to put files in the mf2005_gwm.1_0 
directory structure, do so only by creating subdirectories.

Included in directory mf2005_gwm.1_0\doc is a Portable Document Format 
(PDF) file. The PDF file is readable and printable on various computer 
platforms using Acrobat Reader from Adobe. The Acrobat Reader is freely 
available from the following World Wide Web sites:
      http://www.adobe.com/
      http://www.shareware.com/
Also, included in mf2005_gwm.1_0\doc is a PDF file called "GWM Addendum" which 
contains information on modifications to GWM since the publication of the
primary documentation.

B. INSTALLING 

To make the executable versions of the programs accessible from any
directory, the directory containing the executable (mf2005_gwm.1_0\bin)
should be included in the PATH environment variable. 

As an alternative, the executable file, mf2005_gwm.exe, in the
mf2005_gwm.1_0\bin directory can be copied into a directory already
included in the PATH environment variable.

On Windows NT systems, from the Start menu, select Settings and then
Control Panel. Double-click System and select the Environment tab.
To add a new user variable, enter "PATH" in the Variable field and 
enter

   %PATH%;C:\GWMpathname\mf2005_gwm.1_0\bin

in the Value field, Where GWMpathname is the directory path you selected 
for installing MF2005-GWM.   Click Set and then click OK.  If a PATH user 
variable already is defined, click on it in the User Variables pane, 
add ";C:\GWMpathname\mf2005_gwm.1_0\bin" to its definition in the Value 
field, and click OK. Initiate and use a new MS-DOS Command Prompt 
window after making this change.

On Windows 2000 or XP systems, from the Start menu, select Settings and 
then Control Panel. Double-click System and select the Advanced tab.  
Click on Environment Variables. If a PATH user variable already is 
defined, click on it in the User Variables pane, then click Edit. In 
the Edit User Variable window, add ";C:\GWMpathname\mf2005_gwm.1_0\bin" 
to the end of the Variable Value(ensure that the current contents of 
the User Value are not deleted) and click OK. If a PATH user variable 
is not already defined, in the User variables pane of the Environment 
Variables window, click New. In the New User Variable window, define a 
new variable PATH as shown above. Click OK. Click OK in the Environment 
Variables window and again in the System Properties window. Initiate 
and use a new MS-DOS Command Prompt window.


C. RUNNING THE SOFTWARE

After the files in the mf2005_gwm.1_0\bin directory are installed in a
directory that is included in your PATH, the programs are initiated in
a DOS Command-Prompt window using the commands:

          mf2005_gwm [Fname]

The optional Fname argument is the MODFLOW name file. If no argument is 
used, the user is prompted to enter the name file. If the name file 
ends in ".nam", then the file name can be specified without including 
".nam".  For example, if the name file is named abc.nam, then the 
simulation can be run by entering:

          mf2005_gwm abc

As an alternative, you can copy the executable (mf2005_gwm.exe) into the 
directory in which your model input files are located (including all of 
the required GWM input files) and double-click on the executable. This 
will activate the program, which will prompt you for the name of the 
MODFLOW name file. 

The data arrays in MF2005-GWM, as with MODFLOW-2005, are dynamically 
allocated, so models are not limited by hard-coded array limits. 
However, it is best to have enough random-access memory (RAM) available 
to hold all of the required data. If there is less available RAM than 
this, the program will use virtual memory, but this slows computations 
significantly.

D. TESTING

Data files for four test problems are provided to confirm that 
MF2005_GWM is correctly installed and running on the system.  
The tests may also be looked at as examples of how to use the program.  
The directory mf2005_gwm.1_0\data contains the input data and expected 
results for the tests.

The directory mf2005_gwm.1_0\test-win can be used to conveniently run the
tests without destroying the original results in the mf2005_gwm.1_0\data
directory. The mf2005_gwm.1_0\test-win directory contains batch (BAT) 
files to run the tests.  Each test can be run by entering the name of 
the test as a command in a DOS command-prompt window with the current 
directory being mf2005_gwm.1_0\test-win or by double-clicking on the 
corresponding BAT file in Windows Explorer.  The output files that are 
created in mf2005_gwm.1_0\test-win can then be compared to those in 
mf2005_gwm.1_0\data.  

The four tests are described in the GWM documentation report (USGS 
Open-File Report 2005-1072).  The supply2 problem is a revised version 
of the original supply problem.  See the GWM Addendum for discussion 
of the revision and Documentation for MF2005_GWM for changes to published 
output.  All three of these documents are in directory mf2005_gwm.1_0/doc.
The four test problems are:


test name      description of test
------------   -------------------------------------------------------
 dewater       DEWATER sample problem, linear formulation
 dewatermb     DEWATER sample problem, mixed-binary linear formulation
 seawater      SEAWATER sample problem, nonlinear formulation
 supply2       SUPPLY sample problem, nonlinear formulation


E. COMPILING

Although an executable version of the program is provided, the source 
code is available in the mf2005_gwm.1_0\src directory so that the 
programs can be recompiled if necessary.  However, no support can be 
provided for users generating their own versions of the software. 

In general, the requirements for compiling MF2005-GWM are a Fortran 
compiler, a C compiler, and the knowledge of using the compilers.  

The Fortran source-code files (files ending in .f, .for .inc, or .com) must be 
compiled with a Fortran (90 or 95) compiler.  To compile MF2005-GWM with 
the GMG Package, the source-code files written in the C language (files 
ending in .c or .h) must be compiled with a C compiler and all the 
resulting object files must be linked together.  Alternatively, 
the GMG solver can be removed so that only a Fortran compiler
is required.  Follow instruction in the MF2005 1_3 release file Nogmg.txt 
in MF2005.1_3\src for removing GMG from MODFLOW.  If the calls to GMG 
subroutines are removed, the GMG Package will not be available when 
MF2005-GWM is run.  For tips on compiling the mixed-language code of 
MF2005-GWM with various compilers, follow the instructions for  
Suggestions on Compiling at the web page 
http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/
MF2005-GWM in this distribution has been compiled using the Intel Fortran
Compiler Integration for Microsoft Visual Studio 2005, Version 
9.1.3427.2005.  The C source code was compiled with Microsoft Visual C++ 
2005.

It is important when compiling MF2005-GWM to enforce default double precision 
for all REAL variables.  The default for most compilers is single precision.
This is needed to obtain adequate precision in perturbation calculations.  
For the executable file in the \bin directory, which was compiled with the 
Intel Fortran Compiler Integration for Microsoft Visual Studio 2005, this was 
accomplished using the default Release configuration, with the exception that 
the default REAL KIND is set to 8, so that all real variables are treated as 
double precision.  This is done by clicking on Project>Properties to open the 
Property Pages.  Then go to Fortran/Data and change the Default Real Kind to 8.

GWM and MODFLOW2005 use FORTRAN 90 MODULE structures and USE statements to 
define and access variables between packages.  During compilation, it is 
important that modules that will be accessed by other packages are compiled 
first.  That is, the order of compilation is important.  Some 
compilers, such as Compaq Visual Fortran, allow multiple compilation 
passes so that modules not available on a first pass will be available 
on subsequent passes.  However, other compilers may have difficulty 
with this.

To facilitate one-pass compilation, two of the GWM packages are divided 
into two separate files: The GWM1BAS1 package consists of the GWM1BAS1 
and GWM1BAS1SUBS files and the GWM1RMS1 package consists of the 
GWM1RMS1MOD and GWM1RMS1 files. The files distributed with GWM should 
be compiled in the following order:
     first compile all MF2005 files then compile:
     GWM1BAS2
     GWM1RMS2MOD
     GWM1DCV2
     GWM1OBJ2
     GWM1DCC2
     GWM1HDC2
     GWM1STC2
     GWM1SMC2
     GWM1RMS2
     GWM1BAS2SUBS
     MF2005_GWM

