
                      GWM-2005 Sample Problems
                            April 2012     

Data files for 13 test problems are provided to confirm that 
GWM-2005 is correctly installed and running on the system.  
The tests also may be looked at as examples of how to use the program.  
The input data are organized by sample problem:

Test name        Description of test
------------     ----------------------------------------------------
DEWATER          DEWATER problem, linear formulation
DEWATER-LGR      DEWATER problem with local grid refinement
DEWATERMB        DEWATER problem, mixed-binary linear formulation
DEWATER-SV       DEWATER problem with state variables
DRAIN            DRAIN problem with state variables
MAXIMIN          MAXIMIN problem with state variables
SEAWATER         SEAWATER problem, nonlinear formulation
SEAWATER-LGR     SEAWATER problem with local grid refinement
SEAWATER-LGRSTA  SEAWATER problem with local grid refinement and
                  state variables
STORAGE          STORAGE problem with state variables
STREAMFLOW       STREAMFLOW problem with state variables
SUPPLY2          SUPPLY problem, nonlinear formulation
SUPPLY-3GRID     A second SUPPLY problem with local grid refinement

The DEWATER, DEWATERMB, and SEAWATER problems are described in Ahlfeld
and others (2005). The SUPPLY2 problem was initially the SUPPLY problem
as described in Ahlfeld and others (2005). However, experience with the
sample problem indicated that there could be numerical-stability
issues associated with its solution. Therefore, the problem was revised
and renamed SUPPLY2, as described in Ahlfeld and others (2009).

The DEWATER-LGR, SEAWATER-LGR, and SUPPLY3-GRID problems are described
in Ahlfeld and others (2009).

The DEWATER-SV, MAXIMIN, STORAGE, and STREAMFLOW problems are described
in Ahlfeld and others (2011).

The SEAWATER-LGRSTA problem is the SEAWATER-LGR problem reformulated
using state variables for some of the head constraints defined in the
original problem. The problem demonstrates an alternative way to 
formulate the problem and that state variables can be defined and
accessed across multiple grids.

The DRAIN sample problem is described in the document 'Drains.pdf,'
which is provided in the doc subdirectory.

The directory GWM2005.1_4_0\test-win can be used to conveniently run the
tests without destroying the original results in the GWM2005.1_4_0\data
subdirectories. The GWM2005.1_4_0\test-win directory contains batch (BAT) 
files to run the tests.  Each test can be run by entering the name of 
the test as a command in a DOS command-prompt window with the current 
directory being GWM2005.1_4_0\test-win or by double-clicking on the 
corresponding BAT file in Windows Explorer.  The output files that are 
created in GWM2005.1_4_0\test-win can then be compared to those in the
GWM2005.1_4_0\data subdirectories. Note that there are three batch files
provided for the DEWATER-LGR test problem, one each for the three levels
of horizontal grid refinement (1:1, 3:1, and 7:1). There are two batch 
files provided for the DRAIN sample problem.


References:

Ahlfeld, D.P., Baker, K.M., and Barlow, P.M., 2009, GWM-2005--A Groundwater-
Management Process for MODFLOW-2005 with Local Grid Refinement (LGR)
capability: U.S. Geological Survey Techniques and Methods, 6-A33, 65 p.

Ahlfeld, D.P., Barlow, P.M., and Baker, K.M., 2011, Documentation for the
State Variables Package for the Groundwater-Management Process of
MODFLOW-2005 (GWM-2005): U.S. Geological Survey Techniques and Methods, 
6-A36, 45 p.

Ahlfeld, D.P., Barlow, P.M., and Mulligan, A.E., 2005, GWM--A ground-water
management process for the U.S. Geological Survey modular ground-water
model (MODFLOW-2000): U.S. Geological Survey Open-File Report 2005-1072,
124 p.
