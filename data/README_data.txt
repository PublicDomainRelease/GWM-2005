
                      GWM-2005 Sample Problems
                           September 2013     

Data files for 14 test problems are provided to confirm that 
GWM-2005 is correctly installed and running on the system. The
tests also may be looked at as examples of how to use the program.  
The input data are organized by sample problem:

Test name        Description of test
------------     ----------------------------------------------------
DEWATER          DEWATER problem, linear formulation
DEWATER-LGR      DEWATER problem with local grid refinement
DEWATERMB        DEWATER problem, mixed-binary linear formulation
DEWATER-SV       DEWATER problem with state variables
DRAIN            DRAIN problem with state variables
MAXIMIN          MAXIMIN problem with state variables
MNWSUPPLY        MNW-Supply problem with multi-node well decision
                  variables
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
in Ahlfeld and others (2009). These sample problems were modified for
version 1.5.0 to be compatible with the Ghost Node method of MODFLOW-LGR
described in (Mehl and Hill, 2013). The previous grids used for the model
domains were regridded to eliminate the overlap in the parent and child 
grid that was used in the shared node method (LGR1). These modifications 
effectively change the size of the child grids.  For all problems except 
the SEAWATER-LGR problem, the child grid was reduced in size on all four 
boundaries. For example, the child 2 grid in the SUPPLY-3GRID problem was
reduced from 21 x 21 to 15 x 15 horizontal cells. For the SEAWATER-LGR 
problem, the child grid was reduced on all sides except the side nearest 
the constant-head boundary where the child grid was expanded to fully 
overlap with column 28 of the parent grid. This was necessary to allow 
incorporation of the gradient constraints that fall within the child grid.
Because all the child grids are regridded, the solution of the flow process 
is slightly different, resulting in minor changes in the optimal solution
from previous versions of the software.

The DEWATER-SV, MAXIMIN, STORAGE, and STREAMFLOW problems are described
in Ahlfeld and others (2011).

The SEAWATER-LGRSTA problem is the SEAWATER-LGR problem reformulated
using state variables for some of the head constraints defined in the
original problem. The problem demonstrates an alternative way to 
formulate the problem and that state variables can be defined and
accessed across multiple grids.

The DRAIN sample problem is described in the document 'Drains.pdf,'
which is provided in the doc subdirectory.

The MNWSUPPLY sample problem is described in Ahlfeld and Barlow (2013).

The 'test-win' subdirectory can be used to conveniently run the
tests without destroying the original results in the 'data'
subdirectories. The 'test-win' directory contains batch (BAT) files
to run the tests.  Each test can be run by entering the name of 
the test as a command in a DOS command-prompt window with the current 
directory being 'test-win' or by double-clicking on the corresponding
BAT file in Windows Explorer.  The output files that are created in
'test-win' can then be compared to those in the 'data' subdirectories.
Note that there are three batch files provided for the DEWATER-LGR 
test problem, one each for the three levels of horizontal grid refinement 
(1:1, 3:1, and 7:1). There are two batch files provided for the DRAIN 
sample problem.


References (PDFs of the GWM references can be found in the 'doc' sub-
directory):

Ahlfeld, D.P., Baker, K.M., and Barlow, P.M., 2009, GWM-2005--A Groundwater-
Management Process for MODFLOW-2005 with Local Grid Refinement (LGR)
capability: U.S. Geological Survey Techniques and Methods, 6-A33, 65 p.

Ahlfeld, D.P., and Barlow, P.M., 2013, Use of multi-node wells in the Ground-
water-Management Process of MODFLOW-2005 (GWM-2005): U.S. Geological Survey 
Techniques and Methods, 6-A47, 26 p.

Ahlfeld, D.P., Barlow, P.M., and Baker, K.M., 2011, Documentation for the
State Variables Package for the Groundwater-Management Process of
MODFLOW-2005 (GWM-2005): U.S. Geological Survey Techniques and Methods, 
6-A36, 45 p.

Ahlfeld, D.P., Barlow, P.M., and Mulligan, A.E., 2005, GWM--A ground-water
management process for the U.S. Geological Survey modular ground-water
model (MODFLOW-2000): U.S. Geological Survey Open-File Report 2005-1072,
124 p.

Mehl, S.W., and Hill, M.C., 2013, MODFLOW-LGR--Documentation of ghost node
local grid refinement (LGR2) for multiple areas and the boundary flow and 
head (BFH2) package: U.S. Geological Survey Techniques and Methods book 6,
chap. A44, 43 p.
