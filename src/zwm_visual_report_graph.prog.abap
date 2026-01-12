*&---------------------------------------------------------------------*
*& Report ZWM_VISUAL_REPORT_GRAPH
*&---------------------------------------------------------------------*
*& SAP WM Visual Monitoring Dashboard - Enhanced Graphical Version
*& Features:
*& - Modern HTML dashboard with KPI cards and charts
*& - Splitter layout (dashboard above, ALV below)
*& - Row coloring in ALV tables
*& - Totals and subtotals
*& - Hotspots for drill-down
*&
*& This report reuses classes from ZWM_VISUAL_REPORT and extends them
*& with enhanced graphical capabilities.
*&---------------------------------------------------------------------*
REPORT zwm_visual_report_graph.

*----------------------------------------------------------------------*
* Includes
*----------------------------------------------------------------------*
* Base includes from original report (reused)
INCLUDE zwm_visual_report_top.            " Base types and globals
INCLUDE zwm_visual_report_cls.            " Base classes

* Extended includes for graphical version
INCLUDE zwm_visual_report_graph_top.      " Extended types with LVC_T_SCOL
INCLUDE zwm_visual_report_graph_cls.      " Enhanced graphical classes
INCLUDE zwm_visual_report_graph_scr.      " Selection screen
INCLUDE zwm_visual_report_graph_pbo.      " PBO modules
INCLUDE zwm_visual_report_graph_pai.      " PAI modules
INCLUDE zwm_visual_report_graph_frm.      " Form routines

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Set default date range
  p_datto = sy-datum.
  p_datfr = sy-datum - 30.

*----------------------------------------------------------------------*
* At Selection Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  " Validate inputs
  IF p_datfr > p_datto.
    MESSAGE e001(00) WITH 'Date From cannot be greater than Date To'.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Initialize controller
  PERFORM initialize_controller.

  " Load data
  PERFORM load_data.

  " Call the main screen
  CALL SCREEN gc_dynnr_graph.
