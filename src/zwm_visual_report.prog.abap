*&---------------------------------------------------------------------*
*& Report ZWM_VISUAL_REPORT
*&---------------------------------------------------------------------*
*& SAP WM Visual Monitoring Dashboard
*& Version: 1.0
*& Compatible with: SAP ECC EHP7 - ABAP 7.40
*&---------------------------------------------------------------------*
*& Features:
*& - Storage bin monitoring with occupancy indicators
*& - Transfer Order tracking and KPIs
*& - Graphical movement simulation
*& - Workload analysis dashboard
*& - Statistical KPIs with time analysis
*&---------------------------------------------------------------------*
REPORT zwm_visual_report LINE-SIZE 1023.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE zwm_visual_report_top.  " Global definitions, types, constants
INCLUDE zwm_visual_report_cls.  " Local classes
INCLUDE zwm_visual_report_scr.  " Selection screen
INCLUDE zwm_visual_report_pbo.  " PBO modules
INCLUDE zwm_visual_report_pai.  " PAI modules
INCLUDE zwm_visual_report_frm.  " Subroutines and helper methods

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM at_selection_screen_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgnum-low.
  PERFORM f4_warehouse.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgtyp-low.
  PERFORM f4_storage_type.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgpla-low.
  PERFORM f4_storage_bin.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM start_of_selection.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM end_of_selection.
