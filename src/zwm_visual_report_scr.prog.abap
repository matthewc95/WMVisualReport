*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_SCR
*&---------------------------------------------------------------------*
*& Selection Screen Definition
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Selection Screen - Main Block: Warehouse Selection
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS:
    s_lgnum FOR t300-lgnum OBLIGATORY,     " Warehouse Number
    s_lgtyp FOR t301-lgtyp,                 " Storage Type
    s_lgpla FOR lagp-lgpla.                 " Storage Bin
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Selection Screen - Date Range
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS:
    p_datfr TYPE sydatum DEFAULT sy-datum - 30, " Date From
    p_datto TYPE sydatum DEFAULT sy-datum.      " Date To
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Selection Screen - Material and Movement Filter
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
  SELECT-OPTIONS:
    s_matnr FOR lqua-matnr,                " Material Number
    s_bwlvs FOR ltap-bwlvs.                " Movement Type
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Selection Screen - Display Options
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
  PARAMETERS:
    rb_over RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND rb_sel, " Overview Dashboard
    rb_bins RADIOBUTTON GROUP rb1,           " Storage Bins
    rb_to   RADIOBUTTON GROUP rb1,           " Transfer Orders
    rb_kpi  RADIOBUTTON GROUP rb1,           " KPIs
    rb_sim  RADIOBUTTON GROUP rb1,           " Movement Simulation
    rb_work RADIOBUTTON GROUP rb1.           " Workload Analysis
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
* Selection Screen - Additional Options
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-b05.
  PARAMETERS:
    p_max   TYPE i DEFAULT 50000,            " Max records
    p_html  AS CHECKBOX DEFAULT ' ',         " Use HTML Dashboard
    p_test  AS CHECKBOX DEFAULT ' '.         " Generate test data
SELECTION-SCREEN END OF BLOCK b5.

*----------------------------------------------------------------------*
* Selection Texts (defined in program texts)
*----------------------------------------------------------------------*
* TEXT-B01: Warehouse Selection
* TEXT-B02: Date Range for Transfer Orders
* TEXT-B03: Material and Movement Filter
* TEXT-B04: Display Mode
* TEXT-B05: Additional Options
*
* Selection texts should be maintained in SE38:
* S_LGNUM: Warehouse Number
* S_LGTYP: Storage Type
* S_LGPLA: Storage Bin
* P_DATFR: Date From
* P_DATTO: Date To
* S_MATNR: Material
* S_BWLVS: Movement Type
* RB_OVER: Overview Dashboard
* RB_BINS: Storage Bins
* RB_TO: Transfer Orders
* RB_KPI: KPI Statistics
* RB_SIM: Movement Simulation
* RB_WORK: Workload Analysis
* P_MAX: Max Records
* P_HTML: HTML Dashboard Mode
* P_TEST: Generate Test Data
