*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_SCR
*&---------------------------------------------------------------------*
*& Selection Screen for Graphical Version
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  " Warehouse selection
  SELECT-OPTIONS: s_lgnum FOR lagp-lgnum.
  SELECT-OPTIONS: s_lgtyp FOR lagp-lgtyp.
  SELECT-OPTIONS: s_lgpla FOR lagp-lgpla.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  " Material and Movement Type
  SELECT-OPTIONS: s_matnr FOR lqua-matnr.
  SELECT-OPTIONS: s_bwlvs FOR ltak-bwlvs.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
  " Date Range
  PARAMETERS: p_datfr TYPE sydatum OBLIGATORY,
              p_datto TYPE sydatum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
  " Initial View Selection
  PARAMETERS:
    rb_bins  RADIOBUTTON GROUP rb1 DEFAULT 'X',  " Storage Bins
    rb_to    RADIOBUTTON GROUP rb1,               " Transfer Orders
    rb_kpi   RADIOBUTTON GROUP rb1,               " Movement KPIs
    rb_stsum RADIOBUTTON GROUP rb1,               " Storage Type Summary
    rb_daily RADIOBUTTON GROUP rb1,               " Daily Statistics
    rb_users RADIOBUTTON GROUP rb1.               " User Workload
SELECTION-SCREEN END OF BLOCK b4.
