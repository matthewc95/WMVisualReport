*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_FRM
*&---------------------------------------------------------------------*
*& Form Routines for Graphical Version
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_CONTROLLER
*&---------------------------------------------------------------------*
FORM initialize_controller.
  DATA: lo_ctrl_init TYPE REF TO lcl_controller_graph.

  lo_ctrl_init = lcl_controller_graph=>get_instance( ).

  lo_ctrl_init->initialize(
    it_lgnum     = s_lgnum[]
    it_lgtyp     = s_lgtyp[]
    it_lgpla     = s_lgpla[]
    it_matnr     = s_matnr[]
    it_bwlvs     = s_bwlvs[]
    iv_date_from = p_datfr
    iv_date_to   = p_datto ).

  " Set initial view based on radio button selection
  IF rb_bins = abap_true.
    lo_ctrl_init->set_view( 1 ).
  ELSEIF rb_to = abap_true.
    lo_ctrl_init->set_view( 2 ).
  ELSEIF rb_kpi = abap_true.
    lo_ctrl_init->set_view( 3 ).
  ELSEIF rb_stsum = abap_true.
    lo_ctrl_init->set_view( 4 ).
  ELSEIF rb_daily = abap_true.
    lo_ctrl_init->set_view( 5 ).
  ELSEIF rb_users = abap_true.
    lo_ctrl_init->set_view( 6 ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_DATA
*&---------------------------------------------------------------------*
FORM load_data.
  DATA: lo_ctrl_load TYPE REF TO lcl_controller_graph.

  lo_ctrl_load = lcl_controller_graph=>get_instance( ).
  lo_ctrl_load->load_all_data( ).
ENDFORM.
